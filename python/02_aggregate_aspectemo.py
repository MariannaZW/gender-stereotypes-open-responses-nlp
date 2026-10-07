#!/usr/bin/env python3
"""Step 2: aggregate AspectEmo (CLARIN) output into one row per response.

Input: ZIP returned by CLARIN, with one JSON file per response
('{Comment ID}_{Task Name}.txt'); each JSON has a token-level 'labels' list in
BIO format (e.g. 'B-a_plus_m', 'I-a_plus_m', 'O').

Aspect labels are mapped to scores:
    a_minus_m -2 | a_minus_s -1 | a_zero 0 | a_plus_s +1 | a_plus_m +2 | a_amb 0
Only 'B-' labels (the first token of each aspect) are counted.

Outcomes:
    score_all_tokens   = sum of scores / number of ALL tokens in the text
    score_only_charged = sum of scores / number of labelled aspects
                         (NA if the text has no labelled aspect). Note that
                         the denominator includes a_zero and a_amb aspects.

The output (data/processed/aspectemo_scores.csv) is joined with the design and
participant-level variables by R/01_prepare_data.R, using comment_id.

Usage:
    python python/02_aggregate_aspectemo.py --results WYNIKI.zip [--corpus PATH]
"""

import argparse
import json
import zipfile
from collections import Counter
from pathlib import Path

import pandas as pd

from common import DEFAULT_CORPUS, PROCESSED_DIR, ensure_dirs, parse_task_name

SCORES = {
    "a_minus_m": -2, "a_minus_s": -1, "a_zero": 0,
    "a_plus_s": 1, "a_plus_m": 2, "a_amb": 0,
}

def dominant_valence(counts: Counter, n_labelled: int) -> str:
    if n_labelled == 0:
        return "neutral"
    has_plus = counts.get("a_plus_s", 0) + counts.get("a_plus_m", 0) > 0
    has_minus = counts.get("a_minus_s", 0) + counts.get("a_minus_m", 0) > 0
    if has_plus and has_minus:
        return "mixed"
    if has_plus:
        return "positive"
    if has_minus:
        return "negative"
    return "neutral"


def parse_filename(path_in_zip: str) -> tuple[int, str]:
    stem = path_in_zip.split("/")[-1].removesuffix(".txt")
    comment_id, task_name = stem.split("_", 1)
    return int(comment_id), task_name


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--results", type=Path, required=True, help="ZIP returned by CLARIN")
    parser.add_argument("--corpus", type=Path, default=DEFAULT_CORPUS,
                        help="corpus, used to report responses missing from the CLARIN output")
    parser.add_argument("--output", type=Path, default=PROCESSED_DIR / "aspectemo_scores.csv")
    args = parser.parse_args()
    ensure_dirs()

    records, skipped = [], []
    with zipfile.ZipFile(args.results) as archive:
        filenames = sorted(f for f in archive.namelist() if f.endswith(".txt"))
        print(f"Files in ZIP: {len(filenames)}")

        for filename in filenames:
            try:
                comment_id, task_name = parse_filename(filename)
                gender, stereotype, valence = parse_task_name(task_name)
            except ValueError:
                print(f"  Skipped (unexpected file name): {filename}")
                skipped.append(filename)
                continue

            with archive.open(filename) as handle:
                labels = json.load(handle).get("labels", [])

            total_tokens = len(labels)
            counts = Counter(label[2:] for label in labels if label.startswith("B-"))
            n_labelled = sum(counts.values())
            weighted_sum = sum(SCORES[k] * v for k, v in counts.items())

            records.append({
                "comment_id": comment_id,
                "task_name": task_name,
                "target_gender": gender,
                "stereotype": stereotype,
                "behaviour": valence,
                **{f"n_{label}": counts.get(label, 0) for label in SCORES},
                "total_tokens": total_tokens,
                "n_labelled_aspects": n_labelled,
                "pct_labelled_aspects": round(100 * n_labelled / total_tokens, 2) if total_tokens else 0.0,
                "score_all_tokens": round(weighted_sum / total_tokens, 4) if total_tokens else None,
                "score_only_charged": round(weighted_sum / n_labelled, 4) if n_labelled else None,
                "dominant_label": counts.most_common(1)[0][0] if n_labelled else "none",
                "valence": dominant_valence(counts, n_labelled),
            })

    df = pd.DataFrame(records).sort_values("comment_id").reset_index(drop=True)
    df.to_csv(args.output, index=False, encoding="utf-8-sig")
    print(f"\nWrote {len(df)} rows to {args.output}")
    if skipped:
        print(f"Skipped {len(skipped)} files: {skipped}")

    # Which corpus responses did not come back from CLARIN?
    if args.corpus.exists():
        corpus = pd.read_excel(args.corpus, engine="odf")
        missing = sorted(set(corpus["Comment ID"]) - set(df["comment_id"]))
        print(f"\nResponses in corpus but not in CLARIN output: {len(missing)}")
        if missing:
            print(corpus.loc[corpus["Comment ID"].isin(missing), ["Comment ID", "Task Name"]]
                  .to_string(index=False))

    print("\nResponses per condition:")
    print(df.groupby(["target_gender", "stereotype", "behaviour"]).size().to_string())
    print("\nValence of the dominant aspect label:")
    print(df["valence"].value_counts().to_string())
    print("\nMean score_all_tokens per condition:")
    print(df.groupby(["target_gender", "stereotype", "behaviour"])["score_all_tokens"]
          .mean().round(3).to_string())


if __name__ == "__main__":
    main()
