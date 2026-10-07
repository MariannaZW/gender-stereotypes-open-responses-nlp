#!/usr/bin/env python3
"""Step 3: response length (word counts) overall and per condition, including
the share of very short responses (<= 5 and <= 3 words).

Outputs (aggregated tables, safe to commit): output/tables/
    word_count_by_condition.csv, short_responses_by_condition.csv
Per-response counts (contain the response text): data/processed/word_counts.csv
"""

import argparse
import re
from pathlib import Path

import pandas as pd

from common import DEFAULT_CORPUS, PROCESSED_DIR, TABLES_DIR, ensure_dirs, load_corpus

WORD_PATTERN = re.compile(r"\w+", re.UNICODE)  # \w covers Polish diacritics


def count_words(text) -> int:
    if pd.isna(text) or str(text).strip() == "":
        return 0
    return len(WORD_PATTERN.findall(str(text)))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--corpus", type=Path, default=DEFAULT_CORPUS)
    args = parser.parse_args()
    ensure_dirs()

    df = load_corpus(args.corpus)
    df["word_count"] = df["Response"].apply(count_words)
    df.to_csv(PROCESSED_DIR / "word_counts.csv", index=False, encoding="utf-8-sig")

    wc = df["word_count"]
    print("\n=== Word counts, whole corpus ===")
    print(f"Responses: {len(df)} | mean {wc.mean():.2f} | SD {wc.std():.2f} | "
          f"median {wc.median():.0f} | min {wc.min()} | max {wc.max()}")
    print(f"Responses with 0 words: {(wc == 0).sum()}")

    by_condition = df.groupby("Task Name")["word_count"].agg(n="count", mean="mean", sd="std").round(2)
    print("\n=== Word counts per condition (Task Name) ===")
    print(by_condition)
    by_condition.to_csv(TABLES_DIR / "word_count_by_condition.csv")

    short = df.groupby("Task Name")["word_count"].agg(
        n="count",
        n_le5=lambda x: (x <= 5).sum(),
        n_le3=lambda x: (x <= 3).sum(),
    )
    short["pct_le5"] = (100 * short["n_le5"] / short["n"]).round(2)
    short["pct_le3"] = (100 * short["n_le3"] / short["n"]).round(2)

    print("\n=== Short responses ===")
    print(f"<= 5 words: {(wc <= 5).sum()} ({100 * (wc <= 5).mean():.2f}%)")
    print(f"<= 3 words: {(wc <= 3).sum()} ({100 * (wc <= 3).mean():.2f}%)")
    print(short)
    short.to_csv(TABLES_DIR / "short_responses_by_condition.csv")


if __name__ == "__main__":
    main()
