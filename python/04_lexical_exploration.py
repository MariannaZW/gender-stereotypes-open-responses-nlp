#!/usr/bin/env python3
"""Step 4 (exploratory): which adjectives, verbs and nouns do participants use
to describe the characters?

Pipeline: spaCy (pl_core_news_lg) for tokenisation and part-of-speech tagging;
optionally Morfeusz 2 for lemmatisation.

Word forms (--form):
    surface  (default) inflected forms are counted separately, e.g. 'niepewna'
             and 'niepewną' are different entries. This is what was used for
             the thesis; inflected variants were merged by hand when the
             summary table was assembled.
    lemma    Morfeusz 2 lemmas (falls back to spaCy lemmas if Morfeusz is not
             installed). NOT used for the thesis numbers.

Passive participles tagged ADJ whose form ends like an infinitive
(e.g. '-ować') are moved to VERB; with --form surface this rule almost never
fires.

Outputs
    output/tables/top_words_by_task.csv      top 20 per part of speech and condition (ties kept)
    output/tables/pos_summary_by_task.csv    counts and shares of ADJ / VERB / NOUN
    data/processed/lexical_annotations.xlsx  per-response word lists + frequency dictionary
    data/processed/lexical_dictionary_by_task.xlsx   alphabetical dictionary, one sheet per condition
(the last two contain participants' words and are git-ignored)

Setup:
    pip install -r requirements.txt
    python -m spacy download pl_core_news_lg
"""

import argparse
from collections import Counter
from pathlib import Path

import pandas as pd
import spacy

from common import DEFAULT_CORPUS, PROCESSED_DIR, TABLES_DIR, ensure_dirs, load_corpus

VERB_ENDINGS = ("ować", "ywać", "iwać", "ać", "eć", "yć", "ić", "nąć")
SKIP_POS = {"PUNCT", "SPACE", "NUM", "SYM", "X"}
MIN_TOKEN_LEN = 3
KEPT_POS = ("ADJ", "VERB", "NOUN")

# Character names used in the vignettes. Matched by stem, so that inflected
# forms ('Marii', 'Piotra', ...) are excluded as well.
NAME_STEMS = ("małgorzat", "katarzyn", "ann", "mari", "andrzej", "krzysztof", "piotr", "tomasz")

# Morfeusz tag prefixes expected for each spaCy part of speech
POS_TO_MORFEUSZ = {
    "ADJ": ["adj"],
    "VERB": ["fin", "inf", "praet", "imps", "ger", "pact", "ppas", "impt"],
    "NOUN": ["subst", "depr", "ger"],
}


def is_name(word: str) -> bool:
    return any(word.startswith(stem) and len(word) <= len(stem) + 3 for stem in NAME_STEMS)


def load_morfeusz():
    try:
        import morfeusz2
        return morfeusz2.Morfeusz()
    except ImportError:
        return None


def morfeusz_lemma(morf, text: str, pos: str) -> str:
    """Lemma of `text` from Morfeusz 2, preferring analyses whose tag matches `pos`.

    Morfeusz2.analyse() returns tuples (start, end, (orth, lemma, tag, name, labels)).
    Lemmas may carry a qualifier after ':' (e.g. 'być:v1') which is stripped.
    """
    try:
        analyses = morf.analyse(text)
        expected = POS_TO_MORFEUSZ.get(pos, [])
        candidates = []
        for _, _, interpretation in analyses:
            lemma, tag = interpretation[1], interpretation[2]
            if any(prefix in tag.lower() for prefix in expected):
                candidates.append(lemma.split(":")[0])
        if candidates:
            return min(candidates, key=len).lower()
        if analyses:
            return analyses[0][2][1].split(":")[0].lower()
    except Exception:
        pass
    return text.lower()


def word_form(token, form_mode: str, morf) -> str:
    if form_mode == "surface":
        return token.text.lower()
    if morf is not None:
        return morfeusz_lemma(morf, token.text, token.pos_)
    return token.lemma_.lower().strip()


def extract_words(doc, form_mode: str, morf) -> dict[str, list[str]]:
    words = {pos: [] for pos in KEPT_POS}
    for token in doc:
        if token.pos_ in SKIP_POS or token.is_space or token.is_punct or token.is_digit:
            continue
        if len(token.text.strip()) < MIN_TOKEN_LEN:
            continue
        form = word_form(token, form_mode, morf)
        if len(form) < MIN_TOKEN_LEN:
            continue
        category = token.pos_
        if category == "ADJ" and form.endswith(VERB_ENDINGS):
            category = "VERB"   # passive participle
        if category in words:
            words[category].append(form)
    return words


def top_n_with_ties(words: list[str], n: int = 20) -> list[tuple[str, int]]:
    ranked = Counter(w for w in words if not is_name(w)).most_common()
    if len(ranked) <= n:
        return ranked
    threshold = ranked[n - 1][1]
    return [(w, c) for w, c in ranked if c >= threshold]


def flatten(series) -> list[str]:
    return [w for items in series for w in items]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--corpus", type=Path, default=DEFAULT_CORPUS)
    parser.add_argument("--form", choices=["surface", "lemma"], default="surface")
    args = parser.parse_args()
    ensure_dirs()

    morf = None
    if args.form == "lemma":
        morf = load_morfeusz()
        print("Lemmatiser:", "Morfeusz 2" if morf else "spaCy (Morfeusz 2 not installed)")
    else:
        print("Counting surface forms (as in the thesis)")

    df = load_corpus(args.corpus)
    nlp = spacy.load("pl_core_news_lg")

    docs = nlp.pipe(df["Response"].astype(str).tolist(), batch_size=50)
    extracted = [extract_words(doc, args.form, morf) for doc in docs]
    df["adjectives"] = [e["ADJ"] for e in extracted]
    df["verbs"] = [e["VERB"] for e in extracted]
    df["nouns"] = [e["NOUN"] for e in extracted]
    column_of = {"ADJ": "adjectives", "VERB": "verbs", "NOUN": "nouns"}

    # ---- Global statistics ---------------------------------------------------
    print("\n=== Unique entries ===")
    for pos, column in column_of.items():
        print(f"{pos}: {len(set(flatten(df[column])))}")

    # ---- Top words per condition ----------------------------------------------
    rows = []
    for task in sorted(df["Task Name"].unique()):
        subset = df[df["Task Name"] == task]
        print(f"\n{'=' * 60}\n  {task} (n = {len(subset)} responses)\n{'=' * 60}")
        for pos, column in column_of.items():
            top = top_n_with_ties(flatten(subset[column]))
            print(f"\n  Top {pos}:")
            for rank, (word, count) in enumerate(top, 1):
                print(f"    {rank:2}. {word:<25} {count}")
                rows.append({"task": task, "pos": pos, "rank": rank, "word": word, "count": count})
    pd.DataFrame(rows).to_csv(TABLES_DIR / "top_words_by_task.csv", index=False)

    # ---- Part-of-speech summary per condition ---------------------------------
    summary = []
    for task in sorted(df["Task Name"].unique()):
        subset = df[df["Task Name"] == task]
        n_adj, n_verb, n_noun = (int(subset[c].apply(len).sum()) for c in column_of.values())
        total = n_adj + n_verb + n_noun
        summary.append({
            "task": task, "n_responses": len(subset),
            "n_adj": n_adj, "n_verb": n_verb, "n_noun": n_noun, "n_total": total,
            "pct_adj": round(100 * n_adj / total, 2) if total else 0,
            "pct_verb": round(100 * n_verb / total, 2) if total else 0,
            "pct_noun": round(100 * n_noun / total, 2) if total else 0,
        })
    pos_summary = pd.DataFrame(summary)
    print("\n=== Part-of-speech summary ===")
    print(pos_summary.to_string(index=False))
    pos_summary.to_csv(TABLES_DIR / "pos_summary_by_task.csv", index=False)

    # ---- Dictionaries (contain participants' words -> data/processed) ----------
    with pd.ExcelWriter(PROCESSED_DIR / "lexical_annotations.xlsx") as writer:
        export = df.copy()
        for column in column_of.values():
            export[column] = export[column].apply(", ".join)
        export.to_excel(writer, sheet_name="data", index=False)

        entries = [(w, pos, c) for pos, column in column_of.items()
                   for w, c in Counter(flatten(df[column])).items()]
        (pd.DataFrame(entries, columns=["word", "pos", "count"])
           .sort_values("count", ascending=False)
           .to_excel(writer, sheet_name="dictionary", index=False))

    with pd.ExcelWriter(PROCESSED_DIR / "lexical_dictionary_by_task.xlsx") as writer:
        for task in sorted(df["Task Name"].unique()):
            subset = df[df["Task Name"] == task]
            entries = [(w, pos, c) for pos, column in column_of.items()
                       for w, c in Counter(flatten(subset[column])).items()]
            task_df = (pd.DataFrame(entries, columns=["word", "pos", "count"])
                         .sort_values("word", key=lambda col: col.str.lower()))
            task_df.to_excel(writer, sheet_name=str(task)[:31], index=False)
            print(f"Sheet '{task}': {len(task_df)} unique entries")

    print("\nDone.")


if __name__ == "__main__":
    main()
