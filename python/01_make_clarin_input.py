#!/usr/bin/env python3
"""Step 1: build the input archive for CLARIN AspectEmo.

Writes one UTF-8 text file per response, named '{Comment ID}_{Task Name}.txt'
(e.g. '1_K_SK_poz.txt'), and packs them into a ZIP. The file names allow the
CLARIN output to be linked back to the corpus (Comment ID) and to the
experimental condition (Task Name).

Usage:
    python python/01_make_clarin_input.py [--corpus PATH] [--output PATH]
Then upload the ZIP to the AspectEmo service on the CLARIN platform.
"""

import argparse
import zipfile
from pathlib import Path

import pandas as pd

from common import DEFAULT_CORPUS, PROCESSED_DIR, ensure_dirs, load_corpus


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--corpus", type=Path, default=DEFAULT_CORPUS)
    parser.add_argument("--output", type=Path, default=PROCESSED_DIR / "clarin_input.zip")
    args = parser.parse_args()

    ensure_dirs()
    df = load_corpus(args.corpus)

    duplicated = df.loc[df["Comment ID"].duplicated(), "Comment ID"].tolist()
    if duplicated:
        raise ValueError(f"Duplicated Comment IDs: {duplicated}")

    empty = df["Response"].isna() | (df["Response"].astype(str).str.strip() == "")
    if empty.any():
        print(f"WARNING: skipping {empty.sum()} empty responses")
        df = df[~empty]

    with zipfile.ZipFile(args.output, "w", compression=zipfile.ZIP_DEFLATED) as archive:
        for _, row in df.iterrows():
            filename = f"{row['Comment ID']}_{row['Task Name']}.txt"
            archive.writestr(filename, str(row["Response"]).strip())

    print(f"Wrote {len(df)} files to {args.output}")


if __name__ == "__main__":
    main()
