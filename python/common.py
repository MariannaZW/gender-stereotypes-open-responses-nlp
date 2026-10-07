"""Shared helpers for the Python part of the thesis pipeline."""

from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
RAW_DIR = ROOT / "data" / "raw"
PROCESSED_DIR = ROOT / "data" / "processed"
TABLES_DIR = ROOT / "output" / "tables"

# Polish-language corpus (one row per participant response)
DEFAULT_CORPUS = RAW_DIR / "Korpus_magisterka.ods"

REQUIRED_COLUMNS = ["Comment ID", "Participant Private ID", "Task Name", "Response"]


def ensure_dirs() -> None:
    for directory in (RAW_DIR, PROCESSED_DIR, TABLES_DIR):
        directory.mkdir(parents=True, exist_ok=True)


def load_corpus(path: Path = DEFAULT_CORPUS) -> pd.DataFrame:
    """Load the ODS corpus and check that the expected columns are present."""
    df = pd.read_excel(path, engine="odf")
    missing = [c for c in REQUIRED_COLUMNS if c not in df.columns]
    if missing:
        raise ValueError(f"Corpus is missing required columns: {missing}")
    print(f"Loaded {len(df)} responses from {path.name}")
    return df


def parse_task_name(task_name: str) -> tuple[str, str, str]:
    """Decode a task name such as 'K_SK_poz' into
    (target_gender, stereotype, valence).

    Parts: K = woman (kobieta) / M = man (mezczyzna);
           SK = feminine / SM = masculine stereotype;
           poz = positive / neg = negative behaviour.
    """
    parts = str(task_name).split("_")
    if len(parts) != 3 or parts[0] not in ("K", "M") \
            or parts[1] not in ("SK", "SM") or parts[2] not in ("poz", "neg"):
        raise ValueError(f"Unexpected task name: {task_name!r}")
    gender, stereotype, valence = parts
    return (
        "woman" if gender == "K" else "man",
        "feminine" if stereotype == "SK" else "masculine",
        "positive" if valence == "poz" else "negative",
    )
