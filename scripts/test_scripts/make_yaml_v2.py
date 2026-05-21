#!/usr/bin/env python

from pathlib import Path
import sys


def usage():
    print(
        "Usage: make_yaml_v2.py "
        "NUM_SHARED_EVENTS SPLIT_TIME_SEP BASELINE_SPLIT_TIME MIGRATION_RATE YAML_OUTPUT_DIR"
    )
    sys.exit(1)


if len(sys.argv) != 6:
    usage()

NUM_SHARED_EVENTS = int(sys.argv[1])
SPLIT_TIME_SEP = int(sys.argv[2])
BASELINE_SPLIT_TIME = int(sys.argv[3])
MIGRATION_RATE = float(sys.argv[4])
YAML_OUTPUT_DIR = Path(sys.argv[5])

YAML_OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# ==========================================
# True divergence times
# ==========================================
TRUE_DIVERGENCE_POPA_POPB = BASELINE_SPLIT_TIME
TRUE_DIVERGENCE_POPC_POPD = BASELINE_SPLIT_TIME + SPLIT_TIME_SEP
TRUE_DIVERGENCE_POPE_POPF = BASELINE_SPLIT_TIME + 2 * SPLIT_TIME_SEP
TRUE_DIVERGENCE_POPG_POPH = BASELINE_SPLIT_TIME + 3 * SPLIT_TIME_SEP
TRUE_DIVERGENCE_POPI_POPJ = BASELINE_SPLIT_TIME + 4 * SPLIT_TIME_SEP
TRUE_DIVERGENCE_POPK_POPL = BASELINE_SPLIT_TIME + 5 * SPLIT_TIME_SEP

PAIR_SETTINGS = [
    ("popA", "popB", TRUE_DIVERGENCE_POPA_POPB),
    ("popC", "popD", TRUE_DIVERGENCE_POPC_POPD),
    ("popE", "popF", TRUE_DIVERGENCE_POPE_POPF),
    ("popG", "popH", TRUE_DIVERGENCE_POPG_POPH),
    ("popI", "popJ", TRUE_DIVERGENCE_POPI_POPJ),
    ("popK", "popL", TRUE_DIVERGENCE_POPK_POPL),
]


def build_model_yaml(pop1, pop2, divergence_time):
    """
    Replace the body of this function with the exact YAML content your current
    make_yaml_v2.py writes for each model_species*.yaml file.

    The important change is that this function returns YAML text, while the
    output path is controlled outside the function by YAML_OUTPUT_DIR.
    """

    return f"""---
population_labels:
    - {pop1}
    - {pop2}

divergence_time: {divergence_time}
migration_rate: {MIGRATION_RATE}
num_shared_events: {NUM_SHARED_EVENTS}
split_time_separation: {SPLIT_TIME_SEP}
baseline_split_time: {BASELINE_SPLIT_TIME}
"""


def main():
    for index, (pop1, pop2, divergence_time) in enumerate(PAIR_SETTINGS, start=1):
        output_path = YAML_OUTPUT_DIR / f"model_species{index}.yaml"

        yaml_text = build_model_yaml(
            pop1=pop1,
            pop2=pop2,
            divergence_time=divergence_time,
        )

        with open(output_path, "w") as out:
            out.write(yaml_text)

    print(f"Wrote YAML files to: {YAML_OUTPUT_DIR}")


if __name__ == "__main__":
    main()