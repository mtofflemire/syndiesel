import copy
import os
import sys

import yaml


def calculate_split_times(num_shared_events, split_time_sep, baseline_split_time):
    if num_shared_events < 1 or num_shared_events > 6:
        raise ValueError("num_shared_events must be between 1 and 6")

    if 6 % num_shared_events != 0:
        raise ValueError("num_shared_events must divide evenly into 6. Use 1, 2, 3, or 6.")

    species_per_event = 6 // num_shared_events
    split_times = []

    for i in range(1, 7):
        event_group = (i - 1) // species_per_event
        split_time = baseline_split_time + event_group * split_time_sep
        split_times.append(split_time)

    return split_times


def update_yaml_file(input_yaml, output_dir, num_shared_events, split_time_sep, baseline_split_time, migration_rate):
    with open(input_yaml, "r") as file:
        base_yaml_data = yaml.safe_load(file)

    split_times = calculate_split_times(
        num_shared_events,
        split_time_sep,
        baseline_split_time,
    )

    os.makedirs(output_dir, exist_ok=True)

    for i in range(1, 7):
        yaml_data = copy.deepcopy(base_yaml_data)
        split_time = split_times[i - 1]

        yaml_data["description"] = (
            f"Species {i} with two populations diverging {split_time} generations ago"
        )

        yaml_data["demes"][0]["epochs"][0]["end_time"] = split_time
        yaml_data["demes"][1]["start_time"] = split_time
        yaml_data["demes"][2]["start_time"] = split_time

        yaml_data["migrations"] = [
            {
                "demes": ["pop1", "pop2"],
                "rate": migration_rate,
                "start_time": split_time,
                "end_time": 0,
            }
        ]

        species_output_yaml = os.path.join(output_dir, f"model_species{i}.yaml")

        with open(species_output_yaml, "w") as file:
            yaml.safe_dump(yaml_data, file, sort_keys=False)

        print(
            f"Updated YAML file: {species_output_yaml} "
            f"for Species {i} with split time: {split_time} "
            f"and migration rate: {migration_rate}"
        )


if __name__ == "__main__":
    if len(sys.argv) != 5:
        print(
            "Usage: python make_yaml_v2.py "
            "NUM_SHARED_EVENTS SPLIT_TIME_SEP BASELINE_SPLIT_TIME MIGRATION_RATE"
        )
        sys.exit(1)

    num_shared_events = int(sys.argv[1])
    split_time_sep = int(sys.argv[2])
    baseline_split_time = int(sys.argv[3])
    migration_rate = float(sys.argv[4])

    script_dir = os.path.dirname(os.path.abspath(__file__))

    input_yaml = os.path.join(script_dir, "yaml", "model_species1.yaml")
    output_dir = os.path.join(script_dir, "yaml")

    update_yaml_file(
        input_yaml,
        output_dir,
        num_shared_events,
        split_time_sep,
        baseline_split_time,
        migration_rate,
    )