import yaml
import os
import sys

def update_yaml_file(yaml_file, output_dir, num_shared_events, split_time_sep, baseline_split_time, migration_rate):
    # Read the current YAML file
    with open(yaml_file, 'r') as file:
        yaml_data = yaml.safe_load(file)
    
    # Calculate split times for each pair based on the number of shared events and separation
    split_times = []
    for i in range(1, 7):  # Six species pairs
        event_group = (i - 1) // (6 // num_shared_events) + 1
        split_time = baseline_split_time + (event_group - 1) * split_time_sep
        split_times.append(split_time)
    
    # Update YAML file content for species based on calculated split times
    for i in range(1, 7):
        yaml_data['description'] = f"Species {i} with two populations diverging {split_times[i-1]} generations ago"
        yaml_data['demes'][0]['epochs'][0]['end_time'] = split_times[i-1]
        yaml_data['demes'][1]['start_time'] = split_times[i-1]
        yaml_data['demes'][2]['start_time'] = split_times[i-1]

        # Add migration between pop1 and pop2
        yaml_data['migrations'] = [
            {
                'demes': ['pop1', 'pop2'],
                'rate': migration_rate,
                'start_time': split_times[i-1],
                'end_time': 0  # until present
            }
        ]

        # Ensure the output directory exists
        os.makedirs(output_dir, exist_ok=True)

        # Write the updated YAML data to a new file with the desired naming convention
        species_output_yaml = os.path.join(output_dir, f'model_species{i}.yaml')
        with open(species_output_yaml, 'w') as file:
            yaml.dump(yaml_data, file)
        print(f"Updated YAML file: {species_output_yaml} for Species {i} with split time: {split_times[i-1]} and migration rate: {migration_rate}")

if __name__ == "__main__":
    # Command-line arguments: num_shared_events, split_time_sep, baseline_split_time, migration_rate
    num_shared_events = int(sys.argv[1])
    split_time_sep = int(sys.argv[2])
    baseline_split_time = int(sys.argv[3])
    migration_rate = float(sys.argv[4])  # New migration rate argument

    input_yaml = './yaml/model_species1.yaml'  # Correct path to the input YAML file
    output_dir = './yaml'  # Directory for updated YAML files

    # Run the function to update and create YAML files with migration
    update_yaml_file(input_yaml, output_dir, num_shared_events, split_time_sep, baseline_split_time, migration_rate)
