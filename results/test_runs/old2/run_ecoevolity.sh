#!/bin/bash

# Define the base directory where all your scripts and files are located
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_RESULTS_DIR="${BASE_DIR}/results"
SIMULATE_SCRIPT="${BASE_DIR}/simulate_data.py"
ECOEVOLITY_CONFIG="${BASE_DIR}/ecoevolity_config.yml"
MAKE_YAML_SCRIPT="${BASE_DIR}/make_yaml.py"

# Path to the correct Python interpreter
PYTHON_INTERPRETER="python"  # Use 'python' or the path to your Python interpreter if necessary

# Command-line arguments for demographic parameters
NUM_SHARED_EVENTS=$1
SPLIT_TIME_SEP=$2
BASELINE_SPLIT_TIME=$3

# Create a unique directory name based on the parameters
CONFIG_DIR="${BASE_RESULTS_DIR}/config_${NUM_SHARED_EVENTS}_${SPLIT_TIME_SEP}_${BASELINE_SPLIT_TIME}_results"
mkdir -p "$CONFIG_DIR"

# Step 1: Update YAML files with new demographic parameters
echo "Updating YAML files with num_shared_events=$NUM_SHARED_EVENTS, split_time_sep=$SPLIT_TIME_SEP, baseline_split_time=$BASELINE_SPLIT_TIME..."
"$PYTHON_INTERPRETER" "$MAKE_YAML_SCRIPT" "$NUM_SHARED_EVENTS" "$SPLIT_TIME_SEP" "$BASELINE_SPLIT_TIME"

# Step 2: Ensure the results directory exists for the current config
mkdir -p "$CONFIG_DIR"

# Number of runs
NUM_RUNS=30

# Header for the summary file
HEADER="Run\tGene_Length\tPopulation_Size\tMutation_Rate\tProb_1_Event\tProb_2_Events\tProb_3_Events\tProb_4_Events\tProb_5_Events\tProb_6_Events\tMean_Divergence_PopA_PopB\tMean_Divergence_PopC_PopD\tMean_Divergence_PopE_PopF\tMean_Divergence_PopG_PopH\tMean_Divergence_PopI_PopJ\tMean_Divergence_PopK_PopL"

# Create or overwrite the summary file and add the header
echo -e "$HEADER" > "${CONFIG_DIR}/summary_output.txt"

# Loop through the desired number of runs
for (( run=1; run<=NUM_RUNS; run++ ))
do
    # Create a unique directory for this run
    RUN_DIR="${CONFIG_DIR}/run${run}/"
    mkdir -p "$RUN_DIR"

    # Change to the run directory
    cd "$RUN_DIR"

    # Step 3: Run the data simulation script using the correct Python interpreter
    echo "Running data simulation for run ${run}..."
    "$PYTHON_INTERPRETER" "$SIMULATE_SCRIPT"

    if [ $? -ne 0 ]; then
        echo "Error: Data simulation failed for run ${run}. Skipping this run."
        continue
    fi

    # Ensure you are in the correct directory before running ecoevolity
    cd "$RUN_DIR"

    # Step 4: Run ecoevolity using the configuration file and specify the output directory for logs
    ecoevolity --relax-triallelic-sites --prefix "${RUN_DIR}/" "$ECOEVOLITY_CONFIG"

    if [ $? -ne 0 ]; then
        echo "Error: ecoevolity failed for run ${run}. Skipping further steps."
        continue
    fi

    # Ensure that the state log file is present before proceeding
    if [ -f "${RUN_DIR}/ecoevolity_config-state-run-1.log" ]; then
        # Step 5: Summarize chains and redirect output
        pyco-sumchains -s 100 "${RUN_DIR}/ecoevolity_config-state-run-1.log" > "${RUN_DIR}/pyco-sumchains-output.txt"

        # Step 6: Summarize ecoevolity results and redirect output
        sumcoevolity -b 101 -c "${RUN_DIR}/ecoevolity_config.yml" -n 1000000 "${RUN_DIR}/ecoevolity_config-state-run-1.log" > "${RUN_DIR}/sumcoevolity-results.txt"

        # Step 7: Process number of events and calculate probabilities
        awk '
        BEGIN {for (i=1; i<=6; i++) events[i]=0; total=0}
        {
            if (NR>1) {
                events[$4]++
                total++
            }
        }
        END {
            for (i=1; i<=6; i++) printf "%.6f\t", events[i]/total;
            print ""
        }' "${RUN_DIR}/ecoevolity_config-state-run-1.log" > "${RUN_DIR}/event_probabilities.txt"

        # Ensure pyco-sumtimes-output.txt is fully written
        sleep 2

        # Step 8: Extract mean divergence times from pyco-sumtimes-output.txt
        if [ -f "${RUN_DIR}/pyco-sumtimes-output.txt" ]; then
            MEAN_DIVERGENCE_TIMES=$(awk -F'\t' 'NR>1 {printf "%.6f\t", $2}' "${RUN_DIR}/pyco-sumtimes-output.txt" | sed 's/\t$//')
            if [ -z "$MEAN_DIVERGENCE_TIMES" ]; then
                echo "Warning: Mean divergence times not found in run ${run}, setting to NA."
                MEAN_DIVERGENCE_TIMES="NA\tNA\tNA\tNA\tNA\tNA"
            fi
        else
            echo "Error: pyco-sumtimes-output.txt not found in run ${run}, setting mean divergence times to NA."
            MEAN_DIVERGENCE_TIMES="NA\tNA\tNA\tNA\tNA\tNA"
        fi

        # Step 9: Create summary file with initial parameters, event probabilities, and divergence times
        INITIAL_PARAMS="X\tY\tZ" # Replace X, Y, Z with actual values or extract them from the config file
        EVENT_PROBABILITIES=$(cat "${RUN_DIR}/event_probabilities.txt")

        # Output everything on one row with tab separation
        echo -e "${run}\t${INITIAL_PARAMS}\t${EVENT_PROBABILITIES}${MEAN_DIVERGENCE_TIMES}" >> "${CONFIG_DIR}/summary_output.txt"

        # Step 10: Process number of events if the sumcoevolity results are present
        if [ -f "${RUN_DIR}/sumcoevolity-results-nevents.txt" ]; then
            pyco-sumevents "${RUN_DIR}/sumcoevolity-results-nevents.txt" > "${RUN_DIR}/pyco-sumevents-output.txt"
        else
            echo "Error: sumcoevolity-results-nevents.txt not found in run${run}. Skipping pyco-sumevents."
        fi

        # Step 11: Summarize times and redirect output
        pyco-sumtimes -b 101 -z "${RUN_DIR}/ecoevolity_config-state-run-1.log" -f > "${RUN_DIR}/pyco-sumtimes-output.txt"

    else
        echo "Error: ecoevolity state log file not found in run ${run}. Skipping further steps."
    fi

    # Return to the base directory
    cd "$BASE_DIR"

    echo "Completed run ${run}."
done

echo "All runs completed for configuration: $NUM_SHARED_EVENTS, $SPLIT_TIME_SEP, $BASELINE_SPLIT_TIME."
