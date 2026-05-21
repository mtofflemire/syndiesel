#!/bin/bash

# Define the directory containing the summary file
RESULTS_DIR='/Users/michaeltofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_6_10000_10000_0.01_results/'

# Default values for the new columns (adjust these values as needed)
GENE_LENGTH=1000  # Replace with actual value
POPULATION_SIZE=10000  # Replace with actual value
MUTATION_RATE=1e-8  # Replace with actual value
SAMPLES_SIMULATED_PER_POP=10  # Replace with actual value
TRUE_NUMBER_EVENTS=6  # Replace with actual value
TRUE_DIVERGENCE_POPA_POPB=10000  # Replace with actual value
TRUE_DIVERGENCE_POPC_POPD=20000  # Replace with actual value
TRUE_DIVERGENCE_POPE_POPF=30000  # Replace with actual value
TRUE_DIVERGENCE_POPG_POPH=40000  # Replace with actual value
TRUE_DIVERGENCE_POPI_POPJ=50000  # Replace with actual value
TRUE_DIVERGENCE_POPK_POPL=60000  # Replace with actual value

# Paths for the summary file and temp file
SUMMARY_FILE="${RESULTS_DIR}summary_output.txt"
TEMP_SUMMARY_FILE="${RESULTS_DIR}summary_output_temp.txt"

# Ensure the summary file exists
if [ ! -f "$SUMMARY_FILE" ]; then
    echo "Summary file not found in directory $RESULTS_DIR. Please ensure it has been created before running this script."
    exit 1
fi

# Add headers for new columns if not already present
NEW_HEADERS="Gene_Length\tPopulation_Size\tMutation_Rate\tSamples_Simulated_Per_Pop\tTrue_Number_Events\tTrue_Divergence_PopA_PopB\tTrue_Divergence_PopC_PopD\tTrue_Divergence_PopE_PopF\tTrue_Divergence_PopG_PopH\tTrue_Divergence_PopI_PopJ\tTrue_Divergence_PopK_PopL"
echo -e "$(head -n 1 "$SUMMARY_FILE")\t$NEW_HEADERS" > "$TEMP_SUMMARY_FILE"

# Number of runs
NUM_RUNS=30

# Process each run
for (( run=1; run<=NUM_RUNS; run++ ))
do
    RUN_DIR="${RESULTS_DIR}run${run}/"
    cd "$RUN_DIR"

    # Extract the existing line from the summary file
    EXISTING_LINE=$(sed -n "$((run + 1))p" "$SUMMARY_FILE")

    # Check for pyco-sumtimes-output.txt file
    SUMTIMES_OUTPUT_PATH="${RUN_DIR}pyco-sumtimes-output.txt"
    if [ ! -f "$SUMTIMES_OUTPUT_PATH" ]; then
        echo "File $SUMTIMES_OUTPUT_PATH not found for run $run. Setting mean divergence times to NA."
        MEAN_DIVERGENCE_TIMES="NA\tNA\tNA\tNA\tNA\tNA"
    else
        # Extract mean divergence times
        MEAN_DIVERGENCE_TIMES=$(awk -F'\t' 'NR>1 {printf "%.6f\t", $2}' "$SUMTIMES_OUTPUT_PATH" | sed 's/\t$//')
        if [ -z "$MEAN_DIVERGENCE_TIMES" ]; then
            echo "Warning: No mean divergence times found in $SUMTIMES_OUTPUT_PATH for run $run. Setting to NA."
            MEAN_DIVERGENCE_TIMES="NA\tNA\tNA\tNA\tNA\tNA"
        fi
    fi

    # Add the new columns with their values
    NEW_COLUMNS="${GENE_LENGTH}\t${POPULATION_SIZE}\t${MUTATION_RATE}\t${SAMPLES_SIMULATED_PER_POP}\t${TRUE_NUMBER_EVENTS}\t${TRUE_DIVERGENCE_POPA_POPB}\t${TRUE_DIVERGENCE_POPC_POPD}\t${TRUE_DIVERGENCE_POPE_POPF}\t${TRUE_DIVERGENCE_POPG_POPH}\t${TRUE_DIVERGENCE_POPI_POPJ}\t${TRUE_DIVERGENCE_POPK_POPL}"

    # Update the line with mean divergence times and new columns
    UPDATED_LINE="${EXISTING_LINE}\t${MEAN_DIVERGENCE_TIMES}\t${NEW_COLUMNS}"

    # Add the updated line to the temp summary file
    echo -e "$UPDATED_LINE" >> "$TEMP_SUMMARY_FILE"

    echo "Updated summary for run $run in $RESULTS_DIR."
done

# Replace the original summary file with the updated one
mv "$TEMP_SUMMARY_FILE" "$SUMMARY_FILE"

echo "Summary file updated with mean divergence times and new columns in $RESULTS_DIR."
