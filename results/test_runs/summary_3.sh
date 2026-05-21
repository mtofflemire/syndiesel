#!/bin/bash

# ==========================================================
# ROBUST SUMMARY UPDATER (SAFE VERSION)
# ==========================================================

RESULTS_DIR='/Users/michaeltofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_6_40000_10000_0.01_results/'

SUMMARY_FILE="${RESULTS_DIR}summary_output.txt"
TEMP_SUMMARY_FILE="${RESULTS_DIR}summary_output_temp.txt"

# ----------------------------------------------------------
# TRUE PARAMETER VALUES
# ----------------------------------------------------------
GENE_LENGTH=1000
POPULATION_SIZE=10000
MUTATION_RATE=1e-8
SAMPLES_SIMULATED_PER_POP=10
TRUE_NUMBER_EVENTS=6
TRUE_DIVERGENCE_POPA_POPB=10000
TRUE_DIVERGENCE_POPC_POPD=50000
TRUE_DIVERGENCE_POPE_POPF=90000
TRUE_DIVERGENCE_POPG_POPH=130000
TRUE_DIVERGENCE_POPI_POPJ=170000
TRUE_DIVERGENCE_POPK_POPL=210000

# ----------------------------------------------------------
# SAFETY CHECK
# ----------------------------------------------------------
if [ ! -f "$SUMMARY_FILE" ]; then
    echo "ERROR: summary_output.txt not found."
    exit 1
fi

# ----------------------------------------------------------
# WRITE HEADER CLEANLY
# ----------------------------------------------------------
ORIGINAL_HEADER=$(head -n 1 "$SUMMARY_FILE")

NEW_HEADERS="Gene_Length\tPopulation_Size\tMutation_Rate\tSamples_Simulated_Per_Pop\tTrue_Number_Events\tTrue_Divergence_PopA_PopB\tTrue_Divergence_PopC_PopD\tTrue_Divergence_PopE_PopF\tTrue_Divergence_PopG_PopH\tTrue_Divergence_PopI_PopJ\tTrue_Divergence_PopK_PopL"

echo -e "${ORIGINAL_HEADER}\tMean_Divergence_PopA_PopB\tMean_Divergence_PopC_PopD\tMean_Divergence_PopE_PopF\tMean_Divergence_PopG_PopH\tMean_Divergence_PopI_PopJ\tMean_Divergence_PopK_PopL\t${NEW_HEADERS}" > "$TEMP_SUMMARY_FILE"

# ----------------------------------------------------------
# PROCESS EACH RUN DIRECTORY THAT EXISTS
# ----------------------------------------------------------

for RUN_DIR in "${RESULTS_DIR}"/run*/
do
    run=$(basename "$RUN_DIR" | sed 's/run//')

    echo "Processing run $run"

    # Match summary row by Run column (NOT line number)
    EXISTING_LINE=$(awk -F'\t' -v r="$run" '$1 == r' "$SUMMARY_FILE")

    if [ -z "$EXISTING_LINE" ]; then
        echo "No summary row found for run $run — skipping."
        continue
    fi

    SUMTIMES_OUTPUT_PATH="${RUN_DIR}/pyco-sumtimes-output.txt"

    if [ ! -f "$SUMTIMES_OUTPUT_PATH" ]; then
        echo "Run $run missing sumtimes file — inserting NA."
        MEAN_DIVERGENCE_TIMES="NA\tNA\tNA\tNA\tNA\tNA"
    else
        MEAN_DIVERGENCE_TIMES=$(awk -F'\t' 'NR>1 {printf "%.6f\t", $2}' "$SUMTIMES_OUTPUT_PATH" | sed 's/\t$//')

        if [ -z "$MEAN_DIVERGENCE_TIMES" ]; then
            echo "Run $run sumtimes empty — inserting NA."
            MEAN_DIVERGENCE_TIMES="NA\tNA\tNA\tNA\tNA\tNA"
        fi
    fi

    NEW_COLUMNS="${GENE_LENGTH}\t${POPULATION_SIZE}\t${MUTATION_RATE}\t${SAMPLES_SIMULATED_PER_POP}\t${TRUE_NUMBER_EVENTS}\t${TRUE_DIVERGENCE_POPA_POPB}\t${TRUE_DIVERGENCE_POPC_POPD}\t${TRUE_DIVERGENCE_POPE_POPF}\t${TRUE_DIVERGENCE_POPG_POPH}\t${TRUE_DIVERGENCE_POPI_POPJ}\t${TRUE_DIVERGENCE_POPK_POPL}"

    UPDATED_LINE="${EXISTING_LINE}\t${MEAN_DIVERGENCE_TIMES}\t${NEW_COLUMNS}"

    echo -e "$UPDATED_LINE" >> "$TEMP_SUMMARY_FILE"

done

# ----------------------------------------------------------
# REPLACE ORIGINAL FILE
# ----------------------------------------------------------
mv "$TEMP_SUMMARY_FILE" "$SUMMARY_FILE"

echo "Summary file rebuilt cleanly."