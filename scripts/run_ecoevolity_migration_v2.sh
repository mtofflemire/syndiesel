#!/bin/bash

set -e
set -o pipefail

# ==========================================
# Base directories
# ==========================================
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_RESULTS_DIR="${BASE_DIR}/results"
SIMULATE_SCRIPT="${BASE_DIR}/simulate_data.py"
ECOEVOLITY_CONFIG="${BASE_DIR}/ecoevolity_config.yml"
MAKE_YAML_SCRIPT="${BASE_DIR}/make_yaml.py"

PYTHON_INTERPRETER="python"

# ==========================================
# Arguments
# ==========================================
NUM_SHARED_EVENTS=$1
SPLIT_TIME_SEP=$2
BASELINE_SPLIT_TIME=$3
MIGRATION_RATE=$4

CONFIG_DIR="${BASE_RESULTS_DIR}/config_${NUM_SHARED_EVENTS}_${SPLIT_TIME_SEP}_${BASELINE_SPLIT_TIME}_${MIGRATION_RATE}_results"
mkdir -p "$CONFIG_DIR"

# ==========================================
# TRUE PARAMETERS (EDIT IF NEEDED)
# ==========================================
GENE_LENGTH=1000
POPULATION_SIZE=10000
MUTATION_RATE=1e-8
SAMPLES_SIMULATED_PER_POP=10
TRUE_NUMBER_EVENTS=6
TRUE_DIVERGENCE_POPA_POPB=10000
TRUE_DIVERGENCE_POPC_POPD=90000
TRUE_DIVERGENCE_POPE_POPF=170000
TRUE_DIVERGENCE_POPG_POPH=250000
TRUE_DIVERGENCE_POPI_POPJ=330000
TRUE_DIVERGENCE_POPK_POPL=410000

# ==========================================
# Update YAML
# ==========================================
echo "Updating YAML..."
"$PYTHON_INTERPRETER" "$MAKE_YAML_SCRIPT" \
    "$NUM_SHARED_EVENTS" \
    "$SPLIT_TIME_SEP" \
    "$BASELINE_SPLIT_TIME" \
    "$MIGRATION_RATE"

# ==========================================
# Create Summary File
# ==========================================
HEADER="Run\tGene_Length\tPopulation_Size\tMutation_Rate\tSamples_Simulated_Per_Pop\tTrue_Number_Events\tTrue_Divergence_PopA_PopB\tTrue_Divergence_PopC_PopD\tTrue_Divergence_PopE_PopF\tTrue_Divergence_PopG_PopH\tTrue_Divergence_PopI_PopJ\tTrue_Divergence_PopK_PopL\tProb_1_Event\tProb_2_Events\tProb_3_Events\tProb_4_Events\tProb_5_Events\tProb_6_Events\tMean_Divergence_PopA_PopB\tMean_Divergence_PopC_PopD\tMean_Divergence_PopE_PopF\tMean_Divergence_PopG_PopH\tMean_Divergence_PopI_PopJ\tMean_Divergence_PopK_PopL"

echo -e "$HEADER" > "${CONFIG_DIR}/summary_output.txt"

# ==========================================
# Run Until 30 Successful Runs
# ==========================================
SUCCESSFUL_RUNS=0
ATTEMPT=0

while [ $SUCCESSFUL_RUNS -lt 30 ]
do
    ATTEMPT=$((ATTEMPT+1))
    RUN_DIR="${CONFIG_DIR}/run${SUCCESSFUL_RUNS}/"
    mkdir -p "$RUN_DIR"
    cd "$RUN_DIR"

    echo "======================================="
    echo "Attempt $ATTEMPT | Successful so far: $SUCCESSFUL_RUNS"
    echo "======================================="

    # --------------------------------------
    # Step 1: Simulate
    # --------------------------------------
    echo "Simulating..."
    if ! SIM_RUN_DIR="$RUN_DIR" "$PYTHON_INTERPRETER" "$SIMULATE_SCRIPT" > simulation.log 2>&1
    then
        echo "Simulation failed — retrying."
        cd "$BASE_DIR"
        continue
    fi

    # --------------------------------------
    # Step 2: Run ecoevolity
    # --------------------------------------
    echo "Running ecoevolity..."
    if ! ecoevolity --relax-triallelic-sites \
        --prefix "${RUN_DIR}/" \
        "$ECOEVOLITY_CONFIG" \
        > ecoevolity.log 2>&1
    then
        echo "Ecoevolity crashed — retrying."
        cd "$BASE_DIR"
        continue
    fi

    STATE_FILE="${RUN_DIR}/ecoevolity_config-state-run-1.log"

    if [ ! -f "$STATE_FILE" ]; then
        echo "State log missing — retrying."
        cd "$BASE_DIR"
        continue
    fi

    # --------------------------------------
    # Step 3: Summaries
    # --------------------------------------
    echo "Summarizing chains..."
    pyco-sumchains -s 100 "$STATE_FILE" > pyco-sumchains-output.txt

    sumcoevolity -b 101 -c "$ECOEVOLITY_CONFIG" \
        -n 1000000 "$STATE_FILE" \
        > sumcoevolity-results.txt

    pyco-sumtimes -b 101 -z "$STATE_FILE" -f \
        > pyco-sumtimes-output.txt

    # --------------------------------------
    # Step 4: Event probabilities
    # --------------------------------------
    awk '
    BEGIN {for (i=1; i<=6; i++) events[i]=0; total=0}
    NR>1 {events[$4]++; total++}
    END {
        for (i=1; i<=6; i++) printf "%.6f\t", events[i]/total;
        print ""
    }' "$STATE_FILE" > event_probabilities.txt

    read P1 P2 P3 P4 P5 P6 < event_probabilities.txt

    # --------------------------------------
    # Step 5: Mean divergence times
    # --------------------------------------
    MEAN_LINE=$(awk -F'\t' 'NR>1 {printf "%.6f\t", $2}' \
        pyco-sumtimes-output.txt | sed 's/\t$//')

    IFS=$'\t' read -r M1 M2 M3 M4 M5 M6 <<< "$MEAN_LINE"

    [ -z "$M1" ] && M1=NA
    [ -z "$M2" ] && M2=NA
    [ -z "$M3" ] && M3=NA
    [ -z "$M4" ] && M4=NA
    [ -z "$M5" ] && M5=NA
    [ -z "$M6" ] && M6=NA

    # --------------------------------------
    # Write Row
    # --------------------------------------
    echo -e "${SUCCESSFUL_RUNS}\t\
${GENE_LENGTH}\t\
${POPULATION_SIZE}\t\
${MUTATION_RATE}\t\
${SAMPLES_SIMULATED_PER_POP}\t\
${TRUE_NUMBER_EVENTS}\t\
${TRUE_DIVERGENCE_POPA_POPB}\t\
${TRUE_DIVERGENCE_POPC_POPD}\t\
${TRUE_DIVERGENCE_POPE_POPF}\t\
${TRUE_DIVERGENCE_POPG_POPH}\t\
${TRUE_DIVERGENCE_POPI_POPJ}\t\
${TRUE_DIVERGENCE_POPK_POPL}\t\
${P1}\t${P2}\t${P3}\t${P4}\t${P5}\t${P6}\t\
${M1}\t${M2}\t${M3}\t${M4}\t${M5}\t${M6}" \
>> "${CONFIG_DIR}/summary_output.txt"

    SUCCESSFUL_RUNS=$((SUCCESSFUL_RUNS+1))
    cd "$BASE_DIR"
done

echo "======================================="
echo "30 SUCCESSFUL runs completed."
echo "======================================="