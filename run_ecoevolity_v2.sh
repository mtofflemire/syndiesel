#!/bin/bash

set -e
set -o pipefail

# ==========================================
# Paths
# ==========================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

BASE_RESULTS_DIR="${PROJECT_DIR}/results"
SIMULATE_SCRIPT="${SCRIPT_DIR}/simulate_data_v2.py"
ECOEVOLITY_CONFIG="${SCRIPT_DIR}/ecoevolity_config.yml"
MAKE_YAML_SCRIPT="${SCRIPT_DIR}/make_yaml_v2.py"
YAML_DIR="${SCRIPT_DIR}/yaml"

PYTHON_INTERPRETER="python"

# ==========================================
# Arguments
# ==========================================
NUM_SHARED_EVENTS=$1
SPLIT_TIME_SEP=$2
BASELINE_SPLIT_TIME=$3
MIGRATION_RATE=$4

if [ -z "$NUM_SHARED_EVENTS" ] || [ -z "$SPLIT_TIME_SEP" ] || [ -z "$BASELINE_SPLIT_TIME" ] || [ -z "$MIGRATION_RATE" ]; then
    echo "Usage: $0 NUM_SHARED_EVENTS SPLIT_TIME_SEP BASELINE_SPLIT_TIME MIGRATION_RATE"
    exit 1
fi

CONFIG_DIR="${BASE_RESULTS_DIR}/config_${NUM_SHARED_EVENTS}_${SPLIT_TIME_SEP}_${BASELINE_SPLIT_TIME}_${MIGRATION_RATE}_results"
mkdir -p "$CONFIG_DIR"

# ==========================================
# True parameters
# ==========================================
GENE_LENGTH=1000
POPULATION_SIZE=10000
MUTATION_RATE=1e-8
SAMPLES_SIMULATED_PER_POP=10
TRUE_NUMBER_EVENTS=6

TRUE_DIVERGENCE_POPA_POPB=$((BASELINE_SPLIT_TIME))
TRUE_DIVERGENCE_POPC_POPD=$((BASELINE_SPLIT_TIME + SPLIT_TIME_SEP))
TRUE_DIVERGENCE_POPE_POPF=$((BASELINE_SPLIT_TIME + 2*SPLIT_TIME_SEP))
TRUE_DIVERGENCE_POPG_POPH=$((BASELINE_SPLIT_TIME + 3*SPLIT_TIME_SEP))
TRUE_DIVERGENCE_POPI_POPJ=$((BASELINE_SPLIT_TIME + 4*SPLIT_TIME_SEP))
TRUE_DIVERGENCE_POPK_POPL=$((BASELINE_SPLIT_TIME + 5*SPLIT_TIME_SEP))

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
# Create summary file
# ==========================================
HEADER="Run\tSeed\tGene_Length\tPopulation_Size\tMutation_Rate\tSamples_Simulated_Per_Pop\tTrue_Number_Events\tBaseline_Split_Time\tSplit_Time_Separation\tMigration_Rate\tTrue_Divergence_PopA_PopB\tTrue_Divergence_PopC_PopD\tTrue_Divergence_PopE_PopF\tTrue_Divergence_PopG_PopH\tTrue_Divergence_PopI_PopJ\tTrue_Divergence_PopK_PopL\tProb_1_Event\tProb_2_Events\tProb_3_Events\tProb_4_Events\tProb_5_Events\tProb_6_Events\tMean_Divergence_PopA_PopB\tMean_Divergence_PopC_PopD\tMean_Divergence_PopE_PopF\tMean_Divergence_PopG_PopH\tMean_Divergence_PopI_PopJ\tMean_Divergence_PopK_PopL"

echo -e "$HEADER" > "${CONFIG_DIR}/summary_output.txt"

# ==========================================
# Run ecoevolity 30 independent times
# ==========================================
SUCCESSFUL_RUNS=0
ATTEMPT=0

while [ $SUCCESSFUL_RUNS -lt 30 ]
do
    ATTEMPT=$((ATTEMPT+1))

    RUN_SEED=$(od -An -N4 -tu4 /dev/urandom | tr -d ' ')
    RUN_DIR="${CONFIG_DIR}/run${SUCCESSFUL_RUNS}_seed${RUN_SEED}"
    RUN_YAML_DIR="${RUN_DIR}/yaml"
    RUN_ECOEVOLITY_CONFIG="${RUN_DIR}/ecoevolity_config.yml"

    mkdir -p "$RUN_DIR"
    mkdir -p "$RUN_YAML_DIR"

    echo "======================================="
    echo "Attempt $ATTEMPT | Successful so far: $SUCCESSFUL_RUNS"
    echo "Seed: $RUN_SEED"
    echo "Run directory: $RUN_DIR"
    echo "======================================="

    echo "$RUN_SEED" > "${RUN_DIR}/seed.txt"

    echo "Saving YAML files and ecoevolity config to run folder..."
    cp "${YAML_DIR}"/model_species*.yaml "$RUN_YAML_DIR/"
    cp "$ECOEVOLITY_CONFIG" "$RUN_ECOEVOLITY_CONFIG"

    cd "$RUN_DIR"

    echo "Simulating..."
    if ! SIM_RUN_DIR="$RUN_DIR" SIM_SEED="$RUN_SEED" "$PYTHON_INTERPRETER" "$SIMULATE_SCRIPT" > simulation.log 2>&1
    then
        echo "Simulation failed — retrying."
        cd "$SCRIPT_DIR"
        continue
    fi

    echo "Running ecoevolity..."
    if ! ecoevolity --relax-triallelic-sites \
        --prefix "${RUN_DIR}/" \
        "$RUN_ECOEVOLITY_CONFIG" \
        > ecoevolity.log 2>&1
    then
        echo "Ecoevolity crashed — retrying."
        cd "$SCRIPT_DIR"
        continue
    fi

    STATE_FILE="${RUN_DIR}/ecoevolity_config-state-run-1.log"

    if [ ! -f "$STATE_FILE" ]; then
        echo "State log missing — retrying."
        cd "$SCRIPT_DIR"
        continue
    fi

    echo "Summarizing chains..."

    pyco-sumchains -s 100 "$STATE_FILE" > pyco-sumchains-output.txt

    sumcoevolity -b 101 -c "$RUN_ECOEVOLITY_CONFIG" \
        -n 1000000 "$STATE_FILE" \
        > sumcoevolity-results.txt

    pyco-sumtimes -b 101 -z "$STATE_FILE" -f \
        > pyco-sumtimes-output.txt

    awk '
    BEGIN {for (i=1; i<=6; i++) events[i]=0; total=0}
    NR>1 {events[$4]++; total++}
    END {
        for (i=1; i<=6; i++) {
            if (total > 0) {
                printf "%.6f\t", events[i]/total;
            } else {
                printf "NA\t";
            }
        }
        print ""
    }' "$STATE_FILE" > event_probabilities.txt

    read P1 P2 P3 P4 P5 P6 < event_probabilities.txt

    MEAN_LINE=$(awk -F'\t' 'NR>1 {printf "%.6f\t", $2}' \
        pyco-sumtimes-output.txt | sed 's/\t$//')

    IFS=$'\t' read -r M1 M2 M3 M4 M5 M6 <<< "$MEAN_LINE"

    [ -z "$M1" ] && M1=NA
    [ -z "$M2" ] && M2=NA
    [ -z "$M3" ] && M3=NA
    [ -z "$M4" ] && M4=NA
    [ -z "$M5" ] && M5=NA
    [ -z "$M6" ] && M6=NA

    echo -e "${SUCCESSFUL_RUNS}\t\
${RUN_SEED}\t\
${GENE_LENGTH}\t\
${POPULATION_SIZE}\t\
${MUTATION_RATE}\t\
${SAMPLES_SIMULATED_PER_POP}\t\
${TRUE_NUMBER_EVENTS}\t\
${BASELINE_SPLIT_TIME}\t\
${SPLIT_TIME_SEP}\t\
${MIGRATION_RATE}\t\
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
    cd "$SCRIPT_DIR"
done

echo "======================================="
echo "30 SUCCESSFUL runs completed."
echo "Results saved in:"
echo "$CONFIG_DIR"
echo "======================================="