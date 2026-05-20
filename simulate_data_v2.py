import logging
import os
import random

import allel
import demes
import msprime
from IPython.display import SVG, display


logging.basicConfig(level=logging.DEBUG)


def simulate_data(
    demes_file,
    output_vcf,
    gene_length=1000,
    recombination_rate=0,
    mutation_rate=1e-8,
    ancestry_seed=None,
    mutation_seed=None,
):
    try:
        graph = demes.load(demes_file)
        logging.info(f"Loaded demes model from {demes_file}")

        demography = msprime.Demography.from_demes(graph)
        logging.info("Converted demes model to msprime demography")

        sample_sets = [
            msprime.SampleSet(10, population="pop1", ploidy=1),
            msprime.SampleSet(10, population="pop2", ploidy=1),
        ]

        logging.info(f"Sample sets: {sample_sets}")
        logging.info(f"Ancestry seed: {ancestry_seed}")
        logging.info(f"Mutation seed: {mutation_seed}")

        tree_sequence = msprime.sim_ancestry(
            samples=sample_sets,
            demography=demography,
            sequence_length=gene_length,
            recombination_rate=recombination_rate,
            random_seed=ancestry_seed,
        )

        mut_tree_sequence = msprime.sim_mutations(
            tree_sequence,
            rate=mutation_rate,
            random_seed=mutation_seed,
        )

        logging.info(f"Number of mutations: {mut_tree_sequence.num_mutations}")

        with open(output_vcf, "w") as vcf:
            mut_tree_sequence.write_vcf(vcf)

        logging.info(f"Simulated data written to {output_vcf}")
        return mut_tree_sequence

    except Exception as e:
        logging.error(f"Error during simulation: {e}")
        raise


def visualize_genealogy(tree_sequence, output_svg):
    try:
        svg = tree_sequence.draw_svg(size=(1000, 400))

        with open(output_svg, "w") as svg_file:
            svg_file.write(svg)

        logging.info(f"Genealogy visualization saved to {output_svg}")

        try:
            display(SVG(output_svg))
        except Exception:
            logging.info("Inline SVG display skipped outside notebook environment.")

    except Exception as e:
        logging.error(f"Error during visualization: {e}")
        raise


def vcf_to_nexus(vcf_file, nexus_file, gene_length, reference_sequence, species_id):
    try:
        callset = allel.read_vcf(vcf_file)
        logging.info(f"Read VCF file: {vcf_file}")

        if callset is None or "calldata/GT" not in callset:
            raise ValueError("VCF file does not contain genotype data")

        genotypes = callset["calldata/GT"]

        if genotypes is None or genotypes.size == 0:
            logging.warning("VCF file contains no genotypes; skipping Nexus conversion.")
            return False

        samples = callset["samples"]
        ref_alleles = callset["variants/REF"]
        alt_alleles = callset["variants/ALT"]
        positions = callset["variants/POS"] - 1

        num_samples = len(samples)
        num_variants = genotypes.shape[0]

        logging.info(f"Number of samples: {num_samples}, Number of variants: {num_variants}")

        sequences = {i: list(reference_sequence) for i in range(num_samples)}

        for i in range(num_variants):
            ref = ref_alleles[i]
            alt = alt_alleles[i][0]
            pos = positions[i]

            if pos >= gene_length:
                logging.warning(f"Position {pos} exceeds gene length {gene_length}. Skipping this variant.")
                continue

            for j in range(num_samples):
                genotype = genotypes[i, j]

                if genotype[0] != -1:
                    sequences[j][pos] = ref if genotype[0] == 0 else alt

                if len(genotype) > 1 and genotype[1] != -1:
                    sequences[j][pos] = ref if genotype[1] == 0 else alt

        sequence_strings = ["".join(seq) for seq in sequences.values()]

        populations = [
            chr(65 + (species_id - 1) * 2),
            chr(66 + (species_id - 1) * 2),
        ]

        with open(nexus_file, "w") as f:
            f.write("#NEXUS\n\n")
            f.write("BEGIN DATA;\n")
            f.write(f"    DIMENSIONS NTAX={num_samples} NCHAR={gene_length};\n")
            f.write("    FORMAT DATATYPE=DNA MISSING=N GAP=-;\n")
            f.write("    MATRIX\n")

            for idx, sequence_str in enumerate(sequence_strings):
                population = populations[0] if idx < 10 else populations[1]
                individual = idx + 1 if idx < 10 else idx - 9
                sample_name = f"Population{population}_ind{individual}"
                f.write(f"{sample_name} {sequence_str}\n")

            f.write("    ;\n")
            f.write("END;\n")

        logging.info(f"Written Nexus file: {nexus_file}")
        return True

    except Exception as e:
        logging.error(f"Error converting VCF to Nexus: {e}")
        return False


script_dir = os.path.dirname(os.path.abspath(__file__))
project_dir = os.path.dirname(script_dir)
sim_run_dir = os.environ.get("SIM_RUN_DIR")
sim_seed = os.environ.get("SIM_SEED")

if sim_seed is not None:
    sim_seed = int(sim_seed)
    seed_rng = random.Random(sim_seed)
    logging.info(f"Using simulation seed: {sim_seed}")
else:
    seed_rng = random.Random()
    logging.info("SIM_SEED not set. Using non-deterministic seed.")

if sim_run_dir:
    base_input_dir = os.path.join(sim_run_dir, "yaml")
    base_output_dir = sim_run_dir
    logging.info(f"Using run-specific YAML directory: {base_input_dir}")
    logging.info(f"Writing outputs directly to run directory: {base_output_dir}")
else:
    base_input_dir = os.path.join(project_dir, "yaml")
    base_output_dir = os.path.join(project_dir, "results")
    logging.info(f"SIM_RUN_DIR not set. Using project YAML directory: {base_input_dir}")
    logging.info(f"SIM_RUN_DIR not set. Writing outputs to: {base_output_dir}")

os.makedirs(base_output_dir, exist_ok=True)

gene_length = 1000
sequence_length = 1000
reference_sequence = "".join(seed_rng.choice("ACGT") for _ in range(sequence_length))

species_files = [
    ("model_species1.yaml", "output_data_species1.vcf", "output_data_species1_genealogy.svg", "output_data_species1.nex", 1),
    ("model_species2.yaml", "output_data_species2.vcf", "output_data_species2_genealogy.svg", "output_data_species2.nex", 2),
    ("model_species3.yaml", "output_data_species3.vcf", "output_data_species3_genealogy.svg", "output_data_species3.nex", 3),
    ("model_species4.yaml", "output_data_species4.vcf", "output_data_species4_genealogy.svg", "output_data_species4.nex", 4),
    ("model_species5.yaml", "output_data_species5.vcf", "output_data_species5_genealogy.svg", "output_data_species5.nex", 5),
    ("model_species6.yaml", "output_data_species6.vcf", "output_data_species6_genealogy.svg", "output_data_species6.nex", 6),
]

seed_log_path = os.path.join(base_output_dir, "simulation_seeds.tsv")

with open(seed_log_path, "w") as seed_log:
    seed_log.write("species_id\tancestry_seed\tmutation_seed\n")

for model_yaml, vcf_file, svg_file, nexus_file, species_id in species_files:
    demes_file_path = os.path.join(base_input_dir, model_yaml)
    output_vcf_path = os.path.join(base_output_dir, vcf_file)
    output_svg_path = os.path.join(base_output_dir, svg_file)
    output_nexus_path = os.path.join(base_output_dir, nexus_file)

    if not os.path.exists(demes_file_path):
        raise FileNotFoundError(f"Missing demes YAML file: {demes_file_path}")

    while True:
        ancestry_seed = seed_rng.randint(1, 2**32 - 1)
        mutation_seed = seed_rng.randint(1, 2**32 - 1)

        with open(seed_log_path, "a") as seed_log:
            seed_log.write(f"{species_id}\t{ancestry_seed}\t{mutation_seed}\n")

        tree_sequence = simulate_data(
            demes_file_path,
            output_vcf_path,
            gene_length=gene_length,
            ancestry_seed=ancestry_seed,
            mutation_seed=mutation_seed,
        )

        visualize_genealogy(tree_sequence, output_svg_path)

        if vcf_to_nexus(output_vcf_path, output_nexus_path, gene_length, reference_sequence, species_id):
            logging.info(f"Completed all steps for {model_yaml}")
            break

        logging.warning(f"Retrying simulation for {model_yaml} due to lack of genotype data.")