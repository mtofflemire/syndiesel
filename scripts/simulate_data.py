import msprime
import demes
import logging
from IPython.display import SVG, display
import allel
import random
import os
sim_run_dir = os.environ.get("SIM_RUN_DIR")
# use `sim_run_dir` to copy files if needed


# Set up logging for the script
logging.basicConfig(level=logging.DEBUG)




def simulate_data(demes_file, output_vcf, gene_length=1000, recombination_rate=0, mutation_rate=1e-8):
    try:
        # Load the demes model from the YAML file
        graph = demes.load(demes_file)
        logging.info(f"Loaded demes model from {demes_file}")

        # Convert the demes model to msprime's demography
        demography = msprime.Demography.from_demes(graph)
        logging.info("Converted demes model to msprime demography")

        # Define the sample sets for the species pair
        sample_sets = [
            msprime.SampleSet(10, population="pop1", ploidy=1),
            msprime.SampleSet(10, population="pop2", ploidy=1)
        ]
        logging.info(f"Sample sets: {sample_sets}")

        # Simulate ancestry
        logging.info("Simulating ancestry")
        tree_sequence = msprime.sim_ancestry(
            samples=sample_sets,
            demography=demography,
            sequence_length=gene_length,
            recombination_rate=recombination_rate
        )
        logging.info("Ancestry simulation complete")

        # Simulate mutations
        mut_tree_sequence = msprime.sim_mutations(tree_sequence, rate=mutation_rate)
        logging.info(f"Number of mutations: {mut_tree_sequence.num_mutations}")

        # Write VCF records to a file
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

        # Display the SVG inline
        display(SVG(output_svg))
    except Exception as e:
        logging.error(f"Error during visualization: {e}")
        raise

def vcf_to_nexus(vcf_file, nexus_file, gene_length, reference_sequence, species_id):
    try:
        # Read the VCF file using scikit-allel
        callset = allel.read_vcf(vcf_file)
        logging.info(f"Read VCF file: {vcf_file}")

        # Check if VCF contains genotype data
        if callset is None or 'calldata/GT' not in callset:
            raise ValueError("VCF file does not contain genotype data")

        # Extract the necessary arrays
        genotypes = callset['calldata/GT']
        if genotypes is None or genotypes.size == 0:
            logging.warning("VCF file contains no genotypes; skipping Nexus conversion.")
            return False  # Indicate no data to trigger a retry

        samples = callset['samples']
        ref_alleles = callset['variants/REF']
        alt_alleles = callset['variants/ALT']
        positions = callset['variants/POS'] - 1  # zero-based positions
        num_samples = len(samples)
        num_variants = genotypes.shape[0]

        logging.info(f"Number of samples: {num_samples}, Number of variants: {num_variants}")

        # Create a dictionary to hold the sequences for each sample
        sequences = {i: list(reference_sequence) for i in range(num_samples)}

        # Fill in the variant positions with the corresponding alleles
        for i in range(num_variants):
            ref = ref_alleles[i]
            alts = alt_alleles[i][0]  # Assuming biallelic sites
            pos = positions[i]
            logging.debug(f"Processing variant at position {pos}: ref={ref}, alt={alts}")

            if pos >= gene_length:
                logging.warning(f"Position {pos} exceeds gene length {gene_length}. Skipping this variant.")
                continue

            for j in range(num_samples):
                genotype = genotypes[i, j]
                logging.debug(f"Sample {samples[j]} genotype at position {pos}: {genotype}")

                if genotype[0] != -1:
                    sequences[j][pos] = ref if genotype[0] == 0 else alts
                if genotype[1] != -1:
                    sequences[j][pos] = ref if genotype[1] == 0 else alts

        # Convert sequences to strings
        sequence_strings = [''.join(seq) for seq in sequences.values()]

        # Manually write the Nexus file
        with open(nexus_file, 'w') as f:
            f.write("#NEXUS\n\n")
            f.write("BEGIN DATA;\n")
            f.write(f"    DIMENSIONS NTAX={num_samples} NCHAR={gene_length};\n")
            f.write("    FORMAT DATATYPE=DNA MISSING=N GAP=-;\n")
            f.write("    MATRIX\n")

            # Create unique population labels
            populations = [chr(65 + (species_id - 1) * 2), chr(66 + (species_id - 1) * 2)]  # ['A', 'B'] for species 1, ['C', 'D'] for species 2, etc.

            for idx, sequence_str in enumerate(sequence_strings):
                population = populations[0] if idx < 10 else populations[1]
                sample_name = f"Population{population}_ind{idx + 1 if idx < 10 else idx - 9}"
                f.write(f"{sample_name} {sequence_str}\n")

            f.write("    ;\n")
            f.write("END;\n")

        logging.info(f"Written Nexus file: {nexus_file}")
        return True  # Successful conversion
    except Exception as e:
        logging.error(f"Error converting VCF to Nexus: {e}")
        return False  # Indicate failure to trigger a retry

# Generate a random reference sequence
sequence_length = 1000
reference_sequence = "".join(random.choice("ACGT") for _ in range(sequence_length))

# Define gene_length
gene_length = 1000  # Length of the single gene

import os

# Base directory paths (relative to where the script is run)
base_dir = os.path.dirname(os.path.abspath(__file__))
base_input_dir = os.path.join(base_dir, 'yaml')
base_output_dir = os.path.join(base_dir, 'results')

# File paths for all species
species_files = [
    ("model_species1.yaml", "output_data_species1.vcf", "output_data_species1_genealogy.svg", "output_data_species1.nex", 1),
    ("model_species2.yaml", "output_data_species2.vcf", "output_data_species2_genealogy.svg", "output_data_species2.nex", 2),
    ("model_species3.yaml", "output_data_species3.vcf", "output_data_species3_genealogy.svg", "output_data_species3.nex", 3),
    ("model_species4.yaml", "output_data_species4.vcf", "output_data_species4_genealogy.svg", "output_data_species4.nex", 4),
    ("model_species5.yaml", "output_data_species5.vcf", "output_data_species5_genealogy.svg", "output_data_species5.nex", 5),
    ("model_species6.yaml", "output_data_species6.vcf", "output_data_species6_genealogy.svg", "output_data_species6.nex", 6),
]

# Iterate over all species files and run the simulations
for model_yaml, vcf_file, svg_file, nexus_file, species_id in species_files:
    demes_file_path = os.path.join(base_input_dir, model_yaml)
    output_vcf_path = os.path.join(base_output_dir, vcf_file)
    output_svg_path = os.path.join(base_output_dir, svg_file)
    output_nexus_path = os.path.join(base_output_dir, nexus_file)


    # Repeat until valid genotype data is generated
    while True:
        # Simulate and visualize for the single gene
        tree_sequence = simulate_data(demes_file_path, output_vcf_path, gene_length=gene_length)
        visualize_genealogy(tree_sequence, output_svg_path)

        # Convert the VCF file to Nexus format including invariant sites for the single gene
        if vcf_to_nexus(output_vcf_path, output_nexus_path, gene_length, reference_sequence, species_id):
            logging.info(f"Completed all steps for {model_yaml}")
            break  # Exit loop if successful
        else:
            logging.warning(f"Retrying simulation for {model_yaml} due to lack of genotype data.")


import shutil
import os

# Copy results to SIM_RUN_DIR if set
sim_run_dir = os.environ.get("SIM_RUN_DIR")
if sim_run_dir:
    os.makedirs(sim_run_dir, exist_ok=True)
    for _, vcf, svg, nex, _ in species_files:
        for file in [vcf, svg, nex]:
            src = os.path.join(base_output_dir, file)
            dst = os.path.join(sim_run_dir, file)
            try:
                shutil.copyfile(src, dst)
                logging.info(f"Copied {src} to {dst}")
            except Exception as e:
                logging.warning(f"Failed to copy {src} to {dst}: {e}")
else:
    logging.warning("SIM_RUN_DIR not set — skipping copy step")