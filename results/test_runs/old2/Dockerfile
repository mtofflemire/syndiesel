# Use Miniconda3 image
FROM continuumio/miniconda3:latest

# Set the working directory
WORKDIR /usr/src/app

# Copy your project files into the container
COPY . .

# Disable GPG checking and install system dependencies
RUN apt-get update --allow-insecure-repositories -o Acquire::AllowInsecureRepositories=true -o 
Acquire::AllowDowngradeToInsecureRepositories=true && \
    apt-get install -y --allow-unauthenticated apt-transport-https ca-certificates gnupg2 software-properties-common && \
    apt-get update --allow-unauthenticated && \
    apt-get install -y --allow-unauthenticated build-essential cmake git && \
    rm -rf /var/lib/apt/lists/*

# Clone and build ecoevolity
RUN git clone https://github.com/phyletica/ecoevolity.git
RUN cd ecoevolity && ./build.sh
RUN cp ./ecoevolity/build/bin/* /usr/local/bin/

# Create the conda environment with Python 3.9
RUN conda create -n ecoevolity-env -y python=3.9

# Install the necessary dependencies using conda
RUN conda run -n ecoevolity-env conda install -c conda-forge -y msprime demes ipython pyyaml

# Install scikit-allel using pip instead of conda
RUN conda run -n ecoevolity-env pip install scikit-allel

# Install pycoevolity using pip from the GitHub repository (using HTTPS)
RUN conda run -n ecoevolity-env pip install git+https://github.com/phyletica/pycoevolity.git

# Ensure the script files are executable
RUN chmod +x run_ecoevolity.sh

# Set the entry point to activate the conda environment and run the main script
ENTRYPOINT [ "conda", "run", "-n", "ecoevolity-env", "/bin/bash", "-c", "./run_ecoevolity.sh \"$@\"", "--" ]

