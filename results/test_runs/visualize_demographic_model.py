import msprime
import demes
import demesdraw
import matplotlib.pyplot as plt

# Load the graph from YAML
graph = demes.load("demographic_split_plot.yaml")

# Print demography (text output)
demography = msprime.Demography.from_demes(graph)
demography.debug()

# Plot the demography
fig, ax = plt.subplots(figsize=(6, 4))
demesdraw.tubes(graph, ax=ax)
ax.set_title("Asymmetric Migration Example", fontsize=12)
plt.tight_layout()
plt.show()