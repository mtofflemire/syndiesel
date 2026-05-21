# === demographic_split_plot.py ===
# Visualize a simple split model with Demes + msprime

import msprime
import demes
import demesdraw
import matplotlib.pyplot as plt

# === Define model using demes.Builder ===
builder = demes.Builder(
    description="Simple split model: ancestral → two populations",
    time_units="generations"
)

# Parameters
Ne = 10000
split_time = 1000

# === Ancestral population ===
builder.add_deme(
    name="Ancestral",
    epochs=[
        {
            "start_time": float("inf"),
            "end_time": split_time,
            "start_size": Ne,
            "end_size": Ne,
            "size_function": "constant"
        }
    ]
)

# === Pop1 ===
builder.add_deme(
    name="Pop1",
    start_time=split_time,
    ancestors=["Ancestral"],
    epochs=[
        {
            "start_time": split_time,
            "end_time": 0,
            "start_size": Ne,
            "end_size": Ne,
            "size_function": "constant"
        }
    ]
)

# === Pop2 ===
builder.add_deme(
    name="Pop2",
    start_time=split_time,
    ancestors=["Ancestral"],
    epochs=[
        {
            "start_time": split_time,
            "end_time": 0,
            "start_size": Ne,
            "end_size": Ne,
            "size_function": "constant"
        }
    ]
)

# === Finalize Graph ===
graph = builder.resolve()

# === Visualization ===
fig, ax = plt.subplots(figsize=(6, 4))
demesdraw.tubes(graph, ax=ax)

# Add a title
ax.set_title("Ancestral Split Model\n(Ne=10,000; Split at 1000 generations ago)", fontsize=12)

# Show plot
plt.tight_layout()
plt.show()

# Optional: save
# fig.savefig("/Users/michaeltofflemire/Desktop/split_model_plot.pdf")