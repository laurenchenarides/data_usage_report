import matplotlib.pyplot as plt

# Define process steps
steps = [
    "Create Seed Corpus",
    "Dataset Extraction",
    "Disambiguation Process",
    "Comparison Across Databases"
]

# Descriptions for each step
descriptions = [
    "Identify dataset name variations and aliases",
    "Extract mentions using different search methods:\n- Kaggle models\n- Full-text search\n- References only",
    "Pre-process and clean data for each citation database:\n- Deduplication\n- Standardization",
    "Compare extracted data across databases using:\n- Fuzzy matching\n- Cross-referencing"
]

# Define figure
fig, ax = plt.subplots(figsize=(10, 6))

# Create process flowchart
for i, (step, desc) in enumerate(zip(steps, descriptions)):
    ax.text(0.5, 1 - i * 0.3, step, fontsize=12, ha="center", va="center",
            bbox=dict(boxstyle="round,pad=0.3", edgecolor="black", facecolor="lightblue"))
    ax.text(0.5, 1 - i * 0.3 - 0.07, desc, fontsize=10, ha="center", va="center")

# Arrows between steps
for i in range(len(steps) - 1):
    ax.annotate("", xy=(0.5, 1 - (i + 1) * 0.3 + 0.05), xytext=(0.5, 1 - i * 0.3 - 0.05),
                arrowprops=dict(arrowstyle="->", lw=1.5))

# Remove axes
ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.axis("off")

# Show the figure
plt.title("Citation Database Analysis Workflow", fontsize=14, weight="bold")
plt.show()

