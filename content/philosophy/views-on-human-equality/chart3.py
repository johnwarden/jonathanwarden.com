import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib import rcParams
import matplotlib.patches as patches

def create_pinker_chart():
    # ----------------------------
    # 1. Global Style & Figure Setup
    # ----------------------------
    plt.style.use("ggplot")  # Base style

    # Font settings (adjust to fonts available on your system)
    rcParams["font.family"] = "Futura"
    rcParams["font.size"] = 14
    rcParams["axes.linewidth"] = 1

    # Create a 16:9 figure (width=12, height=6.75)
    fig, ax = plt.subplots(figsize=(12, 6.75))
    # Transparent figure background
    fig.patch.set_facecolor("none")

    # Reduce margins to minimize empty space
    # (top=0.75 => more room for the chart area vs. the title)
    # (bottom=0.08 => ensures attribution is visible but not too large)
    fig.subplots_adjust(top=0.75, bottom=0.08, left=0.15, right=0.95)

    # ----------------------------
    # 2. White Chart (Axes) Area
    # ----------------------------
    ax.set_facecolor("white")
    ax.set_xlim(0, 2)
    ax.set_ylim(0, 2)

    # Hide default spines & ticks
    for spine in ["top", "right", "bottom", "left"]:
        ax.spines[spine].set_visible(False)
    ax.set_xticks([])
    ax.set_yticks([])

    # ----------------------------
    # 3. Title
    # ----------------------------
    # Larger font for better visibility in a tweet
    plt.title(
        "Views of Human Equality",
        fontsize=24, fontweight="bold", color="#333333", pad=20
    )

    # ----------------------------
    # 4. Quadrant Lines (dashed)
    # ----------------------------
    ax.axvline(x=1, color="#444444", linewidth=1.5, linestyle="--")
    ax.axhline(y=1, color="#444444", linewidth=1.5, linestyle="--")

    # ----------------------------
    # 5. Filled Quadrants & Labels
    # ----------------------------
    quadrant_colors = ["#bed4fa", "#e6c7ea", "#f7c5c2", "#a6dfe3"]
    quadrants = [
        (0, 1, 1, 1),  # Top-left
        (1, 1, 1, 1),  # Top-right
        (1, 0, 1, 1),  # Bottom-right
        (0, 0, 1, 1)   # Bottom-left
    ]
    quadrant_labels = [
        "Social Constructionist",  # Top-left
        "Enlightenment Liberal",   # Top-right
        "Fascist",                 # Bottom-right
        "Woke"                     # Bottom-left
    ]

    # Slightly larger quadrant label font size for visibility
    label_font_size = 20

    for (rect, color, label) in zip(quadrants, quadrant_colors, quadrant_labels):
        x, y, w, h = rect
        rect_patch = patches.Rectangle((x, y), w, h,
                                       facecolor=color, edgecolor="none", alpha=0.95)
        ax.add_patch(rect_patch)
        ax.text(x + w/2, y + h/2, label,
                ha="center", va="center",
                fontsize=label_font_size, color="#333333", fontweight="bold")

    # ----------------------------
    # 6. Column & Row Headers
    # ----------------------------
    # Slightly larger axis label font size
    axis_label_size = 16

    ax.text(0.5, 2.03, "People are the same", ha="center", va="bottom",
            fontsize=axis_label_size, color="#333333", fontweight="bold")
    ax.text(1.5, 2.03, "People are different", ha="center", va="bottom",
            fontsize=axis_label_size, color="#333333", fontweight="bold")

    ax.text(-0.02, 1.5, "We should be\nequal before the law",
            ha="right", va="center", fontsize=axis_label_size, color="#333333", fontweight="bold")
    ax.text(-0.02, 0.5, "We should NOT be\nequal before the law",
            ha="right", va="center", fontsize=axis_label_size, color="#333333", fontweight="bold")

    # ----------------------------
    # 7. Attribution at the Bottom
    # ----------------------------
    # Bump to 12 for better visibility
    fig.text(0.5, 0.02, "jonathanwarden.com",
             ha="center", va="center", fontsize=12, color="#333333")

    # Save the figure as PNG with transparency
    # Increase dpi for sharper text; 200 is a good middle ground
    plt.savefig("chart.png", transparent=True, dpi=200)

if __name__ == "__main__":
    create_pinker_chart()

