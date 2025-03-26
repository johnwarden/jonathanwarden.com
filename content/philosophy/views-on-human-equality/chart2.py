import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib import rcParams
import matplotlib.patches as patches

def create_pinker_chart():
    # 1. Global Style & Figure Setup
    plt.style.use("ggplot")  # As a base style

    # Font settings; change "Futura" if needed
    rcParams["font.family"] = "Futura"
    rcParams["font.size"] = 12
    rcParams["axes.linewidth"] = 1

    # Create a 16:9 figure (12 inches wide, 6.75 inches tall)
    fig, ax = plt.subplots(figsize=(12, 6.75))
    # Transparent figure background
    fig.patch.set_facecolor("none")

    # Adjust margins so there's some space for titles and labels
    fig.subplots_adjust(top=0.85, bottom=0.1, left=0.21, right=0.95)

    # 2. White Chart (Axes) Area
    ax.set_facecolor("white")
    ax.set_xlim(0, 2)
    ax.set_ylim(0, 2)

    # Hide default spines & ticks for a clean look
    for spine in ["top", "right", "bottom", "left"]:
        ax.spines[spine].set_visible(False)
    ax.set_xticks([])
    ax.set_yticks([])

    # 3. Title
    plt.title(
        "Views of Human Equality",
        fontsize=20, fontweight="bold", color="#333333", pad=40
    )

    # 4. Quadrant Lines (dashed)
    ax.axvline(x=1, color="#444444", linewidth=1.5, linestyle="--")
    ax.axhline(y=1, color="#444444", linewidth=1.5, linestyle="--")

    # 5. Filled Quadrants & Labels
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

    for (rect, color, label) in zip(quadrants, quadrant_colors, quadrant_labels):
        x, y, w, h = rect
        rect_patch = patches.Rectangle((x, y), w, h,
                                       facecolor=color, edgecolor="none", alpha=0.95)
        ax.add_patch(rect_patch)
        ax.text(x + w/2, y + h/2, label,
                ha="center", va="center",
                fontsize=18, color="#333333", fontweight="bold")

    # 6. Column & Row Headers
    ax.text(0.5, 2.03, "People are the same", ha="center", va="bottom",
            fontsize=13, color="#333333", fontweight="bold")
    ax.text(1.5, 2.03, "People are different", ha="center", va="bottom",
            fontsize=13, color="#333333", fontweight="bold")

    ax.text(-0.02, 1.5, "We should be\nequal before the law",
            ha="right", va="center", fontsize=13, color="#333333", fontweight="bold")
    ax.text(-0.02, 0.5, "We should NOT be\nequal before the law",
            ha="right", va="center", fontsize=13, color="#333333", fontweight="bold")

    # 7. Attribution at the Bottom
    fig.text(0.5, 0.02, "jonathanwarden.com",
             ha="center", va="center", fontsize=10, color="#333333")

    # Save the figure to a PNG with transparency
    # Increase dpi if you want higher resolution
    plt.savefig("chart.png", transparent=True, dpi=150)

if __name__ == "__main__":
    create_pinker_chart()

