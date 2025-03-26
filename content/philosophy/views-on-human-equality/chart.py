import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib import rcParams
import matplotlib.patches as patches

def create_pinker_chart():
    # ----------------------------
    # 1. Global Style & Figure Setup
    # ----------------------------
    plt.style.use("ggplot")  # As a base style

    # Font settings; change "Martian Mono" if needed
    rcParams["font.family"] = "Futura"
    rcParams["font.size"] = 12
    rcParams["axes.linewidth"] = 1

    # Create figure with a teal/greenish background
    fig, ax = plt.subplots(figsize=(10, 8))
    fig.patch.set_facecolor("none")  # teal/greenish figure background

    # Adjust margins so there's teal around the white chart
    fig.subplots_adjust(top=0.85, bottom=0.05, left=0.21, right=0.95)

    # ----------------------------
    # 2. White Chart (Axes) Area
    # ----------------------------
    ax.set_facecolor("white")
    ax.set_xlim(0, 2)
    ax.set_ylim(0, 2)

    # Hide default spines & ticks for a clean look
    for spine in ["top", "right", "bottom", "left"]:
        ax.spines[spine].set_visible(False)
    ax.set_xticks([])
    ax.set_yticks([])

    # ----------------------------
    # 3. Title
    # ----------------------------
    plt.title(
        "Views of Human Equality",
        fontsize=20, fontweight="bold", color="#333333", pad=40
    )

    # ----------------------------
    # 4. Quadrant Lines
    # ----------------------------
    # Dashed lines to divide quadrants
    ax.axvline(x=1, color="#444444", linewidth=1.5, linestyle="--")
    ax.axhline(y=1, color="#444444", linewidth=1.5, linestyle="--")

    # ----------------------------
    # 5. Filled Quadrants with Colors and Labels
    # ----------------------------
    # Define colors for each quadrant:
    # Top-left, Top-right, Bottom-right, Bottom-left (clockwise order)
    quadrant_colors = [ "#bed4fa", "#e6c7ea", "#f7c5c2", "#a6dfe3"]

    #quadrant_colors = ["rgb(255, 231, 255)","rgb(255, 231, 255)","rgb(255, 231, 255)","rgb(255, 231, 255)"]


    # Define quadrant positions: (x, y, width, height)
    quadrants = [
        (0, 1, 1, 1),  # Top-left
        (1, 1, 1, 1),  # Top-right
        (1, 0, 1, 1),  # Bottom-right
        (0, 0, 1, 1)   # Bottom-left
    ]

    # Define quadrant labels for each area
    quadrant_labels = [
        "Social Constructionist",  # Top-left
        "Enlightenment Liberal",          # Top-right
        "Fascist",                  # Bottom-right
        "Woke"                      # Bottom-left
    ]

    # Add filled rectangles and centered text labels
    for (rect, color, label) in zip(quadrants, quadrant_colors, quadrant_labels):
        x, y, width, height = rect
        rect_patch = patches.Rectangle((x, y), width, height,
                                       facecolor=color, edgecolor="none", alpha=0.95)
        ax.add_patch(rect_patch)
        # Place the label in the center of the rectangle
        ax.text(x + width/2, y + height/2, label,
                ha="center", va="center",
                fontsize=18, color="#333333", fontweight="bold")

    # ----------------------------
    # 6. Column & Row Headers
    # ----------------------------
    # Top headers (for columns)
    ax.text(0.5, 2.03, "People are the same", ha="center", va="bottom",
            fontsize=13, color="#333333", fontweight="bold")
    ax.text(1.5, 2.03, "People are different", ha="center", va="bottom",
            fontsize=13, color="#333333", fontweight="bold")

    # Left headers (for rows)
    ax.text(-0.02, 1.5, "We should be\nequal before the law",
            ha="right", va="center", fontsize=13, color="#333333", fontweight="bold")
    ax.text(-0.02, 0.5, "We should NOT be\nequal before the law",
            ha="right", va="center", fontsize=13, color="#333333", fontweight="bold")
    
    # ----------------------------
    # 7. Attribution at the Bottom
    # ----------------------------
    fig.text(0.5, 0.02, "jonathanwarden.com",
             ha="center", va="center", fontsize=10, color="#333333")


#    plt.show()
    plt.savefig("chart.png", transparent=True)

if __name__ == "__main__":
    create_pinker_chart()

