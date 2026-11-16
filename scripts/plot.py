"""Plots rounds to converge against network size from the benchmark results.

Reads results/results.csv (written by scripts/benchmark.sh) and writes docs/convergence.png: one panel per
algorithm, one line per topology, log-log axes, the median over the seeds of each size. Only runs that
converged and passed --verify are plotted; the others are listed on stderr, and the note under the figure
says how many gossip runs were left out because the rumor stopped spreading before every node had heard it.

    uv run --no-project --with matplotlib python scripts/plot.py [results.csv] [convergence.png]
"""

import csv
import statistics
import sys
from collections import defaultdict
from pathlib import Path

import matplotlib
import matplotlib.pyplot as plt
from matplotlib.path import Path as LinePath
from matplotlib.ticker import FuncFormatter, LogLocator, NullFormatter
from matplotlib.transforms import Bbox

matplotlib.use("Agg")  # draws to a file, without a display

ROOT = Path(__file__).resolve().parent.parent
TOPOLOGIES = ["full", "3D", "line", "imp3D"]
LABELS = {"full": "full", "3D": "3D grid", "line": "line", "imp3D": "imperfect 3D"}
ALGORITHMS = [("gossip", "Gossip: every node has heard the rumor"), ("push-sum", "Push-sum: every node has converged")]

# Colorblind-checked categorical colors in a fixed order, plus a marker shape per topology so that color is
# never the only cue.
COLORS = {"full": "#2a78d6", "3D": "#eb6834", "line": "#1baf7a", "imp3D": "#eda100"}
MARKERS = {"full": "o", "3D": "s", "line": "^", "imp3D": "D"}

SURFACE = "#fcfcfb"
INK = "#0b0b0b"
INK_SECONDARY = "#52514e"
INK_MUTED = "#898781"
GRID = "#e1e0d9"
AXIS = "#c3c2b7"


def compact(value, _position=None):
    """Tick labels such as 10, 100, 1K, 10K, 1M."""
    for limit, suffix in ((1e9, "B"), (1e6, "M"), (1e3, "K")):
        if value >= limit:
            return f"{value / limit:g}{suffix}"
    return f"{value:g}"


def load(path):
    """Returns {algorithm: {topology: [(nodes, median rounds), ...]}} from the runs that converged and passed
    --verify, and the number of gossip runs in which the rumor stopped spreading before every node had heard it."""
    rounds = defaultdict(list)
    skipped = []
    stopped = 0
    with open(path, newline="") as f:
        for number, row in enumerate(csv.DictReader(f), start=2):
            # csv.DictReader fills missing columns with None and puts extra ones under the key None.
            if None in row or None in row.values():
                skipped.append(f"line {number} does not have the columns of the header")
                continue
            run = f"{row['algorithm']} {row['topology']} {row['nodes_requested']} seed {row['seed']}"
            if row["status"] != "converged":
                skipped.append(f"{run}: {row['status']}")
                if row["status"] in ("cut-off", "died-out"):
                    stopped += 1
            elif row["verify"] == "failed":
                skipped.append(f"{run}: converged, but failed --verify")
            elif row["verify"] != "passed":
                skipped.append(f"{run}: converged, but was not run with --verify")
            else:
                try:
                    rounds[(row["algorithm"], row["topology"], int(row["nodes"]))].append(int(row["rounds"]))
                except ValueError:
                    skipped.append(f"line {number}: nodes or rounds is not a number")
    for line in skipped:
        print(f"not plotted: {line}", file=sys.stderr)
    series = defaultdict(lambda: defaultdict(list))
    for (algorithm, topology, nodes), values in sorted(rounds.items()):
        series[algorithm][topology].append((nodes, statistics.median(values)))
    return series, stopped


def style_axes(ax):
    ax.set_facecolor(SURFACE)
    ax.set_xscale("log")
    ax.set_yscale("log")
    for axis in (ax.xaxis, ax.yaxis):
        axis.set_major_locator(LogLocator(base=10))
        axis.set_major_formatter(FuncFormatter(compact))
        axis.set_minor_formatter(NullFormatter())
    ax.grid(True, which="major", color=GRID, linewidth=0.8)
    ax.set_axisbelow(True)
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for side in ("left", "bottom"):
        ax.spines[side].set_color(AXIS)
        ax.spines[side].set_linewidth(0.8)
    ax.tick_params(colors=INK_MUTED, labelcolor=INK_SECONDARY, labelsize=11.5, which="both")
    ax.set_xlabel("Nodes", color=INK_SECONDARY, fontsize=12.5)
    ax.set_ylabel("Rounds", color=INK_SECONDARY, fontsize=12.5)


def label_line_end(ax, name, line):
    """Names a line at its last point, in text ink. Returns the name and the line."""
    x, y = line.get_xydata()[-1]
    label = ax.annotate(
        name, (x, y), xytext=(8, 0), textcoords="offset points", va="center", fontsize=11, color=INK_SECONDARY
    )
    return label, line


def remove_clashing_labels(fig, axes, labels):
    """Removes the line-end names that overlap another line or its markers, or the tick labels and axis labels of
    either panel, and then each name that overlaps a name kept before it. The legend names those lines, since
    moving a name away from its line would detach it. `labels` holds (name, line) pairs."""
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    boxes = [label.get_window_extent(renderer).expanded(1.0, 1.15) for label, _ in labels]
    texts = [
        text.get_window_extent(renderer)
        for ax in axes
        for text in ax.get_xticklabels() + ax.get_yticklabels() + [ax.xaxis.label, ax.yaxis.label, ax.title]
        if text.get_visible() and text.get_text()
    ]
    # Every plotted line in display coordinates, with a box around each of its markers
    lines = []
    for ax in axes:
        for line in ax.get_lines():
            points = line.get_transform().transform(line.get_xydata())
            half = (line.get_markersize() + line.get_markeredgewidth()) / 2 * fig.dpi / 72
            markers = [Bbox.from_extents(x - half, y - half, x + half, y + half) for x, y in points]
            lines.append((line, LinePath(points), markers))

    def blocked(i):
        box, own = boxes[i], labels[i][1]
        return any(box.overlaps(text) for text in texts) or any(
            path.intersects_bbox(box, filled=False) or any(box.overlaps(marker) for marker in markers)
            for line, path, markers in lines
            if line is not own
        )

    kept = []
    for i, (label, _) in enumerate(labels):
        if blocked(i) or any(boxes[i].overlaps(boxes[j]) for j in kept):
            label.remove()
        else:
            kept.append(i)


def main():
    source = Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT / "results" / "results.csv"
    target = Path(sys.argv[2]) if len(sys.argv) > 2 else ROOT / "docs" / "convergence.png"
    series, stopped = load(source)

    plt.rcParams["font.family"] = "sans-serif"
    fig, axes = plt.subplots(1, 2, figsize=(11, 4.8), dpi=150, facecolor=SURFACE)
    labels = []
    for ax, (algorithm, title) in zip(axes, ALGORITHMS):
        style_axes(ax)
        ax.set_title(title, color=INK, fontsize=13.5, loc="left", pad=10)
        for topology in TOPOLOGIES:
            points = series.get(algorithm, {}).get(topology)
            if not points:
                continue
            xs, ys = zip(*points)
            (line,) = ax.plot(
                xs,
                ys,
                color=COLORS[topology],
                linewidth=1.8,
                marker=MARKERS[topology],
                markersize=8,
                markeredgecolor=SURFACE,
                markeredgewidth=1.4,
                solid_capstyle="round",
                solid_joinstyle="round",
                label=LABELS[topology],
            )
            labels.append(label_line_end(ax, LABELS[topology], line))
        ax.margins(x=0.14, y=0.08)

    # One legend entry per topology that appears in either panel, in the order of TOPOLOGIES
    entries = {}
    for ax in axes:
        for handle, label in zip(*ax.get_legend_handles_labels()):
            entries.setdefault(label, handle)
    order = [LABELS[topology] for topology in TOPOLOGIES if LABELS[topology] in entries]
    legend = fig.legend(
        [entries[label] for label in order],
        order,
        loc="upper center",
        ncol=4,
        frameon=False,
        fontsize=12,
        bbox_to_anchor=(0.5, 1.0),
    )
    for text in legend.get_texts():
        text.set_color(INK_SECONDARY)
    note = (
        "Median over seeds of the runs that converged and passed --verify. 3D and imperfect 3D sizes are perfect cubes."
    )
    if stopped:
        runs = "run" if stopped == 1 else "runs"
        note += (
            f"\nLeft out: {stopped} gossip {runs} in which the rumor stopped spreading before every node had heard it."
        )
    caption = fig.text(0.5, 0.01, note, ha="center", va="bottom", color=INK_MUTED, fontsize=10.5, linespacing=1.5)
    # The line-end names are in place before the layout, so that it leaves room for them between the panels.
    fig.canvas.draw()
    caption_top = caption.get_window_extent().y1 / fig.bbox.height
    fig.tight_layout(rect=(0, caption_top + 0.01, 1, 0.92))
    remove_clashing_labels(fig, axes, labels)
    target.parent.mkdir(parents=True, exist_ok=True)
    # The names at the line ends may reach past the axes; the tight bounding box keeps them in the image.
    fig.savefig(target, facecolor=SURFACE, bbox_inches="tight", pad_inches=0.15)
    print(f"wrote {target}", file=sys.stderr)


if __name__ == "__main__":
    main()
