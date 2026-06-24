import numpy as np
import matplotlib.pyplot as plt
from matplotlib.collections import PolyCollection
import matplotlib.gridspec as gridspec

plt.rcParams['text.usetex'] = True
gap = 0.1

# -----------------------------
# 1. Simulate epidemic curve
# -----------------------------
np.random.seed(1)

days = np.arange(0, 60)

curve = (
    40 * np.exp(-((days - 25) ** 2) / (2 * 9 ** 2))
    + 15 * np.exp(-((days - 45) ** 2) / (2 * 6 ** 2))
)

curve += np.random.normal(0, 2, size=len(days))
curve = np.clip(curve, 0, None)

selected_days = [1, 10, 20, 40, 53]

# -----------------------------
# 2. Precompute histograms
# -----------------------------
delay_bins = np.arange(0, 12)
histograms = {}

for d in selected_days:
    delays = np.random.gamma(shape=2.5, scale=1.2, size=200)
    hist, bins = np.histogram(delays, bins=delay_bins)
    histograms[d] = (hist, bins)

z_max = np.max(curve) + 15

# -----------------------------
# 3. Create Figure & GridSpec
# -----------------------------
fig = plt.figure(figsize=(16, 8))

# Added 2 rows with a height ratio to make the top row (2D plots) shorter
gs = gridspec.GridSpec(3, 3, width_ratios=[1.2, 1.2, 2], height_ratios=[0.25, 0.5, 0.25])

# Assign 2D subplots ONLY to the middle row (row 0)
ax1 = fig.add_subplot(gs[1, 0])
ax2 = fig.add_subplot(gs[1, 1])
# Assign 3D subplot to span BOTH rows (row :) to keep its full height
ax3 = fig.add_subplot(gs[:, 2], projection='3d')

# ==========================================
# PANEL 1: Epidemic Process (2D)
# ==========================================
ax1.plot(days, curve, linewidth=3, color="#5F7E62")
ax1.fill_between(days, curve, color="#5F7E62", alpha=0.35)

ax1.set_xlabel(r'Time ($t$)', fontsize=12)
ax1.set_ylabel(r'Number of observations (counts)', fontsize=12)
ax1.set_title(r'Epidemic process ($N_t$)', fontsize=16, y=1.15)
ax1.set_xlim(0, 60)
ax1.set_ylim(0, z_max)

ax1.spines['right'].set_visible(False)
ax1.spines['top'].set_visible(False)

# ==========================================
# PANEL 2: Delay Process (2D)
# ==========================================
d_chosen = 20
hist, bins = histograms[d_chosen]

ax2.bar(
    bins[:-1] + gap / 2,
    hist / 10,
    width=0.8 - gap,
    align='edge',
    color="#B85348",
    edgecolor="black",
    alpha=0.9
)

ax2.set_xlabel(r'Reporting delay ($d$)', fontsize=12)
ax2.set_ylabel(r'Number of observations (counts)', fontsize=12)
ax2.set_title(r'Reporting delay process ($D_t$)', fontsize=16, y=1.15)
ax2.set_xlim(0, 11)

ax2.set_ylim(0, np.max(hist/10) * 1.3)
ax2.spines['right'].set_visible(False)
ax2.spines['top'].set_visible(False)

# ==========================================
# PANEL 3: Observed Coupled Process (3D)
# ==========================================
ax3.plot(days, curve, zs=0, zdir='y', linewidth=3, color="#5F7E62")

verts3 = [(0,0)] + list(zip(days, curve)) + [(days[-1],0)]
poly3 = PolyCollection([verts3], alpha=0.35, facecolor="#5F7E62")
ax3.add_collection3d(poly3, zs=0, zdir='y')

for d in selected_days:
    base_cases = curve[d]
    hist, bins = histograms[d]
    for i in range(len(hist)):
        ax3.bar3d(
            x=days[d] - 0.4,
            y=bins[i] + gap / 2,
            z=base_cases,
            dx=0.8 - gap,
            dy=0.8 - gap,
            dz=hist[i] / 10,
            shade=False,
            color="#B85348",
            edgecolor="black",
            alpha=0.9
        )

ax3.scatter(
    days[selected_days],
    np.zeros(len(selected_days)),
    curve[selected_days],
    color="#e78b7f",
    depthshade=False,
    s=100,
)

ax3.set_xlabel(r'Time ($t$)', fontsize=12, labelpad=10)
ax3.set_ylabel(r'Reporting delay ($d$)', fontsize=12, labelpad=10)
ax3.set_zlabel(r'Case count', fontsize=12, labelpad=10)

# Changed y=1.12 to elevate and align the 3D title with the 2D panel titles
ax3.set_title("Observed coupled process", fontsize=16, y=1)

ax3.view_init(elev=18, azim=-55)
ax3.grid(False)
ax3.set_proj_type('persp', focal_length=0.5)
ax3.set_xlim(0, 60)
ax3.set_ylim(0, 11)
ax3.set_zlim(0, curve.max()*1.05)
ax3.set_box_aspect((1.2, 0.8, 1))


# -----------------------------
# 4. Add + and = Annotations
# -----------------------------
# Shifted y up to 0.65 to keep signs centered with the shorter 2D panels
fig.text(0.31, 0.5, '+', fontsize=45, ha='center', va='center', fontweight='bold')
fig.text(0.59, 0.5, '=', fontsize=45, ha='center', va='center', fontweight='bold')

# Set explicit right margin (0.91) so 3D labels on the right side never cut off
plt.subplots_adjust(wspace=0.5)
plt.savefig("nowcasting_panels_resized.pdf")
plt.savefig("nowcasting_panels_resized.png", transparent=True, dpi=750)
plt.savefig("nowcasting_panels_resized.svg", transparent=True)
plt.show()