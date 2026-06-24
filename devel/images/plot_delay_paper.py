import numpy as np
import matplotlib.pyplot as plt
from matplotlib.collections import PolyCollection
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
# 2. Create 3D figure
# -----------------------------
fig = plt.figure(figsize=(10,7))
ax = fig.add_subplot(111, projection='3d')

# -----------------------------
# 3. Epidemic curve
# -----------------------------
ax.plot(days, curve, zs=0, zdir='x', linewidth=3, color="#94b594")

# -----------------------------
# 4. Shaded area under curve
# -----------------------------
verts = [(0,0)] + list(zip(days, curve)) + [(days[-1],0)]
poly = PolyCollection([verts], alpha=0.35, facecolor="#94b594")
ax.add_collection3d(poly, zs=0, zdir='x')

# -----------------------------
# 5. Reporting delay histograms
# -----------------------------
delay_bins = np.arange(0,12)

for d in selected_days:
    base_cases = curve[d]
    delays = np.random.gamma(shape=2.5, scale=1.2, size=200)
    hist, bins = np.histogram(delays, bins=delay_bins)
    for i in range(len(hist)):
        ax.bar3d(
            bins[i] + gap / 2,
            days[d] - 0.4,
            base_cases,
            0.8 - gap,
            0.8 - gap,
            hist[i] / 10,
            shade=False,
            color="#b75347",
            edgecolor="black",  # outline bars
            alpha=0.9
        )

# -----------------------------
# 6. Highlight curve points
# -----------------------------
ax.scatter(
    np.zeros(len(selected_days)),
    days[selected_days] - 1,
    curve[selected_days],
    color="#edc775",
    depthshade=False,
    s=100,
)

# -----------------------------
# 7. Labels
# -----------------------------
ax.set_xlabel(r'Reporting delay process ($D_t$)', fontsize=12)
ax.set_ylabel(r'Time ($t$)', fontsize=12)
ax.set_zlabel(r'Epidemic process ($N_t$)', fontsize=12)

ax.view_init(elev=12, azim=-122)
ax.grid(False)
ax.set_box_aspect((0.5, 2, 1))

plt.tight_layout()
plt.savefig("nowcasting_idea.pdf", bbox_inches='tight')
plt.show()
