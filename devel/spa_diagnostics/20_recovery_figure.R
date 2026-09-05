# Figure: parameter recovery + the finite-horizon confounding mechanism.
a <- readRDS("devel/spa_diagnostics/param_recovery.rds")
agg <- do.call(rbind, by(a, list(a$scen, a$p_true), function(s) data.frame(
  scen = s$scen[1], p_true = s$p_true[1], Gbar_h = s$Gbar_h[1],
  mean = mean(s$phat), sd = sd(s$phat))))

png("devel/spa_diagnostics/recovery_figure.png", 1100, 520, res = 120)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1), cex.lab = 1.05)
col <- c(A = "#1b7837", B = "#2166ac", C = "#b2182b")

# panel 1: recovery
plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = expression(p[true]),
     ylab = expression(hat(p)~"(mean "%+-%" sd, R=12)"),
     main = "Recovery under the exact interval model")
abline(0, 1, col = "grey60", lty = 2)
for (sc in c("A", "B", "C")) { s <- agg[agg$scen == sc, ]; s <- s[order(s$p_true), ]
  arrows(s$p_true, s$mean - s$sd, s$p_true, s$mean + s$sd, angle = 90, code = 3,
         length = 0.03, col = col[sc])
  lines(s$p_true, s$mean, col = col[sc], type = "b", pch = 19) }
legend("topleft", bty = "n", pch = 19, col = col, title = "retraction tail",
       legend = c("A short  (Gbar_h ~ 0)", "B moderate (0.035)", "C long  (0.80)"))
text(0.62, 0.10, "p_true=0.60  ->  p_hat=0.15", col = col["C"], cex = 0.85)

# panel 2: the mechanism -- likelihood sees r(a) = p + (1-p) Gbar(a), not p
pp <- seq(0, 1, 0.01)
plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = expression(p),
     ylab = expression("retained fraction at horizon  r(a) = p + (1-p)"~bar(G)[C]*"(a)"),
     main = "Why p is confounded")
for (i in seq_along(c(0, 0.035, 0.80))) { g <- c(0, 0.035, 0.80)[i]
  lines(pp, pp + (1 - pp) * g, col = col[i], lwd = 2) }
abline(h = 0.95, col = "grey40", lty = 3)
text(0.05, 0.965, "observed r ~ 0.95", col = "grey30", pos = 4, cex = 0.85)
# the same observed r=0.95 is consistent with very different p across tails
pts <- sapply(c(0, 0.035, 0.80), function(g) (0.95 - g) / (1 - g))
points(pts, rep(0.95, 3), pch = 19, col = col)
legend("bottomright", bty = "n", lwd = 2, col = col,
       legend = c("short: r=p, so p=0.95", "moderate: p=0.948", "long: p=0.75"))
dev.off()
cat("wrote devel/spa_diagnostics/recovery_figure.png\n")
