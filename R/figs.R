
# @knitr setup

fillcolor <- "gray90"

# @knitr Fig1 -------------------------------------------------------------------

library(VGAM)

## Variant A

svg("figs/Taavi1a.svg", width = 4, height = 2.76, pointsize = 12)
Scale <- 0.3
par(mar = rep(2, 4))
plot.new()
plot.window(xlim = c(0, 1.2), ylim = c(0, 2))
qq <- qrayleigh(0.2, scale = Scale)
coord.x <- c(0, seq(0, qq, 0.01), qq)
coord.y <- c(0, drayleigh(seq(0, qq, 0.01), scale = Scale), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA)
curve(drayleigh(x, Scale), 
      xlim = c(0, 1.2),
      ylab = NA, 
      xlab = NA,
      add = T)
text(c(0.12, 0.45), y = 0.4, c(expression(alpha), expression(1 - alpha)))
axis(side = 1, at = qq, labels = expression(x[alpha]), pos = 0)
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, drayleigh(qq, scale = Scale), lty = 3, type = "h")
abline(h = 0, lty = 1)
loc <- par("usr")
text(loc[1], loc[4], labels = expression(f(x)), pos = 2, xpd = T)
text(loc[2], loc[3], expression(x), pos = 4, xpd = T)
dev.off()

## Variant B
svg("figs/Taavi1b.svg", width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2, 4))
plot.new()
plot.window(xlim = c(0, 1.2), ylim = c(0, 2))
qq <- qrayleigh(0.2, scale = Scale)
coord.x <- c(0, seq(0, qq, 0.01), qq)
coord.y <- c(0, drayleigh(seq(0, qq, 0.01), scale = Scale), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA)
curve(drayleigh(x, Scale), 
      xlim = c(0, 1.2), 
      ylab = NA, 
      xlab = NA,
      add = T)
axis(side = 1, at = qq, labels = expression(x[0.2]), pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, drayleigh(qq, scale = Scale), lty = 3, type = "h")
abline(h = 0, lty = 1)
text(c(0.12, 0.45), y = 0.4, c(0.2, 0.8))
loc <- par("usr")
text(loc[1], loc[4], expression(f(x)), pos = 2, xpd = T)
text(loc[2], loc[3], expression(x), pos = 4, xpd = T)
dev.off()

# @knitr Fig2 -------------------------------------------------------------------

svg("figs/Taavi2.svg", width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2, 4))
plot.new()
plot.window(xlim = c(-3, 3), ylim = c(0, .4))
qq <- round(qnorm(c(0.025, 0.975)), 2)
coord.x <- c(-3, seq(-3, qq[1], 0.01), qq[1])
coord.y <- c(0, dnorm(seq(-3, qq[1], 0.01)), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
coord.x <- c(qq[2], seq(qq[2], 3, 0.01), 3)
coord.y <- c(0, dnorm(seq(qq[2], 3, 0.01)), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA, density = NA)
text(0, y = dnorm(0.5)/2, 0.95)
curve(dnorm(x), 
      xlim = c(-3, 3), 
      ylab = NA, 
      xlab = NA,
      add = T)
axis(side = 1, at = qq, pos = 0)
lines(qq, dnorm(qq), lty = 3, type = "h")
arrows(0, 0, 0, 5, lwd = 1, length = 0.15)
abline(h = 0, lty = 1)
loc <- par("usr")
text(0, dnorm(0) + 0.02, expression(f(x)), pos = 2, xpd = T)
text(loc[2], loc[3], expression(x), pos = 4, xpd = T)
dev.off()

# @knitr Fig3 -------------------------------------------------------------------

# two-tailed
svg("figs/Taavi3_two-tailed.svg", width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2, 4))
plot.new()
plot.window(xlim = c(-3, 3), ylim = c(0, .4))
qq <- round(qnorm(c(0.025, 0.975)), 2)
coord.x <- c(-3, seq(-3, qq[1], 0.01), qq[1])
coord.y <- c(0, dnorm(seq(-3, qq[1], 0.01)), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
coord.x <- c(qq[2], seq(qq[2], 3, 0.01), 3)
coord.y <- c(0, dnorm(seq(qq[2], 3, 0.01)), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA, density = NA)
text(qnorm(c(0.025, 0.975)), y = 0.017, 
     labels = expression(p), 
     pos = c(2, 4)) # U+1D4D7
axis(side = 1, 
     at = qq, 
     labels =  c(expression(-t), expression(t)), 
     pos = 0,
     font = 3)
lines(qq, dnorm(qq), lty = 3, type = "h")
arrows(0, 0, 0, 5, lwd = 1, length = 0.15)
abline(h = 0, lty = 1)
curve(dnorm(x), 
      xlim = c(-3, 3), 
      ylab = NA, 
      xlab = NA,
      add = T)
loc <- par("usr")
text(0, dnorm(0) + 0.02, expression(f(x)), pos = 2, xpd = T)
text(loc[2], loc[3], expression(x), pos = 4, xpd = T)
dev.off()

## Right
svg("figs/Taavi3_right-tailed.svg", width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2, 4))
plot.new()
plot.window(xlim = c(-3, 3), ylim = c(0, .4))
qq <- round(qnorm(0.95), 2)
coord.x <- c(qq, seq(qq, 3, 0.01), 3)
coord.y <- c(0, dnorm(seq(qq, 3, 0.01)), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA, density = NA)
text(qnorm(0.95), y = 0.035, labels = expression(p), pos = 4) # U+1D4D7
axis(side = 1, 
     at = qq, 
     labels = expression(t), 
     pos = 0)
lines(qq, dnorm(qq), lty = 3, type = "h")
arrows(0, 0, 0, 5, lwd = 1, length = 0.15)
abline(h = 0, lty = 1)
curve(dnorm(x), 
      xlim = c(-3, 3), 
      ylab = NA, 
      xlab = NA,
      add = T)
loc <- par("usr")
text(0, dnorm(0) + 0.02, expression(f(x)), pos = 2, xpd = T)
text(loc[2], loc[3], expression(x), pos = 4, xpd = T)
dev.off()

## Left
svg("figs/Taavi3_left-tailed.svg", width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2, 4))
plot.new()
plot.window(xlim = c(-3, 3), ylim = c(0, .4))
qq <- round(qnorm(0.05), 2)
coord.x <- c(-3, seq(-3, qq, 0.01), qq)
coord.y <- c(0, dnorm(seq(-3, qq, 0.01)), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
text(qnorm(0.05), y = 0.035, labels = expression(p), pos = 2) # U+1D4D7
axis(side = 1, 
     at = qq, 
     labels = expression(-t), 
     pos = 0)
lines(qq, dnorm(qq), lty = 3, type = "h")
arrows(0, 0, 0, 5, lwd = 1, length = 0.15)
abline(h = 0, lty = 1)
curve(dnorm(x), 
      xlim = c(-3, 3), 
      ylab = NA, 
      xlab = NA,
      add = T)
loc <- par("usr")
text(0, dnorm(0) + 0.02, expression(f(x)), pos = 2, xpd = T)
text(loc[2], loc[3], expression(x), pos = 4, xpd = T)
dev.off()

# @knitr Fig4 -------------------------------------------------------------------

## left
svg("figs/Taavi4_left.svg", width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2, 4))
plot.new()
plot.window(xlim = c(0, 30), ylim = c(0, .1))
df <- 4
ncp <- 4
qq <- round(qchisq(0.05, df = df, ncp = ncp), 2)
coord.x <- c(0, seq(0, qq, 0.01), qq)
coord.y <- c(0, dchisq(seq(0, qq, 0.01), df = df, ncp = ncp), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
text(qchisq(0.01, df = df, ncp = ncp), y = 0.018, labels = expression(p), pos = 1)
axis(side = 1, 
     at = qq, 
     labels = expression(t), 
     pos = 0)
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dchisq(qq, df = df, ncp = ncp), lty = 3, type = "h")
abline(h = 0, lty = 1)
curve(dchisq(x, df = df, ncp = ncp), 
      xlim = c(0, 30),
      ylab = NA, 
      xlab = NA,
      add = T)
loc <- par("usr")
text(loc[1], loc[4], expression(f(x)), pos = 2, xpd = T)
text(loc[2], loc[3], expression(x), pos = 4, xpd = T)
dev.off()

## right
svg("figs/Taavi4_right.svg", width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2, 4))
plot.new()
plot.window(xlim = c(0, 30), ylim = c(0, .1))
qq <- round(qchisq(0.95, df = df, ncp = ncp), 2)
coord.x <- c(qq, seq(qq, 30, 0.01), 30)
coord.y <- c(0, dchisq(seq(qq, 30, 0.01), df = df, ncp = ncp), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
text(qchisq(0.96, df = df, ncp = ncp), y = 0.005, labels = expression(p))
axis(side = 1, 
     at = qq, 
     labels = expression(t), 
     pos = 0) 
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dchisq(qq, df = df, ncp = ncp), lty = 3, type = "h")
abline(h = 0, lty = 1)
curve(dchisq(x, df = df, ncp = ncp), 
      xlim = c(0, 30),
      ylab = NA, 
      xlab = NA,
      add = T)
loc <- par("usr")
text(loc[1], loc[4], expression(f(x)), pos = 2, xpd = T)
text(loc[2], loc[3], expression(x), pos = 4, xpd = T)
dev.off()

# @knitr Fig6 -------------------------------------------------------------------

svg("figs/Taavi6.svg", width = 5, height = 2.76, pointsize = 12)
df <- 5
ncp <- 0
par(mar = rep(2, 4))
plot.new()
plot.window(xlim = c(0, 20), ylim = c(0, .15))
qq <- round(qchisq(c(0.708, 0.95), df = df, ncp = ncp), 2)
coord.x <- c(qq[1], seq(qq[1], 20, 0.01), 20)
coord.y <- c(0, dchisq(seq(qq[1], 20, 0.01), df = df, ncp = ncp), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
text(qchisq(mean(c(0.708, 0.95)), df = df, ncp = ncp) + 0.8, 
     y = dchisq(mean(c(0.708, 0.95)), df = df, ncp = ncp)/3.2, 
     labels = round(1 - 0.708, 2))
axis(side = 1, 
     at = qq, 
     labels =  qq, 
     pos = 0)
axis(side = 2, labels = NA, lwd.ticks = 0)
abline(h = 0, lty = 1)
lines(qq, dchisq(qq, df = df, ncp = ncp), lty = 3, type = "h")
curve(dchisq(x, df = df, ncp = ncp), 
      xlim = c(0, 20),
      ylab = NA, 
      xlab = NA,
      add = T)
loc <- par("usr")
text(loc[1], loc[4], expression(f(x)), pos = 2, xpd = T)
text(loc[2], loc[3], expression(x), pos = 4, xpd = T)
dev.off()
