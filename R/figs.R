
# @knitr setup

fillcolor <- "gray90"
library(tikzDevice)

# @knitr Fig1 -------------------------------------------------------------------

library(VGAM)

## Variant A
Scale <- 0.3
tikz("figs/Taavi1a.tex", standAlone = TRUE, width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2,4))
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
text(c(0.12, 0.45), y = 0.4, c("$\\alpha$", "$1-\\alpha$"))
axis(side = 1, at = qq, labels = "$x_\\alpha$", pos = 0)
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, drayleigh(qq, scale = Scale), lty = 3, type = "h")
abline(h = 0, lty = 1)
loc <- par("usr")
text(loc[1], loc[4], labels = "$f(x)$", pos = 2, xpd = T)
text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
dev.off()

## Variant B
tikz("figs/Taavi1b.tex", standAlone = T, width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2,4))
plot.new()
plot.window(xlim = c(0, 1.2), ylim = c(0, 2))
qq <- qrayleigh(0.2, scale = Scale)
axis(side = 1, at = qq, labels = "$x_{0.2}$", pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, drayleigh(qq, scale = Scale), lty = 3, type = "h")
abline(h = 0, lty = 1)
coord.x <- c(0, seq(0, qq, 0.01), qq)
coord.y <- c(0, drayleigh(seq(0, qq, 0.01), scale = Scale), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA)
curve(drayleigh(x, Scale), 
      xlim = c(0, 1.2), 
      ylab = NA, 
      xlab = NA,
      add = T)
text(c(0.12, 0.45), y = 0.4, c("$0.2$", "$0.8$"))
loc <- par("usr")
text(loc[1], loc[4], "$f(x)$", pos = 2, xpd = T)
text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
dev.off()

# @knitr Fig2 -------------------------------------------------------------------

tikz("figs/Taavi2.tex", standAlone = TRUE, width = 4, height = 2.76, pointsize = 12)
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
text(0, dnorm(0)+0.02, "$f(x)$", pos = 2, xpd = T)
text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
dev.off()

# @knitr Fig3 -------------------------------------------------------------------

# two-tailed
tikz("figs/Taavi3_two-tailed.tex", standAlone = TRUE, width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2,4))
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
     labels = "$p$", 
     pos = c(2,4)) # U+1D4D7
axis(side = 1, 
     at = qq, 
     labels =  c("$-t$", "$t$"), 
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
text(0, dnorm(0)+0.02, "$f(x)$", pos = 2, xpd = T)
text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
dev.off()

## Right
tikz("figs/Taavi3_right-tailed.tex", standAlone = TRUE, width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2,4))
plot.new()
plot.window(xlim = c(-3, 3), ylim = c(0, .4))
qq <- round(qnorm(0.95), 2)
coord.x <- c(qq, seq(qq, 3, 0.01), 3)
coord.y <- c(0, dnorm(seq(qq, 3, 0.01)), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA, density = NA)
text(qnorm(0.95), y = 0.035, labels = "$p$", pos = 4) # U+1D4D7
axis(side = 1, 
     at = qq, 
     labels = "$t$", 
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
text(0, dnorm(0)+0.02, "$f(x)$", pos = 2, xpd = T)
text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
dev.off()

## Left
tikz("figs/Taavi3_left-tailed.tex", standAlone = T, width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2,4))
plot.new()
plot.window(xlim = c(-3, 3), ylim = c(0, .4))
qq <- round(qnorm(0.05), 2)
coord.x <- c(-3, seq(-3, qq, 0.01), qq)
coord.y <- c(0, dnorm(seq(-3, qq, 0.01)), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
text(qnorm(0.05), y = 0.035, labels = "$p$", pos = 2) # U+1D4D7
axis(side = 1, 
     at = qq, 
     labels = "$-t$", 
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
text(0, dnorm(0)+0.02, "$f(x)$", pos = 2, xpd = T)
text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
dev.off()

# @knitr Fig4 -------------------------------------------------------------------

## left
tikz("figs/Taavi4_left.tex", standAlone = T, width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2,4))
plot.new()
plot.window(xlim = c(0, 30), ylim = c(0, .1))
df <- 4
ncp <- 4
qq <- round(qchisq(0.05, df = df, ncp = ncp), 2)
coord.x <- c(0, seq(0, qq, 0.01), qq)
coord.y <- c(0, dchisq(seq(0, qq, 0.01), df = df, ncp = ncp), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
text(qchisq(0.01, df = df, ncp = ncp), y = 0.018, labels = "$p$", pos = 1)
axis(side = 1, 
     at = qq, 
     labels = "$t$", 
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
text(loc[1], loc[4], "$f(x)$", pos = 2, xpd = T)
text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
dev.off()

## right
tikz("figs/Taavi4_right.tex", standAlone = T, width = 4, height = 2.76, pointsize = 12)
par(mar = rep(2,4))
plot.new()
plot.window(xlim = c(0, 30), ylim = c(0, .1))
qq <- round(qchisq(0.95, df = df, ncp = ncp), 2)
coord.x <- c(qq, seq(qq, 30, 0.01), 30)
coord.y <- c(0, dchisq(seq(qq, 30, 0.01), df = df, ncp = ncp), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
text(qchisq(0.96, df = df, ncp = ncp), y = 0.005, labels = "$p$")
axis(side = 1, 
     at = qq, 
     labels =  "$t$", 
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
text(loc[1], loc[4], "$f(x)$", pos = 2, xpd = T)
text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
dev.off()

# # @knitr Fig5 -------------------------------------------------------------------
# 
# ## Two-tailed
# tikz("figs/Taavi5_two-tailed.tex", standAlone = T, width = 4, height = 2.76, pointsize = 12)
# curve(dnorm(x), 
#       xlim = c(-3, 3), 
#       ylab = NA, 
#       xlab = NA,
#       axes = FALSE)
# qq <- round(qnorm(c(0.025, 0.975)), 2)
# axis(side = 1, 
#      at = qq, 
#      labels =  parse(text = c("-t[crit]", "t[crit]")), 
#      pos = 0) 
# axis(side = 2, labels = NA, lwd.ticks = 0)
# lines(qq, dnorm(qq), lty = 3, type = "h", lwd = 2)
# abline(h = 0, lty = 1)
# coord.x <- c(-3, seq(-3, qq[1], 0.01), qq[1])
# coord.y <- c(0, dnorm(seq(-3, qq[1], 0.01)), 0)
# polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
# coord.x <- c(qq[2], seq(qq[2], 3, 0.01), 3)
# coord.y <- c(0, dnorm(seq(qq[2], 3, 0.01)), 0)
# polygon(coord.x, coord.y, col = fillcolor, border = NA, density = NA)
# text(c(-2.1, 2.1), y = 0.021, labels = parse(text = "H[1]")) # U+1D4D7
# text(qnorm(0.5), y = dnorm(0.5)/2, labels = parse(text = "H[0]")) # U+1D4D7
# loc <- par("usr")
# text(loc[1], loc[4], "$f(x)$", pos = 2, xpd = T)
# text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
# dev.off()
# 
# tools::texi2dvi("figs/Taavi5_two-tailed.tex", clean = T)
# 
# ## Right tailed
# tikz("figs/Taavi5_right-tailed.tex", standAlone = T, width = 4, height = 2.76, pointsize = 12)
# curve(dnorm(x), 
#       xlim = c(-3, 3), 
#       ylab = NA, 
#       xlab = NA,
#       axes = FALSE)
# qq <- round(qnorm(0.95), 2)
# axis(side = 1, 
#      at = qq, 
#      labels =  parse(text = c("t[crit]")), 
#      pos = 0)
# axis(side = 2, labels = NA, lwd.ticks = 0)
# lines(qq, dnorm(qq), lty = 3, type = "h", lwd = 2)
# abline(h = 0, lty = 1)
# coord.x <- c(qq, seq(qq, 3, 0.01), 3)
# coord.y <- c(0, dnorm(seq(qq, 3, 0.01)), 0)
# polygon(coord.x, coord.y, col = fillcolor, border = NA, density = NA)
# text(qnorm(0.95), y = 0.023, labels = parse(text = "H[1]"), pos = 4) # U+1D4D7
# text(qnorm(0.5), y = dnorm(0.5)/2, labels = parse(text = "H[0]")) # U+1D4D7
# loc <- par("usr")
# text(loc[1], loc[4], "$f(x)$", pos = 2, xpd = T)
# text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
# dev.off()
# 
# tools::texi2dvi("figs/Taavi5_right-tailed.tex", clean = T)
# 
# ## Left tailed
# tikz("figs/Taavi5_left-tailed.eps", standAlone = T, width = 4, height = 2.76, pointsize = 12)
# curve(dnorm(x), 
#       xlim = c(-3, 3), 
#       ylab = NA, 
#       xlab = NA,
#       axes = FALSE)
# qq <- round(qnorm(0.05), 2)
# axis(side = 1, 
#      at = qq, 
#      labels =  parse(text = c("-t[crit]")), 
#      pos = 0)
# axis(side = 2, labels = NA, lwd.ticks = 0)
# lines(qq, dnorm(qq), lty = 3, type = "h", lwd = 2)
# abline(h = 0, lty = 1)
# coord.x <- c(-3, seq(-3, qq, 0.01), qq)
# coord.y <- c(0, dnorm(seq(-3, qq, 0.01)), 0)
# polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
# text(qnorm(0.05), y = 0.022, labels = parse(text = "H[t]"), pos = 2) # U+1D4D7
# text(qnorm(0.5), y = dnorm(0.5)/2, labels = parse(text = "H[0]")) # U+1D4D7
# loc <- par("usr")
# text(loc[1], loc[4], "$f(x)$", pos = 2, xpd = T)
# text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
# dev.off()
# 
# tools::texi2dvi("figs/Taavi5_left-tailed.eps", clean = T)

# @knitr Fig6 -------------------------------------------------------------------

tikz("figs/Taavi6.tex", standAlone = T, width = 5, height = 2.76, pointsize = 12)
df <- 5
ncp <- 0
par(mar = rep(2,4))
plot.new()
plot.window(xlim = c(0, 20), ylim = c(0, .15))
qq <- round(qchisq(c(0.708, 0.95), df = df, ncp = ncp), 2)
coord.x <- c(qq[1], seq(qq[1], 20, 0.01), 20)
coord.y <- c(0, dchisq(seq(qq[1], 20, 0.01), df = df, ncp = ncp), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
text(qchisq(mean(c(0.708, 0.95)), df = df, ncp = ncp)+0.8, 
     y = dchisq(mean(c(0.708, 0.95)), df = df, ncp = ncp)/3.2, 
     labels = round(1-0.708, 2))
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
text(loc[1], loc[4], "$f(x)$", pos = 2, xpd = T)
text(loc[2], loc[3], "$x$", pos = 4, xpd = T)
dev.off()

# tools::texi2dvi("figs/Taavi6.tex", clean = T)
