
# @knitr setup

fillcolor <- "gray90"

# @knitr Fig1 -------------------------------------------------------------------

library(VGAM)

## Variant A
Scale <- 0.3
curve(drayleigh(x, Scale), 
      xlim = c(0, 1.2), 
      ylab = NA, 
      xlab = NA,
      axes = FALSE)
qq <- qrayleigh(0.2, scale = Scale)
axis(side = 1, at = qq, labels = parse(text = "italic(x[alpha])"), pos = 0)
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, drayleigh(qq, scale = Scale), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
coord.x <- c(0, seq(0, qq, 0.01), qq)
coord.y <- c(0, drayleigh(seq(0, qq, 0.01), scale = Scale), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA)
text(c(0.12, 0.45), y = 0.4, c(expression(alpha), expression(1-alpha)))
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)

## Variant B
Scale <- 0.3
curve(drayleigh(x, Scale), 
      xlim = c(0, 1.2), 
      ylab = NA, 
      xlab = NA,
      axes = FALSE)
qq <- qrayleigh(0.2, scale = Scale)
axis(side = 1, at = qq, labels = expression(italic("x")), pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, drayleigh(qq, scale = Scale), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
coord.x <- c(0, seq(0, qq, 0.01), qq)
coord.y <- c(0, drayleigh(seq(0, qq, 0.01), scale = Scale), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA)
text(c(0.12, 0.45), y = 0.4, c(0.2, 0.8))
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)


# Fig 2 -------------------------------------------------------------------

curve(dnorm(x), 
      xlim = c(-3, 3), 
      ylab = NA, 
      xlab = NA,
      main = "Normal Density",
      axes = FALSE)
qq <- round(qnorm(c(0.025, 0.975)), 2)
axis(side = 1, at = qq, pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dnorm(qq), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
coord.x <- c(-3, seq(-3, qq[1], 0.01), qq[1])
coord.y <- c(0, dnorm(seq(-3, qq[1], 0.01)), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
coord.x <- c(qq[2], seq(qq[2], 3, 0.01), 3)
coord.y <- c(0, dnorm(seq(qq[2], 3, 0.01)), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA, density = NA)
text(0, y = dnorm(0.5)/2, 0.95)
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)

# Fig 3 -------------------------------------------------------------------

# library(latex2exp)
curve(dnorm(x), 
      xlim = c(-3, 3), 
      ylab = NA, 
      xlab = NA,
      main = "Normal Density",
      axes = FALSE)
qq <- round(qnorm(c(0.025, 0.975)), 2)
axis(side = 1, 
     at = qq, 
     labels =  c(expression(italic("-t")), expression(italic("t"))), 
     pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dnorm(qq), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
coord.x <- c(-3, seq(-3, qq[1], 0.01), qq[1])
coord.y <- c(0, dnorm(seq(-3, qq[1], 0.01)), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
coord.x <- c(qq[2], seq(qq[2], 3, 0.01), 3)
coord.y <- c(0, dnorm(seq(qq[2], 3, 0.01)), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA, density = NA)
text(qnorm(c(0.025, 0.975)), y = 0.021, labels = parse(text = "H[t]"), pos = c(2,4)) # U+1D4D7
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)

## Right
curve(dnorm(x), 
      xlim = c(-3, 3), 
      ylab = NA, 
      xlab = NA,
      main = "Normal Density",
      axes = FALSE)
qq <- round(qnorm(0.95), 2)
axis(side = 1, 
     at = qq, 
     labels =  c(expression(italic("t"))), 
     pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dnorm(qq), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
## code below needs customisation
coord.x <- c(qq, seq(qq, 3, 0.01), 3)
coord.y <- c(0, dnorm(seq(qq, 3, 0.01)), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA, density = NA)
text(qnorm(0.95), y = 0.022, labels = parse(text = "H[t]"), pos = 4) # U+1D4D7
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)


## Left
curve(dnorm(x), 
      xlim = c(-3, 3), 
      ylab = NA, 
      xlab = NA,
      main = "Normal Density",
      axes = FALSE)
qq <- round(qnorm(0.05), 2)
axis(side = 1, 
     at = qq, 
     labels =  c(expression(italic("-t"))), 
     pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dnorm(qq), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
coord.x <- c(-3, seq(-3, qq, 0.01), qq)
coord.y <- c(0, dnorm(seq(-3, qq, 0.01)), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
text(qnorm(0.05), y = 0.022, labels = parse(text = "H[t]"), pos = 2) # U+1D4D7
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)

# Fig 4 -------------------------------------------------------------------

## left
df <- 4
ncp <- 4
curve(dchisq(x, df = df, ncp = ncp), 
      xlim = c(0, 30),
      ylab = NA, 
      xlab = NA,
      main = "Chi-Squared Density",
      axes = FALSE)
qq <- round(qchisq(0.05, df = df, ncp = ncp), 2)
axis(side = 1, 
     at = qq, 
     labels =  c(expression(italic("t"))), 
     pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dchisq(qq, df = df, ncp = ncp), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
coord.x <- c(0, seq(0, qq, 0.01), qq)
coord.y <- c(0, dchisq(seq(0, qq, 0.01), df = df, ncp = ncp), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)

## right
curve(dchisq(x, df = df, ncp = ncp), 
      xlim = c(0, 30),
      ylab = NA, 
      xlab = NA,
      main = "Chi-Squared Density",
      axes = FALSE)
qq <- round(qchisq(0.95, df = df, ncp = ncp), 2)
axis(side = 1, 
     at = qq, 
     labels =  c(expression(italic("t"))), 
     pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dchisq(qq, df = df, ncp = ncp), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
coord.x <- c(qq, seq(qq, 30, 0.01), 30)
coord.y <- c(0, dchisq(seq(qq, 30, 0.01), df = df, ncp = ncp), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)


# Fig 5 -------------------------------------------------------------------

## Two-tailed
curve(dnorm(x), 
      xlim = c(-3, 3), 
      ylab = NA, 
      xlab = NA,
      main = "Normal Density",
      axes = FALSE)
qq <- round(qnorm(c(0.025, 0.975)), 2)
axis(side = 1, 
     at = qq, 
     labels =  parse(text = c("-t[crit]", "t[crit]")), 
     pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dnorm(qq), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
coord.x <- c(-3, seq(-3, qq[1], 0.01), qq[1])
coord.y <- c(0, dnorm(seq(-3, qq[1], 0.01)), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
coord.x <- c(qq[2], seq(qq[2], 3, 0.01), 3)
coord.y <- c(0, dnorm(seq(qq[2], 3, 0.01)), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA, density = NA)
text(c(-2.1, 2.1), y = 0.021, labels = parse(text = "H[1]")) # U+1D4D7
text(qnorm(0.5), y = dnorm(0.5)/2, labels = parse(text = "H[0]")) # U+1D4D7
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)

## Right tailed
curve(dnorm(x), 
      xlim = c(-3, 3), 
      ylab = NA, 
      xlab = NA,
      main = "Normal Density",
      axes = FALSE)
qq <- round(qnorm(0.95), 2)
axis(side = 1, 
     at = qq, 
     labels =  parse(text = c("t[crit]")), 
     pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dnorm(qq), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
coord.x <- c(qq, seq(qq, 3, 0.01), 3)
coord.y <- c(0, dnorm(seq(qq, 3, 0.01)), 0)
polygon(coord.x, coord.y, col = fillcolor, border = NA, density = NA)
text(qnorm(0.95), y = 0.023, labels = parse(text = "H[1]"), pos = 4) # U+1D4D7
text(qnorm(0.5), y = dnorm(0.5)/2, labels = parse(text = "H[0]")) # U+1D4D7
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)

## Left tailed
curve(dnorm(x), 
      xlim = c(-3, 3), 
      ylab = NA, 
      xlab = NA,
      main = "Normal Density",
      axes = FALSE)
qq <- round(qnorm(0.05), 2)
axis(side = 1, 
     at = qq, 
     labels =  parse(text = c("-t[crit]")), 
     pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dnorm(qq), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
coord.x <- c(-3, seq(-3, qq, 0.01), qq)
coord.y <- c(0, dnorm(seq(-3, qq, 0.01)), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
text(qnorm(0.05), y = 0.022, labels = parse(text = "H[t]"), pos = 2) # U+1D4D7
text(qnorm(0.5), y = dnorm(0.5)/2, labels = parse(text = "H[0]")) # U+1D4D7
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)

# Fig 6 -------------------------------------------------------------------

df <- 5
ncp <- 0
curve(dchisq(x, df = df, ncp = ncp), 
      xlim = c(0, 30),
      ylab = NA, 
      xlab = NA,
      main = parse(text = "Chi^2*(5)"),
      axes = FALSE)
qq <- round(qchisq(c(0.708, 0.95), df = df, ncp = ncp), 2)
axis(side = 1, 
     at = qq, 
     labels =  qq, 
     pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dchisq(qq, df = df, ncp = ncp), lty = 3, type = "h", lwd = 2)
abline(h = 0, lty = 1)
coord.x <- c(qq, seq(qq, 30, 0.01), 30)
coord.y <- c(0, dchisq(seq(qq, 30, 0.01), df = df, ncp = ncp), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA, density = NA)
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)
