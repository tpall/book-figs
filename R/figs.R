

# Fig 1 -------------------------------------------------------------------

library(VGAM)

## Variant A
Scale <- 0.3
curve(drayleigh(x, Scale), 
      xlim = c(0, 1.2), 
      ylab = NA, 
      xlab = NA,
      main = "Rayleigh Density",
      axes = FALSE)
qq <- qrayleigh(0.2, scale = Scale)
axis(side = 1, at = qq, labels = parse(text = "x[alpha]"), pos = 0)
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, drayleigh(qq, scale = Scale), lty = 3, type = "h")
abline(h = 0, lty = 1)
coord.x <- c(0, seq(0, qq, 0.01), qq)
coord.y <- c(0, drayleigh(seq(0, qq, 0.01), scale = Scale), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA)
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
      main = "Rayleigh Density",
      axes = FALSE)
qq <- qrayleigh(0.2, scale = Scale)
axis(side = 1, at = qq, labels = expression(italic("x")), pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, drayleigh(qq, scale = Scale), lty = 3, type = "h")
abline(h = 0, lty = 1)
coord.x <- c(0, seq(0, qq, 0.01), qq)
coord.y <- c(0, drayleigh(seq(0, qq, 0.01), scale = Scale), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA)
text(c(0.12, 0.45), y = 0.4, c(expression(italic("0.2")), expression(italic("0.8"))))
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
qq <- qnorm(c(0.025, 0.975))
axis(side = 1, at = qq, labels = c(-1.96, 1.96), pos = 0) # Kas tavalise fondiga või italicus?
axis(side = 2, labels = NA, lwd.ticks = 0)
lines(qq, dnorm(qq), lty = 3, type = "h")
abline(h = 0, lty = 1)
coord.x <- c(-3, seq(-3, qq[1], 0.01), qq[1])
coord.y <- c(0, dnorm(seq(-3, qq[1], 0.01)), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA)
coord.x <- c(qq[2], seq(qq[2], 3, 0.01), 3)
coord.y <- c(0, dnorm(seq(qq[2], 3, 0.01)), 0)
polygon(coord.x, coord.y, col = 'gray90', border = NA)
text(0, y = 0.18, 0.95)
loc <- par("usr")
text(loc[1], loc[4], "f(x)", pos = 2, xpd = T)
text(loc[2], loc[3], "x", pos = 4, xpd = T)




