
#' Save projects working directory
projwd <- getwd()
projwd

#' Set working directory to tex files folder
setwd("figs")

#' List tex files and convert to dvi
texfiles <- list.files(pattern = "tex")
lapply(texfiles, tools::texi2dvi, clean = T, pdf = T)

#' Convert pdf files to eps
pdfiles <- list.files(pattern = "pdf")

#' Function to wrap dvips
#' Downloaded and installed xpdf Mac 32/64-bit from 
#' http://www.xpdfreader.com/download.html
pdf2psr <- function(x) {
  x <- sub("\\.pdf", "", x)
  system(sprintf("pdftops -eps %s.pdf %s.eps", x, x))
}

lapply(pdfiles, pdf2psr)

#' Set working directory back to project wd
setwd(projwd)
projwd
