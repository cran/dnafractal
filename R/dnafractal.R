#' @name dnafractal
#' @title Generates a Fractal Image of a DNA Sequence
#' @description The function takes a DNA sequence up to 100 Kbp, a start point, an end point in the sequence,
#' dot size and dot color and draws a fractal image of the sequence. The fractal starts in
#' the center of the canvas. The image is drawn by moving base by base along the sequence
#' and dropping a midpoint between the actual point and the corner designated by the actual
#' base.
#'
#' Version 0.0.2.
#' Author: Dr. Matthew Cserhati
#' Email: matthew.cserhati@cui.edu
#' May 9, 2025
#'
#' @importFrom grDevices dev.off jpeg
#' @importFrom graphics lines
#' @import stringr DescTools
#'
#' @param mx a DNA sequence
#' @param start the starting position in the sequence to be fractalized
#' @param end the ending position in the sequence to be fractalized
#' @param cex the size of the dots in the fractal image
#' @param dotcol the color of the fractal image dots
#' @return nil
#'
#' @references Jeffrey, H. J. (1990) Chaos game representation of gene structure. Nucleic Acids Research 18(8):2163-70.
#' @references Hill, K. A., Schisler, N. J., and Singh, S. M. (1992) Chaos game representation of coding regions of human globin genes and alcohol dehydrogenase genes of phylogenetically divergent species. Journal of Molecular Evolution 35:261-269.
#' @references Löchel, H. F., and Heider, D. (2021) Chaos game representation and its applications in bioinformatics. Computational and Structural Biotechnology Journal 19(2021): 6263-6271.
#'
#' @examples
#' dnafractal(human_mitogenome)
#' dnafractal(human_mitogenome,start=100,end=1000)
#' dnafractal(human_mitogenome,cex=1,dotcol="blue")
#'@export
utils::globalVariables(c("exit"))
dnafractal <- function(seq,start=1,end=str_length(seq),cex=0.1,dotcol="black") {
  if (length(seq) > 100000) {
    seq <- substr(seq,1,100000)
  }

  bpx <- c("A"=-100,"C"=100,"G"=100,"T"=-100)
  bpy <- c("A"=100,"C"=100,"G"=-100,"T"=-100)

  xlim <- 130
  ylim <- 130
  Canvas(xlim = xlim, ylim = ylim, main = NULL, xpd = par("xpd"),
         mar=c(1,1,1,1), asp = 1, bg = par("bg"), usrbg = "white")

  X=0
  Y=0
  Xcoords = c(X)
  Ycoords = c(Y)

  for (i in start:end) {
    b2 <- substr(seq,i,i)
    if (b2 %in% names(bpx)) {
      xcoord <- bpx[b2][[1]]
      ycoord <- bpy[b2][[1]]
      X <- (X+xcoord)/2
      Y <- (Y+ycoord)/2
      Xcoords <- c(Xcoords,X)
      Ycoords <- c(Ycoords,Y)
    }
  }

  h <- plot(1, type = "n", xlab = "", ylab = "", xlim = c(-xlim, xlim), ylim = c(-ylim, ylim), xaxt="n", yaxt="n")
  points(x = Xcoords, y = Ycoords, pch = 19, col = dotcol, cex = cex)
  text(125, 125, "C", cex=2.5, col="blue")
  text(-125, 125, "A", cex=2.5, col="red")
  text(125, -125, "G", cex=2.5, col="yellow")
  text(-125, -125, "T", cex=2.5, col="green")
  l <- 1.05
  x <- c(-100,100,100,-100,-100)
  y <- c(100,100,-100,-100,100)
  lines(x,y,col="red")
  h
}

