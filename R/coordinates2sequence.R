#' @name coordinates2sequence
#' @title Generates a DNA Sequence Based on an X and Y Coordinate over several iterations
#' @description The function takes an X and Y coordinate and a number of bases to be represented in the DNA sequence.
#' The function starts at the provided X and Y coordinates and then works backward, calculating every preceding base
#' in the DNA sequence that led to that particular point in the fractal. Based on the actual X, Y point's coordinate,
#' the base pair corresponding to that coordinate is inferred, and the DNA string will be built up and returned as the
#' product of the function.
#'
#' Version 0.0.2.
#' Author: Dr. Matthew Cserhati
#' Email: matthew.cserhati@cui.edu
#' May 9, 2025
#'
#' @param xstart the starting x coordinate in the fractal image
#' @param ystart the starting y coordinate in the fractal image
#' @param n the number of bases to be calculated in the DNA sequence
#' @return The DNA sequence
#'
#' @references Jeffrey, H. J. (1990) Chaos game representation of gene structure. Nucleic Acids Research 18(8):2163-70.
#' @references Hill, K. A., Schisler, N. J., and Singh, S. M. (1992) Chaos game representation of coding regions of human globin genes and alcohol dehydrogenase genes of phylogenetically divergent species. Journal of Molecular Evolution 35:261-269.
#' @references Löchel, H. F., and Heider, D. (2021) Chaos game representation and its applications in bioinformatics. Computational and Structural Biotechnology Journal 19(2021): 6263-6271.
#'
#' @examples
#' coordinates2sequence(-10,90,25)
#'@export
utils::globalVariables(c("exit"))
coordinates2sequence <- function(xstart, ystart, n) {
  X <- xstart
  Y <- ystart
  seq <- c()
  bpx <- c("A"=-100,"C"=100,"G"=100,"T"=-100)
  bpy <- c("A"=100,"C"=100,"G"=-100,"T"=-100)

  for (i in 1:n) {
    xsign <- sign(X)
    ysign <- sign(Y)
    bp <- sign2base(xsign, ysign)
    seq <- c(seq, bp)

    xb <- bpx[bp]
    yb <- bpy[bp]

    X <- 2*X - xb
    Y <- 2*Y - yb
  }

  seq <- paste(seq,collapse="")
  seq
}
