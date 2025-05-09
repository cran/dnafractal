#' @name sign2base
#' @title Returns a DNA base based on the sign value of an X and Y coordinate
#' @description The function takes the sign of an X and Y value and returns the corresponding DNA base.
#'
#' Version 0.0.2.
#' Author: Dr. Matthew Cserhati
#' Email: matthew.cserhati@cui.edu
#' May 9, 2025
#'
#' @param sx sign of X coordinate
#' @param sy sign of Y coordinate
#' @return The corresponding base
#'
#' @references Jeffrey, H. J. (1990) Chaos game representation of gene structure. Nucleic Acids Research 18(8):2163-70.
#' @references Hill, K. A., Schisler, N. J., and Singh, S. M. (1992) Chaos game representation of coding regions of human globin genes and alcohol dehydrogenase genes of phylogenetically divergent species. Journal of Molecular Evolution 35:261-269.
#' @references Löchel, H. F., and Heider, D. (2021) Chaos game representation and its applications in bioinformatics. Computational and Structural Biotechnology Journal 19(2021): 6263-6271.
#'
#' @examples
#' sign2base(-1,1)
#'@export
utils::globalVariables(c("exit"))
sign2base <- function(sx, sy) {
  bp <- "N"
  if ((sx == -1) && (sy == 1)) {
    bp <- "A"
  } else if ((sx == 1) && (sy == 1)) {
    bp <- "C"
  } else if ((sx == 1) && (sy == -1)) {
    bp <- "G"
  } else if ((sx == -1) && (sy == -1)) {
    bp <- "T"
  }
  bp
}
