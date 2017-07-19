#' Double density plots
#'
#' A double density plot shows the densities of two groups. Both have an AUC
#' of 1. It can be used to compare the results of a predictive model, but
#' also the predictive potential of a single feature.
#' @param x A numeric vector containing the values.
#' @param g A vector containing two unique values, indicating the groups.
#' @return An object of class `ggplot`.
#' @examples
#' x <- c(rnorm(100,1), rnorm(100, 2))
#' g <- rep(letters[1:2], each = 100)
#' dd(x, g)
#' @export
dd <- function(x, g) {
  stopifnot(class(x) == "numeric")
  stopifnot(length(unique(g)) == 2)
  plot_data <- data.frame(x = x, g = g)
  ggplot(plot_data, aes(x, fill = g)) +
    geom_density(alpha = 0.4)
}
