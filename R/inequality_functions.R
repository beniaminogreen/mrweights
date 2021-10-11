#' Calculate the Gini Coefficient of a Vector
#'
#' @title gini: calculate the Gini coefficient of a vector
#'
#' @param vec the vector for which you want to calculate the Gini coefficients
#' @return the Gini coefficient
#' @export
gini <- function(vec) {
  sorted <- sort(vec)
  mean_x <- mean(vec)
  n <- length(vec)

  tally <- sum(1:n * (sorted - mean_x))
  2 * tally / (mean_x * n^2)
}


lorenz_plot <- function(x) UseMethod("lorenz_plot", x)

#' Plot the lorenz curve of a vector
#'
#' @title plot_lorenz: plot the Lorenz curve associated with a vector
#'
#' @param vec the vector you want to plot the Lorenz curve for
#' @return ggplot object of Lorenz curve
lorenz_plot.numeric <- function(vec) {
  require(ggplot2)

  sorted <- sort(vec)
  cumvec <- cumsum(sorted) / sum(sorted)
  x <- (1:length(cumvec)) / length(cumvec)

  ggplot() +
    geom_step(aes(x = x, y = cumvec), col = "blue", alpha = .5) +
    geom_abline(slope = 1, linetype = 2, alpha = .2) +
    theme_bw()
}

#' Plot the lorenz curve of a set of vectors
#'
#' @title plot_lorenz: plot the Lorenz curve associated with a vector
#'
#' @param df a dataframe of numeric variables you would like to plot the lorenz cuves for
#' @return ggplot object of Lorenz curve
lorenz_plot.data.frame <- function(df) {
  require(ggplot2)
  require(dplyr)

  df %>%
    pivot_longer(
      everything(),
      names_to = "coefficient",
      values_to = "weight"
    ) %>%
    group_by(coefficient) %>%
    arrange(weight) %>%
    mutate(
      cum_per = cumsum(weight) / sum(weight),
      per = (1:n()) / n()
    ) %>%
    ggplot(aes(x = per, y = cum_per, col = coefficient)) +
    geom_step() +
    geom_abline(slope = 1, linetype = 2, alpha = .2) +
    theme_bw()
}
