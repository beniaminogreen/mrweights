#' Expand a model frame, turning factors into dummy variables
#'
#' @title expand_model_frame: Turn factor variabes into dummies in a data frame
#'
#' @param frame a dataframe you want to transform
#' @return frame a dataframe with the factor variables transformed into dummy
#' variables
expand_model_frame <- function(frame) {
  for (variable in names(frame)) {
    column <- frame[, variable]
    if (is.factor(column)) {
      for (level in levels(column)[-1]) {
        col_name <- paste0(variable, level)
        frame[, col_name] <- as.numeric(column == level)
      }
      frame[, variable] <- NULL
    }
  }
  return(frame)
}

#' Test if a vector is a dummy variable
#'
#' @title is_dummy: Test if a vector is a dummy variable
#'
#' @param vec the vector you want to test
#' @return logical. is the vector a dummy?
is_dummy <- function(vec) {
  if (all(vec == 0 | vec == 1)) {
    return(TRUE)
  }
  return(is.logical(vec))
}
