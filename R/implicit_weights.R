implicit_weights <- function(model) {
  if (!is.null(model$weights)) {
    stop("Calculations not implemented for models with specified weights")
  }
  model_frame <- expand_model_frame(model$model)[, -1]
  variables <- names(model_frame)
  resids <- list()

  for (outcome in variables) {
    predictors <- variables[variables != outcome]
    form <- paste(paste(outcome, "~ "), paste(predictors, collapse = " + "))

    if (is_dummy(model_frame[, outcome])) {
      mod <- suppressWarnings(
        glm(form, family = "binomial", data = model_frame)
      )
    } else {
      mod <- lm(form, data = model_frame)
    }

    weights <- residuals(mod)^2
    std_weights <- weights / max(weights)
    resids[paste0(outcome, "_weight")] <- list(std_weights)
  }

  return(as.data.frame(resids))
}
