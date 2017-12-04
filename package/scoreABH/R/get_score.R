library("Matrix")
library("xgboost")
xgbmodel = xgb.load(model)
#' calculate score
#'
#' @param ... parametry modelu
#'
#' @return numeric
#' @export
get_score <- function(...) {
  df <- data.frame(...)
  predict(xgbmodel, newdata = df)
}
