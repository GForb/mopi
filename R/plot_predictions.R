#' plot_predictions
#'
#' @param predictions a tibble of predictions created by mopi::predict_weighted_lavaan
#'
#' @return
#' @export
#'
#' @examples
plot_predictions <- function(predictions) {
  requireNamespace("ggplot2", quietly = TRUE)
  ggplot2::ggplot(data = predictions, mapping = aes(x = outcome, y = .fitted, colour = outcome)) +
                  geom_point(size = 3) +
                  geom_errorbar(aes(ymin = .pred.low, ymax = .pred.high), width = 0.5) +
                  labs(y = "Prediction", x = "Outcome") +
                  theme(legend.position = "none")

}
