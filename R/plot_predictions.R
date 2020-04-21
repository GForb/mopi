plot_predictions <- function(predictions) {
  requireNamespace("ggplot2", quietly = TRUE)
  ggplot2::ggplot(data = predictions,
                  aes(x = outcome, y = .fitted, colour = outcome)) +
                  geom_point(size = 3) +
                  geom_errorbar(aes(ymin = pred.low, ymax = pred.high), width = 0.5) +
                  labs(y = "Prediction", x = "Outcome") +
                  theme(legend.position = "none")

}
