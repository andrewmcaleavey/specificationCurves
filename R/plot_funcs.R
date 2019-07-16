# PLOTTING FUNCTIONS

#' Plot the main specification curve
#'
#' @param data Data to use
#' @param y y variable, defaults to data$Estimate
#' @param lower vector representing lower bounds of CIs
#' @param upper vector representing upper bounds of CIs
#' @param title title to pass to ggtitle()
#'
#' @return a ggplot plot object
#' @export
#'
#' @examples
#' spec_curve(sim_out)
spec_curve <- function(data,
                       y = data$Estimate,
                       lower = data$lower,
                       upper = data$upper,
                       title = NULL){
  ggplot(data = data, aes(x = reorder(permutation, y),
                          y = y,
                          ymin = lower,
                          ymax = upper)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(color = "lightblue", alpha = .5) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank()) +
    ylab("Estimate (95% CI)") +
    xlab("Permutation") +
    ggtitle(title)
}
