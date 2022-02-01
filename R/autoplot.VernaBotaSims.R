#' plot the accuracy of scenarios
#'
#' @param object an object of class ""VernaBotaSims"
#' @param ... other arguments passed to or from other methods
#'
#' @return a ggplot of the accuracy of the different scenarios
#'
#' @export
#'
#' @importFrom ggplot2 autoplot
#'
autoplot.VernaBotaSims <- function(object, ...)
{
  d <- data.frame(accuracy = unlist(object@pc_results),
                  scenario = rep(letters[1:length(object@pc_results)],times = sapply(object@pc_results,length)))
  ggplot(d,aes(x = scenario, y = accuracy, color = scenario)) +
    geom_boxplot()
}
