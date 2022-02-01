#' plot the accuracy of scenarios
#'
#' @param object an object of class "VernaBotaSims"
#' @param ... other arguments passed to or from other methods
#'
#' @return a ggplot of the accuracy of the different scenarios
#'
#' @export
#'
#' @importFrom ggplot2 autoplot
#' @export autoplot
#'
autoplot.VernaBotaSims <- function(object, ...)
{
  d <- data.frame(accuracy = unlist(object@pc_results),
                  scenario = rep(letters[1:length(object@pc_results)],times = sapply(object@pc_results,length)))
  ggplot(d,aes(x = scenario, y = accuracy, color = scenario)) +
    geom_boxplot()
}


#' plot the accuracy of scenarios
#'
#' @param x an object of class "VernaBotaSims"
#' @param ... other arguments passed to or from other methods
#'
#' @export
#'
#' @importFrom graphics plot
#' @export plot
#'
#' @importFrom graphics boxplot
#'
plot.VernaBotaSims <- function(x, ...)
{
  d <- data.frame(accuracy = unlist(object@pc_results),
                  scenario = rep(letters[1:length(object@pc_results)],times = sapply(object@pc_results,length)))
  boxplot(accuracy~scenario, data=d)
}
