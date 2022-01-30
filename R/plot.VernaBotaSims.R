#' plot S3 method for class "VernaBotaSims"
#'
#' @param x an object of class ""VernaBotaSims"
#' @param ... other arguments passed to or from other methods
#'
#' @return a plot of the accuracy of the different scenarios
#'
#' @example
#'
#' @export
plot.VernaBotaSims <- function(x, ...)
{
  d <- data.frame(accuracy = unlist(x@pc_results),
                  scenario = rep(letters[1:length(x@pc_results)],times = sapply(x@pc_results,length)))
  ggplot(d,aes(x = scenario, y = accuracy, color = scenario)) +
    geom_boxplot()
}
