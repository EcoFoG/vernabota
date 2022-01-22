#' VernaBotaSims class and associated methods
#'
#' @description The VernaBotaSims class is the output of the CompareSim function.
#'
#' Plot function to be applied on *VernaBotaSims* objects. This function draw
#' a boxplot of the percentages of well associated species for each scenario.
#'
#' Summary function to be applied on *VernaBotaSims* objects. This function print
#' the results of each scenario, together with its parameters.
#'
#' @slot NScenar numeric: number of scenario simulated
#' @slot ParamScenar data.frame: a datatable with the parameters for each scenario (input of CompareSim)
#' @slot D2fill list of datasets to fill for each scenario (input of CompareSim)
#' @slot DAsso list of datasets of observation used for each scenario (input of CompareSim)
#' @slot priors list of datasets containing the priors for each scenario (input of CompareSim)
#' @slot pc_results list of double corresponding to the percentage of well simulated species
#' @slot results list of results from SimFullCom for each scenario.

#'
#' @export
#'
#' @importFrom methods setClass
#' @importFrom ggplot2 ggplot geom_boxplot aes
#' @importFrom stats quantile
#'
#'
VernaBotaSims <- setClass("VernaBotaSims", slots = c(NScenar = "numeric",
                                                     ParamScenar =  "data.frame",
                                                     D2fill = "list",
                                                     DAsso = "list",
                                                     priors = "list",
                                                     pc_results = "list",
                                                     results = "list"))


#' @param x an object of class ""VernaBotaSims"
#'
#' @describeIn S3 method for class "VernaBotaSims"
plot.VernaBotaSims <- function(x)
{
  d <- data.frame(accuracy = unlist(x@pc_results),
                  scenario = rep(letters[1:length(x@pc_results)],times = sapply(x@pc_results,length)))
  ggplot(d,aes(x = scenario, y = accuracy, color = scenario)) +
    geom_boxplot()
}

#' @param x an object of class ""VernaBotaSims"
#'
#' @describeIn 3 method for class "VernaBotaSims"
summary.VernaBotaSims <- function(x)
{
  for (s in 1:x@NScenar)
  {
    cat("scenario", letters[s], "\n")
    cat(x@ParamScenar$NbSim[s], " repetition(s) with priors nb", x@ParamScenar$priors[s],
        ", Data2Fill nb", x@ParamScenar$dataFill[s],
        ", dataAsso nb", x@ParamScenar$dataAsso[s],
        ", weights nb", x@ParamScenar$weights[s],
        ", pc2fill", x@ParamScenar$pc2fill[s],
        ", and eps", x@ParamScenar$eps[s], "\n")
    res <- unlist(x@pc_results[[s]])
    print(quantile(res))
  }
}
