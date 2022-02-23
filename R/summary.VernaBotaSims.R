#' summary of scenarios
#'
#' @param object an object of class "VernaBotaSims"
#' @param ... other arguments passed to or from other methods
#'
#' @return a summary of the scenarios tested
#'
#' @export
summary.VernaBotaSims <- function(object, ...)
{
  for (s in 1:object@NScenar)
  {
    cat("scenario", letters[s], "\n")
    cat(object@NbSim, " repetition(s) with priors nb", object@ParamScenar$priors[s],
        ", dataAsso nb", object@ParamScenar$dataAsso[s],
        ", weights ", object@ParamScenar$weights[s],
        ", pc2fill", object@pc2fill, "%",
        ", and eps", object@ParamScenar$eps[s], "\n")
    res <- unlist(object@pc_results[[s]])
    print(quantile(res))
  }
}
