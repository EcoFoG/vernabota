#' summary S3 method for class "VernaBotaSims"
#'
#' @param object an object of class ""VernaBotaSims"
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
    cat(object@ParamScenar$NbSim[s], " repetition(s) with priors nb", object@ParamScenar$priors[s],
        ", Data2Fill nb", object@ParamScenar$dataFill[s],
        ", dataAsso nb", object@ParamScenar$dataAsso[s],
        ", weights nb", object@ParamScenar$weights[s],
        ", pc2fill", object@ParamScenar$pc2fill[s],
        ", and eps", object@ParamScenar$eps[s], "\n")
    res <- unlist(object@pc_results[[s]])
    print(quantile(res))
  }
}
