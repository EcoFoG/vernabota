#' summary S3 method for class "VernaBotaSims"
#'
#' @param x an object of class ""VernaBotaSims"
#' @param ... other arguments passed to or from other methods
#'
#' @return a summary of the scenarios tested
#'
#' @export
summary.VernaBotaSims <- function(x, ...)
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
