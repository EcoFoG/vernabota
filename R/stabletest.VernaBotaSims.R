#' test the stability of the results of CompareSim
#'
#' @param object an object of class "VernaBotaSims"
#' @param s the scenario for which the user want to test the stability of the simulations
#' @param ... other arguments passed to or from other methods
#'
#' @export
#'
#' @importFrom graphics boxplot
#'
stabletest <- function(object, s, ...)
{

  if (length(VBS_test@results) == 0)
    stop("You must save the results of the simulations to use the function stabletest. Use Results_Simulations = TRUE.")

  if (s>object@NScenar)
    stop("Check the number of the scenario you want to test, it is higher than the available number of scnerios.")

  pc_valid_tot <- pc_valid <- c()

  for (i in 1:object@NbSim){
    result_sim <- object@results[[s]][[i]]
    pc_valid <- c(pc_valid, (length(which(result_sim$ValidAsso))/length(which(result_sim$TestData))))
    pc_valid_tot <- c(pc_valid_tot, pc_valid)
  }

  d <- data.frame(accuracy = pc_valid_tot,
                    nbSim = rep(1:object@NbSim ,times = 1:object@NbSim))
  boxplot(accuracy~nbSim, data=d)
}
#' @export
graphics::boxplot


