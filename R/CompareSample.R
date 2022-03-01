#' Perform simulation and compare different ways of sampling the data to fill
#'
#' @description This function performs several simulations like CompareSim, but with different samples of the dataset used as test.
#'
#'
#' @param NbSamples an integer: the number of test dataset sampling to compare. All other parameters are the same parameters as for the function CompareSim.
#' @param Param a datatable with the parameters for each scenario :
#'  - priors : a vector with the rank of the priors to use in the priors list
#'  - dataAsso : a vector with the rank of the observation data to use in
#'  the DAsso list (if Dasso is not provided, put 1)
#'  - weights : a vector with the weights of the priors
#'  - eps : a vector with the epsilon value for each scenario
#'  - Determ : a vector with the value of Determ (boolean)
#' @param priors a list of datasets containing the priors for each scenario.
#' These datasets must have been prepared using the function PrepPrior.
#' (Default is NULL: no prior information is used).
#' @param D2fill a dataset to fill, this dataset must
#' have been prepared using the function PrepData.
#' @param DAsso a list of datasets of observation used for each scenario, these datasets must have been
#' prepared using the function PrepData (Default is NULL: the dataset Data2fill is used
#' to built the association matrix).
#' @param pc2fill the percentage of data (dataFill) to fill.
#' @param pcFamilyDet the percentage of data determined
#' at the family level (from the subset of dataFill to fill).
#' We recommend using a percentage equivalent than in the data to gapfill.
#' @param pcGenusDet the percentage of data determined at
#'  the genus level (from the subset of dataFill to fill, the rest isn't determined at all).
#'  We recommend using a percentage equivalent than in the data to gapfill.
#' @param NbSim the number of simulations.
#' @param Results_Simulations a boolean specifying if the
#'  user wants to keep the results of the simulations
#'
#' @return This function returns a list of 2 objects:
#' - a list of objects of the class VernaBotaSims, resulting from the simulations
#' - a datasets with one line per simulation : accuracy, scenario, and sampled test dataset
#'
#' @details This function performs the following steps *NbSamples* times:
#'  - Get the data,
#'  - Split between train and test set and remove taxonomic information from the test set (see *SampleTestDataset* function),
#'  - for each scenario: perform simulations (see *SimFullCom* function), compare simulations with original taxonomic information, with the function
#'  *CompareTaxo*, create an object of the class VernaBotaSims
#'
#' @export
CompareSample <- function(NbSamples = 3,Param = NULL,
                       priors = NULL, D2fill, DAsso = NULL,
                       pc2fill = NULL, pcFamilyDet = NULL, pcGenusDet = NULL,
                       NbSim = 1, Results_Simulations = FALSE)

{
  VBS_tot <- list()
  accuracy <- scenario <- sample <- c()
  NScenar <- dim(Param)[1]
  pc_ok_results <- tot_results <- list()

  for (sa in 1:NbSamples)
  {
    # creation of the dataset
    dat <- D2fill
    dat <- dat[!duplicated(dat$idTree),]

    tot_test <- SampleTestDataset(dat, pc2fill, pcGenusDet, pcFamilyDet)
    tot <- tot_test[[1]]
    test_taxo <- tot_test[[2]]
    idTest <- tot_test[[3]]

    # loop for each scenario (can be parallelized)
    for (s in 1:NScenar){

      # Get DataAsso and remove test trees info from DataAsso (if present in the dataset)
      datAsso <- DAsso[[Param$dataAsso[s]]]
      datAsso <- datAsso[which(!datAsso$idTree%in%idTest),]

      # run the function SimFullCom with parameters from the scenario s
      Results_Sim <- SimFullCom(Data2fill = tot,
                                DataAsso = datAsso,
                                prior = priors[[Param$priors[s]]],
                                wp = Param$weights[s],
                                NSim = NbSim,
                                eps = Param$eps[s],
                                Determ = Param$Determ[s])

      # compare results with data
      pc_ok_results[[s]] <- lapply(Results_Sim, CompareTaxo, test_taxo)

      if (Results_Simulations)
      {
        tot_results[[s]] <- lapply(Results_Sim, ValidTaxo, test_taxo)
        for (i in 1:NbSim) {
          tot_results[[s]][[i]] <- tot_results[[s]][[i]][, c("idTree",
                                                             "Family", "Genus", "Species",
                                                             "BotaSource", "BotaCertainty",
                                                             "VernName", "GenSp",
                                                             "GensSpCor", "BotaCorCode",
                                                             "ValidAsso", "TestData")]
        }
      }
    }

    # creation of the VernaBotaSims object
    VBS <- new(Class = "VernaBotaSims", NScenar = NScenar, ParamScenar = Param,
               D2fill = D2fill, DAsso = DAsso, priors = priors,
               pc2fill = pc2fill, pcFamilyDet = pcFamilyDet, pcGenusDet=pcGenusDet,
               NbSim = NbSim,
               pc_results = pc_ok_results,
               results = tot_results)

    VBS_tot[[sa]] <- VBS
    accuracy <- c(accuracy, unlist(pc_ok_results))
    scenario <- c(scenario, rep(letters[1:length(pc_ok_results)],times = sapply(pc_ok_results,length)))
    sample <- c(sample, rep(sa, NbSim*NScenar))
  }

  d <- data.frame(accuracy = accuracy,
                  scenario = scenario, sample = factor(sample))
  return(list(VBS_tot, d))

}
