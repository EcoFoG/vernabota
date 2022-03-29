#' Perform simulation and compare them to original species
#'
#' @description This function performs several simulations for trees that have a
#'   complete taxonomic identification and compare the results with the original
#'   species.
#'
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
#' @return This function returns an object of the class VernaBotaSims.
#' The functions summary and plot can be used on this object.
#'
#' @details This function performs the following steps for each scenario:
#'  - Get the data,
#'  - Split between train and test set and remove taxonomic information from the test set (see *SampleTestDataset* function),
#'  - Perform simulations (see *SimFullCom* function),
#'  - Compare simulations with original taxonomic information, with the function
#'  *CompareTaxo*,
#'  - Create and return an object of the class VernaBotaSims.
#'
#' @export
#'
#' @importFrom methods new
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel dopar
#'
#'
CompareSim <- function(Param = NULL,
                       priors = NULL, D2fill, DAsso = NULL,
                       pc2fill = NULL, pcFamilyDet = NULL, pcGenusDet = NULL,
                       NbSim = 1, Results_Simulations = FALSE)

{
  NScenar <- dim(Param)[1]

  pc_ok_results <- tot_results <- list()

  # creation of the dataset to fill (the same dataset will then be used for all scenarios)
  # Get data and remove duplicated lines corresponding to a single tree
  dat <- D2fill
  dat <- dat[!duplicated(dat$idTree),]

  # creation of the dataset
  tot_test <- SampleTestDataset(dat, pc2fill, pcGenusDet, pcFamilyDet)
  tot <- tot_test[[1]]
  test_taxo <- tot_test[[2]]
  idTest <- tot_test[[3]]

  # # loop for each scenario (can be parallelized)
  # for (s in 1:NScenar){
  #
  #   # Get DataAsso and remove test trees info from DataAsso (if present in the dataset)
  #   datAsso <- DAsso[[Param$dataAsso[s]]]
  #   datAsso <- datAsso[which(!datAsso$idTree%in%idTest),]
  #
  #   # run the function SimFullCom with parameters from the scenario s
  #   Results_Sim <- SimFullCom(Data2fill = tot,
  #                             DataAsso = datAsso,
  #                             prior = priors[[Param$priors[s]]],
  #                             wp = Param$weights[s],
  #                             NSim = NbSim,
  #                             eps = Param$eps[s],
  #                             Determ = Param$Determ[s])
  #
  #   # compare results with data
  #   pc_ok_results[[s]] <- lapply(Results_Sim, CompareTaxo, test_taxo)
  #
  #   if (Results_Simulations)
  #   {
  #     tot_results[[s]] <- lapply(Results_Sim, ValidTaxo, test_taxo)
  #     for (i in 1:NbSim) {
  #       tot_results[[s]][[i]] <- tot_results[[s]][[i]][, c("idTree",
  #                                                          "Family", "Genus", "Species",
  #                                                          "BotaSource", "BotaCertainty",
  #                                                          "VernName", "GenSp",
  #                                                          "GensSpCor", "BotaCorCode",
  #                                                          "ValidAsso", "TestData")]
  #     }
  #   }
  # }

  # loop for each scenario (parallelized)

  numCores  <- parallel::detectCores()
  cl <- parallel::makeCluster(numCores-1)
  doParallel::registerDoParallel(cl)

  pc_ok_results <- foreach (s = 1:NScenar) %dopar% {

    # Get DataAsso and remove test trees info from DataAsso (if present in the dataset)
    datAsso <- DAsso[[Param$dataAsso[s]]]
    datAsso <- datAsso[which(!datAsso$idTree%in%idTest),]

    # run the function SimFullCom with parameters from the scenario s
    Results_Sim <- vernabota::SimFullCom(Data2fill = tot,
                              DataAsso = datAsso,
                              prior = priors[[Param$priors[s]]],
                              wp = Param$weights[s],
                              NSim = NbSim,
                              eps = Param$eps[s],
                              Determ = Param$Determ[s])
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

    # compare results with data
    lapply(Results_Sim, vernabota::CompareTaxo, test_taxo)}
  parallel::stopCluster(cl)

  # creation of the VernaBotaSims object
  VBS <- new(Class = "VernaBotaSims", NScenar = NScenar, ParamScenar = Param,
             D2fill = D2fill, DAsso = DAsso, priors = priors,
             pc2fill = pc2fill, pcFamilyDet = pcFamilyDet, pcGenusDet=pcGenusDet,
             NbSim = NbSim,
             pc_results = pc_ok_results,
             results = tot_results)
  return(VBS)
}
