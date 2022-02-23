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
#'  - Split between train and test set according to the parameter pc2fill (keep only fully identified trees in the test set),
#'  - Remove taxonomic information from the test set (at the species, genus, or family level according to the parameters pcFamilyDet and pcGenusDet),
#'  - Perform simulations (see *SimFullCom* function),
#'  - Compare simulations with original taxonomic information, with the function
#'  *CompareTaxo*,
#'  - Create and return an object of the class VernaBotaSims.
#'
#' @export
#'
#' @importFrom methods new
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
  Ndat <- nrow(dat)

  ## Split data between training and test

  # tested trees must have complete taxonomic identification (to have something to compare the attributed species with)
  identified_trees <- dat$idTree[which(dat$BotaCertainty %in% c(3,4))]
  if (length(identified_trees) < round(Ndat * pc2fill/100))
    stop("You must provide a dataset with enough trees identified to species, or you must decrease the parameter pc2fill")

  # sample data
  identified_trees <- identified_trees[sample(1:length(identified_trees))]
  # test dataset
  test_id <- identified_trees[1:round(Ndat * pc2fill/100)]
  test <- dat[dat$idTree%in%test_id,]
  # train dataset
  train <- dat[!dat$idTree%in%test_id,]

  # keep taxonomic info to test the attributed species
  test_taxo <- test[,c('idTree','Family','Genus','Species')]

  # remove taxonomic info from test:

  # keep only family for pcFamilyDet % of the test dataset:
  test[1:round(nrow(test) * pcFamilyDet/100),c('Genus','Species')] <- "Indet."
  # keep family and genus for pcGenusDet % of the test dataset:
  test[(round(nrow(test) * pcFamilyDet/100) + 1):
         (round(nrow(test) * pcFamilyDet/100) + round(nrow(test) * pcGenusDet/100)),
       'Species'] <- "Indet."
  # remove all taxonomic info for the rest of the dataset:
  test[(round(nrow(test) * pcFamilyDet/100) + round(nrow(test) * pcGenusDet/100) + 1):
         nrow(test),
       c('Family','Genus','Species')] <- "Indet."

  # rewrite GenSp column:
  test$GenSp <- paste(test$Genus, test$Species, sep="-")

  tot <- rbind(test,train)

  # loop for each scenario (can be parallelized)
  for (s in 1:NScenar){

    # Get DataAsso and remove test trees info from DataAsso (if present in the dataset)
    datAsso <- DAsso[[Param$dataAsso[s]]]
    datAsso <- datAsso[which(!datAsso$idTree%in%test$idTree),]

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
  return(VBS)
}
