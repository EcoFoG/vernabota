#' Split data between training and test
#'
#' @description This function is used in the *CompareSim* and *CompareSample* functions.
#' It splits the data between train an test datasets, and remove the taxonomic information from the test dataset.
#'
#' @param dat The data to split
#' @param pc2fill the percentage of data (dat) to fill
#' @param pcGenusDet the percentage of data determined at
#' the genus level (from the subset of dataFill to fill, the rest isn't determined at all).
#' @param pcFamilyDet the percentage of data determined
#' at the family level (from the subset of dataFill to fill).
#'
#' @return This function returns a list with 3 elements:
#'  - The dataset with some taxonomis information removed
#'  - The taxonomic information that has been removed
#'  - The corresponding tree ID (trees from the test dataset)
#'
#' @details This function
#'  - Split between train and test set according to the parameter pc2fill (keep only fully identified trees in the test set),
#'  - Remove taxonomic information from the test set (at the species, genus, or family level according to the parameters pcFamilyDet and pcGenusDet),
#'
#'  @export

SampleTestDataset <- function(dat, pc2fill, pcGenusDet, pcFamilyDet)
{
  Ndat <- nrow(dat)

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

  tot <- list(rbind(test,train),test_taxo, test$idTree)
  return(tot)
}
