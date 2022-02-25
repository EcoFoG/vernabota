## Split data between training and test
SampleTestDataset <- function(dat, Ndat, pc2fill, pcGenusDet, pcFamilyDet)
{
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


