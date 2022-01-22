#' Compare the original taxonomic information of a dataset with predicted
#'one
#'
#' @description This function is used in the CompareSim function.
#'It compares the original taxonomic information of a dataset with predicted
#'one and return the percentage of matching species.
#'
#' @param list_res Results of SimFullCom function applied to the test dataset:
#'  a list of datatables with simulated taxonomic information.
#' @param test_taxo The original taxonomic information for the test dataset.
#'
#' @return A double corresponding to the percentage of well simulated species.
#'
#' @details This function compares the taxonomic information in the column
#' GenSpCor of the simulated dataset list_res with the original information
#' from the columns Genus and Species of the dataset test_taxo.
#' The number of good estimation is divided by the length of the test dataset
#' to return a percentage good simulations.
#'
#' @export
#'
#'
CompareTaxo <- function(list_res, test_taxo)
{
  # keep results corresponding to test data
  new_taxo <- list_res[which(!duplicated(list_res$idTree)),]
  rownames(new_taxo) <- new_taxo$idTree
  new_taxo <- new_taxo[as.character(test_taxo$idTree),]

  # compare results to test data
  pc_resOK <- length(which(new_taxo$GensSpCor == paste(test_taxo$Genus, test_taxo$Species, sep='-'))) / nrow(new_taxo)
}
