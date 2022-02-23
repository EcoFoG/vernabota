#' Store results of simulation in CompareSim
#'
#' @description This function is used in the *CompareSim* function.
#'  It adds two columns to the dataset if the option *Results_Simulations*
#'  is set to TRUE : *TestData* is a boolean to know if the tree was
#'   in the test dataset, and *ValidAsso* is a boolean to know if the
#'   associated species is the right one.
#'
#' @param list_res Results of *SimFullCom* function applied to the
#'  test dataset: a list of data.tables with simulated taxonomic information.
#' @param test_taxo The original taxonomic information for the test dataset.
#'
#' @return This function returns a data.table similar to Data inputted in
#' argument, with two additional columns (TestData and ValidAsso).
#'
#' @details This function compares the taxonomic information in the column
#' GenSpCor of the simulated dataset *list_res* with the original information
#' from the columns Genus and Species of the dataset *test_taxo*. It then
#' adds two columns to the dataset, specifying if each tree is used in the
#' test and if the estimated species is the same as the original one.
#'
#' @export
#'
ValidTaxo <- function(list_res, test_taxo)
{
  # keep results corresponding to test data
  new_taxo <- list_res[which(!duplicated(list_res$idTree)),]
  rownames(new_taxo) <- new_taxo$idTree
  new_taxo <- new_taxo[as.character(test_taxo$idTree),]


  # compare results to test data
  idTreeOK <- new_taxo$idTree[which(new_taxo$GensSpCor ==
                                      paste(test_taxo$Genus, test_taxo$Species, sep='-'))]

  list_res$TestData <- list_res$ValidAsso <- FALSE
  list_res$TestData[which(list_res$idTree %in% test_taxo$idTree)] <- TRUE
  list_res$ValidAsso[which(list_res$idTree %in% idTreeOK)] <- TRUE
  return(list_res)
}
