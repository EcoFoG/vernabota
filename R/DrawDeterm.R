#' Attribute the most likely full species name
#'
#' @description This function attributes the full species name corresponding to the highest
#' value of alpha to a given tree.
#'
#' @param AlphaV datatable with the botanical information in the first 4 columns and one colum
#'  for the vernacular name of interested, created in the function DrawBota
#' @param levelAsso level of association depending on the available information
#'  (AssoByGenus, AssoByFam or AssoByVern)
#' @param VernName2Sim vernacular name
#'
#' @return This function returns a list with two elements: the first one is the associated botanical
#'  name and the second one the method of association (AssoByGenus, AssoByGenusT, AssoByFam,
#'   AssoByFamT, AssoByVern or AssoByVernT).
#'
#' @details This function performs the following steps:
#'  - Get the species for which the value of alpha is the highest
#'  - if there are no tie, return the value of this species and the associated correction code
#'  - if there is a tie, draw a species randomly within these species
#'
#' @export
#'
#'
#'
DrawDeterm <- function(AlphaV, levelAsso, VernName2Sim) {

  # create an empty vector to store the result
  res <- list(as.character(NA),as.character(NA))

  Spmax <- AlphaV[alphaV==max(alphaV),GenSp]
  # if no tie
  if (length(Spmax)==1) {
    res[1] <- as.character(Spmax)
    res[2] <- paste(levelAsso, "Determ", sep="")
  } else {
    # if tie
    res[1] <- as.character(sample(x=Spmax, size=1))
    res[2] <- paste(levelAsso, "DetermT", sep="")
    warning(paste("There were several species with the same probability of association for ", VernName2Sim,
                  ", the species was chosen randomly on a tree by tree basis"))
  }
  return(res)
}
