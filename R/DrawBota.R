#' Gives a botanical name to a tree with a vernacular name
#'
#' @description This function gives a botanical name to a tree with a vernacular
#'   name, using a Dirichlet-Categorical method, and restricting the possible
#'   association according to the family or the genus when these have been
#'   identified. This function is performed within the function Get1Sim, for
#'   each tree without botanical species name but with a vernacular name.
#'
#' @param Alpha data.table containing the matrix of posterior alpha between
#'   botanical and vernacular names, obtained with the funcion CreateAlpha
#' @param VernName2Sim The vernacular name of the tree for which we want to get
#'   a full botanical name (character)
#' @param Family2Sim the family name of the tree for which we want to get a full
#'   botanical name (character)
#' @param Genus2Sim the genus name of the tree for which we want to get a full
#'   botanical name (character)
#' @param eps epsilon: background noise for species not associated with a given
#'   vernacular name. Default is 0.01.
#' @param Determ boolean: if TRUE the more likely botanical names are returned
#'   when a association vernacular-botanical is performed. If FALSE, the
#'   botanical names are drawn using a categorical-Dirichlet association scheme.
#'
#' @return This function returns a list with two elements: the first one is the
#'   associated botanical name and the second one the method of association.
#'
#' @details This function performs the following steps:
#' - get alphaV a vector of posterior alpha for the given vernacular name, with
#'   all botanical name of Alpha
#' - give a null value in alphaV for botanical name associated with
#'   this vernacular name but from a different genus (if the genus is known) or
#'   a different family (if the family is known)
#' - the resulting alphaV contains only 0, then attributed the correctionn
#'   Det2Genus or Det2Fam
#' - if there are non-null values in alphav, replace the null values by epsilon
#'   divided by the number of null values and attributes a corrected name using
#'   the function DrawDeterm if Determ is set to TRUE or the function DirichCat
#'   if Determ is set to FALSE
#'
#' @export
#'
#' @importFrom data.table := data.table
#'
#'
#'
DrawBota <- function(Alpha, VernName2Sim, Family2Sim, Genus2Sim, eps=0.01, Determ) {

  # get AlphaV, a data.frame with the bota info (4 1er colums) and one colum for the
  # vernacular name we are interested in
  AlphaV <- data.table::data.table(Alpha[, 1:4, with=FALSE], Alpha[,VernName2Sim, with=FALSE])
  colnames(AlphaV)[length(colnames(AlphaV))] <- "alphaV"

  # create an empty vector to store the result
  res <- list(as.character(NA),as.character(NA))

  ## case with  no species , ok vernacular name, ok genus
  if (Genus2Sim!="Indet.") {
    # Put 0 to alphaV for bota names associated with this vernacular but of different genus
    AlphaV[alphaV!=0 & Genus!=Genus2Sim, alphaV:=0]
    # case with no compatibility between the Genus name and the vernname in Alpha
    if (sum(AlphaV$alphaV)==0) {
      res <- list(paste(Genus2Sim, "Indet.", sep="-"), "Det2Genus")
    } else {
      # case with compatibility
      # for same vernacular name but different genus And for different vernacular name
      # gives a freq =eps/nb
      AlphaV[alphaV==0, alphaV := eps/dim(AlphaV[alphaV==0 ])[1]]
      if(Determ==TRUE){
        res <- DrawDeterm(AlphaV, levelAsso="AssoByGenus", VernName2Sim=VernName2Sim)
      }
      if(Determ==FALSE) {
        # Draw in Dirichlet-Categorical
        res[1] <- DirichCat(alphaV = AlphaV$alphaV, Names = AlphaV$GenSp, Vern = VernName2Sim)
        res[2] <- "AssoByGenus"
      }
    }
  }


  # case with  no species, ok vernacular name, no genus, OK family
  if (Family2Sim!="Indet." & Genus2Sim=="Indet.") {
    # Put 0  to alphaV for bota names associated with this vernacular but of different family
    AlphaV[alphaV!=0 & Family!=Family2Sim, alphaV:=0]
    # case with no compatibility between the Family name and the vernname in Alpha
    if (sum(AlphaV$alphaV)==0) {
      res <- list(paste(Family2Sim, "Indet.", sep="-"), "Det2Fam")
    } else {
      # case with compatibility
      # for same vernacular name but different family And for different vernacular name
      # gives a freq =eps/nb
      AlphaV[alphaV==0, alphaV := eps/dim(AlphaV[alphaV==0])[1]]

      if(Determ==TRUE){
        res <- DrawDeterm(AlphaV, levelAsso="AssoByFam", VernName2Sim=VernName2Sim)
      }
      if(Determ==FALSE) {
        # Draw in Dirichlet-Categorical
        res[1] <- DirichCat(alphaV = AlphaV$alphaV, Names = AlphaV$GenSp, Vern = VernName2Sim)
        res[2] <- "AssoByFam"
      }
    }
  }

  # case with  no species, ok vernacular name, no genus, no family
  if (Family2Sim=="Indet." & Genus2Sim=="Indet.") {
    # gives a freq =eps/nb for different vernacular name
    AlphaV[alphaV==0, alphaV := eps/dim(AlphaV[alphaV==0])[1]]
    if(Determ==TRUE){
      res <- DrawDeterm(AlphaV, levelAsso="AssoByVern", VernName2Sim=VernName2Sim)
    }
    if(Determ==FALSE) {
      # Draw in Dirichlet-Categorical
      res[1] <- DirichCat(alphaV = AlphaV$alphaV, Names = AlphaV$GenSp, Vern = VernName2Sim)
      res[2] <- "AssoByVern"
    }
  }

  return(res)
}
