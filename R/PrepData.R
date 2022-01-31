#' Data preparation
#'
#' @description This function prepares a dataset to be used for the association of vernacular and botanical name.
#'
#' @param Data dataframe of inventory data, formatted as
#' it is when obtained using the function EcoFoG::Guyafor2df or from the online data platform of Paracou.
#'
#' @return a dataframe (formatted as a data.table) ready to be used for the association of vernacular and botanical name.
#' The column GenSp has been added, which contains the Genus and species of the original data set
#' (all trees for which BotaSource=Vern have a GenSp="Indet.-Indet.")
#'
#' @details This function performs the following steps:
#'  - convert all vernacular names to lower case (for consistency with the prior)
#'  - remove botanical names when BotaSource=Vern (for the reason explained
#'  in the description of the function SimFullCom)
#'  - remove subspecies and variety names
#'  - add a column GenSp with the full botanical name
#'
#' @export
#'
#' @importFrom data.table as.data.table :=
#'
#'
PrepData <- function(Data) {

  if (any(!(c("CodeAlive", "idTree", "BotaCertainty", "BotaSource", "VernName",
              "Family", "Genus", "Species")
            %in%  colnames(Data)))) {
    stop("You must provide a dataframe containing at least the following columns names for Data:
         CodeAlive, idTree, BotaCertainty, VernName, Family, Genus, Species")
  }

  # convert to data.table
  Data <- as.data.table(Data)

  # put all vernacular names in lower case
  Data[, VernName:=tolower(VernName)]

  # housekeeping
  Data$idTree <- as.factor(Data$idTree)
  Data$BotaCertainty <- as.factor(Data$BotaCertainty)
  Data$VernName <- as.factor(Data$VernName)

  # check that a same tree cannot have several names
  if (any(
    unique(Data[, length(unique(VernName)), by=idTree]$V1) !=1,
    unique(Data[, length(unique(Family)), by=idTree]$V1) !=1,
    unique(Data[, length(unique(Genus)), by=idTree]$V1) !=1,
    unique(Data[, length(unique(Species)), by=idTree]$V1) !=1 )) {
    stop("Function PrepData: a given individual (same idTree) can have only one VernName, Family, Genus and Species")
  }
  # !!! COULD BE SPEEDED UP

  # remove the Family, Genus and Species name for indiv for which the BotaSource is Verna
  # because for these indiv, the scientific name as been infered from most probable asso on Guyafor
  # we don't want to keep this info as we are going to reconstruct it better
  Data[BotaSource=="Vern", Family:= "Indet."]
  Data[BotaSource=="Vern", Genus:= "Indet."]
  Data[BotaSource=="Vern", Species:= "Indet."]
  Data[grepl("Indet.", Genus) & Genus!="Indet.", Genus:="Indet."] # for cases when genus is for eg Indet.Annonaceae
  Data[grepl("Indet.", Species) & Species!="Indet.", Species:="Indet."] # same for species (not sure it occurs)

  # remove the subspecies
  Data[grepl("subsp.", Species, fixed = TRUE), # all trees with a subspecies
       Species := unlist(strsplit(as.character(Species), " subsp. ", fixed = TRUE))[1], # keep only the species name
       by="Species"]
  # remove the variety
  Data[grepl("var.", Species, fixed = TRUE), # all trees with a var.
       Species := unlist(strsplit(as.character(Species), " var. ", fixed = TRUE))[1], # keep only the species name
       by="Species"]

  # add a column for the full botanical name
  Data$GenSp <- as.factor(Data[, paste(Genus,Species, sep="-")])

  return(Data)
}
