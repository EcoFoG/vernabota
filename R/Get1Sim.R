#' Simulate a fully determined dataset
#'
#' @description This function simulates a fully determined dataset (one simulation)
#'
#' @param Data datatable for which the gap filling of botanical names from vernacular
#'  names will be done, formatted as shown in the vignette and after the data preparation
#'   performed by SimFullCom
#' @param Alpha datatable containing the matrix of posterior alpha between botanical and
#' vernacular names, obtained with the fonction CreateAlpha
#' @param eps epsilon: background noise for species not associated with a given vernacular name
#' @param Determ boolean: if TRUE the more likely botanical names are return when a association
#'  vernacular-botanical is performed.
#'  If FALSE, the botanical names are drawn using a categorical-Dirichlet association Scheme.
#'
#' @return This function returns a datatable similar to Data inputted in argument,
#' with two additional columns (GenSpCor and BotaCorCode).
#'
#' @details This function performs the following steps:
#'  - create a datatable with only one row per individual trees (as we want the gap filling
#'   to be the same across all census for a given tree)
#'  - Perform the corrections to obtain a corrected identification (GensSpCor) and specify
#'   the type of correction that has been performed (BotaCorCode) in the cases where the tree
#'   is fully determined (BotaCorCode="fullyDet") or when there is no vernacular name (or a vernacular
#'    name not in Alpha) (BotaCorCode="Det2Genus" or "Det2Fam" or "NoCor", depending on the cases
#'  - Call the function DrawBota to get GenSpCor and BotaCorCode in cases with no species name but
#'  with a vernacular name present in Alpha
#'  - join the resulting datatable with Data to get the correction for each tree and each census
#'
#' @export
#'
#'
#'
Get1Sim <- function(Data, Alpha, eps=eps, Determ){
  # for a given simu, we want to give the same bota name to all measurment of a given individuals
  # so we first create a list of trees (one row per indiv not by measurment)
  DataTree <- unique(Data[,list(idTree, VernName, Family, Genus, Species, GenSp)])
  # we add a colum to store the Simulated botanical name (GensSpCor)
  # and a colum to store the way the name was simulated (BotaCorCode)
  DataTree <- cbind(DataTree, GensSpCor=as.factor(NA), BotaCorCode=as.factor(NA))


  ## case of tree with a fully determined name => no need for replacement, give GenSp name
  DataTree[Species!="Indet.", GensSpCor:=GenSp]
  DataTree[Species!="Indet.", BotaCorCode:="fullyDet"]


  ## cases with no Species name, no vernacular name (or a vernacurlar name not in Alpha)
  # case with no species name, no vernacular name (or a vernacurlar name not in Alpha), Ok genus name
  # we give it a Name in Genus-Indet.
  DataTree[Species=="Indet." & VernName=="-" & Genus!="Indet.", GensSpCor:=GenSp]
  DataTree[Species=="Indet." & VernName=="-" & Genus!="Indet.", BotaCorCode:="Det2Genus"]
  DataTree[Species=="Indet." & VernName!="-"  & !(VernName %in% colnames(Alpha)) &
             Genus!="Indet.", GensSpCor:=GenSp]
  DataTree[Species=="Indet." & VernName!="-"  & !(VernName %in% colnames(Alpha)) &
             Genus!="Indet.", BotaCorCode:="Det2Genus"]

  # case with  no species name, no vernacular name (or a vernacurlar name not in Alpha), no genus name, OK Family name
  # we give the name Family-Indet.
  DataTree[Species=="Indet." & VernName=="-" & Genus=="Indet." & Family!="Indet.",
           GensSpCor:=paste(Family, "Indet.", sep="-")]
  DataTree[Species=="Indet." & VernName=="-" & Genus=="Indet." & Family!="Indet.",
           BotaCorCode:="Det2Fam"]
  DataTree[Species=="Indet." & VernName!="-" & !(VernName %in% colnames(Alpha)) &
             Genus=="Indet." & Family!="Indet.",
           GensSpCor:=paste(Family, "Indet.", sep="-")]
  DataTree[Species=="Indet." & VernName!="-" & !(VernName %in% colnames(Alpha)) &
             Genus=="Indet." & Family!="Indet.",
           BotaCorCode:="Det2Fam"]

  # case with  no species name, no vernacular name (or a vernacurlar name not in Alpha), no genus name, no Family name
  # we accept that we don't have the information and we leave NA in GensSpCor
  DataTree[Species=="Indet." & VernName=="-" & Genus=="Indet." & Family=="Indet.",
           GensSpCor:=GenSp]
  DataTree[Species=="Indet." & VernName=="-" & Genus=="Indet." & Family=="Indet.",
           BotaCorCode:="NoCor"]
  DataTree[Species=="Indet." & VernName!="-" & !(VernName %in% colnames(Alpha)) &
             Genus=="Indet." & Family=="Indet.",
           GensSpCor:=GenSp]
  DataTree[Species=="Indet." & VernName!="-" & !(VernName %in% colnames(Alpha)) &
             Genus=="Indet." & Family=="Indet.",
           BotaCorCode:="NoCor"]


  ## cases with  no species , ok vernacular name
  # => we use the function DrawBota for each row of DataTree that are in these cases
  DataTree[Species=="Indet." & VernName!="-" & VernName %in% colnames(Alpha), # trees in these cases
           c("GensSpCor", "BotaCorCode") := DrawBota(Alpha=Alpha, VernName2Sim=as.character(VernName),
                                                     Family2Sim=as.character(Family), Genus2Sim=as.character(Genus),
                                                     eps=eps, Determ=Determ),
           by=idTree]

  # once DataTree has a value of GensSpCor for all trees
  # and we attribute this name to all the measurment of the tree (with a merge)
  DataSim <- merge(Data, DataTree[,list(idTree, GensSpCor, BotaCorCode)], by="idTree")
  # and get one fully determined dataset
  return(DataSim)

}
