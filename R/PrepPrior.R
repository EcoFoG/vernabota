#' Prior preparation
#'
#' @description This function prepares the prior for further steps.
#'
#' @param prior dataframe of prior, formatted as as shown in the vignette.
#' @param RemoveIndetSp Should the botanical names with non-determined species
#' be removedfrom the prior? (Default is TRUE)
#' @param RemoveNotGuyafor Should the botanical names with non-determined
#' species be removedfrom the prior? (Default is TRUE)
#'
#' @return This function returns a dataframe (formatted as a data.table)
#'  ready to be used for the following steps.
#'
#' @details This function performs the following steps:
#'  - perform some checks on the file
#'  - add a column GenSp with the full botanical name
#'  - remove botanical names no fully determined or not in Guyafor (is option selected)
#'  - remove columns with no association
#'
#'One reason for choosing the default setting for RemoveIndetSp and RemoveNotGuyafor is
#'that these names would always lead to incorrect association when using the CompareSim function.
#'However, one may decide to keep them, this would lead to
#'  - possible associations with a botanical name of the form Genus-Indet.
#'  with a BotaCodeCor="AssoByGenus" or "AssoByFam" (with RemoveIndetSp==TRUE) .
#'  - possible associations with a botanical name that has never been observed
#'  in Guyafor (with RemoveNotGuyafor==TRUE).
#'In these case, only the prior information would be used.
#'
#' @export
#'
#' @importFrom data.table as.data.table := .SD
#'
#' @examples
PrepPrior <- function(prior, RemoveIndetSp=TRUE, RemoveNotGuyafor=TRUE) {
  # check of columns names
  if(any(!(c("Family", "Genus", "Species", "PresentInGuyaFor") %in%
           colnames(prior)))) {
    stop("You must provide a dataframe containing at least the following columns names for prior:
         Family, Genus, Species, PresentInGuyaFor")
  }
  # transform a datatable
  prior <- as.data.table(prior)
  # check for incorrect values  in vernacular names
  if (any(prior[, lapply(.SD, function(v){length(unique(v))}),
                .SDcols = colnames(prior)[which(!(colnames(prior) %in%
                                                  c("Family", "Genus","Species", "PresentInGuyaFor")))]]>2)) {
    stop("you must only have 0 and 1 for the vernacular columns in prior,
      and the column names must be Family, Genus, Species, PresentInGuyaFor")
  } else {
    if (any(prior[, lapply(.SD, function(v){sort(unique(v))}),
                  .SDcols = colnames(prior)[which(!(colnames(prior) %in%
                                                    c("Family", "Genus","Species", "PresentInGuyaFor")))]]!= c(0,1))) {
      stop("you must only have 0 and 1 for the vernacular columns in prior, have at least one value of 1
        and the column names must be Family, Genus, Species, PresentInGuyaFor")
    }
  }
  # check no NA in the first 4 columns
  if (dim(prior[is.na(Family) | is.na(Genus) | is.na(Species) | is.na(PresentInGuyaFor)])[1] != 0) {
    stop("There should not be NA in the columns Family, Genus, Species, PresentInGuyaFor")
  }
  # check the format of columns
  if (!any(is.character(prior$Family),
           is.character(prior$Genus),
           is.character(prior$Genus))) {
    stop("The columns Family, Genus, Species should be character strings")
  }
  if(!(is.logical(prior$PresentInGuyaFor))) {
    stop("The colum PresentInGuyaFor should be a boolean")
  }
  # add GenSp
  prior$GenSp <- prior[, paste(Genus,Species, sep="-")]

  # remove Indet. (if selected)
  if(RemoveIndetSp==TRUE) {
    prior <- prior[Species!="Indet."]
  }

  # remove not in Guyafor (if selected)
  if(RemoveNotGuyafor==TRUE) {
    prior <- prior[PresentInGuyaFor==TRUE]
  }

  # remove columns with a sum=0
  colnull <- names(which(colSums(prior[, which(
    !(colnames(prior) %in% c("Family", "Genus","Species", "PresentInGuyaFor", "GenSp"))), with=FALSE])==0))
  if(length(colnull)>0) {prior[, (colnull):=NULL]}

  # remove columns "PresentInGuyaFor"
  prior[, PresentInGuyaFor:=NULL]

  return(prior)
}
