#' Create matrix of posterior alpha
#'
#' @description This function creates a datatable containing the matrix
#' of posterior alpha and taxonomic info, using a prior expert knowledge (optional)
#'  and observation for trees with a confirmed identification or
#'  a temporary identification (BotaCertainty 4 and 3 in Guyafor)
#'
#' @param DataAsso datatable of inventory data, formatted as as shown in the vignette
#' @param prior datatable with prior expert knowledge, formatted as as shown in the vignette
#' @param wp numeric value giving the weighting of the prior information
#'
#' @return This function returns a datatable with a row per full botanical names (GenSp).
#' The first columns are GenSp, Family, Genus and Species and the following colums are each of the vernacular name.
#' These columns are the vectors of alphav.
#'
#' @details This function performs the following steps:
#'  - create matrix of lambdav: get lambdav, equal to 1/number of possible association
#'  to the vernacular name v if the association is possible according to the prior and 0 if not
#'  - create matrix of fv using DataObs
#'  - keep only trees with a confirmed identification or a temporary
#'  identification (BotaCertainty 4 and 3 in Guyafor)
#'  - keep one line per individual (to avoid giving more weigth in individuals present in multiple censuses)
#'  - get fv, the observed frequencies of association between the vernacular name v and each botanical name fv
#'  - sum the two matrix with a weigh of wp for the prior and (1-wp) for the observation
#'
#' @export
#'
#' @importFrom data.table := data.table .N .SD dcast setorder setcolorder
#'
#' @examples
#'
CreateAlpha <- function(DataAsso, prior, wp) {

  # create matrix of lambda
  if (!(is.null(prior))) {
    prior$GenSp <- prior[, paste(Genus,Species, sep="-")]

    # remove columns with a sum=0 (there shouldn't be but in case)
    colnull <- names(which(colSums(prior[, which(
      !(colnames(prior) %in% c("Family", "Genus","Species", "GenSp"))), with=FALSE])==0))
    if(length(colnull)>0) {prior[, (colnull):=NULL]}
    # create a frequency matrix
    lambda <- data.table(prior[,.(Family, Genus, Species, GenSp)],
                         prior[, lapply(.SD, function(v){v/sum(v)}),
                               .SDcols = colnames(prior)[
                                 which(!(colnames(prior) %in% c("Family", "Genus","Species", "GenSp")))]])
    lambda[,(c("Family", "Genus","Species", "GenSp")):=lapply(.SD, as.character),.SDcols=c("Family", "Genus","Species", "GenSp")]
  }


  # create matrix of f using DataObs (dataframe with vernacular as colum and bota as row + the information bota )
  # keep only trees with a "Confirmed identification" (BotaCertainty 4) or "Temporary identification (BotaCerntainty=3)
  Data4asso <- DataAsso[BotaCertainty %in% c(3,4)]
  Data4asso <- droplevels(Data4asso)
  # keep only the column needed
  Data4asso <- Data4asso[,.(idTree, GenSp, Family, Genus, Species, VernName)]
  # keep only one line per individual tree
  Data4asso <- unique(Data4asso, by="idTree")
  # remove row with Vernname=-"
  Data4asso <- Data4asso[VernName!="-",]

  # get a contingency table
  ContMatL <- Data4asso[,.N, by=.(GenSp, Family, Genus, Species, VernName)] # long data
  # make it a wide table
  ContMat <- dcast(ContMatL, GenSp + Family + Genus + Species~VernName, value.var="N")
  # replace NA per 0
  ContMat[is.na(ContMat)] <- 0
  # transform into frequency
  f <- data.table(ContMat[, .(Family, Genus, Species, GenSp)],
                  ContMat[, lapply(.SD, function(v){v/sum(v)}),
                          .SDcols = colnames(ContMat)[
                            which(!(colnames(ContMat) %in% c("Family", "Genus","Species", "GenSp")))]])
  f[,(c("Family", "Genus","Species", "GenSp")):=lapply(.SD, as.character),.SDcols=c("Family", "Genus","Species", "GenSp")]


  if (!(is.null(prior))) {
    # get Alpha posterior (dataframe with vernacular as colum and bota as row + the information bota, sum by colum = 2 for bota columns)
    # create an empty Alpha with only the 4 first columns and all the bota names
    Alpha <- unique(rbind(lambda[ ,.(Family, Genus, Species, GenSp)],
                          f[ ,.(Family, Genus, Species, GenSp)]))# all the bota sp
    setorder(Alpha, Family, Genus,Species, GenSp)
    # extend lamda to the dimension of Alpha, filling with NA
    lambdaAll <- merge(Alpha, lambda, by=c("Family", "Genus","Species", "GenSp"), all.x = TRUE)
    col2add <- colnames(f)[!colnames(f) %in% colnames(lambda)]
    data2add <- as.data.table(matrix(data=NA, nrow= dim(Alpha)[1], ncol= length(col2add)))
    colnames(data2add) <- col2add
    lambdaAll <- cbind(lambdaAll, data2add)
    lambdaAll[is.na(lambdaAll)] <- 0 # replace na by 0
    # extend lf to the dimension of Alpha, filling with NA
    fAll <- merge(Alpha, f, by=c("Family", "Genus","Species", "GenSp"), all.x = TRUE)
    col2add <- colnames(lambda)[!colnames(lambda) %in% colnames(f)]
    data2add <- as.data.table(matrix(data=NA, nrow= dim(Alpha)[1], ncol= length(col2add)))
    colnames(data2add) <- col2add
    fAll <- cbind(fAll, data2add)
    setcolorder(fAll, colnames(lambdaAll))
    fAll[is.na(fAll)] <- 0 # replace na by 0
    # do some checking of the ordering (all the row and all the columns in the same order)
    if (!(all(c(unique(lambdaAll[,.(Family, Genus, Species, GenSp)] == Alpha),
                unique(fAll[,.(Family, Genus, Species, GenSp)] == Alpha),
                unique(lambdaAll[,.(Family, Genus, Species, GenSp)] == fAll[,.(Family, Genus, Species, GenSp)]),
                unique(colnames(lambdaAll) == colnames(fAll)))
    ))) {
      stop("There is a problem with the function CreateAlpha, check the code")
    }

    # get the sum of lambda + f
    Sumfl <- wp * as.matrix(lambdaAll[, which(!(colnames(lambdaAll) %in% c("Family", "Genus","Species", "GenSp"))), with=FALSE]) +
      (1-wp) * as.matrix(fAll[, which(!(colnames(fAll) %in% c("Family", "Genus","Species", "GenSp"))), with=FALSE])
    # put this in Alpha
    Alpha <- cbind(Alpha, as.data.table(Sumfl))
  } else {
    Alpha <- f
  }
}
