#' Simulates Nsim fully determined communities
#'
#' @description This function simulates Nsim fully determined communities by
#'   attributing a full botanical names to all trees with a vernacular name that
#'   were not fully determined or for which the BotaSource=Vern, using a
#'   categorical-Dirichlet association Scheme. The reason for replacing trees
#'   with BotaSource=Vern is because in these cases, the botanical name has been
#'   obtained from the vernacular name using the more likely association in the
#'   Guyafor database. As our objective is to assign a botanical name to trees
#'   with only a vernacular name using a probability of association, we don't
#'   want to keep the botanical name when BotaSource=Vern.
#'
#' @param Data2fill data.table of the dataset for which the gap filling of
#'   botanical names from vernacular names will be done. This dataset must have
#'   been prepared using the function PrepData.
#' @param DataAsso data.table of the dataset that will be used to built the
#'   association matrix, formatted as shown in the vignette. This dataset must
#'   have been prepared using the function PrepData. (Default is NULL: the
#'   dataset Data2fill is used to built the association matrix)
#' @param prior data.frame of expert knowledge association used as a prior. This
#'   dataset must have been prepared using the function PrepPrior. (Default is
#'   NULL: no prior information is used)
#' @param wp numeric value giving the weighting of the prior information
#'   (Default is 0.5).
#' @param NSim positive integer: number of simulated communities that we want to
#'   obtain
#' @param eps epsilon: background noise for species not associated with a given
#'   vernacular name. Default is 0.01.
#' @param Determ boolean: if TRUE the more likely botanical names are return
#'   when a association vernacular-botanical is performed. If FALSE, the
#'   botanical names are drawn using a categorical-Dirichlet association Scheme.
#'   If NSim i set to 1, a value needs to be provided for Determ. If NSim is set
#'   to more than 1, default is FALSE.
#'
#' @return This function returns a list of NSim data.tables, each one being the
#'   original data with two additional columns:
#'  - GensSpCor: The Genus and species after gap filling
#'  - BotaCorCode : the type of correction
#'       - fullyDet: tree with a fully determined name => no correction.
#'       - Det2Genus: identified to the genus => the corrected name is of the
#'       form Genus-Indet. (so we keep the botanically identified genus name and
#'       say that the species is Indet.)
#'       - Det2Fam: identified to the family => the corrected name is of the
#'       form Family-Indet. (so we keep the botanically identified Family name
#'       and say that the species is Indet.)
#'       - NoCor: no correction is made => the corrected name is Indet.-Indet.
#'       - AssoByGenus, AssoByGenusDeterm and AssoByGenusDetermT: a full
#'       identification is given with the method of association with the
#'       vernacular name using a Dirichlet-Categorical scheme (if Determ is set
#'       to FALSE) or with the more likely association (if Determ is set to
#'       TRUE), limiting the possibility to the species of the same genus
#'       - AssoByFam, AssoByFamDeterm and AssoByFamDetermT: a full
#'       identification is given with the method of association with the
#'       vernacular name using a Dirichlet-Categorical scheme described (if
#'       Determ is set to FALSE) or with the more likely association (if Determ
#'       is set to TRUE), limiting the possibility to the species of the same
#'       family
#'       - AssoByVerna, AssoByVerna and AssoByVernaT: full identification is
#'       given with the method of association with the vernacular name using a
#'       Dirichlet-Categorical scheme described (if Determ is set to FALSE) or
#'       with the more likely association (if Determ is set to TRUE)
#' For the three last cases, if Determ is set to TRUE and there are more than
#' one species having the maximum likelihood, a random drawn is done between
#' these species and a T is added at the end of the code (for tie).
#'
#'@details This function performs the following steps:
#'  - get a data.table containing the matrix of posterior Alpha (using function
#'  CreateAlpha) from prior knowledge in the dataset prior updated with
#'  observation of the dataset DataAsso, using a Dirichlet-Categorical scheme
#'  - get Nsim fully determined community using the function Get1Sim Nsim time
#'
#' @export
#'
#' @importFrom data.table is.data.table as.data.table
#'
#'
#'
SimFullCom <- function(Data2fill, DataAsso=NULL, prior=NULL, wp=0.5, NSim, eps=0.01, Determ=NULL) {

  # check that the input data are correct
  if (data.table::is.data.table(Data2fill)==FALSE) {
    stop("Data2fill must be formated as a data.table.
         Data must have been prepared using the function PrepData.")
  }

  if (any(!(c("CodeAlive", "idTree", "BotaCertainty", "BotaSource", "VernName",
              "Family", "Genus", "Species", "GenSp")
            %in%  colnames(Data2fill)))) {
    stop("You must provide a dataframe containing at least the following columns names for Data2Fill:
         CodeAlive, idTree, BotaCertainty, VernName, Family, Genus, Species, GenSp.
         Data must have been prepared using the function PrepData.")
  }

  if (!is.null(DataAsso)) {
    if (data.table::is.data.table(DataAsso)==FALSE) {
      stop("DataAsso must be formated as a data.table.
         Data must have been prepared using the function PrepData.")
    }
    if (any(!(c("CodeAlive", "idTree", "BotaCertainty", "BotaSource",
                "VernName", "Family", "Genus", "Species", "GenSp")
              %in% colnames(DataAsso)))) {
      stop("You must provide a dataframe containing at least the following columns names for DataAsso:
         CodeAlive, idTree, BotaCertainty, VernName, Family, Genus, Species, GenSp.
         Data must have been prepared using the function PrepData.")
    }
  }

  if(wp!=0.5 & is.null(prior)) {
    warning("you have modified the default weighing without provinding a prior, the value of wp is ignored")
  }


  # get Determ if not provided and return the appropriate warnings
  if(is.null(Determ)) {
    # if NSim
    if(NSim==1) {
      stop("If you set NSim to 1, you must provide a value for Determ")
    }
    if (NSim>1) {
      Determ <- FALSE
    }
  }
  if(Determ==TRUE & NSim>1) {
    warning("You want to get more than 1 communities simulated (NSim>1) by getting the more likely associations (Determ=TRUE),
            all the simulated communities will be identical, except for cases trees there are equally likely associations")
  }


  # get Alpha
  # if the dataset used to create Alpha is the same than the one on which we want to make the association
  if (is.null(DataAsso)) {
    DataAsso <- Data2fill
  }
  Alpha <- CreateAlpha(DataAsso=DataAsso, prior=prior, wp=wp)

  # get Nsim full determined community
  DataNSim <- vector("list", NSim)
  for (r in 1:NSim) {
    DataNSim[[r]] <- Get1Sim(Data=Data2fill, Alpha=Alpha, eps=eps, Determ=Determ)
  }

  return(DataNSim)
}
