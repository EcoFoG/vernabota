#' Draw a full species name using a Categorical-Dirichlet scheme
#'
#' @description This function draws a full species name for a given tree within the vector alphaV
#'  using a Categorical-Dirichlet scheme.
#'
#' @param alphaV vector alphaV obtained in the DrawBota function (posterior alpha
#'  for the given vernacular name, with all botanical name of Alpha)
#' @param Names vector of all botanical name of Alpha
#' @param Vern vernacular name
#'
#' @return A botanical name obtained from a Dirichlet-Categorical association.
#'
#' @details This function performs the following steps:
#'  - draw one vector in a Dirichlet distribution of shape parameters Vect
#'  - draw one value in a Categorical distribution of parameter p the vector
#'  obtained from the Dirichlet (i.e draw one botanical name one time with
#'  the probability being the vector obtained from the Dirichlet)
#'  - retrieve the botanical name corresponding to the non null value in
#'   this vector

#' @export
#'
#' @importFrom gtools rdirichlet
#' @importFrom stats rmultinom
#'
#'
#'
DirichCat <- function(alphaV, Names, Vern) {
  # draw in Dirichlet
  Vdir <- gtools ::rdirichlet(1,alphaV)
  names(Vdir) <- Names

  # Do not allow NA values in Vdir
  coutNA <- 0
  while (anyNA(Vdir) == TRUE)   {
    Vdir <- gtools ::rdirichlet(1,alphaV)
    names(Vdir) <- Names
    countNA <- coutNA + 1
  }

  if(coutNA > 0) {warning(paste('the dirichlet provided NA values', coutNA,
                                'times for the vernacular name', Vern,
                                ". The draw has been redone untill there was no more NA"))}

  # drawn in categorical  (using the function rmultinom because available in stats::)
  Draw <- rmultinom(1,1,Vdir) # gives one named vectors (names being the bota names) of only 0 except for one bota name
  resDraw <- rownames(Draw)[which(Draw>0)] # gives GensSpCor
  return(resDraw)
}
