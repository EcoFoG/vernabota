#' Paracou6_2016
#'
#' Tree inventory of plot 6 of Paracou research station (French Guiana) for 2016
#' Public dataset extracted from the Guyafor database (downloaded on 21/01/2022).
#'
#' @usage data(Paracou6_2016)
#'
#' @format A data.frame with 3620 rows and 24 variables:
#' \describe{
#'   \item{Forest}{Forest name (character)}
#'   \item{Plot}{Plot number (character)}
#'   \item{PlotArea}{Plot area, in hectare (numeric)}
#'   \item{SubPlot}{Subplot number (integer)}
#'   \item{idTree}{Unique tree identifier from the database (integer)}
#'   \item{Xfield}{Euclidean position of the tree relative to
#'   the Southwestern corner of the subplot, in m (numeric)}
#'   \item{Yfield}{Euclidean position of the tree relative to
#'   the Southwestern corner of the subplo, in m (numeric)}
#'   \item{Xutm}{Coordinate X for the tree in UTM 22 N (EPSG: 32 622) (numeric)}
#'   \item{Yutm}{Coordinate Y for the tree in UTM 22 N (EPSG: 32 622) (numeric)}
#'   \item{Lat}{Tree latitude (WGS 84, EPSG : 4326) (numeric)}
#'   \item{Lon}{Tree longitude (WGS 84, EPSG : 4326) (numeric)}
#'   \item{Family}{Botanical family (character)}
#'   \item{Genus}{Botanical genus (character)}
#'   \item{Species}{Botanical species (character)}
#'   \item{BotaSource}{Source of botanical name (character)}
#'   \item{BotaCertainty}{Level of certainty
#'   for the botanical identification (numeric)}
#'   \item{VernName}{Vernacular name (character)}
#'   \item{CensusYear}{Census year (integer)}
#'   \item{CensusDate}{Census Day (when available) (character)}
#'   \item{CensusDateCertainty}{Is the CensusDate precise (TRUE) or not (FALSE)?
#'   If FALSE, this information must not be considered for calculation (logical)}
#'   \item{CodeAlive}{Is the tree alive (TRUE) or dead (FALSE)? (logical)}
#'   \item{MeasCode}{Information on the method for measuring the circumference
#'   or on the state of the tree (integer)}
#'   \item{Circ}{Circumference of the tree at 1.30m above ground (DBH level),
#'   in cm (numeric)}
#'   \item{CircCorr}{Corrected circumference
#'   (mathematical correction of abnormal circumferences), in cm (numeric)}
#'   \item{CorrCode}{Information on the reason why and how the circumference has
#'   been corrected, multiple values are possible(character)}
#'   ...
#' }
#' @source <http://paracou.cirad.fr>
#' @source <https://paracoudata.cirad.fr/public/pdf/Paracou_data_dictionnary.pdf>
#' @source <https://paracoudata.cirad.fr/main/login/>
"Paracou6_2016"
