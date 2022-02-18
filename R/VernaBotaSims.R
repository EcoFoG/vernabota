#' VernaBotaSims class and associated methods
#'
#' @description The VernaBotaSims class is the output of the CompareSim function.
#'
#' Plot function to be applied on *VernaBotaSims* objects. This function draw
#' a boxplot of the percentages of well associated species for each scenario.
#'
#' Summary function to be applied on *VernaBotaSims* objects. This function print
#' the results of each scenario, together with its parameters.
#'
#' @slot NScenar numeric: number of scenarios simulated
#' @slot ParamScenar data.frame: a datatable with the parameters for each scenario (input of CompareSim)
#' @slot D2fill dataset to fill (input of CompareSim)
#' @slot DAsso list of datasets of observation used for each scenario (input of CompareSim)
#' @slot priors list of datasets containing the priors for each scenario (input of CompareSim)
#' @slot pc2fill percentage of data to fill (input of CompareSim)
#' @slot pcFamilyDet percentage of data determined
#' at the family level (from the subset of dataFill to fill, input of CompareSim)
#' @slot pcGenusDet percentage of data determined at
#'  the genus level (from the subset of dataFill to fill, the rest isn't determined at all, input of CompareSim)
#' @slot NbSim number of simulations (input of CompareSim)
#' @slot pc_results list of double corresponding to the percentage of well simulated species
#' @slot results list of results from SimFullCom for each scenario.
#'
#' @export
#'
#' @importFrom methods setClass
#' @importFrom ggplot2 ggplot geom_boxplot aes
#' @importFrom stats quantile
#'
#'
VernaBotaSims <- setClass("VernaBotaSims", slots = c(NScenar = "numeric",
                                                     ParamScenar =  "data.frame",
                                                     D2fill = "data.table",
                                                     DAsso = "list",
                                                     priors = "list",
                                                     pc2fill = "numeric",
                                                     pcFamilyDet = "numeric",
                                                     pcGenusDet = "numeric",
                                                     NbSim = "numeric",
                                                     pc_results = "list",
                                                     results = "list"))
