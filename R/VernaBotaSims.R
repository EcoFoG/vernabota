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
#' @slot NScenar numeric: number of scenario simulated
#' @slot ParamScenar data.frame: a datatable with the parameters for each scenario (input of CompareSim)
#' @slot D2fill list of datasets to fill for each scenario (input of CompareSim)
#' @slot DAsso list of datasets of observation used for each scenario (input of CompareSim)
#' @slot priors list of datasets containing the priors for each scenario (input of CompareSim)
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
                                                     D2fill = "list",
                                                     DAsso = "list",
                                                     priors = "list",
                                                     pc_results = "list",
                                                     results = "list"))
