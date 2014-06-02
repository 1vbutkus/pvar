#' @name DataSets
#' @aliases PvarQuantileDF MeanCoef SdCoef
#' @title Data sets of Monte-Carlo simulations results
#' @description   The test \code{\link{PvarBreakTest}} uses quantiles from Monte-Carlo simulations. 
#' The results of the simulations are saved in these data sets.
#' 
#' @details
#' The distribution of p-variation of \code{BridgeT(x)} are unknown. 
#' The distribution of p-variance is approximated form Monte-Carlo simulation based on 141 millions iterations. 
#' The data frame \code{PvarQuantile} summarise the distribution of normalised statistics.
#' Meanwhile, \code{MeanCoef} and \code{SdCoef} defines the coefficients of functional form of \code{mean} and \code{sd} statistics of
#' PvarBreakTest statistics.
#'
#' @docType data
#' @usage
#' PvarQuantileDF
#' MeanCoef
#' SdCoef
#' @format the \code{PvarQuantileDF} is a \code{data.frame} with fields \code{prob} an {Qaunt}. 
#' The field \code{brob} represent the probability and \code{Quant} gives coresponging quantile.
#' \code{MeanCoef} and \code{SdCoef} is a named vector used in functions \code{\link{getMean}} and \code{\link{getSd}}. 
#'     
#' @source Monte-Carlo simulation
#' @author Vygantas Butkus <Vygantas.Butkus@@gmail.com>
#' 
NULL 
