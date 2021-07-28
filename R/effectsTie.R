#' Remulate Tie Effects
#' 
#' This page lists the effects that are available in the remulate package.
#'
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the DyNAM model.
#' @param scaling the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling.
#' @param  variable string with the name of the column in the \code{attributes} data.frame for which the statistic has to be computed.
#' @param  attributes a data.frame object that contains the exogenous attributes. See details.
#' 
#' @param indices an integer vector specifying the indices of effects which are to be computed in the interaction effect. 
#'  
#' @details
#' The attributes object contains at least three columns (actor,time,attribute). It should be constructed as follows: Each row refers to the attribute value of actor i at timepoint t. The first column contains the actor names (corresponding to the vector of names in the \code{actors} argument of \code{\link{remulateTie}}). The second column contains the time when attributes change (set to zero if the attributes do not vary over time). Subsequent columns contain the attributes that are called in the specifications of exogenous statistics. The same \code{attributes} object can be used with multiple exogenous statistics.
# by specifying the \code{attributes} argument of \code{\link{remulateTie}}.
#' 
#' The indices aregument in the interact effect corresponds to the position of the specified effects in the \code{effects} argument of \code{\link{remulateTie}} for which the interaction needs to be computed. The individual constitutive effects for an interaction must be specified before the interact term in the \code{effects} argument. To omit the individual constitutive effects in the generation, specify the \code{param} arugment to zero.
#' @return The effect functions do not return anything when called individually. They are only used to specify statistics in the \code{effects} argument for the function \code{\link{remulateTie}}.
#' 
#' @usage
#' #Specification of endogenous effect
#' endo(param, scaling=c("raw","std","prop"))
#' 
#' #Specification of exogenous effect
#' exo(param, variable, attributes, scaling=c("raw","std"))
#' 
#' #Specification of interaction effects
#' interact(param, indices)
#' @section Remulate Effects:
#' \describe{
#' \item{\code{baseline}}{Baseline tendency for dyads to create events. The statistic equals to 1 for all dyads (i,j) in the riskset. The parameter for baseline controls the average number of events per unit time.}
#' }
#' 
#' \strong{Endogenous effects (Dyad statistics):}
#' \describe{
#'  \item{\code{inertia}}{Inertia is the tendency to create an event i->j if the event i->j occured in the past. The statistic at timepoint t for dyad (i,j) is 
#' equal to the number of (i,j) events before timepoint t.  Note: if \code{scaling} is "prop" for inertia, the statistic for dyad (i,j) at time t is divided by the out degree of the sender i at time t.}
#'
#' \item{\code{reciprocity}}{Reciprocity is the tendency to create an event i->j if j->i occured in the past.The statistic at timepoint t for dyad (i,j) is 
#' equal to the number of (j,i) events before timepoint t.  Note: if \code{scaling} is "prop" for inertia, the statistic for dyad (i,j) at time t is divided by the in degree of the sender i at time t.}
#' \item{\code{tie}}{ Tie effect is the tendency to create an event i->j if the event i->j occured at least once in the past. The statistic at timepoint t for dyad (i,j) is 
#' equal to 1 if a an event i->j occured before timepoint t}
#' }
#' 
#' \strong{Endogenous effects (Triadic statistics):}
#' \describe{
#' \item{\code{otp}}{Outgoing Two Path effect is the 
#' tendency  to create an event i->j if they have past 
#' outgoing two-paths between them (i->h->j). The statistic for dyad (i,j) 
#' at timepoint t is equal to the minimum of past 
#' (i,h), (h,j) events, summed over all h.}
#' 
#' \item{\code{itp}}{Incoming Two Path effect is the tendency  to create an event i->j if 
#' they have past incoming two-paths between them (i<-h<-j). The statistic for dyad (i,j) 
#' at timepoint t is equal to the minimum of past 
#' (j,h), (h,i) events, summed over all h.}
#' 
#' \item{\code{osp}}{Outgoing Shared Partners effect is the tendency  to create an event i->j if 
#' they have past outgoing shared partners between them (i->h<-j). The statistic for dyad (i,j) 
#' at timepoint t is equal to the minimum of past 
#' (i,h), (j,h)  events, summed over all h. }
#' 
#' \item{\code{isp}}{Incoming Shared Partners effect is the tendency  to create an event i->j if 
#' they have past incoming shared partners between them (i<-h->j). The statistic for dyad (i,j) 
#' at timepoint t is equal to the minimum of past 
#' (h,i), (h,j) events, summed over all h. }
#' }
#' 
#' \strong{Endogenous effects (Node statistics):}
#' \describe{
#' \item{\code{indegreeSender}}{ In degree effect of the sender is the tendency to create an event i->j if i has received more events in the past. The statistic at timepoint t for dyad (i,j) is equal to the number of events received by actor i before timepoint t. Note: if \code{scaling} is "prop" for indegreeSender, the statistic for dyad (i,j) at time t is divided by the total degree of the sender i at time t. }
#' \item{\code{indegreeReceiver}}{In degree effect of receiver is the tendency to create an event i->j if j has received more events in the past.  The statistic at timepoint t for dyad (i,j) is equal to the number of events received by actor j before timepoint t. Note: if \code{scaling} is "prop" for indegreeReceiver, the statistic for dyad (i,j) at time t is divided by the total degree of the receiver j at time t. }
#' \item{\code{outdegreeSender}}{Out degree effect of sender is the tendency to create an event i->j if i has sent more events in the past. Note: if \code{scaling} is "prop" for outdegreeSender, the statistic for dyad (i,j) at time t is divided by the total degree of the sender i at time t. }
#' \item{\code{outdegreeReceiver}}{Out degree effect of receiver is the tendency to create an event i->j if j has sent more events in the past. Note: if \code{scaling} is "prop" for outdegreeReceiver, the statistic for dyad (i,j) at time t is divided by the total degree of the receiver j at time t. }
#' \item{\code{totaldegreeSender}}{Total degree effect of sender is the tendency to create an event i->j if i has sent and received more events in the past.}
#' \item{\code{totaldegreeReceiver}}{Total degree effect of receiver is the tendency to create an event i->j if j has sent and received more events in the past.}
#' 
#' }
#' 
#' \strong{Endogenous effects (Participating Shifts):}
#' \describe{
#' \item{\code{psABBA}}{ AB-BA Pacticipating shift (turn receiving) is the tendency to create an event j->i at timepoint t if event i->j occured at timepoint t-1. The psABBA statistic is equal to one for the dyad (j.i) that will create the participation shift at timepoint t.}
#' \item{\code{psABBY}}{ AB-BY Participating shift (turn receiving) is the tendency to create an event j->h at timepoint t if event i->j occured at timepoint t-1. The psABBY statistic is equal to one for the dyads (j,h) for all h not equal to i, that will create the participation shift at timepoint t.}
#' \item{\code{PSABAY}}{AB-AY Participating shifts  (turn continuing) is the tendency to create an event i->h at timepoint t if event i->j occured at timepoint t-1. The PSABAY statistic is equal to one for the dyads (i,h) for all h not equal to j, that will create the participation shift at timepoint t. }
#' \item{\code{psABXA}}{ AB-XA Participating shifts (turn usurping) is the tendency to create an event h->i at timepoint t if event i->j occured at timepoint t-1. The psABXA statistic is equal to one for the dyads (h,i) for all h not equal to j, that will create the participation shift at timepoint t.}
#' \item{\code{psABXB}}{ AB-XB Participating shifts (turn usurping) is the tendency to create an event h->j at timepoint t if event i->j occured at timepoint t-1. The psABXB statistic is equal to one for the dyads (h,j) for all h not equal to i, that will create the participation shift at timepoint t.}
#' \item{\code{psABXY}}{ AB-XY Participating shifts (turn usurping) is the tendency to create an event h->k at timepoint t if event i->j occured at timepoint t-1. The psABXB statistic is equal to one for the dyads (h,k) for all h not equal to i and k not equal to j, that will create the participation shift at timepoint t. }
#' }
#' 
#' \strong{Exogenous effects (Node Attributes):}
#' \describe{
#' \item{\code{send}}{The tendency to create an event i->j when i has a high attribute value.}
#' \item{\code{receive}}{The tendency to create an event i->j when j has a high attribute value.}
#' }
#' 
#' \strong{Exogenous effects (Tie Attribute):}
#' \describe{
#' \item{\code{same}}{ (Homophily) is the tendency to create an event i->j if actors i and j have the same attribute values}
#' \item{\code{difference}}{ (Heterophily) is the tendency to create an event i->j if actors i and j have a high absolute difference in attribute values}
#' \item{\code{average}}{ average attribute value for dyad (i,j) is the average of the attribute values for actors i, j}
#' \item{\code{minimum}}{minimum attribute value for dyad (i,j) is the smaller of the attribute values for actors i , j}
#' \item{\code{maximum}}{maximum attribute value for dyad (i,j) is the bigger of the attribute values for actors i , j}
#' }
#' 
#' @examples
#' #To specify an endogenous effect (example inertia)
#' effects <- ~ inertia(0.1 , scaling = "std")
#' 
#' # To specify an exogenous effect (example same)
#' cov <- data.frame(actor = 1:10, time = rep(0,10), gender = sample(c(0,1), replace=T, 10), age=sample(20:30, 10, replace=T))
#' effects <- ~ same(0.2 , variable="gender", attributes = cov)
#'
#' #If parameter is constant
#' effects <- ~ inertia(0.3) + same(0.2 , variable="gender", attributes = cov) + reciprocity(-0.1) + itp(0.01)
#' 
#' #If parameter varies with time
#' effects <- ~ inertia(param = function(t) exp(-t)) + same(0.2 , variable="gender", attributes = cov) + reciprocity(-0.1) + itp(0.01)
#' 
#' #To specify an interaction (example between inertia and same constitutive effects)
#' effects <- ~ inertia(0.3) + same(0.2 , variable="gender", attributes = cov) + reciprocity(-0.1) + itp(0.01) + interact(0.1,indices = c(1,2))
remulateTieEffects <- function() {
  print("")
}


#baseline
baseline <- function(param = NULL) {
  scaling <- "raw"
  out <- prepEndoVar("baseline", param, scaling)
  out
}


#tie
# 
# This function specifies the input for the tie effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the tie statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
tie <- function(param = NULL, scaling = c("raw", "std", "prop"), start = 0, end = 0) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("tie", param, scaling, start, end)
  out
}


#inertia
# 
# This function specifies the input for the inertia effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the inertia statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
inertia <- function(param = NULL, scaling = c("raw", "std", "prop"), start = 0, end = 0) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("inertia", param, scaling, start, end)
  out
}


#reciprocity
# 
# This function specifies the input for the reciprocity effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the reciprocity statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the in degree of the sender at time t.
reciprocity <- function(param = NULL, scaling = c("raw", "std", "prop"), start = 0, end = 0) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("reciprocity", param, scaling, start, end)
  out
}

#indegreeSender
# 
# This function specifies the input for the indegreeSender effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the indegreeSender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
indegreeSender <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("indegreeSender", param, scaling)
  out
}

#indegreeReceiver
# 
# This function specifies the input for the indegreeReceiver effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the indegreeReceiver statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
indegreeReceiver <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("indegreeReceiver", param, scaling)
  out
}

#outdegreeSender
# 
# This function specifies the input for the outdegreeSender effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the outdegreeSender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the total degree of the sender at time t.
outdegreeSender <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("outdegreeSender", param, scaling)
  out
}


#outdegreeReceiver
# 
# This function specifies the input for the outdegreeReceiver effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the outdegreeReceiver statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the total degree of the sender at time t.
outdegreeReceiver <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("outdegreeReceiver", param, scaling)
  out
}

#totaldegreeSender
# 
# This function specifies the input for the totaldegreeSender effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the totaldegreeSender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
totaldegreeSender <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("totaldegreeSender", param, scaling)
  out
}


#totaldegreeReceiver
# 
# This function specifies the input for the totaldegreeReceiver effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the totaldegreeReceiver statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
totaldegreeReceiver <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("totaldegreeReceiver", param, scaling)
  out
}

#otp
# 
# This function specifies the input for the otp effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the otp statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
otp <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("otp", param, scaling)
  out
}

#itp
# 
# This function specifies the input for the itp effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the itp statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
itp <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("itp", param, scaling)
  out
}

#osp
# 
# This function specifies the input for the osp effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the osp statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
osp <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("osp", param, scaling)
  out
}

#isp
# 
# This function specifies the input for the isp effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the isp statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
isp <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("isp", param, scaling)
  out
}

#psABBA
# 
# This function specifies the input for the psABBA effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the psABBA statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
psABBA <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("psABBA", param, scaling)
  out
}


#psABBY
# 
# This function specifies the input for the psABBY effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the psABBY statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
psABBY <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("psABBY", param, scaling)
  out
}


#psABXA
# 
# This function specifies the input for the psABXA effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the psABXA statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
psABXA <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("psABXA", param, scaling)
  out
}


#psABXB
# 
# This function specifies the input for the psABXB effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the psABXB statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
psABXB <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("psABXB", param, scaling)
  out
}


#psABXY
# 
# This function specifies the input for the psABXY effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the psABXY statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
psABXY <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("psABXY", param, scaling)
  out
}

#psABAY
# 
# This function specifies the input for the psABAY effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param scaling the method for scaling the psABAY statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
psABAY <- function(param = NULL, scaling = c("raw", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("psABAY", param, scaling)
  out
}

#send
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# @param variable character vector: specifies the name of the column with covariate value in data dataframe
# @param data data.frame object with rows specifying values of data for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the data with column name corresponding to variable name
# @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
send <- function(param = NULL, variable, attributes, scaling = c("raw", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar("send", param, scaling, variable, attributes)
  out
}

#receive
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# @param variable character vector: specifies the name of the column with covariate value in attributes data.frame
# @param attributes attributes.frame object with rows specifying values of attributes for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
# @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time  
receive <- function(param = NULL, variable, attributes, scaling = c("raw", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar("receive", param, scaling, variable, attributes)
  out
}

#same
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# @param variable character vector: specifies the name of the column with covariate value in attributes data.frame
# @param attributes attributes.frame object with rows specifying values of attributes for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
# @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time  
same <- function(param = NULL, variable, attributes, scaling = c("raw", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar("same", param, scaling, variable, attributes)
  out
}

#difference
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# @param variable character vector: specifies the name of the column with covariate value in attributes data.frame
# @param attributes attributes.frame object with rows specifying values of attributes for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
# @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time  
difference <- function(param = NULL, variable, attributes, scaling = c("raw", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar("difference", param, scaling, variable, attributes)
  out
}

#average
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# @param variable character vector: specifies the name of the column with covariate value in attributes data.frame
# @param attributes attributes.frame object with rows specifying values of attributes for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
# @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time  
average <- function(param = NULL, variable, attributes, scaling = c("raw", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar("average", param, scaling, variable, attributes)
  out
}

#minimum
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# @param variable character vector: specifies the name of the column with covariate value in attributes data.frame
# @param attributes attributes.frame object with rows specifying values of attributes for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
# @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time  
minimum <- function(param = NULL, variable, attributes, scaling = c("raw", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar("minimum", param, scaling, variable, attributes)
  out
}

#maximum
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# @param variable character vector: specifies the name of the column with covariate value in attributes data.frame
# @param attributes attributes.frame object with rows specifying values of attributes for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
# @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
maximum <- function(param = NULL, variable, attributes, scaling = c("raw", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar("maximum", param, scaling, variable, attributes)
  out
}

# interact
#
# This function specifies the input for the interact effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
# 
# @param indices is a numeric vector of indices on which the interaction term needs to be computed.
interact <- function(param = NULL, indices) {
  out <- prepInteractVar(param, indices)
  out
}