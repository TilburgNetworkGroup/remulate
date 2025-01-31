#' Remulate Actor Effects
#' 
#' This page lists the effects that are available in the remulate package for the actor oriented relational event model.
#'
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the model.
#' @param scaling the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling.
#' @param  variable string with the name of the column in the \code{attributes} data.frame for which the statistic has to be computed.
#' @param  attributes a data.frame object that contains the exogenous attributes (see details).
#'  
#' @return The effect functions do not return anything when called individually. They are only used to specify statistics in the \code{effects} argument for the function \code{\link{remulateActor}}.
#' 
#' @details
#' The attributes object contains at least three columns (actor,time,attribute). It should be constructed as follows: Each row refers to the attribute value of actor i at timepoint t. The first column contains the actor names (corresponding to the vector of names in the \code{actors} argument of \code{\link{remulateActor}}). The second column contains the time when attributes change (set to zero if the attributes do not vary over time). Subsequent columns contain the attributes that are called in the specifications of exogenous statistics. The same \code{attributes} object can be used with multiple exogenous statistics.
# by specifying the \code{attributes} argument of \code{\link{remulateActor}}.
#' 
#' @usage
#' #Specification of endogenous effect
#' endo(param, scaling=c("raw","std","prop))
#' 
#' #Specification of exogenous effect
#' exo(param, scaling=c("raw","std","prop), variable, attributes)
#' 
#' @section remulateActor Rate Effects:
#' \strong{Endogenous effects:}
#' \describe{
#' \item{\code{baseline}}{Baseline tendency for actors to create events. The statistic equals to 1 for all actors in the riskset. The parameter for baseline controls the average number of events per unit time.}
#' 
#' \item{\code{indegreeSender}}{ In degree effect of the sender is the tendency for actor i to create an event when i has received more events in the past. The statistic at timepoint t for dyad (i,j) is equal to the number of events received by actor i before timepoint t. Note: if \code{scaling} is "prop" for indegreeSender, the statistic for dyad (i,j) at time t is divided by the total degree of the sender i at time t. }
#' 
#' \item{\code{outdegreeSender}}{Out degree effect of sender is the tendency for actor i to create an event when i has sent more events in the past. Note: if \code{scaling} is "prop" for outdegreeSender, the statistic for dyad (i,j) at time t is divided by the total degree of the sender i at time t. }
#' 
#' \item{\code{totaldegreeSender}}{Total degree effect of sender is the tendency for actor i to create an event when i has sent and received more events in the past.}

#' 
#' \item{\code{ospSender}}{Outgoing Shared Partners actor effect is the tendency for actor i to create an event 
#' if actor i is the source in a transitive structure (i->h<-j<-i). }
#' 
#' \item{\code{otpSender}}{Outgoing Two Path actor effect is the tendency for sender i to create an event 
#' if actor i is the source in a transitive structure (i->h->j<-i). }
#' 
#' }
#' 
#' \strong{Exogenous effects:}
#' \describe{
#' \item{\code{send}}{The tendency for actor i to create an event when i has a high attribute value.}
#' 
#' }
#' 
#' @section remulateActor Choice Effects:
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
#' 
#' \strong{Endogenous effects (Node statistics):}
#' \describe{
#' \item{\code{indegreeReceiver}}{In degree effect of receiver is the tendency to create an event i->j if j has received more events in the past.  The statistic at timepoint t for dyad (i,j) is equal to the number of events received by actor j before timepoint t. Note: if \code{scaling} is "prop" for indegreeReceiver, the statistic for dyad (i,j) at time t is divided by the total degree of the receiver j at time t. }
#' \item{\code{outdegreeReceiver}}{Out degree effect of receiver is the tendency to create an event i->j if j has sent more events in the past. Note: if \code{scaling} is "prop" for outdegreeReceiver, the statistic for dyad (i,j) at time t is divided by the total degree of the receiver j at time t. }
#' \item{\code{totaldegreeReceiver}}{Total degree effect of receiver is the tendency to create an event i->j if j has sent and received more events in the past.}
#' 
#' }
#'
#' \strong{Exogenous effects:}
#' \describe{
#' \item{\code{dyad}}{ Dyadic attribute value is tendency to create an event i -> j when (i,j) has a high attribute value.}
#' \item{\code{receive}}{Receiver attribute value is the tendency to create an event i->j when j has a high attribute value.}
#' \item{\code{same}}{Same attribute value (Homophily) is the tendency to create an event i->j if actors i and j have the same attribute values}
#' \item{\code{Difference}}{difference attribute value (Heterophily) is the tendency to create an event i->j if actors i and j have a high absolute difference in attribute values}
#' }
#' 
#' @examples 
#' 
#' # To specify an endogenous effects (example inertia)
#' effects <- ~  inertia(0.1 , scaling = "std") 
#' 
#' # To specify an exogenous effects (example same)
#' cov <- data.frame(actor = 1:10, time = rep(0,10), gender = sample(c(0,1), replace=TRUE, 10), age=sample(20:30, 10, replace=TRUE))
#' effects <- ~ same(0.2 , variable="gender", attributes = cov)
#' 
#' #Rate Effects:
#' #If parameter is constant
#' rateEffects <- ~ outdegreeSender(0.3) + send(0.1,variable = "age",attributes = cov)
#'
#' #If parameter varies with time
#' rateEffects <- ~ outdegreeSender(param = function(t) exp(-t)) + send(0.1,variable="age",attributes=cov)
#' 
#' #Choice Effects:
#' #If parameter is constant
#' choiceEffects <- ~ inertia(0.4) + reciprocity(-0.1) + same(0.2,variable="gender",attributes=cov) + receive(0.1, variable="age", attributes=cov)
#' 
#' #If parameter varies with time
#' choiceEffects <- ~ inertia(param = function(t) exp(-t)) +  reciprocity(-0.1) + same(0.2,variable="gender",attributes=cov) + receive(0.1,variable="age",attributes=cov)
#' 
remulateActorEffects <- function() {
  print("")
}

#otp sender
# 
# This function specifies the input for the otp  sender effect in the \code{s_formula} argument for the function \code{\link{remulateActor}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the Actor Oriented Model
# 
# @param scaling the method for scaling the otp sender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.

ospSender <- function(param = NULL, scaling = c("raw", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("ospSender", param, scaling)
  out
}

#osp sender
# 
# This function specifies the input for the osp  sender effect in the \code{s_formula} argument for the function \code{\link{remulateActor}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the Actor Oriented Model
# 
# @param scaling the method for scaling the osp sender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.

otpSender <- function(param = NULL, scaling = c("raw", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("otpSender", param, scaling)
  out
}

#itp sender
# 
# This function specifies the input for the itp  sender effect in the \code{s_formula} argument for the function \code{\link{remulateActor}}. Not to be used independently
# 
# @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the Actor Oriented Model
# 
# @param scaling the method for scaling the itp sender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.

itp_s <- function(param = NULL, scaling = c("raw", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar("itp_s", param, scaling)
  out
}
