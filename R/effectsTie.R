#' Remulate Tie Effects
#' 
#' This page lists the effects that are available in the remulate package for the tie oriented relational event model.
#' 
#' @param endogenous Logical or NULL.   If `TRUE`, returns only endogenous effects.  If `FALSE`, returns only exogenous effects.  If `NULL` (default), returns both endogenous and exogenous effects.
#'
#' @details
#' 
#' The attr_actors object for exogenous effects based on actor covariates (\code{send}, \code{receive}, \code{same}, \code{difference}, \code{average}, \code{max}, \code{min}) contains at least three columns (actor,time,attribute). It should be constructed as follows: Each row refers to the attribute value of actor i at timepoint t. The first column contains the actor names (corresponding to the vector of names in the \code{actors} argument of \code{\link{remulateTie}}). The second column contains the time when attr_actors change (set to zero if the attr_actors do not vary over time). At least one of the subsequent columns must contain values for the attr_actors with column name corresponding to variable name specified in the effect specification.
# by specifying the \code{attr_actors} argument of \code{\link{remulateTie}}.
#' 
#' The attribute object for exogenous effect \code{dyad} contains at least three columns (sender_id,receiver_id,attribute). It should be constructed as follows: First column must contain sender id, second column receiver id, at least one of the subsequent columns must contain values for the attr_actors with column name corresponding to variable name specified in the effect specification.
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' 
#' 
#' The indices aregument in the interact effect corresponds to the position of the specified effects in the \code{effects} argument of \code{\link{remulateTie}} for which the interaction needs to be computed. The individual constitutive effects for an interaction must be specified before the interact term in the \code{effects} argument. To omit the individual constitutive effects in the generation, specify the \code{param} arugment to zero.
#' @return Returns a character vector of available effects for the \code{effects} argument for the function \code{\link{remulateTie}}.
#' @section Remulate Effects:
#' \describe{
#' \item{\code{baseline}}{Baseline tendency for dyads to create events. The statistic equals to 1 for all dyads (i,j) in the riskset. The parameter for baseline controls the average number of events per unit time.}
#' }
#' 
#' \strong{Endogenous effects (Dyad statistics):}
#' \describe{
#'  \item{\code{inertia}}{Inertia is the tendency to create an event i->j if the event i->j occurred in the past. The statistic at timepoint t for dyad (i,j) is 
#' equal to the number of (i,j) events before timepoint t.  Note: if \code{scaling} is "prop" for inertia, the statistic for dyad (i,j) at time t is divided by the out degree of the sender i at time t.}
#'
#' \item{\code{reciprocity}}{Reciprocity is the tendency to create an event i->j if j->i occurred in the past.The statistic at timepoint t for dyad (i,j) is 
#' equal to the number of (j,i) events before timepoint t.  Note: if \code{scaling} is "prop" for reciprocity, the statistic for dyad (i,j) at time t is divided by the in degree of the sender i at time t.}
#' \item{\code{tie}}{ Tie effect is the tendency to create an event i->j if the event i->j occurred at least once in the past. The statistic at timepoint t for dyad (i,j) is 
#' equal to 1 if a an event i->j occurred before timepoint t}
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
#' \item{\code{indegreeSender}}{ In degree effect of the sender is the tendency 
#' to create an event i->j if i has received more events in the past. 
#' The statistic at timepoint t for dyad (i,j) is equal to the number of events 
#' received by actor i before timepoint t. Note: if \code{scaling} is "prop" for indegreeSender, 
#' the statistic for dyad (i,j) at time t is divided by the number of past events until time t. }
#' \item{\code{indegreeReceiver}}{In degree effect of receiver is the tendency 
#' to create an event i->j if j has received more events in the past.  The statistic at timepoint t for dyad (i,j) is equal to the number of events received by actor j before timepoint t. Note: if \code{scaling} is "prop" for indegreeReceiver, the statistic for dyad (i,j) at time t is divided by the number of past events until time t. }
#' \item{\code{outdegreeSender}}{Out degree effect of sender is the tendency
#'  to create an event i->j if i has sent more events in the past. Note: if \code{scaling} is "prop" for outdegreeSender, the statistic for dyad (i,j) at time t is divided by by the number of past events until time t. }
#' \item{\code{outdegreeReceiver}}{Out degree effect of receiver is the tendency to create an event i->j if j has sent more events in the past. Note: if \code{scaling} is "prop" for outdegreeReceiver, the statistic for dyad (i,j) at time t is divided by the number of past events until time t. }
#' \item{\code{totaldegreeSender}}{Total degree effect of sender is the tendency to create an event i->j if i has sent and received more events in the past.}
#' \item{\code{totaldegreeReceiver}}{Total degree effect of receiver is the tendency to create an event i->j if j has sent and received more events in the past.}
#' 
#' }
#' 
#' \strong{Endogenous effects (Participating Shifts):}
#' \describe{
#' \item{\code{psABBA}}{ AB-BA Pacticipating shift (turn receiving) is the tendency to create an event j->i at timepoint t if event i->j occurred at timepoint t-1. The psABBA statistic is equal to one for the dyad (j.i) that will create the participation shift at timepoint t.}
#' \item{\code{psABBY}}{ AB-BY Participating shift (turn receiving) is the tendency to create an event j->h at timepoint t if event i->j occurred at timepoint t-1. The psABBY statistic is equal to one for the dyads (j,h) for all h not equal to i, that will create the participation shift at timepoint t.}
#' \item{\code{PSABAY}}{AB-AY Participating shifts  (turn continuing) is the tendency to create an event i->h at timepoint t if event i->j occurred at timepoint t-1. The PSABAY statistic is equal to one for the dyads (i,h) for all h not equal to j, that will create the participation shift at timepoint t. }
#' \item{\code{psABXA}}{ AB-XA Participating shifts (turn usurping) is the tendency to create an event h->i at timepoint t if event i->j occurred at timepoint t-1. The psABXA statistic is equal to one for the dyads (h,i) for all h not equal to j, that will create the participation shift at timepoint t.}
#' \item{\code{psABXB}}{ AB-XB Participating shifts (turn usurping) is the tendency to create an event h->j at timepoint t if event i->j occurred at timepoint t-1. The psABXB statistic is equal to one for the dyads (h,j) for all h not equal to i, that will create the participation shift at timepoint t.}
#' \item{\code{psABXY}}{ AB-XY Participating shifts (turn usurping) is the tendency to create an event h->k at timepoint t if event i->j occurred at timepoint t-1. The psABXB statistic is equal to one for the dyads (h,k) for all h not equal to i and k not equal to j, that will create the participation shift at timepoint t. }
#' }
#' 
#' \strong{Endogenous effects (Recency statistics):}
#' \describe{
#' \item{\code{recencyContinue}}{The recencyContinue effect refers to a recency statistic similar to what is
#' described in Vu et al. (2017) and Mulder and Leenders (2019). For each
#' timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the time
#' that has past since the dyad was last active + 1).}
#' \item{\code{recencySendSender}}{ The recencySendSender effect refers to a recency statistic similar to what
#' is described in Vu et al. (2017) and Mulder and Leenders (2019). For each
#' timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the time
#' that has past since sender i was last active as sender + 1).}
#' \item{\code{recencySendReceiver}}{The recencySendReceiver effect refers to a recency statistic similar to what
#' is described in Vu et al. (2017) and Mulder and Leenders (2019). For each
#' timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the time
#' that has past since receiver j was last active as sender + 1). }
#' \item{\code{recencyReceiveSender}}{The recencyReceiveSender effect refers to a recency statistic similar to
#' what is described in Vu et al. (2017) and Mulder and Leenders (2019). For
#' each timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the
#' time that has past since sender i was last active as receiver + 1). }
#' \item{\code{recencyReceiveReceiver}}{The recencyReceiveReceiver effect refers to a recency statistic similar to
#' what is described in Vu et al. (2017) and Mulder and Leenders (2019). For
#' each timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the
#' time that has past since receiver j was last active as receiver + 1).}
#'} 
#' 

#' \strong{Exogenous effects (Node attr_actors):}
#' \describe{
#' \item{\code{send}}{Sender covariate: The tendency to create an event i->j when i has a high attribute value.}
#' \item{\code{receive}}{Receiver covariate: The tendency to create an event i->j when j has a high attribute value.}
#' }
#' 
#' \strong{Exogenous effects (Tie Attribute):}
#' \describe{
#' \item{\code{dyad}}{ dyad attribute value is the tendency to create an event i -> j when (i,j) has a high attribute value.}
#' \item{\code{same}}{ (Homophily) is the tendency to create an event i->j if actors i and j have the same attribute values}
#' \item{\code{difference}}{ (Heterophily) is the tendency to create an event i->j if actors i and j have a high absolute difference in attribute values}
#' \item{\code{average}}{average attribute value for dyad (i,j) is the average of the attribute values for actors i, j}
#' \item{\code{minimum}}{minimum attribute value for dyad (i,j) is the smaller of the attribute values for actors i , j}
#' \item{\code{maximum}}{maximum attribute value for dyad (i,j) is the bigger of the attribute values for actors i , j}
#' }
#' 
#' @examples
#' #To specify an endogenous effect (example: inertia)
#' 
#' effects <- ~ inertia(0.1, scaling = "std")
#' 
#' #To specify an exogenous effect (example: same)
#' 
#' cov <- data.frame(
#'   actor = 1:10,
#'   time = rep(0, 10),
#'   gender = sample(c(0, 1), replace = TRUE, 10),
#'   age = sample(20:30, 10, replace = TRUE)
#' )
#' 
#' effects <- ~ same(0.2, variable = "gender", attr_actors = cov)
#' 
#' #To specify an exogenous dyadic effect (example: dyad)
#' 
#' cov <- expand.grid(1:10, 1:10)
#' cov <- cov[cov[, 1] != cov[, 2], ]
#' cov$grade <- runif(90, 1, 10)
#' 
#' effects <- ~ dyad(0.2, variable = "grade", attr_actors = cov)
#' 
#' #If parameter is constant
#' 
#' effects <- ~ inertia(0.3) + 
#'   same(0.2, variable = "gender", attr_actors = cov) + 
#'   reciprocity(-0.1) + 
#'   itp(0.01)
#' 
#' #If parameter varies with time
#' 
#' effects <- ~ inertia(param = function(t) exp(-t)) + 
#'   same(0.2, variable = "gender", attr_actors = cov) +
#'   reciprocity(-0.1) + 
#'   itp(0.01)
#' 
#'  #If parameter varies across dyads or actors
#'  rs <- expand.grid(1:10,1:10)
#'  rs <- rs[rs[,1] != rs[, 2],]
#' 
#'  param_df <- as.data.frame(rs)
#'  param_df$beta = runif(nrow(rs),-0.1,0.1)
#' 
#'  effects <- ~ remulate::baseline(-3)+
#'     remulate::inertia(param_df) +
#'     remulate::reciprocity(0.1)
#' 
#'  
#' #To specify an interaction (example: between inertia and same constitutive effects)
#' 
#' effects <- ~ inertia(0.3) + 
#'   same(0.2, variable = "gender", attr_actors = cov) + 
#'   reciprocity(-0.1) + 
#'   itp(0.01) + 
#'   interact(0.1, indices = c(1, 2))
remulateTieEffects <- function(endogenous = NULL) {
  if(is.null(endogenous)){
    effects <- c(
        "baseline", "send", "receive", 
        "same", "difference", "average",        
        "minimum", "maximum", 
        "tie", "inertia", "reciprocity",
        "indegreeSender", "indegreeReceiver",
         "outdegreeSender", "outdegreeReceiver",
        "totaldegreeSender", "totaldegreeReceiver",
         "otp", "itp", "osp", "isp",
        "psABBA", "psABBY", "psABXA", 
        "psABXB", "psABXY", "psABAY", "dyad",
        "interact", "recencyContinue", "recencySendSender", "recencySendReceiver",
        "recencyReceiveSender", "recencyReceiveReceiver", "rrankSend", "rrankReceive")
    return(effects)
  }
  if(endogenous){
    effects <- c("send", "receive", #2 #3
    "same", "difference", "average", #4 #5 #6
    "minimum", "maximum", "dyad")
    return(effects)
  }
  if(!endogenous){
     effects <- c(
    "baseline", #1    
    "tie", "inertia", "reciprocity", #9 #10 #11
    "indegreeSender", "indegreeReceiver", #12 #13
    "outdegreeSender", "outdegreeReceiver", #14 #15
    "totaldegreeSender", "totaldegreeReceiver", #16, #17
    "otp", "itp", "osp", "isp", #18 #19 #20 #21
    "psABBA", "psABBY", "psABXA",  #22 #23 #24
    "psABXB", "psABXY", "psABAY",  #25 #26 #27
    
    "recencyContinue", #30
    "recencySendSender","recencySendReceiver", #31,#32
    "recencyReceiveSender","recencyReceiveReceiver", #33, #34
    "rrankSend","rrankReceive" #35, #36
  )
  return(effects)
  }
  
}

#'baseline
#'
#' This function specified the input for the baseline effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. 
#'
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the baseline in the REM model
#' 
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
baseline <- function(param = NULL) {
  out <- prepEndoVar(effect_name = "baseline", param = param, scaling = "none")
  out
}


#'tie
#' 
#' This function specifies the input for the tie effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the tie statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
tie <- function(param = NULL, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "tie", param = param, scaling = scaling, start=0, end=0)
  out
}


#'inertia
#' 
#' This function specifies the input for the inertia effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the inertia statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
inertia <- function(param = NULL, scaling = c("none", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "inertia", param = param, scaling = scaling, start=0, end=0)
  out
}


#'reciprocity
#' 
#' This function specifies the input for the reciprocity effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the reciprocity statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the in degree of the sender at time t.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
reciprocity <- function(param = NULL, scaling = c("none", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "reciprocity", param = param, scaling = scaling, start=0, end=0)
  out
}

#'indegreeSender
#' 
#' This function specifies the input for the indegreeSender effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the indegreeSender statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the number of past events until time t.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
indegreeSender <- function(param = NULL, scaling = c("none", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "indegreeSender", param = param, scaling = scaling, start=0, end=0)
  out
}

#'indegreeReceiver
#' 
#' This function specifies the input for the indegreeReceiver effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the indegreeReceiver statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the number of past events until time t.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
indegreeReceiver <- function(param = NULL, scaling = c("none", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "indegreeReceiver", param = param, scaling = scaling, start=0, end=0)
  out
}

#'outdegreeSender
#' 
#' This function specifies the input for the outdegreeSender effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the outdegreeSender statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the number of past events until time t.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
outdegreeSender <- function(param = NULL, scaling = c("none", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "outdegreeSender", param = param, scaling = scaling, start=0, end=0)
  out
}


#'outdegreeReceiver
#' 
#' This function specifies the input for the outdegreeReceiver effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the outdegreeReceiver statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the number of past events until time t.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
outdegreeReceiver <- function(param = NULL, scaling = c("none", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "outdegreeReceiver", param = param, scaling = scaling, start=0, end=0)
  out
}

#'totaldegreeSender
#' 
#' This function specifies the input for the totaldegreeSender effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the totaldegreeSender statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
totaldegreeSender <- function(param = NULL, scaling = c("none", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "totaldegreeSender", param = param, scaling = scaling, start=0, end=0)
  out
}


#'totaldegreeReceiver
#' 
#' This function specifies the input for the totaldegreeReceiver effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the totaldegreeReceiver statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
totaldegreeReceiver <- function(param = NULL, scaling = c("none", "std", "prop")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "totaldegreeReceiver", param = param, scaling = scaling, start=0, end=0)
  out
}

#'otp
#' 
#' This function specifies the input for the otp effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the otp statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
otp <- function(param = NULL, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "otp", param = param, scaling = scaling, start=0, end=0)
  out
}

#'itp
#' 
#' This function specifies the input for the itp effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the itp statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
itp <- function(param = NULL, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "itp", param = param, scaling = scaling, start=0, end=0)
  out
}

#'osp
#' 
#' This function specifies the input for the osp effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the osp statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
osp <- function(param = NULL, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "osp", param = param, scaling = scaling, start=0, end=0)
  out
}

#'isp
#' 
#' This function specifies the input for the isp effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the isp statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
isp <- function(param = NULL, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "isp", param = param, scaling = scaling, start=0, end=0)
  out
}

#'psABBA
#' 
#' This function specifies the input for the psABBA effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABBA statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
psABBA <- function(param = NULL, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "psABBA", param = param, scaling = scaling)
  out
}


#'psABBY
#' 
#' This function specifies the input for the psABBY effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABBY statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
psABBY <- function(param = NULL, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "psABBY", param = param, scaling = scaling)
  out
}


#'psABXA
#' 
#' This function specifies the input for the psABXA effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABXA statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
psABXA <- function(param = NULL, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "psABXA", param = param, scaling = scaling)
  out
}


#'psABXB
#' 
#' This function specifies the input for the psABXB effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABXB statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
psABXB <- function(param = NULL, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "psABXB", param = param, scaling = scaling)
  out
}


#'psABXY
#' 
#' This function specifies the input for the psABXY effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABXY statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
psABXY <- function(param = NULL, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "psABXY", param = param, scaling = scaling)
  out
}

#'psABAY
#' 
#' This function specifies the input for the psABAY effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABAY statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point.
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
psABAY <- function(param = NULL, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepEndoVar(effect_name = "psABAY", param = param, scaling = scaling)
  out
}

#' recencyContinue
#' 
#' This function specifies the input for the recencyContinue effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
recencyContinue <- function(param = NULL) {
  out <- prepEndoVar(effect_name = "recencyContinue", param = param, scaling = "none")
  out
}

#' recencySendSender
#' 
#' This function specifies the input for the recencySendSender effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
recencySendSender <- function(param = NULL) {
  out <- prepEndoVar(effect_name = "recencySendSender", param = param, scaling = "none")
  out
}

#' recencySendReceiver
#' 
#' This function specifies the input for the recencySendReceiver effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
recencySendReceiver <- function(param = NULL) {
  out <- prepEndoVar(effect_name = "recencySendReceiver", param = param, scaling = "none")
  out
}


#' recencyReceiveSender
#' 
#' This function specifies the input for the recencyReceiveSender effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
recencyReceiveSender <- function(param = NULL) {
  out <- prepEndoVar(effect_name = "recencyReceiveSender", param = param, scaling = "none")
  out
}

#' recencyReceiveReceiver
#' 
#' This function specifies the input for the recencyReceiveReceiver effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
recencyReceiveReceiver <- function(param = NULL) {
  out <- prepEndoVar(effect_name = "recencyReceiveReceiver", param = param, scaling = "none")
  out
}


#' rrankReceive
#' 
#' This function specifies the input for the rrankReceive effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
rrankReceive <- function(param = NULL) {
  out <- prepEndoVar(effect_name = "rrankReceive", param = param, scaling = "none")
  out
}


#' rrankSend
#' 
#' This function specifies the input for the rrankSend effect in the \code{formula} argument for the function \code{\link{remulateTie}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
rrankSend <- function(param = NULL) {
  out <- prepEndoVar(effect_name = "rrankSend", param = param, scaling = "none")
  out
}


#'send
#' 
#' This function specifies the input for the send effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' Sender covariate: The tendency to create an event i->j when i has a high attribute value.
#'
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector specifies the name of the column with covariate value in data dataframe
#' @param attr_actors data.frame object with rows specifying values of data for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the data with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
send <- function(param = NULL, variable, attr_actors, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar("send", param = param, scaling = scaling, variable = variable, attr_actors = attr_actors)
  out
}

#'receive
#' 
#' This function specifies the input for the receive effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' Receiver covariate: The tendency to create an event i->j when j has a high attribute value.
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector specifies the name of the column with covariate value in attr_actors data.frame
#' @param attr_actors data.frame object with rows specifying values of attr_actors for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time  
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
receive <- function(param = NULL, variable, attr_actors, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar(effect_name = "receive", param = param, scaling = scaling, variable = variable, attr_actors = attr_actors)
  out
}

#'same
#' 
#' This function specifies the input for the same effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' Dyadic covariate: (Homophily) is the tendency to create an event i->j if actors i and j have the same attribute values
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector specifies the name of the column with covariate value in attr_actors data.frame
#' @param attr_actors data.frame object with rows specifying values of attr_actors for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time  
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
same <- function(param = NULL, variable, attr_actors, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar(effect_name = "same", param = param, scaling = scaling, variable = variable, attr_actors = attr_actors)
  out
}

#'difference
#' 
#' This function specifies the input for the difference effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' Dyadic covariate: (Heterophily) is the tendency to create an event i->j if actors i and j have a high absolute difference in attribute values
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector specifies the name of the column with covariate value in attr_actors data.frame
#' @param attr_actors data.frame object with rows specifying values of attr_actors for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time  
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
difference <- function(param = NULL, variable, attr_actors, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar(effect_name = "difference", param = param, scaling = scaling, variable = variable, attr_actors = attr_actors)
  out
}

#'average
#' 
#' This function specifies the input for the average effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' Dyadic covariate: average attribute value for dyad (i,j) is the average of the attribute values for actors i, j
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector specifies the name of the column with covariate value in attr_actors data.frame
#' @param attr_actors data.frame object with rows specifying values of attr_actors for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time  
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
average <- function(param = NULL, variable, attr_actors, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar(effect_name = "average", param = param, scaling = scaling, variable = variable, attr_actors = attr_actors)
  out
}

#'minimum
#' 
#' This function specifies the input for the minimum effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' Dyadic covariate: minimum attribute value for dyad (i,j) is the smaller of the attribute values for actors i , j
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector specifies the name of the column with covariate value in attr_actors data.frame
#' @param attr_actors data.frame object with rows specifying values of attr_actors for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time  
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
minimum <- function(param = NULL, variable, attr_actors, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar(effect_name = "minimum", param = param, scaling = scaling, variable = variable, attr_actors = attr_actors)
  out
}

#'maximum
#' 
#' This function specifies the input for the maximum effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' Dyadic covariate: maximum attribute value for dyad (i,j) is the bigger of the attribute values for actors i , j
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector specifies the name of the column with covariate value in attr_actors data.frame
#' @param attr_actors data.frame object with rows specifying values of attr_actors for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the attributes with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
maximum <- function(param = NULL, variable, attr_actors, scaling = c("none", "std")) {
  scaling <- match.arg(scaling)
  out <- prepExoVar(effect_name = "maximum", param = param, scaling = scaling, variable = variable, attr_actors = attr_actors)
  out
}

#'dyad
#' 
#' This function specifies the input for the dyad effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' Dyadic covariate: dyad attribute value is the tendency to create an event i -> j when (i,j) has a high attribute value.
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector specifies the name of the column with covariate value in attr_actors data.frame
#' @param attr_dyads data.frame object with rows specifying values of attr_dyads for a pair of actors (dyad). First column must contain sender id, Second column receiver id, Third column contains values for the attributes with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
dyad <- function(param = NULL, variable, attr_dyads, scaling=c("none","std")){
  scaling <- match.arg(scaling)
  force(attr_dyads)
  out <- prepExoVar(effect_name = "dyad", param = param, scaling = scaling, variable = variable, attr_actors = attr_dyads)
  out
}

#' interact
#'
#' This function specifies the input for the interact effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value, data.frame  or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param indices is a numeric vector of indices corresponding to the effects specified in \code{effects} argument of function \code{\link{remulateTie}} or \code{\link{remulateActor}} on which the interaction term needs to be computed.
#' @param scaling specifies the method for scaling the statistic after the interaction has been computed. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
#' @details
#' 
#' if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
#' representing the parameter value for thay dyadic pair. The data.frame must contain 
#' all pairs of actors or dyads corresponding to the riskset. 
#' 
#' if param is a function, it's first argument must be 't', corresponding to the time. The
#' function may have additional arguments.
#' @returns List with all information required by `remulate::remulateTie()` or 'remulate::remulateActor()' to compute the statistic.
#' @export
interact <- function(param = NULL, indices,scaling=c("none","std")) {
  scaling <- match.arg(scaling)
  out <- prepInteractVar( param = param, effects = indices, scaling = scaling)
  out
}