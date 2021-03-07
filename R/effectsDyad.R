#'baseline
#'@export 
baseline <- function(param){
    scaling <-  "raw"
    out <- prepEndoVar("baseline",param,scaling)
}

#' interact
#'
#' This function specifies the input for the interact effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. The interaction is always between the first two terms of the formula argument. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the interaction statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point after multiplication of the interaction components
#'@export 
interact <- function(param=NULL,scaling=c("raw","std")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("interact",param,scaling)
    out
}

#'inertia
#' 
#' This function specifies the input for the inertia effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the inertia statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export 
inertia <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("inertia",param,scaling)
    out
}


#'reciprocity
#' 
#' This function specifies the input for the reciprocity effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the reciprocity statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
reciprocity <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("reciprocity",param,scaling)
    out
}

#'indegreeSender
#' 
#' This function specifies the input for the indegreeSender effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the indegreeSender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
indegreeSender <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("indegreeSender",param,scaling)
    out
}

#'indegreeReceiver
#' 
#' This function specifies the input for the indegreeReceiver effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the indegreeReceiver statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
indegreeReceiver <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("indegreeReceiver",param,scaling)
    out
}

#'outdegreeSender
#' 
#' This function specifies the input for the outdegreeSender effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the outdegreeSender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
outdegreeSender <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("outdegreeSender",param,scaling)
    out
}


#'outdegreeReceiver
#' 
#' This function specifies the input for the outdegreeReceiver effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the outdegreeReceiver statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
outdegreeReceiver <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("outdegreeReceiver",param,scaling)
    out
}

#'totaldegreeSender
#' 
#' This function specifies the input for the totaldegreeSender effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the totaldegreeSender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
totaldegreeSender <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("totaldegreeSender",param,scaling)
    out
}


#'totaldegreeReceiver
#' 
#' This function specifies the input for the totaldegreeReceiver effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the totaldegreeReceiver statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
totaldegreeReceiver <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("totaldegreeReceiver",param,scaling)
    out
}

#'otp
#' 
#' This function specifies the input for the otp effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the otp statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
otp <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("otp",param,scaling)
    out
}

#'itp
#' 
#' This function specifies the input for the itp effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the itp statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
itp <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("itp",param,scaling)
    out
}

#'osp
#' 
#' This function specifies the input for the osp effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the osp statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
osp <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("osp",param,scaling)
    out
}

#'isp
#' 
#' This function specifies the input for the isp effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the isp statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
isp <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("isp",param,scaling)
    out
}


#'psABBA
#' 
#' This function specifies the input for the psABBA effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABBA statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
psABBA <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("psABBA",param,scaling)
    out
}


#'psABBY
#' 
#' This function specifies the input for the psABBY effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABBY statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
psABBY <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("psABBY",param,scaling)
    out
}


#'psABXA
#' 
#' This function specifies the input for the psABXA effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABXA statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
psABXA <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("psABXA",param,scaling)
    out
}


#'psABXB
#' 
#' This function specifies the input for the psABXB effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABXB statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
psABXB <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("psABXB",param,scaling)
    out
}


#'psABXY
#' 
#' This function specifies the input for the psABXY effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABXY statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
psABXY <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("psABXY",param,scaling)
    out
}

#'psABAY
#' 
#' This function specifies the input for the psABAY effect in the \code{formula} argument for the function \code{\link{remulateDyad}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' 
#' @param scaling the method for scaling the psABAY statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export
psABAY <- function(param=NULL, scaling=c("raw","std","prop")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("psABAY",param,scaling)
    out
}

#'send
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector: specifies the name of the column with covariate value in covariates dataframe
#' @param covariates data.frame object with rows specifying values of covariates for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the covariates with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
#' @export
send <- function(param=NULL, variable,covariates,scaling=c("raw","std")){
    scaling <-  match.arg(scaling)
    out <- prepExoVar("send",param,scaling, variable,covariates)
    out
}

#'receive
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector: specifies the name of the column with covariate value in covariates dataframe
#' @param covariates data.frame object with rows specifying values of covariates for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the covariates with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
#' @export 
receive <- function(param=NULL, variable,covariates,scaling=c("raw","std")) {
    scaling <-  match.arg(scaling)
    out <- prepExoVar("receive",param,scaling, variable,covariates)
    out
}

#'same
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector: specifies the name of the column with covariate value in covariates dataframe
#' @param covariates data.frame object with rows specifying values of covariates for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the covariates with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
#' @export 
same <- function(param=NULL, variable,covariates,scaling=c("raw","std")) {
    scaling <-  match.arg(scaling)
    out <- prepExoVar("same",param,scaling, variable,covariates)
    out
}

#'difference
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector: specifies the name of the column with covariate value in covariates dataframe
#' @param covariates data.frame object with rows specifying values of covariates for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the covariates with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
#' @export 
difference <- function(param=NULL, variable,covariates,scaling=c("raw","std")) {
    scaling <-  match.arg(scaling)
    out <- prepExoVar("difference",param,scaling, variable,covariates)
    out
}

#'average
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector: specifies the name of the column with covariate value in covariates dataframe
#' @param covariates data.frame object with rows specifying values of covariates for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the covariates with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
#' @export 
average <- function(param=NULL, variable,covariates,scaling=c("raw","std")) {
    scaling <-  match.arg(scaling)
    out <- prepExoVar("average",param,scaling, variable,covariates)
    out
}

#'minimum
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector: specifies the name of the column with covariate value in covariates dataframe
#' @param covariates data.frame object with rows specifying values of covariates for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the covariates with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
#' @export 
minimum <- function(param=NULL, variable,covariates,scaling=c("raw","std")) {
    scaling <-  match.arg(scaling)
    out <- prepExoVar("minimum",param,scaling, variable,covariates)
    out
}

#'maximum
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model
#' @param variable character vector: specifies the name of the column with covariate value in covariates dataframe
#' @param covariates data.frame object with rows specifying values of covariates for an actor. First column must contain actor id, Second column time when covariate value changes (default zero if no change), Third column contains values for the covariates with column name corresponding to variable name
#' @param scaling specifies the method for scaling the statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time 
#' @export
maximum <- function(param=NULL, variable,covariates,scaling=c("raw","std")) {
   scaling <-  match.arg(scaling)
    out <- prepExoVar("maximum",param,scaling, variable,covariates)
    out
}

# equate <- function(variable, equal_val, covariates) {
#     out <- prepExoVar("equate", variable, covariates)
#     out <- lapply(X = 1:length(out), function(X) {
# 	    list(x = out[[X]]$x, equal_val = equal_val[X])
#     })
#     names(out) <- rep("equate", length(out))
#     out
# }



