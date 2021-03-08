#'inertia sender
#' 
#' This function specifies the input for the inertia  sender effect in the \code{s_formula} argument for the function \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the Actor Oriented Model
#' 
#' @param scaling the method for scaling the inertia sender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export 
inertia_s <- function(param=NULL, scaling=c("raw","prop","std")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("inertia_s",param,scaling)
    out
}

#'reciprocity sender
#' 
#' This function specifies the input for the reciprocity  sender effect in the \code{s_formula} argument for the function \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the Actor Oriented Model
#' 
#' @param scaling the method for scaling the reciprocity sender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export 
reciprocity_s <- function(param=NULL, scaling=c("raw","prop","std")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("reciprocity_s",param,scaling)
    out
}

#'otp sender
#' 
#' This function specifies the input for the otp  sender effect in the \code{s_formula} argument for the function \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the Actor Oriented Model
#' 
#' @param scaling the method for scaling the otp sender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export 
otp_s <- function(param=NULL, scaling=c("raw","std")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("otp_s",param,scaling)
    out
}

#'osp sender
#' 
#' This function specifies the input for the osp  sender effect in the \code{s_formula} argument for the function \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the Actor Oriented Model
#' 
#' @param scaling the method for scaling the osp sender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export 
osp_s <- function(param=NULL, scaling=c("raw","std")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("osp_s",param,scaling)
    out
}

#'itp sender
#' 
#' This function specifies the input for the itp  sender effect in the \code{s_formula} argument for the function \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the Actor Oriented Model
#' 
#' @param scaling the method for scaling the itp sender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export 
itp_s <- function(param=NULL, scaling=c("raw","std")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("itp_s",param,scaling)
    out
}

#'isp sender
#' 
#' This function specifies the input for the isp  sender effect in the \code{s_formula} argument for the function \code{\link{remulateActor}}. Not to be used independently
#' 
#' @param param numeric value or function with time parameter. Specifies the value of the effect for the statistic in the Actor Oriented Model
#' 
#' @param scaling the method for scaling the isp sender statistic. \code{"raw"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.
#' @export 
isp_s <- function(param=NULL, scaling=c("raw","std")){
    scaling <-  match.arg(scaling)
    out <- prepEndoVar("isp_s",param,scaling)
    out
}
