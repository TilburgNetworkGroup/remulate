% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effectsTie.R
\name{psABXB}
\alias{psABXB}
\title{psABXB}
\usage{
psABXB(param = NULL, scaling = c("none", "std", "prop"))
}
\arguments{
\item{param}{numeric value or function with time parameter. Specifies the value of the effect for the statistic in the REM model}

\item{scaling}{the method for scaling the psABXB statistic. \code{"none"} [default] gives raw value of the statistic at time t, \code{"std"} the statistic is standardized per time point, and \code{"prop"} denotes proportional scaling in which raw counts are divided by the out degree of the sender at time t.}
}
\description{
This function specifies the input for the psABXB effect in the \code{formula} argument for the function \code{\link{remulateTie}} or \code{\link{remulateActor}}. Not to be used independently
}
