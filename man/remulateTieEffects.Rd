% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effectsTie.R
\name{remulateTieEffects}
\alias{remulateTieEffects}
\title{Remulate Tie Effects}
\usage{
remulateTieEffects()
}
\value{
The effect functions do not return anything when called individually. They are only used to specify statistics in the \code{effects} argument for the function \code{\link{remulateTie}}.
}
\description{
This page lists the effects that are available in the remulate package.
}
\details{
The attr_actors object for exogenous effects based on actor covariates (\code{send}, \code{receive}, \code{same}, \code{difference}, \code{average}, \code{max}, \code{min}) contains at least three columns (actor,time,attribute). It should be constructed as follows: Each row refers to the attribute value of actor i at timepoint t. The first column contains the actor names (corresponding to the vector of names in the \code{actors} argument of \code{\link{remulateTie}}). The second column contains the time when attr_actors change (set to zero if the attr_actors do not vary over time). At least one of the subsequent columns must contain values for the attr_actors with column name corresponding to variable name specified in the effect specification.

The attribute object for exogenous effect \code{dyad} contains at least three columns (sender_id,receiver_id,attribute). It should be constructed as follows: First column must contain sender id, second column receiver id, at least one of the subsequent columns must contain values for the attr_actors with column name corresponding to variable name specified in the effect specification.



if param is a data frame, it must have three columns: sender, receiver, and value (numeric), 
representing the parameter value for thay dyadic pair. The data.frame must contain 
all pairs of actors or dyads corresponding to the riskset. 

if param is a function, it's first argument must be 't', corresponding to the time. The
function may have additional arguments.


The indices aregument in the interact effect corresponds to the position of the specified effects in the \code{effects} argument of \code{\link{remulateTie}} for which the interaction needs to be computed. The individual constitutive effects for an interaction must be specified before the interact term in the \code{effects} argument. To omit the individual constitutive effects in the generation, specify the \code{param} arugment to zero.
}
\section{Remulate Effects}{

\describe{
\item{\code{baseline}}{Baseline tendency for dyads to create events. The statistic equals to 1 for all dyads (i,j) in the riskset. The parameter for baseline controls the average number of events per unit time.}
}

\strong{Endogenous effects (Dyad statistics):}
\describe{
 \item{\code{inertia}}{Inertia is the tendency to create an event i->j if the event i->j occured in the past. The statistic at timepoint t for dyad (i,j) is 
equal to the number of (i,j) events before timepoint t.  Note: if \code{scaling} is "prop" for inertia, the statistic for dyad (i,j) at time t is divided by the out degree of the sender i at time t.}

\item{\code{reciprocity}}{Reciprocity is the tendency to create an event i->j if j->i occured in the past.The statistic at timepoint t for dyad (i,j) is 
equal to the number of (j,i) events before timepoint t.  Note: if \code{scaling} is "prop" for inertia, the statistic for dyad (i,j) at time t is divided by the in degree of the sender i at time t.}
\item{\code{tie}}{ Tie effect is the tendency to create an event i->j if the event i->j occured at least once in the past. The statistic at timepoint t for dyad (i,j) is 
equal to 1 if a an event i->j occured before timepoint t}
}

\strong{Endogenous effects (Triadic statistics):}
\describe{
\item{\code{otp}}{Outgoing Two Path effect is the 
tendency  to create an event i->j if they have past 
outgoing two-paths between them (i->h->j). The statistic for dyad (i,j) 
at timepoint t is equal to the minimum of past 
(i,h), (h,j) events, summed over all h.}

\item{\code{itp}}{Incoming Two Path effect is the tendency  to create an event i->j if 
they have past incoming two-paths between them (i<-h<-j). The statistic for dyad (i,j) 
at timepoint t is equal to the minimum of past 
(j,h), (h,i) events, summed over all h.}

\item{\code{osp}}{Outgoing Shared Partners effect is the tendency  to create an event i->j if 
they have past outgoing shared partners between them (i->h<-j). The statistic for dyad (i,j) 
at timepoint t is equal to the minimum of past 
(i,h), (j,h)  events, summed over all h. }

\item{\code{isp}}{Incoming Shared Partners effect is the tendency  to create an event i->j if 
they have past incoming shared partners between them (i<-h->j). The statistic for dyad (i,j) 
at timepoint t is equal to the minimum of past 
(h,i), (h,j) events, summed over all h. }
}

\strong{Endogenous effects (Node statistics):}
\describe{
\item{\code{indegreeSender}}{ In degree effect of the sender is the tendency 
to create an event i->j if i has received more events in the past. 
The statistic at timepoint t for dyad (i,j) is equal to the number of events 
received by actor i before timepoint t. Note: if \code{scaling} is "prop" for indegreeSender, 
the statistic for dyad (i,j) at time t is divided by the total degree of the sender i at time t. }
\item{\code{indegreeReceiver}}{In degree effect of receiver is the tendency 
to create an event i->j if j has received more events in the past.  The statistic at timepoint t for dyad (i,j) is equal to the number of events received by actor j before timepoint t. Note: if \code{scaling} is "prop" for indegreeReceiver, the statistic for dyad (i,j) at time t is divided by the total degree of the receiver j at time t. }
\item{\code{outdegreeSender}}{Out degree effect of sender is the tendency
 to create an event i->j if i has sent more events in the past. Note: if \code{scaling} is "prop" for outdegreeSender, the statistic for dyad (i,j) at time t is divided by the total degree of the sender i at time t. }
\item{\code{outdegreeReceiver}}{Out degree effect of receiver is the tendency to create an event i->j if j has sent more events in the past. Note: if \code{scaling} is "prop" for outdegreeReceiver, the statistic for dyad (i,j) at time t is divided by the total degree of the receiver j at time t. }
\item{\code{totaldegreeSender}}{Total degree effect of sender is the tendency to create an event i->j if i has sent and received more events in the past.}
\item{\code{totaldegreeReceiver}}{Total degree effect of receiver is the tendency to create an event i->j if j has sent and received more events in the past.}

}

\strong{Endogenous effects (Participating Shifts):}
\describe{
\item{\code{psABBA}}{ AB-BA Pacticipating shift (turn receiving) is the tendency to create an event j->i at timepoint t if event i->j occured at timepoint t-1. The psABBA statistic is equal to one for the dyad (j.i) that will create the participation shift at timepoint t.}
\item{\code{psABBY}}{ AB-BY Participating shift (turn receiving) is the tendency to create an event j->h at timepoint t if event i->j occured at timepoint t-1. The psABBY statistic is equal to one for the dyads (j,h) for all h not equal to i, that will create the participation shift at timepoint t.}
\item{\code{PSABAY}}{AB-AY Participating shifts  (turn continuing) is the tendency to create an event i->h at timepoint t if event i->j occured at timepoint t-1. The PSABAY statistic is equal to one for the dyads (i,h) for all h not equal to j, that will create the participation shift at timepoint t. }
\item{\code{psABXA}}{ AB-XA Participating shifts (turn usurping) is the tendency to create an event h->i at timepoint t if event i->j occured at timepoint t-1. The psABXA statistic is equal to one for the dyads (h,i) for all h not equal to j, that will create the participation shift at timepoint t.}
\item{\code{psABXB}}{ AB-XB Participating shifts (turn usurping) is the tendency to create an event h->j at timepoint t if event i->j occured at timepoint t-1. The psABXB statistic is equal to one for the dyads (h,j) for all h not equal to i, that will create the participation shift at timepoint t.}
\item{\code{psABXY}}{ AB-XY Participating shifts (turn usurping) is the tendency to create an event h->k at timepoint t if event i->j occured at timepoint t-1. The psABXB statistic is equal to one for the dyads (h,k) for all h not equal to i and k not equal to j, that will create the participation shift at timepoint t. }
}

\strong{Endogenous effects (Recency statistics):}
\describe{
\item{\code{recencyContinue}}{The recencyContinue effect refers to a recency statistic similar to what is
described in Vu et al. (2017) and Mulder and Leenders (2019). For each
timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the time
that has past since the dyad was last active + 1).}
\item{\code{recencySendSender}}{ The recencySendSender effect refers to a recency statistic similar to what
is described in Vu et al. (2017) and Mulder and Leenders (2019). For each
timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the time
that has past since sender i was last active as sender + 1).}
\item{\code{recencySendReceiver}}{The recencySendReceiver effect refers to a recency statistic similar to what
is described in Vu et al. (2017) and Mulder and Leenders (2019). For each
timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the time
that has past since receiver j was last active as sender + 1). }
\item{\code{recencyReceiveSender}}{The recencyReceiveSender effect refers to a recency statistic similar to
what is described in Vu et al. (2017) and Mulder and Leenders (2019). For
each timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the
time that has past since sender i was last active as receiver + 1). }
\item{\code{recencyReceiveReceiver}}{The recencyReceiveReceiver effect refers to a recency statistic similar to
what is described in Vu et al. (2017) and Mulder and Leenders (2019). For
each timepoint t, for directed dyad (i,j) the statistic is equal to 1/(the
time that has past since receiver j was last active as receiver + 1).}
} 

\strong{Exogenous effects (Node attr_actors):}
\describe{
\item{\code{send}}{Sender covariate: The tendency to create an event i->j when i has a high attribute value.}
\item{\code{receive}}{Receiver covariate: The tendency to create an event i->j when j has a high attribute value.}
}

\strong{Exogenous effects (Tie Attribute):}
\describe{
\item{\code{dyad}}{ dyad attribute value is the tendency to create an event i -> j when (i,j) has a high attribute value.}
\item{\code{same}}{ (Homophily) is the tendency to create an event i->j if actors i and j have the same attribute values}
\item{\code{difference}}{ (Heterophily) is the tendency to create an event i->j if actors i and j have a high absolute difference in attribute values}
\item{\code{average}}{average attribute value for dyad (i,j) is the average of the attribute values for actors i, j}
\item{\code{minimum}}{minimum attribute value for dyad (i,j) is the smaller of the attribute values for actors i , j}
\item{\code{maximum}}{maximum attribute value for dyad (i,j) is the bigger of the attribute values for actors i , j}
}
}

\examples{
#To specify an endogenous effect (example: inertia)

effects <- ~ inertia(0.1, scaling = "std")

#To specify an exogenous effect (example: same)

cov <- data.frame(
  actor = 1:10,
  time = rep(0, 10),
  gender = sample(c(0, 1), replace = TRUE, 10),
  age = sample(20:30, 10, replace = TRUE)
)

effects <- ~ same(0.2, variable = "gender", attr_actors = cov)

#To specify an exogenous dyadic effect (example: dyad)

cov <- expand.grid(1:10, 1:10)
cov <- cov[cov[, 1] != cov[, 2], ]
cov$grade <- runif(90, 1, 10)

effects <- ~ dyad(0.2, variable = "grade", attr_actors = cov)

#If parameter is constant

effects <- ~ inertia(0.3) + 
  same(0.2, variable = "gender", attr_actors = cov) + 
  reciprocity(-0.1) + 
  itp(0.01)

#If parameter varies with time

effects <- ~ inertia(param = function(t) exp(-t)) + 
  same(0.2, variable = "gender", attr_actors = cov) +
  reciprocity(-0.1) + 
  itp(0.01)

 #If parameter varies across dyads or actors
 rs <- expand.grid(1:10,1:10)
 rs <- rs[rs[,1] != rs[, 2],]

 param_df <- as.data.frame(rs)
 param_df$beta = runif(nrow(rs),-0.1,0.1)

 effects <- ~ remulate::baseline(-3)+
    remulate::inertia(param_df) +
    remulate::reciprocity(0.1)

 
#To specify an interaction (example: between inertia and same constitutive effects)

effects <- ~ inertia(0.3) + 
  same(0.2, variable = "gender", attr_actors = cov) + 
  reciprocity(-0.1) + 
  itp(0.01) + 
  interact(0.1, indices = c(1, 2))
}
