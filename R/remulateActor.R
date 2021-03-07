#'Simulate REM Dyad
#' @description
#'  A function to simulate relational event data by sampling from an
#' actor oriented event model.
#'
#' @details
#' A list of available sender statistics follows: 
#' \itemize{
#'  \item \code{\link{baseline}()}
#'  \item \code{\link{send}()}
#'  \item \code{\link{indegreeSender}()}
#'  \item \code{\link{outdegreeSender}()}
#'  \item \code{\link{totaldegreeSender}()}
#'  \item \code{\link{interact}()}
#' }
#' A list of available dyadic statistics follows: 
#'\itemize{
#'  \item \code{\link{baseline}()}
#'  \item \code{\link{receive}()}
#'  \item \code{\link{same}()}
#'  \item \code{\link{difference}()}
#'  \item \code{\link{average}()}
#'  \item \code{\link{minimum}()}
#'  \item \code{\link{maximum}()}
#'  \item \code{\link{equate}()}
#'  \item \code{\link{event}()}
#'  \item \code{\link{inertia}()}
#'  \item \code{\link{reciprocity}()}
#'  \item \code{\link{indegreeReceiver}()}
#'  \item \code{\link{outdegreeReceiver}()}
#'  \item \code{\link{totaldegreeReceiver}()}
#'  \item \code{\link{otp}()}
#'  \item \code{\link{itp}()}
#'  \item \code{\link{osp}()}
#'  \item \code{\link{isp}()}
#'  \item \code{\link{sp}()}
#'  \item \code{\link{psABBA}()}
#'  \item \code{\link{psABBY}()}
#'  \item \code{\link{psABXA}()}
#'  \item \code{\link{psABXB}()}
#'  \item \code{\link{psABXY}()}
#'  \item \code{\link{psABAY}()}
#'  \item \code{\link{interact}()}
#' }
#'
#' interact is always between the first two effects in the formula
#' 
#' @param sender_formula an object of class \code{"\link[stats]{formula}"}: a symbolic description of statistics used to sample the sender. See 'Details' for a list of available sender statistics.
#' @param dyad_formula an object of class \code{"\link[stats]{formula}"}: a symbolic description of statistics used to sample the receiver given sender. See 'Details' for a list of available dyadic statistics.
#' @param actors Vector of actor names
#' @param M Number of events to generate
#' @param burn_in Number of random events to sample before beginning with the data generation
#' @param risk_set \code{"\link[base]{data.frame}"} object wtih columns (sender, receiver) indicating which pair cannot be in risk set
#' @param memory [Optional] (default = full) String indicating which
#'  memory type to use. "full" uses the entire event history to compute statistics, "window" memory indicates a window in the past upto
#' which occured events will be remembered for computing statistics, "brandes" memory type uses past events
#' weighted by their time, "vu" memory type uses past events weighted by 1/time difference
#' @param memory_param [Optional] memory_param value > 0. For memory type "window" this parameter indicates the length (in time units) of the window. For memory type "brandes" the memory_param will be the half-life i.e the time until an event has a weight of one half.
#' @return edgelist data.frame object with columns (time,sender name,receiver name)
#' @return statistics 3 dimensional array of statistics of dimensions M x D x P (M: Number of events, D: Number of dyads in the risk set, P: Number of statistics)
#' @return evls matrix containing the event list  with columns (event,time) where event represents the index of the dyad or the (sender,receiver) pair in the risk set
#' @return actors_map  data.frame object containing the mapping of actor names provided by user to the integer ids used in the internal computations
#' @return riskset matrix object containing the risket set used for the dyad id (sender id, receiver id) indices in the statistics and event list 
#' @examples 
#'  
#' form <- ~baseline(1)+inertia(0.1)+reciprocity(0.4)
#' actors <- c(1:10)
#' remulateDyad(form,actors,100)
#' @export
remulateActor <- function(
    sender_formula,
    dyad_formula,
    actors,
    M,
    burn_in = 0,
    risk_set =NULL,
    waiting_time=c("exp","weibull","gompertz"),
    time_param=1,
    memory=c("full","window","window_m","brandes","vu"),
    memory_param = NULL,
    seeds=NULL){
    
    #process input for sender
    s_effects <- parse_formula_Sender(sender_formula)
    s_params <- s_effects$params
    s_scaling <- s_effects$scaling
    s_int_effects <- s_effects$int_effects
    s_covariates <- s_effects$covariates
    s_effects <- unname(s_effects$effects)
    s_P <- length(s_effects)



    #process input for receiver choice
    d_effects <- parse_formula_Dyad(dyad_formula)
    d_params <- d_effects$params
    d_scaling <- d_effects$scaling
    d_int_effects <- d_effects$int_effects
    d_covariates <- d_effects$covariates
    d_effects <- unname(d_effects$effects)
    d_P <- length(d_effects)

    memory<- match.arg(memory)
    waiting_time <- match.arg(waiting_time)
    
    #checking memory specification
    if(! memory[1] %in% c("full","window","window_m","brandes","vu")){
        stop(paste("\n'",memory[1], "'memory method not defined"))
    }
    if(memory != "full" && is.null(memory_param)){
        if(memory[1]=="window" || memory[1]=="window_m"){
            stop(paste("Cannot use window memory technique without a memory_param value"))
        }else if(memory_param <= 0){
            stop(paste("memory_param must be positive"))
        }
    }

    #create a map for user given actor references - integer actor ids for computing
    N <- length(actors)
    actors_map <- data.frame(id=1:N,given = actors)
    
    #Create a risk set
    #TODO: allow risk set to vary with time
    rs <- as.matrix(expand.grid(actors_map$id,actors_map$id))
    colnames(rs) <- c("sender", "receiver")
    rs <- rs[rs[,"sender"]!=rs[,"receiver"],]

    if(!is.null(risk_set)){
        if(any(!risk_set[[1]] %in% actors_map$given)){
            stop("risk set contains sender actor not specified in actor's list")
        } else if (any(!risk_set[[2]] %in% actors_map$given)) {
           stop("risk set contains receiver actor not specified in actor's list")
        }
        #remove dyads present in risk_set data frame
        rs <- rs[!(rs[,1] %in% actors_map$id[actors_map$given %in% risk_set[[1]]] & rs[,2] %in% actors_map$id[actors_map$given %in% risk_set[[2]] ]),]
    }
    
    #initialize start time as t=0
    t <- 0

    #initialize covariates
    s_covariates <- initialize_exo_effects(s_covariates,actors_map,s_effects)
    
    d_covariates <- initialize_exo_effects(d_covariates,actors_map,d_effects)

    #initialize params for sender
    gamma <- vector(length = s_P)
    for(i in 1:s_P){
        if(class(s_params[[i]])=="function"){#function must be defined at t=0
            gamma[i] <- s_params[[i]](t)
        }else{
            gamma[i] <- s_params[[i]]
        }
    }

    #initialize params for receiver choice
    beta <- vector(length = d_P)
    for(i in 1:d_P){
        if(class(d_params[[i]])=="function"){#function must be defined at t=0
            beta[i] <- d_params[[i]](t)
        }else{
            beta[i] <- d_params[[i]]
        }
    }

    #initialize statistics cube of dimension (M x N x #params) for all events that are going to be sampled
    s_stats <- array(0,dim=c(M,N,s_P))

    d_stats <- array(0,dim=c(M,nrow(rs),d_P))

    #pre-allocate space for edgelist,event list
    edgelist <- data.frame(time=rep(0,M),sender = rep(0,M),receiver=rep(0,M))
    evls <- data.frame(dyad = rep(0,M),time = rep(0,M))
    
    #adjacency matrix ( #sender x #recv matrix)
    adj_mat <- burn_in_adj_mat(actors,burn_in,rs)

    #position of the first occurence of each unique actor id in the risk set
    sender_rs_indx <- match(actors_map$id,rs[,1])

    #saving stats previous row for s_stats, used for exogenous stats
    s_statsprev <- array(0,dim=c(N,s_P))

    for(i in 1:M){
        #updating event rate / lambda for dyads
        if(i==1){
            if(d_P==1){
                d_lambda <- exp(d_stats[1,,] * beta)
            } else{
                d_lambda <- exp(d_stats[1,,] %*% beta)
            }
        }
        else{
            if(d_P==1){
                d_lambda <- exp(d_stats[i-1,,] * beta)
            } else{
                d_lambda <- exp(d_stats[i-1,,] %*% beta)
            }
        }
        # updating event rate / s_lambda for senders
        if(i==1){
            if(s_P==1){
                s_lambda <- exp(s_stats[1,,] * gamma)
            } else{
                s_lambda <- exp(s_stats[1,,] %*% gamma)
            }
        }
        else{
            if(s_P==1){
                s_lambda <- exp(s_stats[i-1,,] * gamma)
            } else{
                s_lambda <- exp(s_stats[i-1,,] %*% gamma)
            }
        }
        
        d_lambda_per_sender <- vapply(actors_map$id,FUN.VALUE = numeric(1), function(x){
            s_indx <- which(rs[,1]==x & rs[,2]!= x)
            sum(d_lambda[s_indx])
        })

        lambda <-apply(rs,MARGIN = 1,function(x){
            s_lambda[x[1]] * d_lambda[x[2]] / d_lambda_per_sender[x[1]]
        })

        #sampling waiting time dt
        if(waiting_time=="exp"){
            if(!is.null(seeds)){
                set.seed(seeds[i])
            }
            dt <- rexp(1,rate = sum(lambda))
            t <- t + dt
        }

        else if (waiting_time=="weibull") {
            #TODO: add checks on time params
            if(!is.null(seeds)){
                set.seed(seeds[i])
            }
           dt <- rweibull(1,shape=time_param,scale = sum(lambda))
           t <- t + dt
        }
        else if (waiting_time=="gompertz") {
           dt <- rgompertz(1,scale = sum(lambda), shape=time_param)
           t <- t + dt
        }
        
        # R sampling slightly faster than arma sampling (due to hashing)
        if(!is.null(seeds)){
                set.seed(seeds[i])
        }
        #Sampling sender and receiver
        sender <- sample(1:N,1,prob = s_lambda/sum(s_lambda))
        

        s_indx <- which(rs[,1]==sender & rs[,2]!= sender)
        d_lambda_given_sender <- d_lambda[s_indx]
        
        if(!is.null(seeds)){
                set.seed(seeds[i])
        }

        dyad <- sample(s_indx,1,prob = d_lambda_given_sender/sum(d_lambda_given_sender))


        #update dyadic stats for t_i 
        if(i==1){
            d_stats[i,,] <- compute_stats_Dyad(d_int_effects, d_P, rs, actors_map$id, as.matrix(edgelist[1,]),adj_mat, d_covariates, d_scaling,as.matrix(d_stats[1,,]))
        }else{
            d_stats[i,,] <- compute_stats_Dyad(d_int_effects, d_P, rs, actors_map$id, as.matrix(edgelist[1:i-1,]),adj_mat, d_covariates, d_scaling,as.matrix(d_stats[i-1,,]))
        }
        #update sender stats for t_i 
        if(i==1){
            s_stats[i,,] <- compute_stats_Actor(s_int_effects, s_P, rs, actors_map$id, as.matrix(edgelist[1,]),adj_mat, s_covariates, s_scaling,as.matrix(s_stats[1,,]))
        }else{
            s_stats[i,,] <- compute_stats_Actor(s_int_effects, s_P, rs, actors_map$id, as.matrix(edgelist[1:i-1,]),adj_mat, s_covariates, s_scaling,as.matrix(s_stats[i-1,,]))
        }

        edgelist$time[i] <- t
        edgelist$sender[i] <- sender
        edgelist$receiver[i] <- rs[dyad,2] 
        evls$dyad[i] <- dyad
        evls$time[i] <- t

        #update event count mat
        if(memory=="full"){
            adj_mat[edgelist$sender[i],edgelist$receiver[i]] =  adj_mat[edgelist$sender[i],edgelist$receiver[i]] + 1;
        }
        else if (memory=="window"){ #window memory takes memory by time window
            #TODO: ask Rwhiz how to make this faster
            adj_mat[] <- 0
            in_window <- which(edgelist$time > t - memory_param) #event indices which are in memory_param
            for(ind in in_window){
               adj_mat[edgelist$sender[ind],edgelist$receiver[ind]] =  adj_mat[edgelist$sender[ind],edgelist$receiver[ind]] + 1;
           }
        }
        else if(memory=="window_m"){ #window_m takes memory by last m events
            if(memory_param<i){
                adj_mat[] <- 0
                for(ind in c(i-memory_param,i)){
                    adj_mat[edgelist$sender[ind],edgelist$receiver[ind]] =  adj_mat[edgelist$sender[ind],edgelist$receiver[ind]] + 1;
                }
            }else{
                adj_mat[edgelist$sender[i],edgelist$receiver[i]] =  adj_mat[edgelist$sender[i],edgelist$receiver[i]] + 1;
            } 
        }
        else if(memory=="brandes"){
            #TODO try to vectorize
            adj_mat [] <- 0
            for(j in 1:i){#loop through edgelist
                adj_mat[edgelist$sender[j], edgelist$receiver[j]] = adj_mat[edgelist$sender[j], edgelist$receiver[j]] + exp((-(t-edgelist$time[j]))*(log(2)/memory_param))
            }
        }
        else if(memory=="vu"){
            adj_mat [] <- 0
            for(j in 1:i-1){#loop through edgelist
                adj_mat[edgelist$sender[j], edgelist$receiver[j]] = adj_mat[edgelist$sender[j], edgelist$receiver[j]] + 1/((t-edgelist$time[j])**memory_param)
            }
        }
        
        #update parameters in case they vary with time
        for(i in 1:s_P){
            if(class(s_params[[i]])=="function"){
                gamma[i] <- s_params[[i]](t)
            }else{
                gamma[i] <- s_params[[i]]
            }
        }

        for(i in 1:d_P){
            if(class(d_params[[i]])=="function"){
                beta[i] <- d_params[[i]](t)
            }else{
                beta[i] <- d_params[[i]]
            }
        }
    }

    #change actor ids in output
    edgelist["sender"]<- lapply(edgelist["sender"], function(x){
        actors_map$given[x]
    })
    edgelist["receiver"]<- lapply(edgelist["receiver"], function(x){
        actors_map$given[x]
    })

    #return objects
    dimnames(d_stats)<-list(NULL,NULL,d_effects)
    dimnames(s_stats) <- list(NULL,NULL,s_effects)

    return(
        list(
            edgelist = edgelist,
            evls = as.matrix(evls),
            sender_statistics = s_stats,
            dyad_statistics = d_stats,
            riskset = rs,
            actors = actors_map
        )
    ) 
}
