#'Simulate REM Dyad
#' @description 
#'  A function to simulate relational event data by sampling from a
#' tie based relational event model.
#'
#' @details
#' A list of available statistics follows: 
#' \itemize{
#'  \item \code{\link{baseline}()}
#'  \item \code{\link{send}()}
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
#'  \item \code{\link{indegreeSender}()}
#'  \item \code{\link{indegreeReceiver}()}
#'  \item \code{\link{outdegreeSender}()}
#'  \item \code{\link{outdegreeReceiver}()}
#'  \item \code{\link{totaldegreeSender}()}
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
#' @param formula an object of class \code{"\link[stats]{formula}"}: a symbolic description of statistics used to generate the data. See 'Details' for a list of available statistics.
#' @param actors Vector of actor names
#' @param M Number of events to generate
#' @param burn_in Number of random events to sample before beginning with the data generation
#' @param risk_set \code{"\link[base]{data.frame}"} object wtih columns (sender, receiver) indicating which pair cannot be in risk set
#' @param memory [Optional] (default = full) String indicating which
#'  memory type to use. "full" uses the entire event history to compute statistics, "window" memory indicates a window in the past upto
#' which occured events will be remembered for computing statistics, "brandes" memory type uses past events
#' weighted by their time, "vu" memory type uses past events weighted by 1/time difference
#' @param memory_param [Optional] memory_param value > 0. For memory type "window" this parameter indicates the length (in time units) of the window. For memory type "brandes" the memory_param will be the half-life i.e the time until an event has a weight of one half.
#' @return edgelist data.frame object with columns (time,sender,receiver)
#' @return statistics 3 dimensional array of statistics of dimensions M x D x P (M: Number of events, D: Number of dyads in the risk set, P: Number of statistics)
#' @return evls matrix containing the event list  with columns (event,time) where event represents the index of the dyad or the (sender,receiver) pair in the risk set
#' @return actors_map  data.frame object containing the mapping of actor names provided by user to the integer ids used in the internal computations
#' @return riskset matrix object containing the risket set used for the dyad indices in the statistics and event list 
#' @examples 
#'  
#' form <- ~baseline(1)+inertia(0.1)+reciprocity(0.4)
#' actors <- c(1:10)
#' remulateDyad(form,actors,100)
#' @export
remulateDyad <- function(
    formula,
    actors,
    M,
    burn_in = 0,
    risk_set=NULL,
    waiting_time=c("exp","weibull","gompertz"),
    time_param=1,
    memory=c("full","window","window_m","brandes","vu"),memory_param = NULL,
    seeds=NULL){
    
    effects <- parse_formula_Dyad(formula)
    
    params <- effects$params
    scaling <- effects$scaling
    int_effects <- effects$int_effects
    covariates <- effects$covariates
    effects <- unname(effects$effects)
    P <- length(effects)

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
    actors_map <- data.frame(id=1:length(actors),given = actors)
    
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
    covariates <- initialize_exo_effects(covariates,actors_map,effects)
    
    #initialize params
    beta <- vector(length = P)
    for(i in 1:P){
        if(class(params[[i]])=="function"){#function must be defined at t=0
            beta[i] <- params[[i]](t)
        }else{
            beta[i] <- params[[i]]
        }
    }


    #initialize statistics cube of dimension (M x #dyads x #params) for all events that are going to be sampled 
    stats <- array(0,dim=c(M,nrow(rs),P))
    
    #pre-allocate space for edgelist,event list
    edgelist <- data.frame(time=rep(0,M),sender = rep(0,M),receiver=rep(0,M))
    evls <- data.frame(dyad = rep(0,M),time = rep(0,M))
    probs <- array(0,dim=c(M,nrow(rs)))
    #stores the event counts for dyads in a #sender x #recv matrix
    adj_mat <- burn_in_adj_mat(actors,burn_in,rs)
    
    for(i in 1:M){
        #updating event rate / lambda
        if(i==1){
            if(P==1){
                lambda <- exp(stats[i,,] * beta)
            } else{
                lambda <- exp(stats[i,,] %*% beta)
            }
        }
        else{
            if(P==1){
                lambda <- exp(stats[i-1,,] * beta)
            } else{
                lambda <- exp(stats[i-1,,] %*% beta)
            }
        }
        probs[i,] <- lambda/sum(lambda)
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
        
        #sampling dyad for next event
        # R sampling slightly faster than arma sampling (due to hashing)
        if(!is.null(seeds)){
                set.seed(seeds[i])
        }
        dyad <- sample(1:nrow(rs),1,prob = lambda/sum(lambda))

        edgelist$time[i] <- t
        edgelist$sender[i] <- rs[dyad,1]
        edgelist$receiver[i] <- rs[dyad,2] 
        evls$dyad[i] <- dyad
        evls$time[i] <- t

        #update the stats for t_i
        if(i==1){
            stats[i,,] <- compute_stats_Dyad(int_effects, P, rs, actors_map$id, as.matrix(edgelist[1,]),adj_mat, covariates, scaling,as.matrix(stats[1,,]))
        }else{
            stats[i,,] <- compute_stats_Dyad(int_effects, P, rs, actors_map$id, as.matrix(edgelist[1:i,]),adj_mat, covariates, scaling,as.matrix(stats[i-1,,]))
        }

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
                print(paste("memory in:",i-memory_param,"to",i))
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
        
        #update beta
        for(i in 1:P){
            if(class(params[[i]])=="function"){
                beta[i] <- params[[i]](t)
            }else{
                beta[i] <- params[[i]]
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
    dimnames(stats)<-list(NULL,NULL,effects)
    density <- get.density(as.matrix(evls),actors)
    return(
        list(
            edgelist = edgelist,
            evls = as.matrix(evls),
            statistics = stats,
            riskset = rs,
            actors = actors_map,
            density = density,
            probs = probs
        )
    )
    
}
