#'Simulate REM Dyad
#' 
#' A function to simulate relational event data by sampling from a
#' tie based relational event model.
#' @param form an object of class \code{"\link[stats]{formula}"}: a symbolic description of statistics used to generate the data. 
#' @param actors Vector of actor IDs
#' @param M Number of events to generate
#' @param covariates fixed effects
#' @param risk_set risk set, a dataframe wtih sender in 1st column and receiver in second column indicating which pair cannot be in risk set
#' @param memory [Optional] (default = full) String indicating which
#'  memory type to use. "full" uses the entire event history to compute statistics, "window" memory indicates a window in the past upto
#' which occured events will be remembered for computing statistics, "brandes" memory type uses past events
#' weighted by their time, "vu" memory type uses past events weighted by 1/time difference
#' @param memory_param [Optional] memory_param value > 0. For memory type "window" this parameter indicates the length of the window. For memory type memory_param for memory type "brandes" the memory_param will be the half-life i.e the time until an event has a weight of one half.
#' @return edgelist: dataframe with columns (time,sender,receiver)
#' @return stats: 3d cube of statistics of dimensions M x D x P
#' @return evls: event list (time, dyad)
#' @return riskset: dataframe with columns(sender,receiver)
#' @examples 
#' form <- ~baseline(1)+inertia(0.1)+reciprocity(0.4)
#' actors <- c(1:10)
#' remulateDyad(form,actors,100)
#'@details A list of available statistics is as follows: 
#' \itemize{
#'  \item{ baseline}
#'  \item{ send}
#'  \item{ receive}
#'  \item{ same}
#'  \item{ difference}
#'  \item{ average}
#'  \item{ minimum}
#'  \item{ maximum}
#'  \item{ equate}
#'  \item{ event}
#'  \item{ inertia}
#'  \item{ reciprocity}
#'  \item{ indegreeSender}
#'  \item{ indegreeReceiver}
#'  \item{ outdegreeSender}
#'  \item{ outdegreeReceiver}
#'  \item{ totaldegreeSender}
#'  \item{ totaldegreeReceiver}
#'  \item{ otp}
#'  \item{ itp}
#'  \item{ osp}
#'  \item{ isp}
#'  \item{ psABBA}
#'  \item{ psABBY}
#'  \item{ psABXA}
#'  \item{ psABXB}
#'  \item{ psABXY}
#'  \item{ psABAY}
#'}
#' scaling options are: 
#'\itemize{
#'    \item {"std" for standardizing}
#'    \item {"raw" for raw counts and,}
#'    \item {"prop" for proportoinal scaling}
#'}
#' @export
remulateDyad <- function(form,actors,M,covariates = list(),risk_set=NULL,waiting_time=c("exp","weibull","gompertz"),time_param=1,memory=c("full","window","brandes","vu"),memory_param = NULL){
    
    effects <- parse_formula(form)
    params <- effects$params
    scaling <- effects$scaling
    P <- effects$P
    effects <- effects$effects
    
    all_effects <- c(
            "baseline", #1
            "send", "receive", #2 #3
            "same", "difference", "average", #4 #5 #6
            "minimum", "maximum", "equate", #7 #8 #9  
            "inertia", "reciprocity", #10 #11
            "indegreeSender", "indegreeReceiver", #12 #13
            "outdegreeSender", "outdegreeReceiver", #14 #15
            "totaldegreeSender", "totaldegreeReceiver", #16, #17
            "otp", "itp", "osp", "isp", #18 #19 #20 #21
            "psABBA", "psABBY", "psABXA",  #22 #23 #24 
            "psABXB", "psABXY", "psABAY"  #25 #26 #27
            )
    
    # checking input arguments
    if(!(all(effects %in% all_effects))) {
         stop(paste("\n'",effects[!(effects%in%all_effects)], "'effect not defined"))
    }
    
    #checking memory specification
    if(! memory[1] %in% c("full","window","brandes","vu")){
        stop(paste("\n'",memory[1], "'memory method not defined"))
    }
    if(memory != "full" && is.null(memory_param)){
        if(memory[1]=="window"){
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
        if(any(!rs$sender %in% actors_map$given)){
            stop("risk set contains sender actor not specified in actor's list")
        } else if (any(!rs$receiver %in% actors_map$given)) {
           stop("risk set contains receiver actor not specified in actor's list")
        }
        #remove dyads present in risk_set argument
        rs <- rs[!(rs$sender %in% risk_set$sender & rs$receiver %in% risk_set$receiver),]
    }
    
    #initialize start time as t=0
    t <- 0

    #initialize covariates
    covariates <- initialize_exo_effects(effects,covariates,actors_map,rs)
    
    # Prepare effects for switch  case
    int_effects <- match(effects,all_effects)
    
    
    #initialize params
    M <- events
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
    
    #pre-allocate space for edgelist,event list, convert to df later
    edgelist <- data.frame(time=rep(0,M),sender = rep(0,M),receiver=rep(0,M))
    evls <- data.frame(time = rep(0,M),dyad = rep(0,M))
    
   
    #stores the event counts for dyads in a #sender x #recv matrix
    adj_mat <- array(0,dim=c(length(actors),length(actors)))
    
    
    for(i in 1:M){
        #updating event rate / lambda
        if(P==1){
            lambda <- exp(stats[i,,] * beta)
        } else{
            lambda <- exp(stats[i,,] %*% beta)
        }
    
        #sampling waiting time dt
        if(waiting_time=="exp"){
            dt <- rexp(1,rate = sum(lambda))
            t <- t + dt
        }
        else if (waiting_time=="weibull") {
            #TODO: add checks on time params
           dt <- rweibull(1,shape=time_param,scale = sum(lambda))
           t <- t + dt
        }
        else if (waiting_time=="gompertz") {
           dt <- rgompertz(1,scale = sum(lambda), shape=time_param)
           t <- t + dt
        }

        #sampling dyad for next event
        # R sampling slightly faster than arma sampling (due to hashing)
        dyad <- sample(1:nrow(rs),size=1,replace=FALSE,prob = lambda/sum(lambda))

        
        #update the stats for t_i
        stats[i,,] <- compute_stats(int_effects, P, rs, actors_map$id, as.matrix(edgelist[1:i-1,]),adj_mat, covariates, scaling,as.matrix(stats[i-1,,]))
    
        edgelist$time[i] <- t
        edgelist$sender[i] <- rs[dyad,1]
        edgelist$receiver[i] <- rs[dyad,2] 
        evls$dyad[i] <- dyad
        evls$time[i] <- t

        #update event count mat
        if(memory=="full"){
            adj_mat[edgelist$sender[i],edgelist$receiver[i]] =  adj_mat[edgelist$sender[i],edgelist$receiver[i]] + 1;
        }
        else if (memory=="window"){
            #TODO: ask Rwhiz how to make this faster
            adj_mat[] <- 0
            in_window <- which(edgelist$time > t - memory_param) #event indices which are in memory_param
            for(ind in in_window){
               adj_mat[edgelist$sender[ind],edgelist$receiver[ind]] =  adj_mat[edgelist$sender[ind],edgelist$receiver[ind]] + 1;
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

    #return objects
    dimnames(stats)<-list(NULL,NULL,effects)
    return(
        list(
            edgelist = edgelist,
            evls = evls,
            stats = stats,
            riskset = rs,
            actors = actors_map
        )
    )
    
}

initialize_exo_effects <- function(effects,covariates,actors_map,rs){
    #initialize statistics cube
    #exogenous: same format as remstats
    return(list(el = NULL))
}

parse_formula <- function(form){
    ft <- stats::terms(form)
    var <- attr(ft,"variables")
    var <- as.list(var)[-1]
    P <- length(var)
    effects <- vector(mode='character',length=P)
    #params <- vector(mode="numeric",length=P)
    params <- list()
    scaling <- vector(mode='numeric',length=P)

    for(i in 1:P){
        parsed_v <- parse(text=var[[i]])
        if(length(parsed_v)==2){
            #no scaling
            effects[i] = as.character(parsed_v[[1]])
            params[i]=as.numeric(parsed_v[[2]])
            scaling[i]=1 #raw
        }
        else if (length(parsed_v)==3) {
            #with scaling
            effects[i] = as.character(parsed_v[[1]])
            params[i]=as.numeric(parsed_v[[2]])
            scaling_chr = as.character(parsed_v[[3]])
            if(scaling_chr=="std"){#standardize
                scaling[i]=2
            }
            else if (scaling_chr=="raw") {
               scaling[i]=1
            }else{ #other
                scaling[i]=3
            }
        }else{
            stop("formula should only have 2 or 3 arguments")
        }
    }
    return(list("effects"=effects,"params"=params,"scaling"=scaling,"P"=P))
}