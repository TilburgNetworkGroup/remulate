#' Simulate Temporal Events Network - Actor oriented model
#' @description
#'  A function to simulate relational event data by sampling from an
#' actor oriented event model.
#'
#' @details
#' 
#' A list of available statistics for actor rate sub-model. See \link{remulateActorEffects} for details on effects: 
#' \itemize{
#'  \item \code{baseline()}
#'  \item \code{send()}
#'  \item \code{indegreeSender()}
#'  \item \code{outdegreeSender()}
#'  \item \code{totaldegreeSender()}
#'  \item \code{ospSender()}
#'  \item \code{otpSender()}
#'  \item \code{interact()}
#' }
#' A list of available statistics for receiver choice sub-model:
#'\itemize{
#'  \item \code{baseline()}
#'  \item \code{receive()}
#'  \item \code{dyad()}
#'  \item \code{same()}
#'  \item \code{difference()}
#'  \item \code{equate()}
#'  \item \code{event()}
#'  \item \code{inertia()}
#'  \item \code{reciprocity()}
#'  \item \code{indegreeReceiver()}
#'  \item \code{outdegreeReceiver()}
#'  \item \code{totaldegreeReceiver()}
#'  \item \code{otp()}
#'  \item \code{itp()}
#'  \item \code{osp()}
#'  \item \code{isp()}
#'  \item \code{interact()}
#' }
#'
#' 
#' @param rateEffects an object of type \code{formula} for specification of statistics used to simulate the network under the actor rate sub-model
#' @param choiceEffects an object of type \code{formula} for specification of statistics used to simulate the network under the receiver choice sub-model
#' @param actors Numeric or character vector of actor names.
#' @param time Numeric, time upto which to simulate network.
#' @param events [Optional] Integer, maximum number of events to simulate.
#' @param initial [Optional] (default = 0) Numeric or data.frame object indicating how to initialize the network. ' integer' value denotes the number of random events to sample before beginning with the data generation. data.frame with columns (time,sender,receiver), it is an edgelist of initial events following which the subsequent events are predicted.
#' @param riskset [Optional] \code{matrix} object wtih columns (sender, receiver) for custom risk set
#' @param memory [Optional] (default = full) String indicating which.
#'  memory type to use. "full" uses the entire event history to compute statistics, "window" memory indicates a window in the past upto.
#' which occured events will be remembered for computing statistics, "brandes" memory type uses past events
#' weighted by their time, "vu" memory type uses past events weighted by 1/time since event occured.
#' @param memory_param [Optional] value > 0. For memory type "window" this parameter indicates the length (in time units) of the window. For memory type "brandes" the memory_param is the half-life i.e the time until an event has a weight of one half. For memory type "vu" the memory_param is power of (1/time since event occured).
#' @param seed [Optional] Seed for random number stream.
#' @return \describe{
#' \item{edgelist}{data.frame object with columns (time,sender,receiver)}
#' \item{statistics}{array of statistics of dimensions M x D x P (M: Number of events, D: Number of dyads in the risk set, P: Number of statistics)}
#' \item{evls}{matrix containing the event list  with columns (dyad,time) where dyad represents the index of the dyad or the (sender,receiver) pair in the riskset}
#' \item{actors_map}{data.frame object containing the mapping of actor names provided by user to the integer ids used in the internal computations}
#' \item{riskset}{data.frame object  wtih columns (sender, receiver) containing the risket set used for the dyad indices in the statistics and evls}
#' \item{density}{value indicating density in the generated network i.e number of observed ties / N*(N-1) (N:number of actors)}
#' }
#' @examples 
#'  # To generate events upto time '50' in a network of 25 actors with 
#'  # 200 random initial events
#'  
#'  #exogenous attributes data.frame
#'  cov <- data.frame(id=1:25, time=rep(0,25), sex=sample(c(0,1),25,replace=T,prob=c(0.4,0.6)), age=sample(20:30,25,replace=T) )
#'  
#'  #effects specification
#'  rateform <- ~ remulate::baseline(-6) + remulate::indegreeSender(0.01) +remulate::send(0.02,variable="age",attributes = cov)+ remulate::interact(0.01,indices=c(2,3))
#'  choiceform <- ~ remulate::inertia(0.01) + remulate::reciprocity(-0.03)+remulate::interact(0.01,indices=c(2,1))
#'  
#'  #calling remulateActor
#'  remulate::remulateActor(rateform,choiceform,actors = 1:25,time=100,initial = 200,events = 500,seed=123)
#'   
#'  # To predict events, given an edgelist of initial events
#'  initialREH <- data.frame(time = seq(0.5,100,0.5), sender = sample(1:25,200,T), receiver = sample(1:25,200,T))
#'  remulate::remulateActor(rateform, choiceform, actors = 1:25, time=200, initial = initialREH, events = 500, seed=123)
#' 
#' @export
remulateActor <- function(
    rateEffects,
    choiceEffects,
    actors,
    time,
    events = NULL,
    initial = 0,
    riskset = NULL,
    memory = c("full","window","brandes","vu"),
    memory_param = NULL,
    seed = NULL){
    

    #waiting_time =c("exp","weibull","gompertz"),
    #time_param = NULL,
    waiting_time="exp"
    #waiting_time <- match.arg(waiting_time)

    #set seed
    if(!is.null(seed)){
        set.seed(seed)
    }
    #process input for rate sub-model
    parsed_s <- parseEffectsRate(rateEffects)
    s_params <- parsed_s$params
    s_scaling <- parsed_s$scaling
    s_int_effects <- parsed_s$int_effects
    s_attributes <- parsed_s$attributes
    s_effects <- unname(parsed_s$effects)
    s_interact_effects <- parsed_s$interact_effects
    s_P <- length(s_effects)
    #process input for receiver choice sub-model
    parsed_d <- parseEffectsChoice(choiceEffects)
    d_params <- parsed_d$params
    d_scaling <- parsed_d$scaling
    mem_start <- parsed_d$mem_start
    mem_end <- parsed_d$mem_end
    d_int_effects <- parsed_d$int_effects
    d_attributes <- parsed_d$attributes
    d_effects <- unname(parsed_d$effects)
    d_interact_effects <- parsed_d$interact_effects
    d_P <- length(d_effects)
    memory<- match.arg(memory)
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
    actors_map <- data.frame(id=1:length(actors),name = actors)
    
    #Create a risk set
    #TODO: allow risk set to vary with time
    rs <- as.matrix(expand.grid(actors_map$id,actors_map$id))
    colnames(rs) <- c("sender", "receiver")
    rs <- rs[rs[,"sender"]!=rs[,"receiver"],]
    if(!is.null(riskset)){
        if(any(!riskset[[1]] %in% actors_map$name)){
            stop("risk set contains sender actor not specified in actor's list")
        } else if (any(!riskset[[2]] %in% actors_map$name)) {
           stop("risk set contains receiver actor not specified in actor's list")
        }
        #convert names in riskset to ids
        rs <- rs[(rs[,1] %in% actors_map$id[actors_map$name %in% riskset[[1]]] & rs[,2] %in% actors_map$id[actors_map$name %in% riskset[[2]] ]),]
    }
    
     #initialize start time as t=0 if simulating cold-start else set t as time of last event in initial edgelist
    if(is.numeric(initial)){
        t <- 0
    }else if(is.data.frame(initial)){
        t <- initial[nrow(initial),1]
        if(t > time){
        stop("Last event of initial data.frame is after 'time' argument")
        }
    }
    #initialize attributes
    s_attributes <- initialize_exo_effects(s_attributes,actors_map,parsed_s)
    
    d_attributes <- initialize_exo_effects(d_attributes,actors_map,parsed_d)

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

    #initialize output objects
    rateStatistics <- list() #list of matrices
    rateStatistics[[1]] <- array(0,dim=c(N,s_P))
    if(any(s_int_effects==1)){ #fill in baseline for first time point
        rateStatistics[[1]][,which(s_int_effects==1)] <- array(1,dim=c(N,1))
    }
    choiceStatistics <- list() #list of matrices
    choiceStatistics[[1]] <- array(0,dim=c(nrow(rs),d_P))

    edgelist <- array(0,dim=c(1,3))
    evls <- array(0,dim=c(1,2))
    
    #adjacency matrix ( #sender x #recv matrix)
    adj_mat <- initialize_adj_mat(actors_map, initial, rs)

    i = 1

    while(t <= time){
                
        #compute rate and choice probabilities
        if(s_P==1){
            s_lambda <- exp(rateStatistics[[1]] * gamma)
        } else{
            s_lambda <- exp(rateStatistics[[i]] %*% gamma)
        }

        if(d_P==1){
                d_lambda <- exp(choiceStatistics[[1]] * beta)
        } else{
            d_lambda <- exp(choiceStatistics[[i]] %*% beta)
        }
 
        #sampling waiting time dt
        if(waiting_time=="exp"){
            dt <- rexp(1,rate = sum(s_lambda))
            t <- t + dt
        }
        else if (waiting_time=="weibull") {
            #TODO: add checks on time params
           dt <- rweibull(1,shape=time_param,scale = sum(s_lambda))
           t <- t + dt
        }
        else if (waiting_time=="gompertz") {
           dt <- rgompertz(1,scale = sum(s_lambda), shape=time_param)
           t <- t + dt
        }
        
        if(t > time){
            cat(i-1, "events generated \n")
            break
        }

        #Sampling sender and receiver for next event
        sender <- sample(1:N,1,prob = s_lambda/sum(s_lambda))

        recv_ind <- which(rs[,1]==sender & rs[,2]!= sender)

        dyad <- sample(recv_ind,1,prob = d_lambda[recv_ind]/sum(d_lambda[recv_ind]))

        edgelist[i,]<- c(t,sender,rs[dyad,2])
        evls[i,] <- c(dyad,t) 

        if(memory=="full"){
            adj_mat[edgelist[i,2],edgelist[i,3]] =  adj_mat[edgelist[i,2],edgelist[i,3]] + 1;
        }
        else if (memory=="window"){ #window memory takes memory by time window
            #TODO: to vectorize
            adj_mat[] <- 0
            in_window <- which(edgelist[,1] > t - memory_param) #event indices which are in memory_param
            for(ind in in_window){
                adj_mat[edgelist[ind,2],edgelist[ind,3]] =  adj_mat[edgelist[ind,2],edgelist[ind,3]] + 1;
            }
        }
        else if(memory=="window_m"){ #window_m takes memory by last m events
            if(memory_param<i){
                adj_mat[] <- 0
                print(paste("memory in:",i-memory_param,"to",i))
                for(ind in c(i-memory_param,i)){
                    adj_mat[edgelist[ind,2],edgelist[ind,3]] =  adj_mat[edgelist[ind,2],edgelist[ind,3]] + 1;
                }
            }else{
                adj_mat[edgelist[i,2],edgelist[i,3]] =  adj_mat[edgelist[i,2],edgelist[i,3]] + 1;
            } 
        }
        else if(memory=="brandes"){
            #TODO: to vectorize
            adj_mat [] <- 0
            for(j in 1:i){#loop through edgelist
                adj_mat[edgelist[j,2], edgelist[j,3]] = adj_mat[edgelist[j,2], edgelist[j,3]] + exp((-(t-edgelist[j,1]))*(log(2)/memory_param))
            }
        }
        else if(memory=="vu"){
            adj_mat [] <- 0
            for(j in 1:i-1){#loop through edgelist
                adj_mat[edgelist[j,2], edgelist[j,3]] = adj_mat[edgelist[j,2], edgelist[j,3]] + 1/((t-edgelist[j,1])**memory_param)
            }
        }
        #stop if max number of events reached
        if(!is.null(events) && i-1>=events){
            cat(paste0("Stopping: maximum number of events (",i-1,") sampled \n"))
            break
        }

        #update choice stats for t_i 
        choiceStatistics[[i+1]] <- computeStatsTie(d_int_effects,rs,actors_map$id, edgelist,adj_mat, d_attributes,d_interact_effects, d_scaling, mem_start, mem_end, choiceStatistics[[i]] )
        
        #update rate stats for t_i 
        rateStatistics[[i+1]] <- computeStatsActor(s_int_effects,rs, actors_map$id, edgelist,adj_mat, s_attributes, s_interact_effects, s_scaling, rateStatistics[[i]] )
        

        #add row for next iteration
        edgelist <- rbind(edgelist,array(0,dim=c(1,3)))
        evls <- rbind(evls,array(0,dim=c(1,2)))        

        #update parameters in case they vary with time
        for(j in 1:d_P){
            if(class(d_params[[j]])=="function"){
                beta[j] <- d_params[[j]](t)
            }else{
                beta[j] <- d_params[[j]]
            }
        }
        
        for(j in 1:s_P){
            if(class(s_params[[j]])=="function"){
                gamma[j] <- s_params[[j]](t)
            }else{
                gamma[j] <- s_params[[j]]
            }
        }
        i = i+1
    }

    #combine stats from list to 3d array
    rateStatistics <- array (
        data = do.call(rbind, lapply(rateStatistics, as.vector)), 
        dim = c(length(rateStatistics), dim(rateStatistics[[1]]))
    )

    choiceStatistics <- array (
        data = do.call(rbind, lapply(choiceStatistics, as.vector)), 
        dim = c(length(choiceStatistics), dim(choiceStatistics[[1]]))
    )

    rateStatistics <- rateStatistics[-dim(rateStatistics)[1],,]
    choiceStatistics <- choiceStatistics[-dim(choiceStatistics)[1],,]
    edgelist <- edgelist[-dim(edgelist)[1],]
    evls <- evls[-dim(evls)[1],]

    edgelist <- as.data.frame(edgelist)
    colnames(edgelist) <- c("time","sender","receiver")

    #change actor ids to names in edgelist
    edgelist["sender"] <- lapply(edgelist["sender"], function(x){
        actors_map$name[x]
    })
    edgelist["receiver"] <- lapply(edgelist["receiver"], function(x){
        actors_map$name[x]
    })

    
    names(s_params) <- s_effects
    names(d_params) <- d_effects


    #return objects
    if(s_P!=1){
        dimnames(rateStatistics) <- list(NULL,NULL,s_effects)
    }
    if(d_P!=1){
        dimnames(choiceStatistics)<-list(NULL,NULL,d_effects)
    }
    
    return(
        list(
            edgelist = edgelist,
            evls = evls,
            rateStatistics = rateStatistics,
            choiceStatistics = choiceStatistics,
            rateParams = s_params,
            choiceParams = d_params,
            riskset = rs,
            actors = actors_map,
            density = get.density(evls,actors)
        )
    ) 
}
