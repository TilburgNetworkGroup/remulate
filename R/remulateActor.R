#' Simulate Relational Event Data - Tie based model
#' 
#' @description
#' A function to simulate relational event data by sampling from an
#' actor oriented event model.
#'
#' @details
#' #' If time is irrelevant and only a specific number of events are desired, set time to Inf. 
#' If both time and events are supplied then the function 
#' stops simulating whenever the first stop condition is met
#' 
#' A list of available statistics for actor rate model. 
#' See \link{remulateActorEffects} for details on effects: 
#' \itemize{
#'  \item \code{baseline()}
#'  \item \code{indegreeSender()}
#'  \item \code{outdegreeSender()}
#'  \item \code{totaldegreeSender()}
#'  \item \code{ospSender()}
#'  \item \code{otpSender()}
#'  \item \code{send()}
#'  \item \code{interact()}
#' }
#'
#' A list of available statistics for receiver choice model. 
#' See \link{remulateActorEffects} for details on effects: :
#'\itemize{
#'  \item \code{baseline()}
#'  \item \code{inertia()}
#'  \item \code{reciprocity()}
#'  \item \code{indegreeReceiver()}
#'  \item \code{outdegreeReceiver()}
#'  \item \code{totaldegreeReceiver()}
#'  \item \code{otp()}
#'  \item \code{itp()}
#'  \item \code{osp()}
#'  \item \code{isp()}
#'  \item \code{receive()}
#'  \item \code{dyad()}
#'  \item \code{same()}
#'  \item \code{average()}
#'  \item \code{difference()}
#'  \item \code{minimum()}
#'  \item \code{maximum()}
#'  \item \code{interact()}
#' }
#'
#' 
#' @param rateEffects A \code{formula} object specifying the statistics used to 
#' simulate the network under the actor rate sub-model.
#' 
#' @param choiceEffects A \code{formula} object specifying the statistics used to 
#' simulate the network under the receiver choice sub-model.
#' 
#' @param actors A numeric or character vector representing the actor names.
#' 
#' @param time A numeric value specifying the time duration up to which the 
#' network should be simulated.
#' 
#' @param events [Optional] An integer specifying the maximum number of events 
#' to simulate.
#' 
#' @param startTime [Optional] A numeric value (default = 0) indicating the time 
#' at which the simulation should start.
#' 
#' @param initial [Optional] A numeric or \code{data.frame} object (default = 0) 
#' specifying how to initialize the network.  
#' - If an integer is provided, it represents the number of random events to 
#' sample before beginning data generation.  
#' - If a \code{data.frame} is provided with columns (time, sender, receiver), 
#' it serves as an edgelist of initial events, after which subsequent events 
#' are predicted.
#' 
#' @param riskset [Optional] A \code{matrix} with columns (sender, receiver) 
#' defining a custom risk set.
#' 
#' @param memory [Optional] A string (default = "full") specifying the memory 
#' type used for computing statistics.  
#' - `"full"`: Uses the entire event history.  
#' - `"window"`: Considers only events occurring within a specified time window.  
#' - `"window_m"`: Considers only a specified number of most recent events.  
#' - `"decay"`: Applies an exponential decay, where older events contribute 
#' less based on elapsed time.
#' 
#' @param memoryParam [Optional] A numeric value (> 0) defining the memory 
#' parameter based on the selected memory type:  
#' - `"window"`: Length of the time window.  
#' - `"window_m"`: Number of past events to consider.  
#' - `"decay"`: Half-life (i.e., time until an event's weight is reduced to half).
#' 
#' @return A list containing:
#' \describe{
#'   \item{edgelist}{A \code{data.frame} with columns (time, sender, receiver) 
#'   representing the generated event sequence.}
#'   \item{evls}{A \code{matrix} containing the event list with columns (dyad, time), 
#'   where \code{dyad} represents the index of the (sender, receiver) pair in the risk set.}
#'   \item{rateStatistics}{An array of dimensions \code{M x N x P}, where:  
#'   - \code{M} is the number of events,  
#'   - \code{N} is the number of actors,  
#'   - \code{P} is the number of sender rate statistics.}
#'   \item{choiceStatistics}{An array of dimensions \code{M x D x Q}, where:  
#'   - \code{M} is the number of events,  
#'   - \code{D} is the number of dyads in the risk set,  
#'   - \code{Q} is the number of receiver choice statistics.}
#'   \item{rateParams}{A named list of rate model parameters corresponding to the 
#'   specified rate statistics.}
#'   \item{choiceParams}{A named list of choice model parameters corresponding to 
#'   the specified choice statistics.}
#'   \item{riskset}{A \code{matrix} with columns (sender, receiver) representing 
#'   the risk set used in the simulation.}
#'   \item{actors}{A \code{data.frame} mapping the actor names provided by the user 
#'   to the integer IDs used in internal computations.}
#'   \item{density}{A numeric value indicating the density of the generated network, 
#'   defined as the number of observed ties divided by \code{N*(N-1)}, where 
#'   \code{N} is the number of actors.}
#' }
#' @examples 
#'  # To generate events up to time '50' in a network of 25 actors with 
#'  # 200 random initial events
#'  
#'  # Exogenous attributes data.frame
#'  cov <- data.frame(
#'    id = 1:25, 
#'    time = rep(0, 25), 
#'    sex = sample(c(0,1), 25, replace = TRUE, prob = c(0.4, 0.6)), 
#'    age = sample(20:30, 25, replace = TRUE) 
#'  )
#'  
#'  # Effects specification
#'  rateform <- ~ remulate::baseline(-6) + 
#'               remulate::indegreeSender(0.01) + 
#'               remulate::send(0.02, variable = "age", attributes = cov) + 
#'               remulate::interact(0.01, indices = c(2, 3))
#'  
#'  choiceform <- ~ remulate::inertia(0.01) + 
#'                remulate::reciprocity(-0.03) + 
#'                remulate::interact(0.01, indices = c(2, 1))
#'  
#'  # Calling remulateActor
#'  remulate::remulateActor(
#'    rateform, 
#'    choiceform, 
#'    actors = 1:25, 
#'    time = 100, 
#'    initial = 200, 
#'    events = 500, 
#'  )
#'   
#'  # To predict events, given an edgelist of initial events
#'  initialREH <- data.frame(
#'    time = seq(0.5, 100, 0.5), 
#'    sender = sample(1:25, 200, TRUE), 
#'    receiver = sample(1:25, 200, TRUE)
#'  )
#'  
#'  remulate::remulateActor(
#'    rateform, 
#'    choiceform, 
#'    actors = 1:25, 
#'    time = 200, 
#'    initial = initialREH, 
#'    events = 500
#'  )
#' @references
#' Lakdawala, R., Mulder, J., & Leenders, R. (2025).
#' *Simulating Relational Event Histories: Why and How*.
#' arXiv:2403.19329.
#'
#' @export
remulateActor <- function(
    rateEffects,
    choiceEffects,
    actors,
    time,
    events = NULL,
    startTime = 0,
    initial = 0,
    riskset = NULL,
    memory = c("full","window","window_m","decay"),
    memoryParam = NULL){

    waiting_time="exp"

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
    if(memory != "full" && is.null(memoryParam)){
        if(memory[1]=="window" || memory[1]=="window_m"){
            stop(paste("Cannot use window memory technique without a memoryParam value"))
        }else if(memoryParam <= 0){
            stop(paste("memoryParam must be positive"))
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
    if(is.data.frame(initial)){
        t <- initial[nrow(initial),1]
        if(t > time){
         stop("Last event of initial data.frame is after 'time' argument")
        }
    }else{
        #in case is.numeric(initial) OR intial == NULL
        t<- startTime
    }

    #initialize attributes
    s_attributes <- initialize_exo_effects(s_attributes,actors_map,parsed_s)
    
    d_attributes <- initialize_exo_effects(d_attributes,actors_map,parsed_d)

    #initialize params for sender
    gamma <- vector(length = s_P)
    for(i in 1:s_P){
        if(is.function(s_params[[i]])){#function must be defined at t=0
            gamma[i] <- s_params[[i]](t)
        }else{
            gamma[i] <- s_params[[i]]
        }
    }
    #initialize params for receiver choice
    beta <- vector(length = d_P)
    for(i in 1:d_P){
        if(is.function(d_params[[i]])){#function must be defined at t=0
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
        # else if (waiting_time=="weibull") {
        #     #TODO: add checks on time params
        #    dt <- rweibull(1,shape=time_param,scale = sum(s_lambda))
        #    t <- t + dt
        # }
        # else if (waiting_time=="gompertz") {
        #    dt <- rgompertz(1,scale = sum(s_lambda), shape=time_param)
        #    t <- t + dt
        # }
        
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
            in_window <- which(edgelist[,1] > t - memoryParam) #event indices which are in memoryParam
            for(ind in in_window){
                adj_mat[edgelist[ind,2],edgelist[ind,3]] =  adj_mat[edgelist[ind,2],edgelist[ind,3]] + 1;
            }
        }
        else if(memory=="window_m"){ #window_m takes memory by last m events
            if(memoryParam<i){
                adj_mat[] <- 0
                print(paste("memory in:",i-memoryParam,"to",i))
                for(ind in c(i-memoryParam,i)){
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
                adj_mat[edgelist[j,2], edgelist[j,3]] = adj_mat[edgelist[j,2], edgelist[j,3]] + exp((-(t-edgelist[j,1]))*(log(2)/memoryParam))
            }
        }
        else if(memory=="vu"){
            adj_mat [] <- 0
            for(j in 1:i-1){#loop through edgelist
                adj_mat[edgelist[j,2], edgelist[j,3]] = adj_mat[edgelist[j,2], edgelist[j,3]] + 1/((t-edgelist[j,1])**memoryParam)
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
            if(is.function(d_params[[j]])){#function must be defined at t=0
                beta[j] <- d_params[[j]](t)
            }else{
                beta[j] <- d_params[[j]]
            }
        }
        
        for(j in 1:s_P){
            if(is.function(s_params[[j]])){#function must be defined at t=0
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
