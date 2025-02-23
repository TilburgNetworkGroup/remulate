#' Simulate Relational Event Data - Tie oriented model
#' 
#' @description 
#'  A function to simulate relational event data by sampling from a
#' tie oriented relational event model.
#'
#' @details
#' If time is irrelevant and only a specific number of events are desired, set time to Inf. 
#' If both time and events are supplied then the function 
#' stops simulating whenever the first stop condition is met
#' 
#' If effects is a \code{'remstimate'} object, then the params are extracted automatically.
#' Furthermore if exogenous statistics are used, the data.frames corresponding to 
#' the 'attr_actors' argument of the \code{'remstats'} formula must be loaded in the environment.
#' 
#' A list of available statistics. See \link{remulateTieEffects} for details:
#' \itemize{
#'  \item \code{baseline(param)}
#'  \item \code{send()}
#'  \item \code{receive()}
#'  \item \code{dyad()}
#'  \item \code{same()}
#'  \item \code{difference()}
#'  \item \code{average()}
#'  \item \code{minimum()}
#'  \item \code{maximum()}
#'  \item \code{inertia()}
#'  \item \code{reciprocity()}
#'  \item \code{tie()}
#'  \item \code{indegreeSender()}
#'  \item \code{indegreeReceiver()}
#'  \item \code{outdegreeSender()}
#'  \item \code{outdegreeReceiver()}
#'  \item \code{totaldegreeSender()}
#'  \item \code{totaldegreeReceiver()}
#'  \item \code{otp()}
#'  \item \code{itp()}
#'  \item \code{osp()}
#'  \item \code{isp()}
#'  \item \code{psABBA()}
#'  \item \code{psABBY()}
#'  \item \code{psABXA()}
#'  \item \code{psABXB()}
#'  \item \code{psABXY()}
#'  \item \code{psABAY()}
#'  \item \code{recencyContinue()}
#'  \item \code{recencySendSender()}
#'  \item \code{recencySendReceiver()}
#'  \item \code{recencyReceiveSender()}
#'  \item \code{recencyReceiveReceiver()} 
#'  \item \code{rrankSend()} 
#'  \item \code{rrankReceive()} 
#'  \item \code{interact()}
#' }
#'
#' @param effects A \code{formula} object specifying the statistics used to 
#' simulate the network or an object of class \code{"\link[remstimate]{remstimate}"} containing 
#' a fitted object.
#' 
#' @param actors A numeric or character vector representing the actor names.
#' 
#' @param time A numeric value specifying the time up to which the 
#' network should be simulated.
#' 
#' @param events [Optional] An integer specifying the maximum number of events 
#' to simulate.
#' 
#' @param startTime [Optional] A numeric value (default = 0) indicating the time 
#' at which the simulation should start.
#' 
#' @param initial [Optional] A numeric or \code{data.frame} object (default = 0) 
#' specifying how to initialize the network. If an integer is provided, it represents the number of random events to 
#' sample before beginning data generation. If a \code{data.frame} is provided with columns (time, sender, receiver), 
#' it serves as an edgelist of initial events, after which subsequent events 
#' are predicted.
#' 
#' @param riskset [Optional] A \code{matrix} with columns (sender, receiver) 
#' defining a custom risk set.
#' 
#' @param memory [Optional] A string (default = "full") specifying the memory 
#' type used for computing statistics. `"full"` uses the entire event history. `"window"` considers only events occurring within a specified time window. 
#' `"window_m"` considers only a specified number of most recent events. `"decay"` applies an exponential decay, where older events contribute 
#' less based on elapsed time.
#' 
#' @param memoryParam [Optional] A numeric value (> 0) defining the memory 
#' parameter based on the selected memory type. `"window"` defines the length of the time window. `"window_m"` specifies the number of past events to consider. `"decay"` represents the half-life (i.e., time until an event's weight is reduced to half).
#' 
#' @return An object of class \code{"remulateTie"}. A data.frame containing the simulated event sequence with columns (time, sender, receiver).
#' The \code{"remulateTie"} object has the following attributes:
#' \describe{
#'   \item{statistics}{An array with dimensions \code{M x D x P}, where \code{M} is the number of events,  
#'   \code{D} is the number of dyads in the risk set, and \code{P} is the number of computed statistics.}
#'   \item{evls}{A \code{matrix} containing the event list with columns (dyad, time), 
#'   where \code{dyad} represents the index of the (sender, receiver) pair in the risk set.}
#'   \item{actors}{A \code{data.frame} mapping the actor names provided by the user 
#'   to the integer IDs used in internal computations.}
#'   \item{riskset}{A \code{data.frame} with columns (sender, receiver) containing 
#'   the risk set used for dyad indices in the computed statistics and event list.}
#'   \item{initial}{A A numeric or \code{data.frame} object representing the network initialization, 
#'   which can be a number of random initialization events or
#'   a \code{data.frame} specifying pre-existing ties.}
#'   \item{effects}{A \code{formula} object specifying the effects included in the model.}
#'   \item{density}{A numeric value indicating the density of the generated network, 
#'   defined as the number of observed ties divided by \code{N*(N-1)}, where 
#'   \code{N} is the number of actors.}
#' }
#' @examples 
#'  # To generate events up to time '50' in a network of 25 actors with 
#'  # 200 random initial events
#'  # Exogenous attributes data.frame
#'
#'  cov <- data.frame(
#'    id = 1:25, 
#'    time = rep(0, 25), 
#'    sex = sample(c(0, 1), 25, replace = TRUE, prob = c(0.4, 0.6)), 
#'    age = sample(20:30, 25, replace = TRUE) 
#'  )
#'
#'  #Effects specification
#'  effects <- ~ remulate::baseline(-5) + 
#'              remulate::inertia(0.01) + 
#'              remulate::reciprocity(-0.04) + 
#'              remulate::itp(0.01, scaling = "std") + 
#'              remulate::same(0.02, variable = "sex", attr_actors = cov) + 
#'              remulate::interact(0.01, indices = c(2, 5))
#'  # Calling remulateTie
#'  remulate::remulateTie(
#'    effects, 
#'    actors = 1:25, 
#'    time = 50, 
#'    events = 500, 
#'    initial = 200
#'  )
#'
#'  # To predict events, given an edgelist of initial events
#'  initialREH <- data.frame(
#'    time = seq(0.5, 100, 0.5), 
#'    sender = sample(1:25, 200, TRUE), 
#'    receiver = sample(1:25, 200, TRUE)
#'  )
#'
#'  remulate::remulateTie(
#'    effects, 
#'    actors = 1:25, 
#'    time = 150, 
#'    events = 500, 
#'    initial = initialREH
#'  )
#'
#'  # Custom risk set
#'  rs <- as.matrix(expand.grid(1:25, 1:25))
#'  rs <- rs[rs[, 1] != rs[, 2], ]
#'
#'  custom_rs <- rs[sample(1:90, 50), ]
#'
#'  remulate::remulateTie(
#'    effects, 
#'    actors = 1:25, 
#'    time = 150, 
#'    events = 500, 
#'    riskset = custom_rs
#'  )
#'
#' @references
#' Lakdawala, R., Mulder, J., & Leenders, R. (2025).
#' *Simulating Relational Event Histories: Why and How*.
#' arXiv:2403.19329.
#'
#' @export
remulateTie <- function(
  effects,
  actors,
  time ,
  events = NULL,
  startTime = 0,
  initial = 0,
  riskset = NULL,
  memory = c("full", "window", "window_m", "decay"),
  memoryParam = NULL) {

  waiting_time="exp"

  if(inherits(effects, "remstimate")){
    parsed_effects <- parseEffectsTieRemstimate(effects)
  }else if(inherits(effects, "formula")){
    parsed_effects <- parseEffectsTie(effects)
  }
  

  params <- parsed_effects$params
  is_param_dyadic <- parsed_effects$is_param_dyadic
  scaling <- parsed_effects$scaling
  mem_start <- parsed_effects$mem_start
  mem_end <- parsed_effects$mem_end
  int_effects <- parsed_effects$int_effects
  attributes <- parsed_effects$attributes
  interact_effects <- parsed_effects$interact_effects
  effect_names <- unname(parsed_effects$effects)
  
  P <- length(effect_names)

  memory<- match.arg(memory)
  #checking memory specification
  if (!memory[1] %in% c("full", "window", "window_m", "decay")) {
    stop(paste("\n'", memory[1], "'memory method not defined"))
  }
  if (memory != "full" && is.null(memoryParam)) {
    if (memory[1] == "window" || memory[1] == "window_m") {
      stop(paste("Cannot use window memory technique without a memoryParam value"))
    } else if (memoryParam <= 0) {
      stop(paste("memoryParam must be positive"))
    }
  }

  #create a map for user name actor references - integer actor ids for computing
  actors_map <- data.frame(id = 1:length(actors), name = actors)

  #Create a risk set
  if (!is.null(riskset)) { #custom riskset
    if (any(!riskset[[1]] %in% actors_map$name)) {
      stop("risk set contains sender actor not specified in actor's list")
    } else if (any(!riskset[[2]] %in% actors_map$name)) {
      stop("risk set contains receiver actor not specified in actor's list")
    }
    #convert names in riskset to ids
    rs <- riskset
    rs[,2] <- sapply(rs[,2], function(x) {
      actors_map$id[match(x,actors_map$name)]
    })
    rs[,1] <- sapply(rs[,1], function(x) {
        actors_map$id[match(x,actors_map$name)]
    })
  }else{
    #TODO: allow risk set to vary with time (enhancement:feature)
    rs <- as.matrix(expand.grid(actors_map$id, actors_map$id))
    colnames(rs) <- c("sender", "receiver")
    rs <- rs[rs[, "sender"] != rs[, "receiver"],]
  }

  #initialize start time as t=0 if simulating cold-start else set t as time of last event in initial edgelist
  if(is.data.frame(initial)){
      t <- max(initial[,1])
      if(t > time){
        stop("Last event of initial data.frame is after 'time' argument")
      }
  }else{
      #in case is.numeric(initial) OR intial == NULL
      t<- startTime
  }
    
  #TODO: check if exogenous effect doesnt change with time only once (enhancement:comp time)
  #initialize attributes
  attributes <- initialize_exo_effects(attributes, actors_map, parsed_effects)

  #initialize params
  if(any(is_param_dyadic)){ 
    beta_mat <- matrix(0, nrow = nrow(rs), ncol = P)
      for (i in 1:P) {
        if(is_param_dyadic[i]) {            
          arranged_params <- initialize_beta_mat(params[[i]], actors_map, rs)
          beta_mat[, i] <- arranged_params  # params[[i]] is a vector of size D
        }else{
            beta_mat[,i] <- rep(params[[i]], nrow(rs))  # params[[i]] is a scalar
        }
      }
  }else{
    beta <- vector(length = P)
    for (i in 1:P) {
      if(is.function(params[[i]])){
        #function must be defined at t=0
        beta[i] <- params[[i]](t)
      } else {
        beta[i] <- params[[i]]
      }
    }
  }
  

  #initialize output objects
  statistics <- list() #list of matrices
  statistics[[1]] <- array(0, dim = c(nrow(rs), P))
  if(any(int_effects==1)){ #fill in baseline for first time point
    statistics[[1]][,which(int_effects==1)] <- array(1,dim=c(nrow(rs),1))
  }
  edgelist <- array(0, dim = c(1, 3))
  evls <- array(0, dim = c(1, 2))
  # probs <- array(0,dim=c(1,nrow(rs)))

  #stores the event counts for dyads in a #sender x #recv matrix
  adj_mat <- initialize_adj_mat(actors_map, initial, rs)

  i = 1

  while(t <= time){
    #updating event rate / lambda
    # if (P == 1) {
    #   lambda <- exp(statistics[[i]] * beta)
    # } else {
    #   lambda <- exp(statistics[[i]] %*% beta)
    # }
    if(any(is_param_dyadic)){      
        lambda <- exp(rowSums(statistics[[i]] * beta_mat))
    }else{
        if (P == 1) {
            lambda <- exp(statistics[[i]] * beta)
        } else {
            lambda <- exp(statistics[[i]] %*% beta)
        }
    }


    #sampling waiting time dt
    if (waiting_time == "exp") {
      dt <- rexp(1, rate = sum(lambda))
      t <- t + dt
    }
    # else if (waiting_time == "weibull") {
    #   #TODO: add checks on time params
    #   dt <- rweibull(1, shape = time_param, scale = sum(lambda))
    #   t <- t + dt
    # }
    # else if (waiting_time == "gompertz") {
    #   dt <- rgompertz(1, scale = sum(lambda), shape = time_param)
    #   t <- t + dt
    # }

    if(t > time){
      cat(i-1, "events generated \n")
      break
    }

    #sampling dyad for next event
    # R sampling slightly faster than arma sampling (due to hashing)
    dyad <- sample(1:nrow(rs), 1, prob = lambda / sum(lambda))    

    edgelist[i,] <- c(t, rs[dyad, 1], rs[dyad, 2])
    evls[i,] <- c(dyad, t)

    #update adj mat
    #TODO: move to C++
    if (memory == "full") {
      adj_mat[edgelist[i, 2], edgelist[i, 3]] = adj_mat[edgelist[i, 2], edgelist[i, 3]] + 1;
    }
    else if (memory == "window") {
      #window memory takes memory by time window
      #TODO: to vectorize
      adj_mat[] <- 0
      in_window <- which(edgelist[, 1] > t - memoryParam) #event indices which are in memoryParam
      for (ind in in_window) {
        adj_mat[edgelist[ind, 2], edgelist[ind, 3]] = adj_mat[edgelist[ind, 2], edgelist[ind, 3]] + 1;
      }
    }
    else if (memory == "window_m") {
      #window_m takes memory by last m events
      if (memoryParam < i) {
        adj_mat[] <- 0
        print(paste("memory in:", i - memoryParam, "to", i))
        for (ind in c(i - memoryParam, i)) {
          adj_mat[edgelist[ind, 2], edgelist[ind, 3]] = adj_mat[edgelist[ind, 2], edgelist[ind, 3]] + 1;
        }
      } else {
        adj_mat[edgelist[i, 2], edgelist[i, 3]] = adj_mat[edgelist[i, 2], edgelist[i, 3]] + 1;
      }
    }
    else if (memory == "decay") {
      #TODO: to vectorize
      adj_mat[] <- 0
      for (j in 1:i) {
        #loop through edgelist
        adj_mat[edgelist[j, 2], edgelist[j, 3]] = adj_mat[edgelist[j, 2], edgelist[j, 3]] + exp((-(t - edgelist[j, 1])) * (log(2) / memoryParam))
      }
    }
    
    #stop if max number of events reached
    if(!is.null(events) && i-1>=events){
      cat(paste0("Stopping: maximum number of events (",i-1,") sampled \n"))
      break
    }

    statistics[[i + 1]] <- computeStatsTie(int_effects, rs, actors_map$id, edgelist, adj_mat, attributes, interact_effects, scaling, mem_start, mem_end, statistics[[i]])
    
    #add row for next iteration
    edgelist <- rbind(edgelist, array(0, dim = c(1, 3)))
    evls <- rbind(evls, array(0, dim = c(1, 2)))
    #probs <- rbind(probs, array(0, dim = c(1,nrow(rs))))

    #update beta only if function
    for (j in 1:P) {
     if(is.function(params[[j]])){
        beta[j] <- params[[j]](t)
      }
    }
    i=i+1
  }
  
  #combine stats from list to 3d array
  statistics <- array(
    data = do.call(rbind, lapply(statistics, as.vector)),
    dim = c(length(statistics), dim(statistics[[1]]))
  )

  statistics <- statistics[-dim(statistics)[1],,]
  edgelist <- edgelist[-dim(edgelist)[1],]
  evls <- evls[-dim(evls)[1],]

  edgelist <- as.data.frame(edgelist)
  colnames(edgelist) <- c("time", "sender", "receiver")

  #change actor ids to names in edgelist
  edgelist["sender"] <- lapply(edgelist["sender"], function(x) {
    actors_map$name[x]
  })
  edgelist["receiver"] <- lapply(edgelist["receiver"], function(x) {
    actors_map$name[x]
  })

  names(params) <- effect_names

  #return objects
  if(P!=1){
    dimnames(statistics) <- list(NULL, NULL, effect_names)
  }

  output <- structure(
  edgelist,
  evls       = evls,
  params     = params,
  statistics = statistics,
  riskset    = rs,
  actors     = actors_map,
  initial    = initial,
  effects    = effects,
  density    = get.density(evls, actors),
  class      = c("remulateTie", "data.frame")
)

return(output)


}
