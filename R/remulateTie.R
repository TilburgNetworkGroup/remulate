#' Simulate Temporal Events Network - Tie based model
#' @description 
#'  A function to simulate relational event data by sampling from a
#' tie based relational event model.
#'
#' @details
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
#'  \item \code{interact()}
#' }
#'
#' @param effects an object of type \code{formula} for specification of statistics used to simulate the network. 
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
#' \item{statistics}{array of statistics with dimensions M x D x P (M: Number of events, D: Number of dyads in the risk set, P: Number of statistics)}
#' \item{evls}{matrix containing the event list  with columns (dyad,time) where dyad represents the index of the dyad or the (sender,receiver) pair in the riskset}
#' \item{actors_map}{data.frame object containing the mapping of actor names provided by user in \code{actors} argument to the integer ids used in the internal computations}
#' \item{riskset}{data.frame object  wtih columns (sender, receiver) containing the risket set used for the dyad indices in the statistics and evls}
#' \item{density}{numeric value indicating density in the generated network i.e number of observed ties / N*(N-1) (N:number of actors)}
#' }
#' @examples 
#'  # To generate events upto time '50' in a network of 25 actors with 
#'  # 200 random initial events
#'  
#'  #exogenous attributes data.frame
#'  cov <- data.frame(id=1:25, time=rep(0,25), sex=sample(c(0,1),25,replace=T,prob=c(0.4,0.6)), age=sample(20:30,25,replace=T) )
#'  
#'  #effects specification
#'  effects <- ~ remulate::baseline(-5) + remulate::inertia(0.01) + remulate::reciprocity(-0.04)+
#'  remulate::itp(0.01,scaling="std") + remulate::same(0.02,variable="sex",attributes = cov) + remulate::interact(0.01,indices=c(2,5))
#'  
#'  #calling remulateTie
#'  remulate::remulateTie(effects, actors = 1:25, time =50, events = 500, initial = 200)
#'  
#'  # To predict events, given an edgelist of initial events
#'  initialREH <- data.frame(time = seq(0.5,100,0.5), sender = sample(1:25,200,T), receiver = sample(1:25,200,T))
#'  remulate::remulateTie(effects, actors = 1:25, time = 150, events = 500, initial=initialREH)
#' 
#' #custom riskset
#' rs <- as.matrix(expand.grid(1:N,1:N))
#' rs <- rs[rs[,1] != rs[, 2],]
#' custom.rs = rs[sample(1:90,50),]
#' remulate::remulateTie(effects, actors = 1:25, time = 150, events = 500, riskset = custom.rs )
#' 
#'  
#' @export
remulateTie <- function(
  effects,
  actors,
  time ,
  events = NULL,
  initial = 0,
  riskset = NULL,
  memory = c("full", "window", "brandes", "vu"),
  memory_param = NULL,
  seed = NULL,
  damping = 0) {

  #waiting_time =c("exp","weibull","gompertz"),
  #time_param = NULL,
  waiting_time="exp"
  #waiting_time <- match.arg(waiting_time)

  #set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  parsed_effects <- parseEffectsTie(effects)

  params <- parsed_effects$params
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
  if (!memory[1] %in% c("full", "window", "window_m", "brandes", "vu")) {
    stop(paste("\n'", memory[1], "'memory method not defined"))
  }
  if (memory != "full" && is.null(memory_param)) {
    if (memory[1] == "window" || memory[1] == "window_m") {
      stop(paste("Cannot use window memory technique without a memory_param value"))
    } else if (memory_param <= 0) {
      stop(paste("memory_param must be positive"))
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
    #rs <- rs[(rs[, 1] %in% actors_map$id[actors_map$name %in% riskset[[1]]] & rs[, 2] %in% actors_map$id[actors_map$name %in% riskset[[2]]]),]
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
  if(is.numeric(initial)){
    t <- 0
  }else if(is.data.frame(initial)){
    t <- initial[nrow(initial),1]
    if(t > time){
      stop("Last event of initial data.frame is after 'time' argument")
    }
  }
    
  #TODO: check if exogenous effect doesnt change with time only once (enhancement:comp time)
  #initialize attributes
  attributes <- initialize_exo_effects(attributes, actors_map, parsed_effects)

  #initialize params
  beta <- vector(length = P)
  for (i in 1:P) {
    if (class(params[[i]]) == "function") {
      #function must be defined at t=0
      beta[i] <- params[[i]](t)
    } else {
      beta[i] <- params[[i]]
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
  probs <- array(0,dim=c(1,nrow(rs)))

  #stores the event counts for dyads in a #sender x #recv matrix
  adj_mat <- initialize_adj_mat(actors_map, initial, rs)

  i = 1

  while(t <= time){
    #updating event rate / lambda
    if (P == 1) {
      lambda <- exp(statistics[[i]] * beta)
    } else {
      lambda <- exp(statistics[[i]] %*% beta)
    }

    #sampling waiting time dt
    if (waiting_time == "exp") {
      dt <- rexp(1, rate = sum(lambda))
      t <- t + dt
    }
    else if (waiting_time == "weibull") {
      #TODO: add checks on time params
      dt <- rweibull(1, shape = time_param, scale = sum(lambda))
      t <- t + dt
    }
    else if (waiting_time == "gompertz") {
      dt <- rgompertz(1, scale = sum(lambda), shape = time_param)
      t <- t + dt
    }

    if(t > time){
      cat(i-1, "events generated \n")
      break
    }

    #sampling dyad for next event
    # R sampling slightly faster than arma sampling (due to hashing)
    if(runif(1) < damping){
      dyad <- sample(1:nrow(rs), 1)
    }else{
      dyad <- sample(1:nrow(rs), 1, prob = lambda / sum(lambda))
    }
    

    edgelist[i,] <- c(t, rs[dyad, 1], rs[dyad, 2])
    evls[i,] <- c(dyad, t)
    probs[i,] <- lambda

    #update adj mat
    #TODO: move to C++
    if (memory == "full") {
      adj_mat[edgelist[i, 2], edgelist[i, 3]] = adj_mat[edgelist[i, 2], edgelist[i, 3]] + 1;
    }
    else if (memory == "window") {
      #window memory takes memory by time window
      #TODO: to vectorize
      adj_mat[] <- 0
      in_window <- which(edgelist[, 1] > t - memory_param) #event indices which are in memory_param
      for (ind in in_window) {
        adj_mat[edgelist[ind, 2], edgelist[ind, 3]] = adj_mat[edgelist[ind, 2], edgelist[ind, 3]] + 1;
      }
    }
    else if (memory == "window_m") {
      #window_m takes memory by last m events
      if (memory_param < i) {
        adj_mat[] <- 0
        print(paste("memory in:", i - memory_param, "to", i))
        for (ind in c(i - memory_param, i)) {
          adj_mat[edgelist[ind, 2], edgelist[ind, 3]] = adj_mat[edgelist[ind, 2], edgelist[ind, 3]] + 1;
        }
      } else {
        adj_mat[edgelist[i, 2], edgelist[i, 3]] = adj_mat[edgelist[i, 2], edgelist[i, 3]] + 1;
      }
    }
    else if (memory == "brandes") {
      #TODO: to vectorize
      adj_mat[] <- 0
      for (j in 1:i) {
        #loop through edgelist
        adj_mat[edgelist[j, 2], edgelist[j, 3]] = adj_mat[edgelist[j, 2], edgelist[j, 3]] + exp((-(t - edgelist[j, 1])) * (log(2) / memory_param))
      }
    }
    else if (memory == "vu") {
      adj_mat[] <- 0
      for (j in 1:i - 1) {
        #loop through edgelist
        adj_mat[edgelist[j, 2], edgelist[j, 3]] = adj_mat[edgelist[j, 2], edgelist[j, 3]] + 1 / ((t - edgelist[j, 1]) ** memory_param)
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
    probs <- rbind(probs, array(0, dim = c(1,nrow(rs))))

    #update beta
    for (j in 1:P) {
      if (class(params[[j]]) == "function") {
        beta[j] <- params[[j]](t)
      } else {
        beta[j] <- params[[j]]
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

  return(
    list(
        edgelist = edgelist,
        evls = evls,
        statistics = statistics,
        params = params,
        riskset = rs,
        actors = actors_map,
        density = get.density(evls, actors),
        probs = probs
    )
  )
}
