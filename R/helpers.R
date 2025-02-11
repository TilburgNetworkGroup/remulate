# Function to generate random events for the burn in period
initialize_adj_mat<- function(actors_map,initial,rs){
  if (is.numeric(initial)) {
    if(initial==0){
      return(array(0,dim=c(nrow(actors_map),nrow(actors_map))))
    }
    #sample random events
    dyads <- sample(1:nrow(rs),initial,replace=T)
    #make a new adj_mat for null events
    adj_mat <- array(0,dim=c(nrow(actors_map),nrow(actors_map)))
    for(ind in 1:initial){
      adj_mat[    rs[dyads[ind],1]  ,rs[dyads[ind],2] ] =  adj_mat[    rs[dyads[ind],1]  ,rs[dyads[ind],2] ] + 1;
    }
    cat(initial,"random events were used to initialize the statistics \n")
  }
  else if(is.data.frame(initial)){
    colnames(initial) <- c("time", "sender", "receiver")
    
    #change actor ids to names in initial
    initial["sender"] <- sapply(initial["sender"], function(x) {
      actors_map$id[match(x,actors_map$name)]
    })
    initial["receiver"] <- sapply(initial["receiver"], function(x) {
      actors_map$id[match(x,actors_map$name)]
    })
    
    adj_mat <- array(0,dim=c(nrow(actors_map),nrow(actors_map)))
    for(i in 1:nrow(initial)){
      adj_mat[initial[i,2],initial[i,3]] =  adj_mat[initial[i,2],initial[i,3]] + 1;
    }
  }
  
  return(adj_mat)
}

# get.density
# Function to compute the global density of the network. Can be used to check for degeneracy of the simulated network
#
# density = number of unique edges in the network / total number of possible edges between nodes
# evls event list for the network i.e a matrix with columns (dyad,time)
get.density <- function(evls,actors){
  edges <- length(unique(evls[,1]))
  total_edges <- length(actors)*(length(actors)-1)
  return(edges/total_edges)
}

#Function to initialize attributes dataframe according to integer actor IDs in actors map
initialize_exo_effects <- function(attributes,actors_map,effects){
  P = length(attributes)
  if(P==0){
    return(list())
  }
  for(i in 1:P){
    if(!is.null(attributes[[i]])){
      if(effects$int_effects[i] %in% c(28)){  #28:dyad
        if(any(! actors_map$name %in% attributes[[i]]$sender_id )){
          stop(paste(names(effects)[i]," dyadic covariate not specified for all senders in actor's list"))
        }
        if(any(! actors_map$name %in% attributes[[i]]$receiver_id )){
          stop(paste(names(effects)[i]," dyadic covariate not specified for all receivers in actor's list"))
        }

        attributes[[i]]$sender_id <- actors_map$id[match(attributes[[i]]$sender_id,actors_map$name)]
        attributes[[i]]$receiver_id <- actors_map$id[match(attributes[[i]]$receiver_id,actors_map$name)]
        attributes[[i]] <- as.matrix(attributes[[i]])

      }else{
        if(any(! actors_map$name %in% attributes[[i]]$id )){
          stop(paste(names(effects)[i]," actor covariate not specified for all actors in actor's list"))
        }
        attributes[[i]]$id <- actors_map$id[match(attributes[[i]]$id,actors_map$name)]
        attributes[[i]] <- as.matrix(attributes[[i]])
      }
      
    }
  }
  return(attributes)
}

# attributes- list of data frames for each each effect in formula, NULL if the effect is not exogenous and a data frame with columns (id,time,value) if exogenous
parseEffectsTie <- function(formula){
  # Get effects information
  ft <- stats::terms(formula,keep.order = TRUE)
  
  var <- attr(ft, "variables")
  var <- as.list(var)[-1]
  
  effects <- lapply(var, eval)
  effects <- unlist(effects, recursive = FALSE)
  
  all_effects <- c(
    "baseline", #1
    "send", "receive", #2 #3
    "same", "difference", "average", #4 #5 #6
    "minimum", "maximum", #7 #8
    "tie", "inertia", "reciprocity", #9 #10 #11
    "indegreeSender", "indegreeReceiver", #12 #13
    "outdegreeSender", "outdegreeReceiver", #14 #15
    "totaldegreeSender", "totaldegreeReceiver", #16, #17
    "otp", "itp", "osp", "isp", #18 #19 #20 #21
    "psABBA", "psABBY", "psABXA",  #22 #23 #24
    "psABXB", "psABXY", "psABAY",  #25 #26 #27
    "dyad", #28
    "interact", #29
    "recencyContinue", #30
    "recencySendSender","recencySendReceiver", #31,#32
    "recencyReceiveSender","recencyReceiveReceiver", #33, #34
    "rrankSend","rrankReceive" #35, #36
  )
  
  if(any(! names(effects) %in% all_effects)){
    stop(paste("An effect specified in effects argument is not a valid effect"))
  }
  
  # Prepare effects for switch  case
  int_effects <- match(names(effects), all_effects)
  
  # Prepare scaling info, default raw counts
  scaling <- sapply(effects, function(x) {
    sc <- x$scaling
    sc
  })
  
  mem_start <- sapply(effects, function(x) {
    mm <- x$mem_start
    mm
  })
  
  mem_end <- sapply(effects, function(x) {
    mm <- x$mem_end
    mm
  })
  
  # Prepare the params, can be NULL if just computing statistics
  params <- sapply(effects,function(x){
    p <- x$param
    p
  })
  
  #Prepare dimnames of stats cube output
  stat_names <- sapply(effects,function(x){
    if(x$stat_name=="interact"){
      s <- paste(vapply(x$interact.vec,FUN.VALUE="character",function(x){effects[[x]]$stat_name}),collapse="*",sep="")
    }else{
      s <- x$stat_name
    }
    s
  })
  
  #Prepare the attributes
  attributes<- lapply(effects,function(x){
    c <- x$cov
    c
  })
  
  #prepare interaction effects
  interact_effects <- lapply(effects,function(x){
    if(x$stat_name=="interact"){
      if(length(x$interact.vec)<2){
        stop(paste("Interact effect cannot have less than two effects"))
      }
      if(! all(x$interact.vec %in% c(1:length(effects)[-which(names(effects)=="interact")]))){
        stop(paste("Interact effect mis-specified"))
      }
      i <- x$interact.vec-1 # -1 for cpp indexing
    }
    else{
      i <- NULL
    }
    i
  })
  
  return(list(
    "int_effects"=int_effects,
    "params"=params,
    "scaling"=scaling,
    "effects"=stat_names,
    "attributes"=attributes,
    "interact_effects"=interact_effects,
    "mem_start"=mem_start,
    "mem_end"=mem_end))
}

# attributes- list of data frames for each each effect in formula, NULL if the effect is not exogenous and a data frame with columns (id,time,value) if exogenous
parseEffectsRate <- function(formula,pred = FALSE){
  # Get effects information
  ft <- stats::terms(formula,keep.order = TRUE)
  
  var <- attr(ft, "variables")
  var <- as.list(var)[-1]
  
  effects <- lapply(var, eval)
  effects <- unlist(effects, recursive = FALSE)
  
  all_effects <- c(
    "baseline", #1
    "send", #2
    "indegreeSender", #3
    "outdegreeSender", #4
    "totaldegreeSender",#5
    "ospSender", "otpSender", #6 #7
    "interact" #8
  )
  
  if(any(!names(effects) %in% all_effects)){
    stop(paste("An effect specified in effectsRate is not a valid effect"))
  }
  
  # Prepare effects for switch  case
  int_effects <- match(names(effects), all_effects)
  
  # Prepare scaling info, default raw counts
  scaling <- sapply(effects, function(x) {
    sc <- x$scaling
    sc
  })
  
  # Prepare the params, can be NULL if just computing statistics
  params <- sapply(effects,function(x){
    p <- x$param
    p
  })
  
  #Prepare dimnames of stats cube output
  stat_names <- sapply(effects,function(x){
    if(x$stat_name=="interact"){
      s <- paste(vapply(x$interact.vec,FUN.VALUE="character",function(x){effects[[x]]$stat_name}),collapse="*",sep="")
    }else{
      s <- x$stat_name
    }
    s
  })
  
  #Prepare the attributes
  attributes<- lapply(effects,function(x){
    c <- x$cov
    c
  })
  
  #prepare interaction effects
  interact_effects <- lapply(effects,function(x){
    if(x$stat_name=="interact"){
      if(length(x$interact.vec)<2){
        stop(paste("Interact effect cannot have less than two effects"))
      }
      if(! all(x$interact.vec %in% c(1:length(effects)[-which(names(effects)=="interact")]))){
        stop(paste("Interact effect mis-specified"))
      }
      i <- x$interact.vec-1 # -1 for cpp indexing
    }
    else{
      i <- NULL
    }
    i
  })
  
  return(list("int_effects"=int_effects,"params"=params,"scaling"=scaling,"effects"=stat_names,"attributes"=attributes,"interact_effects"=interact_effects))
}

parseEffectsChoice <- function(formula){
  # Get effects information
  ft <- stats::terms(formula,keep.order = TRUE)
  
  var <- attr(ft, "variables")
  var <- as.list(var)[-1]
  
  effects <- lapply(var, eval)
  effects <- unlist(effects, recursive = FALSE)
  
  all_effects <- c(
    "baseline", #1
    "", "receive", #2 #3
    "same", "difference", "average", #4 #5 #6
    "minimum", "maximum", #7 #8
    "tie", "inertia", "reciprocity", #9 #10 #11
    "", "indegreeReceiver", #12 #13
    "", "outdegreeReceiver", #14 #15
    "", "totaldegreeReceiver", #16, #17
    "otp", "itp", "osp", "isp", #18 #19 #20 #21
    "", "", "",  #22 #23 #24
    "", "", "",  #25 #26 #27
    "dyad", #28
    "interact" #29
  )
  
  # Prepare effects for switch  case
  int_effects <- match(names(effects), all_effects)
  
  # Prepare scaling info, default raw counts
  scaling <- sapply(effects, function(x) {
    sc <- x$scaling
    sc
  })
  
  mem_start <- sapply(effects, function(x) {
    mm <- x$mem_start
    mm
  })
  
  mem_end <- sapply(effects, function(x) {
    mm <- x$mem_end
    mm
  })
  
  
  # Prepare the params, can be NULL if just computing statistics
  params <- sapply(effects,function(x){
    p <- x$param
    p
  })
  
  #Prepare dimnames of stats cube output
  stat_names <- sapply(effects,function(x){
    if(x$stat_name=="interact"){
      s <- paste(vapply(x$interact.vec,FUN.VALUE="character",function(x){effects[[x]]$stat_name}),collapse="*",sep="")
    }else{
      s <- x$stat_name
    }
    s
  })
  
  #Prepare the attributes
  attributes<- lapply(effects,function(x){
    c <- x$cov
    c
  })
  
  #prepare interaction effects
  interact_effects <- lapply(effects,function(x){
    if(x$stat_name=="interact"){
      if(length(x$interact.vec)<2){
        stop(paste("Interact effect cannot have less than two effects"))
      }
      if(! all(x$interact.vec %in% c(1:length(effects)[-which(names(effects)=="interact")]))){
        stop(paste("Interact effect mis-specified"))
      }
      i <- x$interact.vec-1 # -1 for cpp indexing
    }
    else{
      i <- NULL
    }
    i
  })
  
  return(list(
    "int_effects"=int_effects,
    "params"=params,
    "scaling"=scaling,
    "effects"=stat_names,
    "attributes"=attributes,
    "interact_effects"=interact_effects,
    "mem_start"=mem_start,
    "mem_end"=mem_end))
}

# Does not work with interact effects(!) and exogenous stats objects must be loaded
parseEffectsTieRemstimate <- function(remstimate_object){
    formula = attr(remstimate_object,"formula")
    coefficients = remstimate_object$coef
    
    formula_str <- deparse(formula)
    
    # Replace remstats:: with remulate::
    formula_str <- gsub("remstats::", "remulate::", formula_str)
    
    # Convert '1' to remulate::baseline
    formula_str <- gsub("\\b1\\b", "remulate::baseline()", formula_str)

    formula_remulate = as.formula(paste(formula_str, collapse = " "))
    
    ft <- stats::terms(formula_remulate, keep.order = TRUE)
    
    var <- attr(ft, "variables")
    var <- as.list(var)[-1]
    
    effects <- lapply(var, eval)
    effects <- unlist(effects, recursive = FALSE)
    
    all_effects <- c(
        "baseline", "send", "receive", "same", "difference", "average",
        "minimum", "maximum", "tie", "inertia", "reciprocity",
        "indegreeSender", "indegreeReceiver", "outdegreeSender", "outdegreeReceiver",
        "totaldegreeSender", "totaldegreeReceiver", "otp", "itp", "osp", "isp",
        "psABBA", "psABBY", "psABXA", "psABXB", "psABXY", "psABAY", "dyad",
        "interact", "recencyContinue", "recencySendSender", "recencySendReceiver",
        "recencyReceiveSender", "recencyReceiveReceiver", "rrankSend", "rrankReceive"
    )
    
    if (any(!names(effects) %in% all_effects)) {
        stop("An effect specified in effects argument is not a valid effect")
    }
    
    int_effects <- match(names(effects), all_effects)
    
    scaling <- sapply(effects, function(x) {
      sc <- x$scaling
      sc
    })
    
    mem_start <- sapply(effects, function(x) {
      mm <- x$mem_start
      mm
    })
    
    mem_end <- sapply(effects, function(x) {
      mm <- x$mem_end
      mm
    })
      
    # params <- sapply(names(effects), function(x){
    #       coefficients[[x]]
    # }) 
    params <- coefficients
    
    stat_names <- sapply(effects, function(x) x$stat_name)
    
    attributes <- lapply(effects, function(x) x$cov)
    
    interact_effects <- lapply(effects,function(x){
      NULL
    })
    
    return(list(
        "int_effects" = int_effects,
        "params" = params,
        "scaling" = scaling,
        "effects" = stat_names,
        "attributes" = attributes,
        "interact_effects" = interact_effects,
        "mem_start" = mem_start,
        "mem_end" = mem_end
    ))
}


# Internal function, modified from remstats
prepExoVar <- function(effect_name, param, scaling, variable, attributes) {
  # Warning for missing values
  if(anyNA(attributes[,variable])) {
    warning(paste("Missing values in attributes object, variable:",variable))
  }
  
  scaling <- match(scaling,c("full","std","inertia"))

  #dyadic covariate
  if(effect_name %in% c("dyad")){
    cov<- data.frame(
      sender_id = attributes[,1],
      receiver_id = attributes[,2],
      val = attributes[,variable]
    )
  }else{
    #TODO: Allow all cov in the same matrix for cpp computation (memory)
    cov <- data.frame(
      id = attributes[,1],
      time = attributes[,2],
      val = attributes[,variable]
    )
    cov <- cov[order(cov$id,cov$time),]
  }
  
  out <- list(
    effect = list(
      param= param,
      scaling = scaling,
      cov = cov,
      mem_start=0,
      mem_end=0,
      stat_name = paste0(effect_name,"_",variable)
    )
  )
  names(out) <- effect_name
  out
}

# Internal function, modified from remstats
prepEndoVar <- function(effect_name, param, scaling,start=0,end=0) {
  
  scaling <- match(scaling,c("full","std","prop","log"))
  if(start!=0 || end!=0){
    stat_name = paste0(c(effect_name,start,end),collapse="_") 
  }else{
    stat_name = effect_name
  }
  out <- list(
    effect = list(
      param= param,
      scaling = scaling,
      mem_start=start,
      mem_end=end,
      stat_name = stat_name,
      cov=NULL
    )
  )
  
  names(out) <- effect_name
  out
}

prepInteractVar <- function(param=NULL,effects,scaling){
  
  scaling <- match(scaling,c("full","std","prop"))
  
  out <- list(
    interact=list(
      param = param,
      interact.vec = effects,
      stat_name = "interact",
      scaling = 3,
      mem_start=0,
      mem_end=0,
      cov = NULL
    )
  )
  names(out) <- "interact"
  out
}