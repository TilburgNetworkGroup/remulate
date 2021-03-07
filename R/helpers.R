#' Function to generate random events for the burn in period
burn_in_adj_mat<- function(actors,burn_in,rs){
    if(burn_in==0){
        return(array(0,dim=c(length(actors),length(actors))))
    }
    #sample random events
    dyads <- sample(1:nrow(rs),burn_in,replace=T)
    #make a new adj_mat for null events
    adj_mat <- array(0,dim=c(length(actors),length(actors)))
    for(ind in 1:burn_in){
        adj_mat[    rs[dyads[ind],1]  ,rs[dyads[ind],2] ] =  adj_mat[    rs[dyads[ind],1]  ,rs[dyads[ind],2] ] + 1;
    }
    cat("Burn in of ",burn_in," events was used")
    return(adj_mat)
}


#' get.density
#' Function to compute the global density of the network. Can be used to check for degeneracy of the simulated network
#' 
#' density = number of unique edges in the network / total number of possible edges between nodes
#' evls event list for the network i.e a matrix with columns (dyad,time)
get.density <- function(evls,actors){
    edges <- length(unique(evls[,1]))
    total_edges <- length(actors)*(length(actors)-1)
    return(edges/total_edges)
}

#Function to initialize covariates dataframe according to integer actor IDs in actors map
initialize_exo_effects <- function(covariates,actors_map,effects){
    P = length(covariates)
    if(P==0){
        return(list())
    }     
    for(i in 1:P){
        if(!is.null(covariates[[i]])){
            if(any(! actors_map$given %in% covariates[[i]]$id )){
                stop(paste(effects[[i]]," effect not specified for all actors in actor's list"))
            }
            covariates[[i]]$id <- actors_map$id[match(covariates[[i]]$id,actors_map$given)]
            covariates[[i]] <- as.matrix(covariates[[i]])
        }
    }
    return(covariates)
}

#' covariates- list of data frames for each each effect in formula, NULL if the effect is not exogenous and a data frame with columns (id,time,value) if exogenous
parse_formula_Dyad <- function(formula){
    # Get effects information
    ft <- stats::terms(formula)
    
    var <- attr(ft, "variables")
    var <- as.list(var)[-1]

    effects <- lapply(var, eval)
    effects <- unlist(effects, recursive = FALSE)
    
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
        "psABXB", "psABXY", "psABAY",  #25 #26 #27
        "interact" #28
    )

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
        s <- x$stat_name
        s
    })

    #Prepare the covariates
    covariates<- lapply(effects,function(x){
        c <- x$cov
        c
    })

    return(list("int_effects"=int_effects,"params"=params,"scaling"=scaling,"effects"=stat_names,"covariates"=covariates))
}


#' covariates- list of data frames for each each effect in formula, NULL if the effect is not exogenous and a data frame with columns (id,time,value) if exogenous
parse_formula_Sender <- function(formula,pred = FALSE){
    # Get effects information
    ft <- stats::terms(formula)
    
    var <- attr(ft, "variables")
    var <- as.list(var)[-1]

    effects <- lapply(var, eval)
    effects <- unlist(effects, recursive = FALSE)
    
    all_effects <- c(
        "baseline", #1
        "send", #2
        "inertia_s", "reciprocity_s", #3 #4
        "indegreeSender", #5
        "outdegreeSender", #6
        "totaldegreeSender",#7
        "otp_s", "itp_s", "osp_s", "isp_s", #8 #9 #10 #11
        "interact" #12
    )
    
    # print(effects)
    # if(any(!effects %in% all_effects)){
    #     stop(paste("An effect specified in sender_formula is not a valid sender effect"))
    # }

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
        s <- x$stat_name
        s
    })

    #Prepare the covariates
    covariates<- lapply(effects,function(x){
        c <- x$cov
        c
    })

    return(list("int_effects"=int_effects,"params"=params,"scaling"=scaling,"effects"=stat_names,"covariates"=covariates))
}


# Internal function, modified from remstats
prepExoVar <- function(effect_name, param, scaling, variable, covariates) {
    # Warning for missing values
    if(anyNA(covariates[,variable])) {
        warning("Missing values in covariates object.")
    }

    scaling <- match(scaling,c("raw","std","prop"))

    cov <- data.frame(
        id = covariates[,1], 
        time = covariates[,2], 
        val = covariates[,variable]
    )
    cov <- cov[order(cov$id,cov$time),]

    out <- list(
        effect = list(
            param= param,
            scaling = scaling,
            cov = cov,
            stat_name = paste0(effect_name,"_",variable)
        )
    )
    names(out) <- effect_name
    out
}

# Internal function, modified from remstats
prepEndoVar <- function(effect_name, param, scaling) {

    scaling <- match(scaling,c("raw","std","prop"))

    out <- list(
        effect = list(
            param= param,
            scaling = scaling,
            stat_name = effect_name
        )
    )

    names(out) <- effect_name
    out
}
