#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace std;

//computes send covariate for actor oriented model, modified from remstats
arma::vec compute_senderEffect(
    const arma::mat& values, 
    const arma::mat& edgelist, 
    const arma::vec& actors,
    const arma::vec& statsprevrow){
    
    arma::vec statsrow(actors.n_elem,arma::fill::zeros);
    
    arma::uword m = edgelist.n_rows;
    //time of first event
    double time0 = edgelist(0,0);
    
    if(m==1){
        //loop over all actors
        for(int i = 0; i<actors.n_elem ; i++){
            arma::uword actor = actors(i);

            // Find the first value for this actor before time point for first event
            arma::uvec index = arma::find(values.col(0) == actor && 
                values.col(1) <= time0);
            arma::mat actor_values = values.rows(index);
            arma::uword max_index = arma::index_max(actor_values.col(1));//index with maximum time
            statsrow(i) = actor_values(max_index,2);
        }
        return (statsrow);
    }
    
    //copy previous row in case no change in covariates
    statsrow = statsprevrow;
    //current time
    double time = edgelist(m-1,0);
    // Find the unique change timepoints
    arma::vec changetimes = sort(unique(values.col(1)));
    changetimes = changetimes(find(changetimes!=0));
    arma::uword counter = 0;
    // Update the statistic if required
    // Do not update after the last changetime
    if(counter < changetimes.n_elem) {
        // Update if the current time of the event is larger than the current 
        // changetime
        if(time > changetimes(counter)) {
            // Update all changes in between
            while((counter < changetimes.n_elem) && 
                (time > changetimes(counter))) {
                    // For loop over dyads
                    arma::uword actor = 0;
                    arma::uvec index;
                    for(arma::uword i = 0; i < actors.n_elem; i++) {
                        actor = actors(i);
                        // Find the value for this actor 
                        index = find((values.col(0) == actor) 
                            && (values.col(1) == changetimes(counter)));
                        // Update if a new value exists
                        if(index.n_elem == 1) {
                            double value = values(index(0), 2);
                            statsrow(i) = value;
                        }                 
                    }
                //Update the counter
                counter+=1;
            }  
        }
    }
    return (statsrow);
}


//Updates a statistic row at each time point
// [[Rcpp::export]]
arma::mat compute_stats_Actor(
    const arma::vec& int_effects,
    int P,
    const arma::mat& rs,
    const arma::vec& actors,
    const arma::mat& edgelist, 
    const arma::mat& adj_mat , 
    Rcpp::List covariates, 
    arma::vec scaling, 
    arma::mat statprevmat
    ){
    
    arma::mat statmat(actors.n_elem , P);
    // loop over all effects
    int i = 0;
    
    while (i < P){
        //TODO: replace statsrow with submatrix view 
        arma::vec statsrow(actors.n_elem,arma::fill::zeros);
        int effect = int_effects(i);
        switch(effect){
            //baseline
            case 1:{
                statsrow.fill(1);
                
                break;
            }
            //send
            case 2:{ 
                statsrow = compute_senderEffect(covariates(i), edgelist, actors,statprevmat.col(i)); 
                
                break;
            }
            //inertia_s
            case 3:{
                statsrow = arma::max(adj_mat,1);
                
                break;
            }
            //reciprocity_s
            case 4:{
                statsrow =  arma::max(adj_mat,0).t();
                
                break;
            }
            //indegreeSender
            case 5:{
                //computing sum only once for each actor
                //arma::rowvec in_degrees = arma::sum(adj_mat,0);
                //statsrow = in_degrees.as_col();
                statsrow =  arma::sum(adj_mat,0).t();
                
                break;
                }
            //outdegreeSender
            case 6:{
                //computing sum only once for each actor
                statsrow = arma::sum(adj_mat,1);
                
                break;
            }
            //totaldegreeSender
            case 7:{
                arma::vec out_degrees = arma::sum(adj_mat,1);
                arma::vec in_degrees = arma::sum(adj_mat,0).t();
                statsrow = in_degrees + out_degrees;
                
                break;
            }
            //otp_s
            case 8:{
                // max over k : j -> h h-> k
                for(arma::uword j=0; j<actors.n_elem;j++){//sender
                    double val = 0;
                    for(arma::uword k=0; k<actors.n_elem;k++){//receiver
                        val = 0;
                        for(arma::uword h = 0; h < actors.n_elem; ++h) {
                            val += std::min(adj_mat(j,h) , adj_mat(h , k));
                        }
                        statsrow(j) = std::max(statsrow(j),val);
                    }
                }
                
                break;
            }
            //itp_s
            case 9:{
                // max over k : k -> h h-> j
                for(arma::uword j=0; j<actors.n_elem;j++){//sender
                    double val = 0;
                    for(arma::uword k=0; k<actors.n_elem;k++){//receiver
                        val = 0;
                        for(arma::uword h = 0; h < actors.n_elem; ++h) {
                            val += std::min(adj_mat(k,h) , adj_mat(h , j));
                        }
                        statsrow(j) = std::max(statsrow(j),val);
                    }
                }
                
                break;
            }
                
            //osp_s
            case 10:{
                 // max over k : j -> h k-> h
                for(arma::uword j=0; j<actors.n_elem;j++){//sender
                    double val = 0;
                    for(arma::uword k=0; k<actors.n_elem;k++){//receiver
                        val = 0;
                        for(arma::uword h = 0; h < actors.n_elem; ++h) {
                            val += std::min(adj_mat(j,h) , adj_mat(k,h));
                        }
                        statsrow(j) = std::max(statsrow(j),val);
                    }
                }
                
                break;
            }
            //isp_s
            case 11:{
                 // max over k : h -> j h-> k
                for(arma::uword j=0; j<actors.n_elem;j++){//sender
                    double val = 0;
                    for(arma::uword k=0; k<actors.n_elem;k++){//receiver
                        val = 0;
                        for(arma::uword h = 0; h < actors.n_elem; ++h) {
                            val += std::min(adj_mat(h,j) , adj_mat(h , k));
                        }
                        statsrow(j) = std::max(statsrow(j),val);
                    }
                }
                
                break;
            }
            //interact
            case 12:{
                arma::vec stat1 = statmat.col(0);
                arma::vec stat2 = statmat.col(1);
                statsrow = stat1 % stat2;
                break;
            }

        //end switch case    
        }
        
        statmat.col(i) = statsrow;
        i++;
    }
    //scaling after
    for(int i = 0;i<P;i++){
        int effect = int_effects(i);
        switch(effect){
            //inertia_s
            case 3:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//outdegreeSender
                {
                    //Perhaps a more efficient (less redundant) solution exists
                    statmat.col(i) = statmat.col(i)/arma::sum(adj_mat,1);
                    statmat.col(i).replace(arma::datum::nan,0);
                }
                break;
            }
            //reciprocity_s
            case 4:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//indegreeSender
                {
                    //Perhaps a more efficient (less redundant) solution exists
                    statmat.col(i) = statmat.col(i)/arma::sum(adj_mat,0).t();
                    statmat.col(i).replace(arma::datum::nan,0);
                }
                break;
            }
            //indegreeSender
            case 5:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//past events
                {
                    statmat.col(i) = statmat.col(i)/edgelist.n_rows;
                }
                break;
            }
            //outdegreeSender
            case 6:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//past events
                {
                    statmat.col(i) = statmat.col(i)/edgelist.n_rows;
                }
                break;
            }
            //totaldegreeSender
            case 7:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//past events
                {
                    statmat.col(i) = statmat.col(i)/edgelist.n_rows;
                }
                break;
            }
            //otp_s
            case 8:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                break;
            }
            //itp_s
            case 9:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                break;
            }
            //osp_s
            case 10:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                break;
            }
            //isp_s
            case 11:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                break;
            }
        }
    }
    
    return(statmat);
}
