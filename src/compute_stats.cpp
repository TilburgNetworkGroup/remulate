#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace std;
// standardize
// 
// Can be used to standardize a statistic row 
// 
//  stat: vector with length == number of dyads
// 
//[[Rcpp::export]]
arma::vec standardize(arma::vec statrow) {
    if(stddev(statrow) > 0) {
        statrow = (statrow-mean(statrow))/stddev(statrow);
    }
    return statrow;
}


//Updates a effect row at each time point
// [[Rcpp::export]]
arma::mat compute_stats(const arma::vec& int_effects,int P,const arma::mat& rs,const arma::vec& actors,const arma::mat& edgelist, const arma::mat& adj_mat , Rcpp::List covariates, arma::vec scaling, arma::mat statprevmat){

    arma::mat statmat(rs.n_rows , P);
    
    // used for skipping double computation of some statistics
    arma::uvec skip_flag(40,arma::fill::zeros);

    // vector to save the index of covariate effects 1 - 8 for send receive same difference average min max equate
    //arma::vec cov_index(8);
    //cov_index.fill(2);
    //arma::uvec indx = {0, 1, 2};

    // loop over all effects
    int i = 0;
    
    while (i < P){
        arma::vec statsrow(rs.n_rows,arma::fill::zeros); //placeholder for values of ith effect for all dyads
        int effect = int_effects(i);
        switch(effect){
            //baseline
            case 1:{
                    statsrow.fill(1);
                    break;
                }
            //send
            // case 2:{  
            //     statsrow = compute_actorEffect(covariates(i), 1, edgelist, rs,statprevmat.col(i)); 
            //     break;
            // }
            // //receive
            // case 3:{
            //     
            //     statsrow = compute_actorEffect(covariates(i), 2, edgelist, rs,statprevmat.col(i));
            //     
            //     break;}
            // //same
            // case 4:{
            //     
            //     statsrow = compute_dyadEffect(covariates(i), 1, edgelist.tail_rows(1), rs, statprevmat.col(i));
            //     
            //     break;}
            // //difference
            // case 5:{
            //     
            //     statsrow = compute_dyadEffect(covariates(i) 2, edgelist.tail_rows(1), rs, statprevmat.col(i));
            //     
            //     break;}
            // //average
            // case 6:{
            //     
            //     statsrow = compute_dyadEffect(covariates(i),3, edgelist.tail_rows(1), rs, statprevmat.col(i));
            //     
            //     break;
            // }
            // //minimum
            // case 7:{
            //   
            //     statsrow = compute_dyadEffect(covariates(i), 4, edgelist.tail_rows(1), rs, statprevmat.col(i));
            //     
            //     break;
            // }
            // //maximum
            // case 8:{
            //     
            //     statsrow = compute_dyadEffect(covariates(i), 5, edgelist.tail_rows(1), rs, statprevmat.col(i));
            //     
            //     break;
            // }
            // //equate
            // case 9: {
            //     break;
            // }

            
            //inertia
            case 10:{
                for(arma::uword j=0; j<rs.n_rows;j++){
                    // -1 because r indexes from 1 and c++ from 0
                   statsrow(j) = adj_mat(rs(j,0)-1,rs(j,1)-1);
                }
                break;
            }
            //reciprocity
            case 11:{
                for(arma::uword j=0; j<rs.n_rows;j++){
                   statsrow(j) = adj_mat(rs(j,1)-1,rs(j,0)-1);
                }
                break;
            }
            //indegree Sender
            case 12:{
                //computing sum only once for each actor
                arma::rowvec in_degrees = arma::sum(adj_mat,0);
                for(arma::uword j=0; j<rs.n_rows;j++){
                   statsrow(j) = in_degrees(rs(j,0)-1);
                }
                //don't break here 
                // check if indegree receiver stat is also present so that in_degrees vec can be used without computing again
                arma::uvec other_index = find(int_effects ==13);
                
                
                if(other_index.n_elem!=0){
                    arma::vec statsrow2(rs.n_rows,arma::fill::zeros);
                    for(arma::uword j=0; j<rs.n_rows;j++){
                        statsrow2(j) = in_degrees(rs(j,1)-1);
                    }
                    statmat.col(other_index(0)) = statsrow2;
                    skip_flag(13) = 1; // will ensure skip ahead to the next stat
                }
                break;
            }
            //in degree receiver
            case 13:{
                if(skip_flag(13)==0){
                    //will only reach here if indegree sender not in effects list
                    arma::rowvec in_degrees = arma::sum(adj_mat,0);
                    for(arma::uword j=0; j<rs.n_rows;j++){
                        statsrow(j) = in_degrees(rs(j,1)-1);
                    }
                }
                break;
            }
            //out degree sender
            case 14:{
                arma::vec out_degrees = arma::sum(adj_mat,1);
                for(arma::uword j=0; j<rs.n_rows;j++){
                    statsrow(j) = out_degrees(rs(j,0)-1);
                }
                arma::uvec other_index = find(int_effects ==15);
                if(other_index.n_elem!=0){
                    arma::vec statsrow2(rs.n_rows,arma::fill::zeros);
                    for(arma::uword j=0; j<rs.n_rows;j++){
                        statsrow2(j) = out_degrees(rs(j,1)-1);
                    }
                    statmat.col(other_index(0)) = statsrow2;
                    skip_flag(15) = 1;
                }
                break;
            }
                //out degree receiver
            case 15:{
                if(skip_flag(15)==0){
                    arma::vec out_degrees = arma::sum(adj_mat,1);
                    for(arma::uword j=0; j<rs.n_rows;j++){
                        statsrow(j) = out_degrees(rs(j,1)-1);
                    }
                }
                break;
            }
            //total degree sender
            case 16:{
                //TODO: avoid double computation if in/out degrees aready computed
                arma::vec out_degrees = arma::sum(adj_mat,1);
                arma::rowvec in_degrees = arma::sum(adj_mat,0);
                for(arma::uword j=0; j<rs.n_rows;j++){
                   statsrow(j) = in_degrees(rs(j,0)-1)+ out_degrees(rs(j,0)-1);
                }
                //total degree recv
                arma::uvec other_index = find(int_effects ==17);
                if(other_index.n_elem!=0){
                     arma::vec statsrow2(rs.n_rows,arma::fill::zeros);
                     for(arma::uword j=0; j<rs.n_rows;j++){
                        statsrow2(j) = in_degrees(rs(j,1)-1)+ out_degrees(rs(j,1)-1);
                    }
                    statmat.col(other_index(0)) = statsrow2;
                    skip_flag(17) = 1;
                }
                break;
            }
            //total degree receiver
            case 17:{
                if(skip_flag(17)==0){
                    arma::vec out_degrees = arma::sum(adj_mat,1);
                    arma::rowvec in_degrees = arma::sum(adj_mat,0);
                    for(arma::uword j=0; j<rs.n_rows;j++){
                    statsrow(j) = in_degrees(rs(j,1)-1)+ out_degrees(rs(j,1)-1);
                    }
                }
                break;
            }
            //otp
            case 18:{
                //i ->h , h ->j
                for(arma::uword j=0; j<rs.n_rows;j++){
                    for(arma::uword h = 0; h < actors.n_elem; ++h) {
                        statsrow(j) += std::min(adj_mat(rs(j,0)-1,h) , adj_mat(h , rs(j,1)-1));
                    }
                }
                break;
            }
            //itp
            case 19:{
                //j -> h , h->i
                for(arma::uword j=0; j<rs.n_rows;j++){
                    for(arma::uword h = 0; h < actors.n_elem; ++h) {
                        statsrow(j) += std::min(adj_mat(rs(j,1)-1,h) , adj_mat(h , rs(j,0)-1));
                    }
                }
                break;
            }
            //osp
            case 20:{
                // i ->h , j ->h
                for(arma::uword j=0; j<rs.n_rows;j++){
                    for(arma::uword h = 0; h < actors.n_elem; ++h) {
                        statsrow(j) += std::min(adj_mat(rs(j,0)-1,h) , adj_mat(rs(j,1)-1 , h));
                    }
                }
                break;
            }
            //isp
            case 21:{
                // h ->i , h ->j
                for(arma::uword j=0; j<rs.n_rows;j++){
                    for(arma::uword h = 0; h < actors.n_elem; ++h) {
                        statsrow(j) += std::min(adj_mat(h,rs(j,0)-1) , adj_mat(h,rs(j,1)-1));
                    }
                }
                break;
            }
            //PS AB-BA
            case 22:{
                if(edgelist.n_rows >0){
                    arma::uword sender = edgelist(edgelist.n_rows-1,1);
                    arma::uword receiver = edgelist(edgelist.n_rows-1,2); 
                    arma::uvec psdyads = find(rs.col(0)==receiver && rs.col(1)==sender);
                    statsrow(psdyads(0)) = 1;
                }
                break;
            }
            //PS AB-BY
            case 23:{
                if(edgelist.n_rows >0){
                    arma::uword sender = edgelist(edgelist.n_rows-1,1);
                    arma::uword receiver = edgelist(edgelist.n_rows-1,2); 
                    arma::uvec psdyads = find(rs.col(0)==receiver && rs.col(1)!=sender && rs.col(1) != receiver);

                    for(arma::uword d = 0; d < psdyads.n_elem; d++) {
                        statsrow(psdyads(d)) = 1;
                    }
                }
                break;
            }
            //PS AB-XA
            case 24:{
                if(edgelist.n_rows >0){
                    arma::uword sender = edgelist(edgelist.n_rows-1,1);
                    arma::uword receiver = edgelist(edgelist.n_rows-1,2); 
                    arma::uvec psdyads = find(rs.col(1)==sender && rs.col(0)!=sender && rs.col(0) != receiver);

                    for(arma::uword d = 0; d < psdyads.n_elem; d++) {
                        statsrow(psdyads(d)) = 1;
                    }
                }
                break;
            }
            //PS AB-XB
            case 25:{
                if(edgelist.n_rows >0){
                    arma::uword sender = edgelist(edgelist.n_rows-1,1);
                    arma::uword receiver = edgelist(edgelist.n_rows-1,2); 
                    arma::uvec psdyads = find(rs.col(1)==receiver && rs.col(0)!=sender && rs.col(0) != receiver);

                    for(arma::uword d = 0; d < psdyads.n_elem; d++) {
                        statsrow(psdyads(d)) = 1;
                    }
                }
                break;
            }
            //PS AB-XY
            case 26:{
                if(edgelist.n_rows >0){
                    arma::uword sender = edgelist(edgelist.n_rows-1,1);
                    arma::uword receiver = edgelist(edgelist.n_rows-1,2); 
                    arma::uvec psdyads = find(rs.col(0)!=sender && rs.col(0) != receiver&& rs.col(1)!=sender && rs.col(1) != receiver);

                    for(arma::uword d = 0; d < psdyads.n_elem; d++) {
                        statsrow(psdyads(d)) = 1;
                    }
                }
                break;
            }
            //PS AB-AY
            case 27:{
                if(edgelist.n_rows >0){
                    arma::uword sender = edgelist(edgelist.n_rows-1,1);
                    arma::uword receiver = edgelist(edgelist.n_rows-1,2); 
                    arma::uvec psdyads = find(rs.col(0)==sender && rs.col(1)!=sender && rs.col(1) != receiver);

                    for(arma::uword d = 0; d < psdyads.n_elem; d++){
                        statsrow(psdyads(d)) = 1;
                    }
                }
                break;
            }
        //end switch case    
        }      
        if(skip_flag(effect)==0){
            statmat.col(i) = statsrow;
        }
        i++;
    }
    //scaling after 
    for(int i = 0;i<P;i++){
        int effect = int_effects(i);
        switch(effect){
            //inertia
            case 10:{
                //cout << "in inertia scaling is:"<<scaling(i)<<endl;
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//outdegreeSender 
                {
                    arma::vec deno(rs.n_rows,arma::fill::zeros);
                    arma::vec out_degrees = arma::sum(adj_mat,1);
                    for(arma::uword j=0; j<rs.n_rows;j++){
                        deno(j) = out_degrees(rs(j,0)-1);  
                    }
                    statmat.col(i) = statmat.col(i)/deno;
                    statmat.col(i).replace(arma::datum::nan,0);
                }
                break;
            }
            //reciprocity
            case 11:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//indegreeSender 
                {
                    arma::vec deno(rs.n_rows,arma::fill::zeros);
                    arma::rowvec in_degrees = arma::sum(adj_mat,0);
                    for(arma::uword j=0; j<rs.n_rows;j++){
                        deno(j) = in_degrees(rs(j,0)-1);
                    }
                    statmat.col(i) = statmat.col(i)/deno;
                    statmat.col(i).replace(arma::datum::nan,0);
                }
                break;
            }
            case 12:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//past events
                {
                    statmat.col(i) = statmat.col(i)/edgelist.n_rows;
                }
                break;
            }
            case 13:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//past events
                {
                    statmat.col(i) = statmat.col(i)/edgelist.n_rows;
                }
                break;
            }
            case 14:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//past events
                {
                    statmat.col(i) = statmat.col(i)/edgelist.n_rows;
                }
                break;
            }
            case 15:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//past events
                {
                    statmat.col(i) = statmat.col(i)/edgelist.n_rows;
                }
                break;
            }
            case 16: {
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//past events
                {
                    statmat.col(i) = statmat.col(i)/(2*edgelist.n_rows);
                }
                break;
            }
            case 17:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                else if (scaling(i)==3)//past events
                {
                    statmat.col(i) = statmat.col(i)/(2*edgelist.n_rows);
                }
                break;
            }
            case 18:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                break;
            }
            case 19:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                break;
            }
            case 20:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                break;
            }
            case 21:{
                if(scaling(i)==2){
                    statmat.col(i) = standardize(statmat.col(i));
                }
                break;
            }
        }          
    }
    
    return(statmat);
}

