#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace std;

// standardize
//
// Can be used to standardize a statistic row
//
//  stat: vector with length == number of dyads
//
arma::vec standardize(arma::vec statrow)
{
    if (stddev(statrow) > 0)
    {
        statrow = (statrow - mean(statrow)) / stddev(statrow);
    }
    return statrow;
}

//modified from remstats
arma::vec compute_actorAttribute(
    const arma::mat &values,
    int type,
    const arma::mat &edgelist,
    const arma::mat &riskset,
    const arma::vec &statsprevrow)
{

    arma::vec statsrow(riskset.n_rows, arma::fill::zeros);

    arma::uword m = edgelist.n_rows;
    //time of first event
    double time0 = edgelist(0, 0);

    if (m == 1)
    {
        //loop over dyads in rs
        for (arma::uword i = 0; i < riskset.n_rows; i++)
        {
            arma::uword actor = 0;
            if (type == 1)
            {
                actor = riskset(i, 0);
            } // Sender
            if (type == 2)
            {
                actor = riskset(i, 1);
            } // Receiver
            // Find the first value for this actor before time point for first event
            arma::uvec index = arma::find(values.col(0) == actor &&
                                          values.col(1) <= time0);
            arma::mat actor_values = values.rows(index);
            arma::uword max_index = arma::index_max(actor_values.col(1)); //index with maximum time
            statsrow(i) = actor_values(max_index, 2);
        }
        return (statsrow);
    }

    //copy previous row in case no change in covariates
    statsrow = statsprevrow;
    //current time
    double time = edgelist(m - 1, 0);
    // Find the unique change timepoints
    arma::vec changetimes = sort(unique(values.col(1)));
    changetimes = changetimes(find(changetimes != 0));
    arma::uword counter = 0;
    // Update the statistic if required
    // Do not update after the last changetime
    if (counter < changetimes.n_elem)
    {
        // Update if the current time of the event is larger than the current
        // changetime
        if (time > changetimes(counter))
        {
            // Update all changes in between
            while ((counter < changetimes.n_elem) &&
                   (time > changetimes(counter)))
            {
                // For loop over dyads
                arma::uword actor = 0;
                arma::uvec index;
                for (arma::uword i = 0; i < riskset.n_rows; i++)
                {
                    // Find the relevant actor
                    if (type == 1)
                    {
                        actor = riskset(i, 0);
                    } // Sender
                    if (type == 2)
                    {
                        actor = riskset(i, 1);
                    } // Receiver
                    // Find the value for this actor
                    index = find((values.col(0) == actor) && (values.col(1) == changetimes(counter)));
                    // Update if a new value exists
                    if (index.n_elem == 1)
                    {
                        double value = values(index(0), 2);
                        statsrow(i) = value;
                    }
                }
                //Update the counter
                counter += 1;
            }
        }
    }
    return (statsrow);
}

//modified from remstats
arma::mat compute_dyadAttribute(
    const arma::mat &values,
    int type,
    const arma::mat &edgelist,
    const arma::mat &riskset,
    const arma::vec &statsprevrow)
{

    arma::vec statsrow(riskset.n_rows, arma::fill::zeros);

    // Storage space for the current covariate values
    arma::vec current_ac1(riskset.n_rows, arma::fill::zeros);
    arma::vec current_ac2(riskset.n_rows, arma::fill::zeros);

    // First time point
    double time0 = edgelist(0, 0);
    arma::uword m = edgelist.n_rows;
    if (m == 1)
    {
        // For loop over dyads
        for (arma::uword i = 0; i < riskset.n_rows; i++)
        {
            // Find the relevant actors
            arma::uword actor1 = riskset(i, 0);
            arma::uword actor2 = riskset(i, 1);

            // Find the values for actor1
            arma::uvec index1 = find(values.col(0) == actor1 &&
                                     values.col(1) <= time0);
            arma::mat actor1_values = values.rows(index1);
            arma::uword max_index1 = index_max(actor1_values.col(1));

            current_ac1(i) = actor1_values(max_index1, 2);

            // Find the values for actor2
            arma::uvec index2 = find(values.col(0) == actor2 &&
                                     values.col(1) <= time0);
            arma::mat actor2_values = values.rows(index2);
            arma::uword max_index2 = index_max(actor2_values.col(1));
            current_ac2(i) = actor2_values(max_index2, 2);

            // Are these values equal?
            if (type == 1)
            {
                statsrow(i) = (current_ac1(i) == current_ac2(i));
            }
            // What is the difference between these values?
            if (type == 2)
            {
                statsrow(i) = abs(current_ac1(i) - current_ac2(i));
            }

            arma::vec both = {current_ac1(i), current_ac2(i)};
            //What is the mean value?
            if (type == 3)
            {
                statsrow(i) = arma::mean(both);
            }
            // What is the minimum value?
            if (type == 4)
            {
                statsrow(i) = arma::min(both);
            }
            // What is the maximum value?
            if (type == 5)
            {
                statsrow(i) = arma::max(both);
            }
            // Are both equal to this value?
            //if(type == 6) {statsrow(i)  = ((current_ac1(i) == equal_val) &&
            //    (current_ac2(i) == equal_val));}
        }
        return (statsrow);
    }
    statsrow = statsprevrow;
    double time = m;

    // Find the unique change timepoints
    arma::vec changetimes = sort(unique(values.col(1)));
    changetimes = changetimes(find(changetimes != 0));
    arma::uword counter = 0;

    // Update the statistic if required
    // Do not update after the last changetime
    if (counter < changetimes.n_elem)
    {
        // Update if the time of the event is larger than the current
        // changetime
        if (time > changetimes(counter))
        {
            // Update all changes in between
            while ((counter < changetimes.n_elem) && (time > changetimes(counter)))
            {

                // For loop over dyads
                for (arma::uword i = 0; i < riskset.n_rows; ++i)
                {
                    // Find the relevant actor
                    arma::uword actor1 = riskset(i, 0);
                    arma::uword actor2 = riskset(i, 1);

                    // Find the values for these actor
                    arma::uvec index1 = find((values.col(0) == actor1) && (values.col(1) == changetimes(counter)));
                    arma::uvec index2 = find((values.col(0) == actor2) && (values.col(1) == changetimes(counter)));

                    // Update if a new value exists
                    if ((index1.n_elem == 1) || (index2.n_elem == 1))
                    {
                        if (index1.n_elem == 1)
                        {
                            current_ac1(i) = values(index1(0), 2);
                        }
                        if (index2.n_elem == 1)
                        {
                            current_ac2(i) = values(index2(0), 2);
                        }

                        // Are these values equal?
                        if (type == 1)
                        {
                            statsrow(i) =
                                (current_ac1(i) == current_ac2(i));
                        }
                        // What is the difference between
                        // these values?
                        if (type == 2)
                        {
                            statsrow(i) = abs(current_ac1(i) - current_ac2(i));
                        }

                        arma::dvec both = {current_ac1(i),
                                           current_ac2(i)};
                        //What is the mean value?
                        if (type == 3)
                        {
                            statsrow(i) = arma::mean(both);
                        }
                        // What is the minimum value?
                        if (type == 4)
                        {
                            statsrow(i) = arma::min(both);
                        }
                        // What is the maximum value?
                        if (type == 5)
                        {
                            statsrow(i) = arma::max(both);
                        }
                        // Are both equal to this value?
                        //if(type == 6) {statsrow(i) =
                        //    ((current_ac1(i) == equal_val) &&
                        //        (current_ac2(i) == equal_val));}
                    }
                }
                //Update the counter
                counter += 1;
            }
        }
    }
    return (statsrow);
}

//Updates a statistic row at each time point
// [[Rcpp::export]]
arma::mat computeStatsTie(const arma::vec &int_effects,
                          const arma::mat &rs,
                          const arma::vec &actors,
                          const arma::mat &edgelist,
                          const arma::mat &adj_mat,
                          Rcpp::List covariates,
                          Rcpp::List interact_effects,
                          arma::vec scaling,
                          arma::vec mem_start,
                          arma::vec mem_end,
                          arma::mat statprevmat){
    int P = int_effects.n_elem;
    arma::mat statmat(rs.n_rows, P);

    // used for skipping double computation of some statistics
    arma::uvec skip_flag(40, arma::fill::zeros);

    // loop over all effects
    int i = 0;
    while (i < P)
    {
        arma::vec statsrow(rs.n_rows, arma::fill::zeros); //placeholder for values of ith effect for all dyads
        int effect = int_effects(i);
        switch (effect){
        //baseline
        case 1:
        {
            statsrow.fill(1);
            break;
        }
            //send
        case 2:
        {
            statsrow = compute_actorAttribute(covariates(i), 1, edgelist, rs, statprevmat.col(i));
            break;
        }
            //receive
        case 3:
        {
            statsrow = compute_actorAttribute(covariates(i), 2, edgelist, rs, statprevmat.col(i));
            break;
        }
            //same
        case 4:
        {

            statsrow = compute_dyadAttribute(covariates(i), 1, edgelist.tail_rows(1), rs, statprevmat.col(i));

            break;
        }
            //difference
        case 5:
        {
            statsrow = compute_dyadAttribute(covariates(i), 2, edgelist.tail_rows(1), rs, statprevmat.col(i));

            break;
        }
            //average
        case 6:
        {
            statsrow = compute_dyadAttribute(covariates(i), 3, edgelist.tail_rows(1), rs, statprevmat.col(i));

            break;
        }
            //minimum
        case 7:
        {

            statsrow = compute_dyadAttribute(covariates(i), 4, edgelist.tail_rows(1), rs, statprevmat.col(i));

            break;
        }
            //maximum
        case 8:
        {

            statsrow = compute_dyadAttribute(covariates(i), 5, edgelist.tail_rows(1), rs, statprevmat.col(i));

            break;
        }
        //tie
        case 9:
        {
            for (arma::uword j = 0; j < rs.n_rows; j++)
            {
                if (adj_mat(rs(j, 0) - 1, rs(j, 1) - 1) > 0)
                {
                    statsrow(j) = 1;
                }
            }
        }
            //inertia
        case 10:
        {
            if (mem_start(i) != 0 || mem_end(i) != 0)
            {
                arma::mat adj_mat_mem(actors.n_elem, actors.n_elem, arma::fill::zeros);
                //which time points in edgelist belong to mem window
                arma::uvec in_window = find(edgelist.col(0) <= edgelist(edgelist.n_rows - 1, 0) - std::max(mem_start(i), mem_end(i)) && edgelist.col(0) <= edgelist(edgelist.n_rows - 1, 0) - std::min(mem_start(i), mem_end(i)));
                if (in_window.n_elem != 0)
                {
                    for (arma::uword ind = 0; ind < in_window.n_elem; ind++)
                    {
                        adj_mat_mem(edgelist(ind, 1) - 1, edgelist(ind, 2) - 1) = adj_mat_mem(edgelist(ind, 1) - 1, edgelist(ind, 2) - 1) + 1;
                    }
                    for (arma::uword j = 0; j < rs.n_rows; j++)
                    {
                        // -1 because r indexes from 1 and c++ from 0
                        statsrow(j) = adj_mat_mem(rs(j, 0) - 1, rs(j, 1) - 1);
                    }
                }
                break;
            }
            for (arma::uword j = 0; j < rs.n_rows; j++)
            {
                // -1 because r indexes from 1 and c++ from 0
                statsrow(j) = adj_mat(rs(j, 0) - 1, rs(j, 1) - 1);
            }
            break;
        }
            //reciprocity
        case 11:
        {
            if (mem_start(i) != 0 || mem_end(i) != 0)
            {
                arma::mat adj_mat_mem(actors.n_elem, actors.n_elem, arma::fill::zeros);
                //which time points in edgelist belong to mem window
                arma::uvec in_window = find(edgelist.col(0) <= edgelist(edgelist.n_rows - 1, 0) - std::max(mem_start(i), mem_end(i)) && edgelist.col(0) <= edgelist(edgelist.n_rows - 1, 0) - std::min(mem_start(i), mem_end(i)));
                if (in_window.n_elem != 0)
                {
                    for (arma::uword ind = 0; ind < in_window.n_elem; ind++)
                    {
                        adj_mat_mem(edgelist(ind, 1) - 1, edgelist(ind, 2) - 1) = adj_mat_mem(edgelist(ind, 1) - 1, edgelist(ind, 2) - 1) + 1;
                    }
                    for (arma::uword j = 0; j < rs.n_rows; j++)
                    {
                        // -1 because r indexes from 1 and c++ from 0
                        statsrow(j) = adj_mat_mem(rs(j, 1) - 1, rs(j, 0) - 1);
                    }
                }
                break;
            }
            for (arma::uword j = 0; j < rs.n_rows; j++)
            {
                statsrow(j) = adj_mat(rs(j, 1) - 1, rs(j, 0) - 1);
            }
            break;
        }
            //indegree Sender
        case 12:
        {
            //computing sum only once for each actor
            arma::rowvec in_degrees = arma::sum(adj_mat, 0);
            for (arma::uword j = 0; j < rs.n_rows; j++)
            {
                statsrow(j) = in_degrees(rs(j, 0) - 1);
            }
            //don't break here
            // check if indegree receiver stat is also present so that in_degrees vec can be used without computing again
            arma::uvec other_index = find(int_effects == 13);

            if (other_index.n_elem != 0)
            {
                arma::vec statsrow2(rs.n_rows, arma::fill::zeros);
                for (arma::uword j = 0; j < rs.n_rows; j++)
                {
                    statsrow2(j) = in_degrees(rs(j, 1) - 1);
                }
                statmat.col(other_index(0)) = statsrow2;
                skip_flag(13) = 1; // will ensure skip ahead to the next stat
            }
            break;
        }
            //in degree receiver
        case 13:
        {
            if (skip_flag(13) == 0)
            {
                //will only reach here if indegree sender not in effects list
                arma::rowvec in_degrees = arma::sum(adj_mat, 0);
                for (arma::uword j = 0; j < rs.n_rows; j++)
                {
                    statsrow(j) = in_degrees(rs(j, 1) - 1);
                }
            }
            break;
        }
            //out degree sender
        case 14:
        {
            arma::vec out_degrees = arma::sum(adj_mat, 1);
            for (arma::uword j = 0; j < rs.n_rows; j++)
            {
                statsrow(j) = out_degrees(rs(j, 0) - 1);
            }
            arma::uvec other_index = find(int_effects == 15);
            if (other_index.n_elem != 0)
            {
                arma::vec statsrow2(rs.n_rows, arma::fill::zeros);
                for (arma::uword j = 0; j < rs.n_rows; j++)
                {
                    statsrow2(j) = out_degrees(rs(j, 1) - 1);
                }
                statmat.col(other_index(0)) = statsrow2;
                skip_flag(15) = 1;
            }
            break;
        }
            //out degree receiver
        case 15:
        {
            if (skip_flag(15) == 0)
            {
                arma::vec out_degrees = arma::sum(adj_mat, 1);
                for (arma::uword j = 0; j < rs.n_rows; j++)
                {
                    statsrow(j) = out_degrees(rs(j, 1) - 1);
                }
            }
            break;
        }
            //total degree sender
        case 16:
        {
            //TODO: avoid double computation if in/out degrees aready computed
            arma::vec out_degrees = arma::sum(adj_mat, 1);
            arma::rowvec in_degrees = arma::sum(adj_mat, 0);
            for (arma::uword j = 0; j < rs.n_rows; j++)
            {
                statsrow(j) = in_degrees(rs(j, 0) - 1) + out_degrees(rs(j, 0) - 1);
            }
            //total degree recv
            arma::uvec other_index = find(int_effects == 17);
            if (other_index.n_elem != 0)
            {
                arma::vec statsrow2(rs.n_rows, arma::fill::zeros);
                for (arma::uword j = 0; j < rs.n_rows; j++)
                {
                    statsrow2(j) = in_degrees(rs(j, 1) - 1) + out_degrees(rs(j, 1) - 1);
                }
                statmat.col(other_index(0)) = statsrow2;
                skip_flag(17) = 1;
            }
            break;
        }
            //total degree receiver
        case 17:
        {
            if (skip_flag(17) == 0)
            {
                arma::vec out_degrees = arma::sum(adj_mat, 1);
                arma::rowvec in_degrees = arma::sum(adj_mat, 0);
                for (arma::uword j = 0; j < rs.n_rows; j++)
                {
                    statsrow(j) = in_degrees(rs(j, 1) - 1) + out_degrees(rs(j, 1) - 1);
                }
            }
            break;
        }
            //otp
        case 18:
        {
            //i ->h , h ->j
            for (arma::uword j = 0; j < rs.n_rows; j++)
            {
                for (arma::uword h = 0; h < actors.n_elem; ++h)
                {
                    statsrow(j) += std::min(adj_mat(rs(j, 0) - 1, h), adj_mat(h, rs(j, 1) - 1));
                }
            }
            break;
        }
            //itp
        case 19:
        {
            //j -> h , h->i
            for (arma::uword j = 0; j < rs.n_rows; j++)
            {
                for (arma::uword h = 0; h < actors.n_elem; ++h)
                {
                    statsrow(j) += std::min(adj_mat(rs(j, 1) - 1, h), adj_mat(h, rs(j, 0) - 1));
                }
            }
            break;
        }
            //osp
        case 20:
        {
            // i ->h , j ->h
            for (arma::uword j = 0; j < rs.n_rows; j++)
            {
                for (arma::uword h = 0; h < actors.n_elem; ++h)
                {
                    statsrow(j) += std::min(adj_mat(rs(j, 0) - 1, h), adj_mat(rs(j, 1) - 1, h));
                }
            }
            break;
        }
            //isp
        case 21:
        {
            // h ->i , h ->j
            for (arma::uword j = 0; j < rs.n_rows; j++)
            {
                for (arma::uword h = 0; h < actors.n_elem; ++h)
                {
                    statsrow(j) += std::min(adj_mat(h, rs(j, 0) - 1), adj_mat(h, rs(j, 1) - 1));
                }
            }
            break;
        }
            //PS AB-BA
        case 22:
        {
            arma::uword sender = edgelist(edgelist.n_rows - 1, 1);
            arma::uword receiver = edgelist(edgelist.n_rows - 1, 2);
            arma::uvec psdyads = find(rs.col(0) == receiver && rs.col(1) == sender);
            statsrow(psdyads(0)) = 1;
            break;
        }
            //PS AB-BY
        case 23:
        {
            arma::uword sender = edgelist(edgelist.n_rows - 1, 1);
            arma::uword receiver = edgelist(edgelist.n_rows - 1, 2);
            arma::uvec psdyads = find(rs.col(0) == receiver && rs.col(1) != sender && rs.col(1) != receiver);
            for (arma::uword d = 0; d < psdyads.n_elem; d++)
            {
                statsrow(psdyads(d)) = 1;
            }
            break;
        }
            //PS AB-XA
        case 24:
        {
            arma::uword sender = edgelist(edgelist.n_rows - 1, 1);
            arma::uword receiver = edgelist(edgelist.n_rows - 1, 2);
            arma::uvec psdyads = find(rs.col(1) == sender && rs.col(0) != sender && rs.col(0) != receiver);
            for (arma::uword d = 0; d < psdyads.n_elem; d++)
            {
                statsrow(psdyads(d)) = 1;
            }
            break;
        }
            //PS AB-XB
        case 25:
        {
            arma::uword sender = edgelist(edgelist.n_rows - 1, 1);
            arma::uword receiver = edgelist(edgelist.n_rows - 1, 2);
            arma::uvec psdyads = find(rs.col(1) == receiver && rs.col(0) != sender && rs.col(0) != receiver);
            for (arma::uword d = 0; d < psdyads.n_elem; d++)
            {
                statsrow(psdyads(d)) = 1;
            }
            break;
        }
            //PS AB-XY
        case 26:
        {
            arma::uword sender = edgelist(edgelist.n_rows - 1, 1);
            arma::uword receiver = edgelist(edgelist.n_rows - 1, 2);
            arma::uvec psdyads = find(rs.col(0) != sender && rs.col(0) != receiver && rs.col(1) != sender && rs.col(1) != receiver);
            for (arma::uword d = 0; d < psdyads.n_elem; d++)
            {
                statsrow(psdyads(d)) = 1;
            }
            break;
        }
            //PS AB-AY
        case 27:
        {
            arma::uword sender = edgelist(edgelist.n_rows - 1, 1);
            arma::uword receiver = edgelist(edgelist.n_rows - 1, 2);
            arma::uvec psdyads = find(rs.col(0) == sender && rs.col(1) != sender && rs.col(1) != receiver);
            for (arma::uword d = 0; d < psdyads.n_elem; d++)
            {
                statsrow(psdyads(d)) = 1;
            }
            break;
        }
            //interact
        case 28:
        {
            arma::vec interact_vec = interact_effects(i);
            statsrow.ones();
            for (arma::uword j = 0; j < interact_vec.n_elem; j++)
            {
                statsrow = statsrow % statmat.col(interact_vec(j));
            }
            break;
        }

        } //end switch case
        if (skip_flag(effect) == 0)
        {
            statmat.col(i) = statsrow;
        }
        i++;
    } //end loop

    //scaling after
    for (arma::uword i = 0; i < P; i++){
        int effect = int_effects(i);
        if (scaling(i) == 1){
            continue;
        }
        if (scaling(i) == 2){
            statmat.col(i) = standardize(statmat.col(i));
            continue;
        }

        //prop scaling
        switch (effect)
        {
        //tie
        case 9:
        {
            if (scaling(i) == 2)
            {
                statmat.col(i) = standardize(statmat.col(i));
            }
        }
        //inertia
        case 10:
        {
            if (scaling(i) == 3) //outdegreeSender
            {
                arma::vec deno(rs.n_rows, arma::fill::zeros);
                arma::vec out_degrees = arma::sum(adj_mat, 1);
                for (arma::uword j = 0; j < rs.n_rows; j++)
                {
                    deno(j) = out_degrees(rs(j, 0) - 1);
                }
                statmat.col(i) = statmat.col(i) / deno;
                statmat.col(i).replace(arma::datum::nan, 0);
            }
            break;
        }
            //reciprocity
        case 11:
        {
            if (scaling(i) == 3) //indegreeSender
            {
                arma::vec deno(rs.n_rows, arma::fill::zeros);
                arma::rowvec in_degrees = arma::sum(adj_mat, 0);
                for (arma::uword j = 0; j < rs.n_rows; j++)
                {
                    deno(j) = in_degrees(rs(j, 0) - 1);
                }
                statmat.col(i) = statmat.col(i) / deno;
                statmat.col(i).replace(arma::datum::nan, 0);
            }
            break;
        }
        case 12:
        {
            if (scaling(i) == 3) //past events
            {
                statmat.col(i) = statmat.col(i) / edgelist.n_rows;
            }
            break;
        }
        case 13:
        {
            if (scaling(i) == 3) //past events
            {
                statmat.col(i) = statmat.col(i) / edgelist.n_rows;
            }
            break;
        }
        case 14:
        {
            if (scaling(i) == 3) //past events
            {
                statmat.col(i) = statmat.col(i) / edgelist.n_rows;
            }
            break;
        }
        case 15:
        {
            if (scaling(i) == 3) //past events
            {
                statmat.col(i) = statmat.col(i) / edgelist.n_rows;
            }
            break;
        }
        case 16:
        {
            if (scaling(i) == 3) //past events
            {
                statmat.col(i) = statmat.col(i) / (2 * edgelist.n_rows);
            }
            break;
        }
        case 17:
        {
            if (scaling(i) == 3) //past events
            {
                statmat.col(i) = statmat.col(i) / (2 * edgelist.n_rows);
            }
            break;
        }
        
    }
    }
    return (statmat);
}
