// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/remulate.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// computeStatsActor
arma::mat computeStatsActor(const arma::vec& int_effects, const arma::mat& rs, const arma::vec& actors, const arma::mat& edgelist, const arma::mat& adj_mat, Rcpp::List covariates, Rcpp::List interact_effects, arma::vec scaling, arma::mat statprevmat);
static SEXP _remulate_computeStatsActor_try(SEXP int_effectsSEXP, SEXP rsSEXP, SEXP actorsSEXP, SEXP edgelistSEXP, SEXP adj_matSEXP, SEXP covariatesSEXP, SEXP interact_effectsSEXP, SEXP scalingSEXP, SEXP statprevmatSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type int_effects(int_effectsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type rs(rsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type actors(actorsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type edgelist(edgelistSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type adj_mat(adj_matSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type covariates(covariatesSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type interact_effects(interact_effectsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type scaling(scalingSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type statprevmat(statprevmatSEXP);
    rcpp_result_gen = Rcpp::wrap(computeStatsActor(int_effects, rs, actors, edgelist, adj_mat, covariates, interact_effects, scaling, statprevmat));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _remulate_computeStatsActor(SEXP int_effectsSEXP, SEXP rsSEXP, SEXP actorsSEXP, SEXP edgelistSEXP, SEXP adj_matSEXP, SEXP covariatesSEXP, SEXP interact_effectsSEXP, SEXP scalingSEXP, SEXP statprevmatSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_remulate_computeStatsActor_try(int_effectsSEXP, rsSEXP, actorsSEXP, edgelistSEXP, adj_matSEXP, covariatesSEXP, interact_effectsSEXP, scalingSEXP, statprevmatSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// computeStatsTie
arma::mat computeStatsTie(const arma::vec& int_effects, const arma::mat& rs, const arma::vec& actors, const arma::mat& edgelist, const arma::mat& adj_mat, Rcpp::List covariates, Rcpp::List interact_effects, arma::vec scaling, arma::vec mem_start, arma::vec mem_end, arma::mat statprevmat);
static SEXP _remulate_computeStatsTie_try(SEXP int_effectsSEXP, SEXP rsSEXP, SEXP actorsSEXP, SEXP edgelistSEXP, SEXP adj_matSEXP, SEXP covariatesSEXP, SEXP interact_effectsSEXP, SEXP scalingSEXP, SEXP mem_startSEXP, SEXP mem_endSEXP, SEXP statprevmatSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type int_effects(int_effectsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type rs(rsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type actors(actorsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type edgelist(edgelistSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type adj_mat(adj_matSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type covariates(covariatesSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type interact_effects(interact_effectsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type scaling(scalingSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type mem_start(mem_startSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type mem_end(mem_endSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type statprevmat(statprevmatSEXP);
    rcpp_result_gen = Rcpp::wrap(computeStatsTie(int_effects, rs, actors, edgelist, adj_mat, covariates, interact_effects, scaling, mem_start, mem_end, statprevmat));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _remulate_computeStatsTie(SEXP int_effectsSEXP, SEXP rsSEXP, SEXP actorsSEXP, SEXP edgelistSEXP, SEXP adj_matSEXP, SEXP covariatesSEXP, SEXP interact_effectsSEXP, SEXP scalingSEXP, SEXP mem_startSEXP, SEXP mem_endSEXP, SEXP statprevmatSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_remulate_computeStatsTie_try(int_effectsSEXP, rsSEXP, actorsSEXP, edgelistSEXP, adj_matSEXP, covariatesSEXP, interact_effectsSEXP, scalingSEXP, mem_startSEXP, mem_endSEXP, statprevmatSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error("%s", CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _remulate_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("arma::mat(*computeStatsActor)(const arma::vec&,const arma::mat&,const arma::vec&,const arma::mat&,const arma::mat&,Rcpp::List,Rcpp::List,arma::vec,arma::mat)");
        signatures.insert("arma::mat(*computeStatsTie)(const arma::vec&,const arma::mat&,const arma::vec&,const arma::mat&,const arma::mat&,Rcpp::List,Rcpp::List,arma::vec,arma::vec,arma::vec,arma::mat)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _remulate_RcppExport_registerCCallable() { 
    R_RegisterCCallable("remulate", "_remulate_computeStatsActor", (DL_FUNC)_remulate_computeStatsActor_try);
    R_RegisterCCallable("remulate", "_remulate_computeStatsTie", (DL_FUNC)_remulate_computeStatsTie_try);
    R_RegisterCCallable("remulate", "_remulate_RcppExport_validate", (DL_FUNC)_remulate_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_remulate_computeStatsActor", (DL_FUNC) &_remulate_computeStatsActor, 9},
    {"_remulate_computeStatsTie", (DL_FUNC) &_remulate_computeStatsTie, 11},
    {"_remulate_RcppExport_registerCCallable", (DL_FUNC) &_remulate_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_remulate(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
