## File Name: ma_lme4_formula_design_matrices.R
## File Version: 0.03


ma_lme4_formula_design_matrices <- function(formula, data, start_index=0)
{
    #*** terms in formula
    formula_terms <- ma_lme4_formula_terms(formula=formula)
    #*** omit missing values
    observed_cases <- which( rowSums( is.na( data[ , formula_terms$all_vars ] ) ) == 0 )
    data <- data[ observed_cases, ]    
    #*** design matrix fixed effects
    X <- stats::model.matrix( object=formula_terms$formula_fixed, data=data)
    #*** design matrices random effects
    NR <- formula_terms$NR
    random_effects_id <- formula_terms$random_effects_id
    formula_random <- formula_terms$formula_random
    Z <- list()
    onlyintercept_list <- list()
    cluster_list <- list()
    idcluster_list <- list()
    ncluster_list <- list()    
    for (rr in seq_len(NR)){
        #-- design matrix random effect rr
        Z[[rr]] <- stats::model.matrix( object=formula_random[[rr]], data=data)
        onlyintercept_list[[rr]] <- mean( attr(Z[[rr]], "assign") == 0 ) == 1
        #-- idcluster random effect rr
        re_id <- random_effects_id[[rr]]
        cl1 <- stats::model.matrix( stats::as.formula( paste0(" ~ 0 +" , re_id)), data )
        cl1 <- cl1[,1]
        cluster_list[[rr]] <- cl1
        ucl1 <- unique(cl1)
        ncluster_list[[rr]] <- length(ucl1)
        idcluster_list[[rr]] <- match( cl1, ucl1) - 1 + start_index
    }
    names(onlyintercept_list) <- names(Z) <- random_effects_id
    names(cluster_list) <- names(idcluster_list) <- names(ncluster_list) <- random_effects_id    
    #--- output
    res <- list( formula_terms=formula_terms, data=data, observed_cases=observed_cases,
                    X=X, Z=Z, NR=NR, onlyintercept_list=onlyintercept_list,
                    idcluster_list=idcluster_list, cluster_list=cluster_list,
                    ncluster_list=ncluster_list)
    return(res)

}
