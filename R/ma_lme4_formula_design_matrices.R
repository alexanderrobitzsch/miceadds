## File Name: ma_lme4_formula_design_matrices.R
## File Version: 0.320


ma_lme4_formula_design_matrices <- function(formula, data, start_index=0,
    formula_terms=NULL, only_design_matrices=FALSE )
{
    #*** terms in formula
    if ( is.null(formula_terms) ){
        formula_terms <- ma_lme4_formula_terms(formula=formula)
    }

    #*** omit missing values
    observed_cases <- which( rowSums( is.na( data[, formula_terms$all_vars ] ) )==0 )
    data <- data[ observed_cases, ]

    #*** design matrix fixed effects
    X <- as.matrix( stats::model.matrix( formula_terms$formula_fixed, data=data) )
    parnames <- list()
    if ( ! only_design_matrices ){
        parnames$beta <- paste0("beta_", colnames(X) )
    }
    y <- data[, formula_terms$formula_lhs ]

    #*** design matrices random effects
    NR <- formula_terms$NR
    random_effects_id <- formula_terms$random_effects_id
    formula_random <- formula_terms$formula_random
    Z <- list()
    onlyintercept_list <- list()
    cluster_list <- list()
    idcluster_list <- list()
    ncluster_list <- list()
    parnames_Psi <- list()

    for (rr in seq_len(NR)){
        #-- design matrix random effect rr
        Z_rr <- stats::model.matrix( formula_random[[rr]], data=data)
        Z[[rr]] <- as.matrix(Z_rr)
        if ( ! only_design_matrices ){
            #-- idcluster random effect rr
            onlyintercept_list[[rr]] <- mean( attr(Z_rr, "assign")==0 )==1
            re_id <- random_effects_id[[rr]]
            cl1 <- stats::model.matrix( stats::as.formula( paste0(" ~ 0 +", re_id)),
                                    data=data )
            cl1 <- cl1[,1]
            cluster_list[[rr]] <- cl1
            ucl1 <- unique(cl1)
            ncluster_list[[rr]] <- length(ucl1)
            idcluster_list[[rr]] <- match( cl1, ucl1) - 1 + start_index
            Z_names <- colnames(Z_rr)
            NZ <- length(Z_names)
            eg1 <- expand.grid( 1:NZ, 1:NZ )
            eg1 <- eg1[ eg1[,1] >=eg1[,2],, drop=FALSE]
            parnames_Psi[[re_id]] <- paste0("vcov_", re_id, "_", Z_names[eg1[,2]],
                                            "-", Z_names[eg1[,1]] )#
        }
    }

    if ( ! only_design_matrices ){
        parnames$Psi <- parnames_Psi
        parnames$sigma2 <- "sigma2"
        names(onlyintercept_list) <- names(Z) <- random_effects_id
        names(cluster_list) <- names(idcluster_list) <-
                        names(ncluster_list) <- random_effects_id
    }
    #--- output
    res <- list( formula_terms=formula_terms, data=data, observed_cases=observed_cases,
                    y=y, X=X, Z=Z, NR=NR, onlyintercept_list=onlyintercept_list,
                    idcluster_list=idcluster_list, cluster_list=cluster_list,
                    ncluster_list=ncluster_list, parnames=parnames)
    return(res)
}
