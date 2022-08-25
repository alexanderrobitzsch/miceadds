## File Name: ml_mcmc.R
## File Version: 0.514

ml_mcmc <- function( formula, data, iter=3000, burnin=500, print_iter=100,
    outcome="normal", nu0=NULL, s0=1, psi_nu0_list=NULL, psi_S0_list=NULL,
    inits_lme4=FALSE, thresh_fac=5.8, ridge=1e-5)
{
    requireNamespace("coda")
    CALL <- match.call()
    s1 <- Sys.time()

    if (iter < burnin){
        stop("Number of burnin iterations 'burnin' must be smaller than 'iter'\n")
    }

    #*** process data
    res <- ml_mcmc_proc_data( formula=formula, data=data, outcome=outcome, verbose=TRUE )
    data <- res$data
    y <- res$y
    X <- as.matrix(res$X)
    Z_list <- as.matrix(res$Z)
    idcluster_list <- res$idcluster_list
    ncluster_list <- res$ncluster_list
    onlyintercept_list <- res$onlyintercept_list
    NR <- res$NR
    formula_terms <- res$formula_terms
    parnames <- res$parnames
    est_probit <- res$est_probit
    est_sigma2 <- res$est_sigma2
    K <- res$K
    est_normal <- res$est_normal
    est_thresh <- res$est_thresh
    verbose <- res$verbose

    #*** generate starting values
    res <- ml_mcmc_initial_values( data=data, y=y, formula_terms=formula_terms,
                est_probit=est_probit, est_thresh=est_thresh, K=K, NR=NR,
                inits_lme4=inits_lme4, X=X, Z_list=Z_list, ncluster_list=ncluster_list,
                formula=formula )
    beta <- res$beta
    sigma2 <- res$sigma2
    Psi_list <- res$Psi_list
    u_list <- res$u_list
    alpha <- res$alpha
    sigma2 <- res$sigma2
    mod_lme4 <- res$mod_lme4

    #*** MCMC preliminaries
    res <- ml_mcmc_create_parameter_index(beta=beta, Psi_list=Psi_list,
                    est_sigma2=est_sigma2, est_thresh=est_thresh, K=K)
    parameter_index <- res$parameter_index
    est_parameter <- res$est_parameter
    npar <- res$npar
    s0e <- s00 <- s0
    nu0e <- nu00 <- nu0
    if (is.null(s00)){
        s00 <- 1
        s0e <- 0
    }
    if (is.null(nu0e) ){
        nu0e <- -3
    }
    if (is.null(psi_nu0_list)){
        psi_nu0_list <- list()
        psi_S0_list <- list()
        for (rr in seq_len(NR) ){
            nu00 <- nu0
            nr <- ncol(Psi_list[[rr]])
            if (is.null(nu00) ){
                nu00 <- -3
            }
            S0 <- diag(0,nr)
            psi_nu0_list[[rr]] <- nu00
            psi_S0_list[[rr]] <- as.matrix(S0)
        }
    }
    save_iter <- rep(-9, iter)
    save_iter[ seq(burnin+1, iter)] <- seq(1, iter-burnin ) - 1
    parnames0 <- unlist(parnames)

    ml_mcmc_fit_args <- list( y=y, X=X, Z_list=Z_list, beta=beta, Psi_list=Psi_list,
            sigma2=sigma2, alpha=alpha, u_list=u_list, idcluster_list=idcluster_list,
            onlyintercept_list=onlyintercept_list, ncluster_list=ncluster_list,
            sigma2_nu0=nu0e, sigma2_sigma2_0=s0e, psi_nu0_list=psi_nu0_list,
            psi_S0_list=psi_S0_list, est_sigma2=est_sigma2, est_probit=est_probit,
            parameter_index=parameter_index, est_parameter=est_parameter, npar=npar,
            iter=iter, save_iter=save_iter, verbose=verbose, print_iter=print_iter,
            parnames0=parnames0, K=K, est_thresh=est_thresh, thresh_fac=thresh_fac,
            ridge=ridge)

    #*** MCMC estimation
    res <- do.call( what=ml_mcmc_fit, args=ml_mcmc_fit_args)

    #*** post processing
    res$parnames <- parnames
    res$burnin <- burnin
    res$formula_terms <- formula_terms
    res$outcome <- outcome
    res$mod_lme4 <- mod_lme4

    #--- sampled values as coda object
    sampled_values <- res$sampled_values
    thin <- round( (iter - burnin )/nrow(sampled_values) )
    res$sampled_values <- coda::mcmc(data=sampled_values, start=burnin+1,
                                    end=iter, thin=thin )

    res$outcome <- outcome
    s2 <- Sys.time()
    res$CALL <- CALL
    res$s1 <- s1
    res$s2 <- s2
    res$diff_time <- s2 - s1
    class(res) <- "ml_mcmc"
    #--- output
    return(res)
}
