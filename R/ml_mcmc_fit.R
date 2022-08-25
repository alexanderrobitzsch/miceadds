## File Name: ml_mcmc_fit.R
## File Version: 0.527

ml_mcmc_fit <- function(y, X, Z_list, beta, Psi_list, sigma2,
    alpha, u_list, idcluster_list, onlyintercept_list, ncluster_list,
    sigma2_nu0, sigma2_sigma2_0, psi_nu0_list, psi_S0_list, est_sigma2,
    est_probit, parameter_index, est_parameter, npar, iter, save_iter,
    verbose=TRUE, print_iter=500, parnames0=NULL, K=9999, est_thresh=FALSE,
    thresh_fac=5.8, ridge=1e-5, parm_summary=TRUE )
{
    requireNamespace("coda")
    #** preliminaries
    xtx <- miceadds_rcpp_ml_mcmc_compute_xtx(X)
    xtx_inv <- miceadds_ginv( x=xtx )
    NR <- length(Z_list)
    ztz_list <- list()
    for (rr in seq_len(NR)){
        ztz_list[[rr]] <- miceadds_rcpp_ml_mcmc_compute_ztz( Z=Z_list[[rr]],
                        idcluster=idcluster_list[[rr]], ncluster=ncluster_list[[rr]] )
    }
    N <- length(y)
    sd_proposal <- sqrt( thresh_fac / N + 0*alpha )
    sd_proposal[ c(1,2, length(alpha))] <- 0

    #** call Rcpp function
    res <- miceadds_rcpp_ml_mcmc_sampler( y_obs=y, X=X, xtx_inv=xtx_inv,
                ztz_list=ztz_list, Z_list=Z_list, beta_init=beta, Psi_list_init=Psi_list,
                sigma2_init=sigma2, alpha_init=alpha, u_list_init=u_list,
                idcluster_list=idcluster_list, onlyintercept_list=onlyintercept_list,
                ncluster_list=ncluster_list, sigma2_nu0=sigma2_nu0,
                sigma2_sigma2_0=sigma2_sigma2_0, psi_nu0_list=psi_nu0_list,
                psi_S0_list=psi_S0_list, NR=NR, est_sigma2=est_sigma2,
                est_probit=est_probit, parameter_index=parameter_index,
                est_parameter=est_parameter, npar=npar, iter=iter, save_iter=save_iter,
                verbose=verbose, print_iter=print_iter, est_thresh=est_thresh, K=K,
                sd_proposal=sd_proposal, ridge=ridge)

    #** output processing
    if ( is.null(parnames0) ){
        NS <- ncol(res$sampled_values)
        parnames0 <- paste0("par",1:NS)
    }
    colnames(res$sampled_values) <- parnames0
    sampled_values <- res$sampled_values

    #-- parameter summary
    if (parm_summary){
        dfr <- data.frame("par"=parnames0)
        dfr$mean <- ma_colMeans(x=sampled_values)
        dfr$mode <- ma_colMode(x=sampled_values)
        dfr$se <- ma_colSD(x=sampled_values)
        dfr$lower95 <- ma_colQuantile(x=sampled_values, prob=.025)
        dfr$upper95 <- ma_colQuantile(x=sampled_values, prob=.975)
        dfr$Rhat <- ma_rhat(x=sampled_values, n_chains=3)
        dfr$Neff <- round( coda::effectiveSize(x=sampled_values), 1 )
        rownames(dfr) <- NULL
        res$par_summary <- dfr
    }
    res$iter <- iter
    res$est_probit <- est_probit
    res$est_sigma2 <- est_sigma2
    res$est_thresh <- est_thresh
    res$X <- X
    res$Z_list <- Z_list
    res$idcluster_list <- idcluster_list
    res$onlyintercept_list <- onlyintercept_list
    res$parameter_index <- parameter_index
    res$est_parameter <- est_parameter
    res$npar <- npar
    res$thresh_fac <- thresh_fac
    res$sigma2_nu0 <- sigma2_nu0
    res$sigma2_sigma2_0 <- sigma2_sigma2_0
    res$psi_nu0_list <- psi_nu0_list
    res$psi_S0_list <- psi_S0_list
    res$K <- K
    res$save_iter <- save_iter
    res$parnames0 <- parnames0
    res$ridge <- ridge

    #--- coef and vcov
    if (parm_summary){
        coef <- dfr$mode
        names(coef) <- parnames0
        res$coef <- coef
        res$vcov <- stats::cov(sampled_values)
    }

    #--- output
    return(res)
}
