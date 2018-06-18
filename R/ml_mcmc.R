## File Name: ml_mcmc.R
## File Version: 0.39

ml_mcmc <- function( formula, data, iter=3000, burnin=500, print_iter=500,
    outcome="normal", nu0=NULL, s0=1)
{
    CALL <- match.call()
    s1 <- Sys.time()

    if (outcome=="probit"){
        est_probit <- TRUE
        est_sigma2 <- FALSE
    } else {
        est_probit <- FALSE
        est_sigma2 <- TRUE
    }    
    
    if (iter < burnin){
        stop("Number of burnin iterations 'burnin' must be smaller than 'iter'\n")
    }
    
    #*** formula processing
    res <- ma_lme4_formula_design_matrices(formula=formula, data=data, start_index=0)
    data <- res$data
    y <- res$y
    X <- as.matrix(res$X)
    Z_list <- res$Z
    idcluster_list <- res$idcluster_list
    ncluster_list <- res$ncluster_list
    onlyintercept_list <- res$onlyintercept_list
    NR <- res$NR
    formula_terms <- res$formula_terms    
    verbose <- TRUE
    parnames <- res$parnames
    if (! est_sigma2){
        parnames$sigma2 <- NULL
    }
    
    #*** generate starting values with lme4::lmer function
    if (est_probit){
        var_lhs <- formula_terms$formula_lhs
        N <- length(y)
        data[, var_lhs] <- stats::qnorm( rank( x=data[,var_lhs], ties.method="average") / ( N + 1) )
    }    
    mod <- lme4::lmer( formula=formula, data=data )
    smod <- summary(mod)
    beta <- smod$coefficients[,"Estimate"]
    vc0 <- lme4::VarCorr(mod)
    Psi_list <- list()
    u_list <- list()
    rmod <- lme4::ranef(mod)
    for (rr in seq_len(NR) ){
        Psi_list[[rr]] <- as.matrix(vc0[[rr]])
        u_list[[rr]] <- as.matrix(rmod[[rr]])
    }
    sigma2 <- attr(vc0,"sc")^2    
    
    #*** MCMC preliminaries
    res <- ml_mcmc_create_parameter_index(beta=beta, Psi_list=Psi_list, est_sigma2=est_sigma2)
    parameter_index <- res$parameter_index
    est_parameter <- res$est_parameter
    npar <- res$npar
    psi_nu0_list <- list()
    psi_S0_list <- list()
    s0e <- s00 <- s0
    nu0e <- nu00 <- nu0    
    if (is.null(s00)){
        s00 <- 1
        s0e <- 0
    }
    if (is.null(nu0e) ){
        nu0e <- -3
    }            
    for (rr in seq_len(NR) ){
        nu00 <- nu0
        nr <- ncol(Psi_list[[rr]])
        if (is.null(nu00) ){
            nu00 <- nr + 1
        }                    
        S0 <- diag(s0,nr)
        psi_nu0_list[[rr]] <- nu00
        psi_S0_list[[rr]] <- as.matrix(S0)
    }
    save_iter <- rep(-9, iter)
    save_iter[ seq(burnin+1, iter)] <- seq(1, iter-burnin ) - 1
    alpha <- c(-9.99, 0, 9.99)

    
    #*** MCMC estimation
    res <- ml_mcmc_fit( y=y, X=X, Z_list=Z_list, beta=beta, Psi_list=Psi_list,
            sigma2=sigma2, alpha=alpha, u_list=u_list, idcluster_list=idcluster_list,
            onlyintercept_list=onlyintercept_list, ncluster_list=ncluster_list,
            sigma2_nu0=nu0e, sigma2_sigma2_0=s0e, psi_nu0_list=psi_nu0_list,
            psi_S0_list=psi_S0_list, est_sigma2=est_sigma2, est_probit=est_probit,
            parameter_index=parameter_index, est_parameter=est_parameter, npar=npar,
            iter=iter, save_iter=save_iter, verbose=verbose, print_iter=print_iter,
            parnames0=unlist(parnames) )        

    #*** post processing
    res$parnames <- parnames
    res$burnin <- burnin

    #--- sampled values as coda object
    sampled_values <- res$sampled_values
    sv <- coda::mcmc(data=sampled_values, start=burnin+1,
                            end=iter, thin=round( (iter - burnin )/nrow(sampled_values)    ) )
    res$sampled_values <- sv
    
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
