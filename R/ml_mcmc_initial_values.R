## File Name: ml_mcmc_initial_values.R
## File Version: 0.152

ml_mcmc_initial_values <- function( data, y, formula_terms, est_probit, est_thresh, K,
            NR, inits_lme4, X, Z_list, ncluster_list, formula )
{
    require_namespace("lme4")
    Psi_list <- list()
    u_list <- list()
    mod <- NULL

    #--- inits lme4
    if (inits_lme4){
        if (est_probit){
            var_lhs <- formula_terms$formula_lhs
            N <- length(y)
            data[, var_lhs] <- stats::qnorm( rank( x=data[,var_lhs],
                                    ties.method="average") / ( N + 1) )
        }
        mod <- lme4::lmer( formula=formula, data=data )
        smod <- summary(mod)
        beta <- smod$coefficients[,"Estimate"]
        vc0 <- lme4::VarCorr(mod)
        rmod <- lme4::ranef(mod)
        for (rr in seq_len(NR) ){
            Psi_list[[rr]] <- as.matrix(vc0[[rr]])
            u_list[[rr]] <- as.matrix(rmod[[rr]])
        }
        sigma2 <- attr(vc0,"sc")^2
    }
    #--- inits without lme4
    if (! inits_lme4){
        vary <- stats::var(y)
        sigma2 <- .8*vary
        for (rr in seq_len(NR) ){
            nrr <- ncol(Z_list[[rr]])
            drr <- rep(1,nrr)
            drr[1] <- 2
            Psi_rr <- matrix(0, nrow=nrr, ncol=nrr)
            diag(Psi_rr) <- vary*.1*drr
            Psi_list[[rr]] <- Psi_rr
            ncluster_rr <- ncluster_list[[rr]]
            umat <- matrix( stats::rnorm(nrr*ncluster_rr), nrow=ncluster_rr, ncol=nrr)
            umat <- umat * matrix( sqrt(diag(Psi_rr) ), nrow=ncluster_rr,
                                ncol=nrr, byrow=TRUE)
            u_list[[rr]] <- umat
        }
        mod <- stats::lm(y~0+X)
        beta <- mod$coefficients
    }

    alpha <- c(-9.99, 0, 9.99)
    if (est_thresh){
        t1 <- table(y)
        al0 <- stats::qnorm( cumsum(t1/sum(t1) ))
        al0 <- al0 - al0[1]
        alpha <- c(-9.99, 0, al0[2:K], 9.99)
    }
    if (est_probit){
        sigma2 <- 1
    }
    #--- output
    res <- list( beta=beta, sigma2=sigma2, Psi_list=Psi_list, u_list=u_list, alpha=alpha,
                sigma2=sigma2, mod_lme4=mod )
    return(res)
}

