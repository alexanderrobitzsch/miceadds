## File Name: latent_regression_em_sampling.R
## File Version: 0.05


# sample parameters for latent regression model
latent_regression_em_sampling <- function( pv, X, Z=rep(1,length(pv)) )
{
    # latent regression model
    mod <- stats::lm( pv ~ 0 + X )
    res <- list( "est.beta"=stats::coef(mod), "vcov.beta"=stats::vcov(mod) )
    # sample beta parameter
    res$samp.beta <- miceadds_import_CDM_CDM_rmvnorm( 1, mean=res$est.beta,
                                    sigma=res$vcov.beta )
    # residual standard deviation
    n <- nrow(X)
    p <- ncol(X)
    res$est.sigma <- summary(mod)$sigma
    residuals.mod <- ( stats::resid(mod) ) ^2   * (n-1) / ( n - p - 1)
    mod1 <- stats::lm( residuals.mod ~ 0 + Z )
    summary(mod1)
    # sample gamma coefficients for heteroscedasticity
    samp.gamma <- miceadds_import_CDM_CDM_rmvnorm( 1, mean=stats::coef(mod1),
                                sigma=stats::vcov(mod1) )
    res$fitted.sigma <- sqrt( stats::fitted(mod1) )
    res$lm.latent.regression <- mod
    res$lm.residuals <- mod1
    return(res)
}


.sampling.latent.regression <- latent_regression_em_sampling
