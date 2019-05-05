## File Name: lmer_vcov2.R
## File Version: 0.12

## covariance matrix for fitted models with lmer including
## variance components
## code mainly adapted from http://rpubs.com/bbolker/varwald

lmer_vcov2 <- function(object, level=.95,  ...)
{
    require_namespace(pkg="lme4")
    require_namespace(pkg="sirt")
    fit0 <- fit <- object
    object <- lme4::VarCorr(fit)
    vdd <- as.data.frame(object, order="lower.tri")
    pars <- vdd[,"sdcor"]

    nms <- apply(vdd[,1:3],1, function(x) paste(na.omit(x),collapse="."))
    names(pars) <- nms
    #--- include fixed effects
    Vcov <- as.matrix( vcov(fit0, useScale=FALSE) )
    betas <- lme4::fixef(fit0)
    np_fixed <- length(betas)
    np_random <- length(pars)
    np <- np_fixed + np_random
    random <- list( coef=pars, vcov=NULL)
    fixed <- list( coef=betas, vcov=Vcov )
    coef1 <- c(fixed$coef, random$coef)
    ind_fixed <- 1:np_fixed
    ind_random <- ind <- np_fixed + 1:np_random
    se <- c( sqrt( diag( fixed$vcov )), rep(NA, np_random) )
    np <- np_fixed + np_random

    #* parameter summary
    dfr <- data.frame( index=1:np,
            type=c( rep("fixed",np_fixed), rep("random",np_random) ) )
    s1 <- strsplit( names(random$coef), split=".", fixed=TRUE )
    s1 <- unlist( lapply(s1, FUN=function(ll){
                    hh <- length(ll)
                    label <- "SD"
                    if (hh==3){ label <- "Cor"}
                    return(label)
                } ) )
    dfr$stat <- c( rep("Beta",np_fixed), s1 )
    dfr$parm <- names(coef1)
    dfr$est <- coef1
    dfr$se <- se
    dfr <- sirt::parmsummary_extend(dfr=dfr, level=level, est_label="est", se_label="se")

    #-- output
    res <- list( par_summary=dfr, coef=coef1, se=se, fixed=fixed, random=random,
                np=np, np_random=np_random, np_fixed=np_fixed,
                ind_fixed=ind_fixed, ind_random=ind_random )
    class(res) <- "lmer_vcov"
    return(res)
}
