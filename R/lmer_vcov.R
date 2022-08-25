## File Name: lmer_vcov.R
## File Version: 0.272

## covariance matrix for fitted models with lmer including
## variance components
## code mainly adapted from http://rpubs.com/bbolker/varwald

lmer_vcov <- function(object, level=.95, use_reml=FALSE, ...)
{
    require_namespace(pkg="numDeriv")
    require_namespace(pkg="lme4")
    require_namespace(pkg="sirt")
    fit0 <- fit <- object
    dd <- NULL
    is_reml <- lme4::isREML(fit)
    use_reml <- use_reml & is_reml
    if (is_reml) {
        fit <- lme4::refitML(fit)
    }
    object <- lme4::VarCorr(fit)
    useSc <- attr(object,"useSc")
    args_devfun2 <- list(fit, useSc=useSc, signames=FALSE)
    pkg <- "lme4"
    subfun <- "devfun2"
    call_dot <- paste0( rep(":",3), collapse="")
    eval( parse( text=paste0("dd <- do.call( ", pkg, call_dot, subfun,
                            ", args=args_devfun2)" ) )  )
    vdd <- as.data.frame(object, order="lower.tri")
    pars <- vdd[,"sdcor"]
    if (use_reml){
        pars <- lmer_extract_sd_random_effects(fit=fit0)
    }
    npar0 <- length(pars)
    if (lme4::isGLMM(fit)) {
        pars <- c(pars, lme4::fixef(fit))
    }
    hh1 <- numDeriv::hessian(dd,pars)
    vv2 <- 2*miceadds_ginv(x=hh1)
    if (lme4::isGLMM(fit)) {
        vv2 <- vv2[1:npar0, 1:npar0, drop=FALSE]
    }
    nms <- apply(vdd[,1:3],1, function(x) paste(na.omit(x),collapse="."))
    dimnames(vv2) <- list(nms,nms)
    names(pars) <- colnames(vv2)
    #--- include fixed effects
    Vcov <- as.matrix( vcov(fit0, useScale=FALSE) )
    betas <- lme4::fixef(fit0)
    np_fixed <- length(betas)
    np_random <- length(pars)
    np <- np_fixed + np_random
    random <- list( coef=pars, vcov=vv2)
    fixed <- list( coef=betas, vcov=Vcov )
    #--- concatenated version including fixed and random effects
    coef1 <- c(fixed$coef, random$coef)
    vcov1 <- matrix(0, nrow=np, ncol=np)
    rownames(vcov1) <- colnames(vcov1) <- names(coef1)
    ind_random <- ind <- np_fixed + 1:np_random
    vcov1[ ind, ind ] <- random$vcov
    ind_fixed <- ind <- 1:np_fixed
    vcov1[ ind, ind ] <- fixed$vcov
    se <- sqrt( diag( vcov1 ))

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
    res <- list( par_summary=dfr, coef=coef1, vcov=vcov1, se=se, fixed=fixed,
                random=random, np=np, np_random=np_random, np_fixed=np_fixed,
                ind_fixed=ind_fixed, ind_random=ind_random )
    class(res) <- "lmer_vcov"
    return(res)
}
