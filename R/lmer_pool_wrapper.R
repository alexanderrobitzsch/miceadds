## File Name: lmer_pool_wrapper.R
## File Version: 0.05


lmer_pool_wrapper <- function( models, level=.95, FUN=lmer_vcov2, ...)
{
    M <- length(models)
    qhat <- list()
    se <- list()
    NMI <- FALSE
    for (mm in 1:M){
        args <- list( object=models[[mm]], level=level)
        res_mm <- do.call( what=FUN, args=args )
        qhat[[mm]] <- res_mm$coef
        se[[mm]] <- res_mm$se
    }
    res <- pool_nmi( qhat=qhat, u=NULL, se=se, NMI=NMI, comp_cov=TRUE, is_list=TRUE,
                    method=1)
    if (!NMI){
        res$lambda_Between <- NA
        res$lambda_Within <- NA
    }
    #--- output
    class(res) <- "lmer_pool"
    return(res)
}
