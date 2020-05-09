## File Name: mice_imputation_imputeR.R
## File Version: 0.06


mice_imputation_imputeR <- function(y, ry, x, Fun=NULL, draw_boot=TRUE,
    add_noise=TRUE, use_cFun=FALSE, default_fun=imputeR::ridgeR, ... )
{
    requireNamespace("imputeR")
    pos <- parent.frame(n=1)
    res <- mice_imputation_get_states(pos=pos)
    vname <- res$vname
    newstate <- res$newstate
    pos <- res$pos
    state_data <- res$data

    lm_fun <- mice_imputation_extract_list_arguments( Fun, vname, default_fun)
    draw_boot <- mice_imputation_extract_list_arguments( draw_boot, vname, TRUE)
    add_noise <- mice_imputation_extract_list_arguments( add_noise, vname, TRUE)

    #- draw bootstrap sample of dataset
    res <- mice_impute_imputeR_draw_bootstrap( y=y, x=x, vname=vname, ry=ry,
                draw_boot=draw_boot )
    dat0 <- res$dat0
    dat1 <- res$dat1

    #- estimate model
    lm_args <- list(y=dat1[,1], x=dat1[,-1,drop=FALSE])
    if (use_cFun){
        yvalues <- sort(unique(lm_args$y))
        if (length(yvalues)!=2){
            stop("Only two y values are allowed!\n")
        }
        lm_args$y <- match( lm_args$y, yvalues)-1
    }
    mod <- do.call(what=lm_fun, args=lm_args)

    #- apply prediction method
    nimp <- sum(!ry)
    if (! use_cFun){
        yimp <- predict(mod, dat0[!ry,,drop=FALSE])
    }
    if (use_cFun){
        yimp <- predict(mod, dat0[!ry,,drop=FALSE], type="response")[,1]
        rn <- stats::runif(nimp)
        yimp <- 1*(rn < yimp)
        yimp <- yvalues[yimp+1]
    }

    if (add_noise){
        #- compute residuals
        yhat <- predict(mod)
        mod_residuals <- lm_args$y - yhat
        #- add empirical residuals
        e <- sample(mod_residuals, size=nimp, replace=TRUE)
        yimp <- yimp + e
    }
    #-- output imputed values
    return(yimp)
}
