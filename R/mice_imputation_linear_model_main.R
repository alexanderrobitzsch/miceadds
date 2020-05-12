## File Name: mice_imputation_linear_model_main.R
## File Version: 0.25

mice_imputation_linear_model_main <- function(x, y, ry, wy=NULL,
    lm_args=NULL, lm_fun="lm", trafo=NULL, antitrafo=NULL, ...)
{
    pos <- parent.frame(n=1)
    res <- mice_imputation_get_states(pos=pos)
    vname <- res$vname

    #- wy
    if ( is.null(wy) ){
        wy <- ! ry
    }

    #- transformations
    trafo <- mice_imputation_extract_list_arguments( trafo, vname, NULL)
    antitrafo <- mice_imputation_extract_list_arguments( antitrafo, vname, NULL )

    #- clean data
    res <- mice_imputation_input_data_frame(x=x, y=y, vname=vname, trafo=trafo)
    dat0 <- res$dat0
    formula_imp <- res$formula_imp

    #- draw bootstrap sample
    dat1 <- mice_imputation_smcfcs_draw_bootstrap_sample(data=dat0, ry=ry)

    #- estimate model
    if (is.null(lm_args)){
        lm_args <- list()
    }
    lm_args$formula <- formula_imp
    lm_args$data <- dat1
    mod <- do.call(what=lm_fun, args=lm_args)

    #- impute values
    nmis <- sum(wy)
    pmod <- predict(mod, dat0[ wy, ] )
    rmod <- residuals(mod)
    resid <- sample(rmod, nmis, replace=TRUE)
    imp <- pmod + resid

    # apply inverse transformation
    imp <- mice_imputation_transformation(x=imp, trafo_fun=antitrafo)

    #--- output
    return(imp)
}
