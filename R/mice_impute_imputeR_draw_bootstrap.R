## File Name: mice_impute_imputeR_draw_bootstrap.R
## File Version: 0.03

mice_impute_imputeR_draw_bootstrap <- function(y, x, vname, ry, draw_boot)
{
    dat0 <- data.frame(y, x)
    colnames(dat0) <- c(vname, colnames(x))
    if (draw_boot){
        dat1 <- mice_imputation_smcfcs_draw_bootstrap_sample(data=dat0, ry=ry)
    } else {
        dat1 <- dat0[ry, ]
    }
    #-- output
    res <- list(dat0=dat0, dat1=dat1)
    return(res)
}
