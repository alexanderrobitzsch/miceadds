## File Name: mice_impute_catpmm_create_dummies_y.R
## File Version: 0.03

mice_impute_catpmm_create_dummies_y <- function(y, dfr, ridge=0)
{
    n <- nrow(dfr)
    y1 <- stats::model.matrix(object=~0+as.factor(y), data=dfr )
    ny <- ncol(y1)
    colnames(y1) <- paste0('y',1:ny)
    y1 <- y1 + matrix( stats::rnorm(n*ny, mean=0, sd=ridge), nrow=n, ncol=ny)
    dfr <- data.frame(dfr, y1)
    #-- outcome
    res <- list(y1=y1, ny=ny, dfr=dfr)
    return(res)
}
