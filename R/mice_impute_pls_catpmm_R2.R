## File Name: mice_impute_pls_catpmm_R2.R
## File Version: 0.05

mice_impute_pls_catpmm_R2 <- function(res, Y, nfac)
{

    R2 <- cumsum(res$Xvar) / res$Xtotvar
    ncomp <- nfac
    ny <- ncol(Y)
    for (dd in 1:ny){
        R21 <- sapply( 1:ncomp, FUN=function(cc){
                    e1 <- res$fitted.values[,dd,cc]
                    y <- Y[,dd]
                    1 - stats::var(y-e1) / stats::var(y)
                } )
        R2 <- rbind( R2, R21)
    }
    rownames(R2) <- c("R2(X)", paste0("R2(Y,", 1:ny, ")") )
    colnames(R2) <- paste0("Comp",1:ncomp)
    res$R2 <- R2
    return(res)
}
