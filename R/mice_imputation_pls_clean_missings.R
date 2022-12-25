## File Name: mice_imputation_pls_clean_missings.R
## File Version: 0.02


mice_imputation_pls_clean_missings <- function(x, eps=1e-12)
{
    csna <- colSums(is.na(x))
    ind_na <- which(csna>0)
    N <- nrow(x)
    L <- length(ind_na)
    if (L>0){
        x[,ind_na] <- matrix( stats::runif(N*L,-eps,eps), ncol=L)
    }
    return(x)
}
