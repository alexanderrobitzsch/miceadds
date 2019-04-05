## File Name: mice_imputation_add_jitter.R
## File Version: 0.01

mice_imputation_add_jitter <- function(x, jitter_val=1e15)
{
    no <- length(x)
    jitter_val1 <- jitter_val * max(x, na.rm=TRUE)
    x <- x + stats::runif(no)/jitter_val1
    return(x)
}
