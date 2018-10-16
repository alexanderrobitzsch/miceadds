## File Name: fleishman_sim.R
## File Version: 0.03

fleishman_sim <- function(N=1, coef=NULL, mean=0, sd=1, skew=0, kurt=0)
{
    if ( is.null(coef) ){
        coef <- fleishman_coef(mean=mean, sd=sd, skew=skew, kurt=kurt)
    }
    Z <- stats::rnorm(N, mean=0, sd=1)
    X <- coef["a"] + coef["b"] * Z + coef["c"] * Z^2 + coef["d"] * Z^3
    return(X)
}
