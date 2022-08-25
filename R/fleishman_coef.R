## File Name: fleishman_coef.R
## File Version: 0.192

fleishman_coef <- function(mean=0, sd=1, skew=0, kurt=0)
{
    #* initial value
    x <- c(1,0,0)  # (b,c,d)

    #* optimization function
    fleishman_fct <- function(x){
        c <- x[2]
        a <- -x[2]
        b <- x[1]
        d <- x[3]
        fct <- rep(0,3)
        fct[1] <- b^2+6*b*d+2*c^2+15*d^2-1
        fct[2] <- 2*c*(b^2+24*b*d+105*d^2+2) - skew
        fct[3] <- 24*(b*d + c^2*(1+b^2+28*b*d)+d^2*(12+48*b*d+141*c^2 + 225*d^2 ))-kurt
        res <- sum( fct^2 )
        return(res)
    }

    #* check whether there is a solution
    check_value <- kurt - ( skew^2 - 2 )
    if ( check_value < 0){
        out <- paste0("The condition 'kurt >=skew^2 - 2' has to be fulfilled.")
        kurt_min <- skew^2 - 2
        out <- paste0( out, "\n  The kurtosis must be at least ",
                                round( kurt_min, 3), ".\n")
        stop(out)
    }

    #* optimization and output coefficients
    res <- stats::optim( par=x, fn=fleishman_fct)
    coeff <- res$par
    coeff <- c( -coeff[2], coeff )
    names(coeff) <- c("a","b","c","d")
    #* check convergence
    if ( res$value > 1E-3 ){
        stop("Unsuccessful convergence!")
    }

    #* mean and SD adjustment
    coeff <- sd*coeff
    coeff["a"] <- coeff["a"] + mean

    #* output
    return(coeff)
}
