## File Name: round2.R
## File Version: 0.06

round2 <- function( vec, digits=0 )
{
    vec0 <- vec
    eps <- 1E-10
    vec <- abs(vec)
    vec <- vec*10^digits
    vec2 <- vec-floor(vec)
    vec <- floor( vec ) + ifelse( ( vec2 - .5 ) < - eps, 0, 1 )
    vec.round <- sign(vec0) * vec / 10^digits
    return(vec.round)
}
