## File Name: grepvec.R
## File Version: 0.02

grepvec <- function( pattern.vec, x, operator="AND", value=FALSE, ...)
{
    z <- grep.vec( pattern.vec=pattern.vec, x=x, operator=operator, ...)
    if (value){
        z <- z$x
    } else {
        z <- z$index.x
    }
    return(z)
}
