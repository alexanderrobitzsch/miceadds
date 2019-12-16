## File Name: ma_inherits_or.R
## File Version: 0.02

ma_inherits_or <- function(x, what)
{
    val <- FALSE
    for (ww in what){
        val <- val | inherits(x=x, what=ww)
    }
    return(val)
}
