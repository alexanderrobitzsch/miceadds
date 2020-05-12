## File Name: syn_mice_create_argument_list.R
## File Version: 0.02


syn_mice_create_argument_list <- function(args, y, x, xp)
{
    n1 <- nrow(x)
    n2 <- nrow(xp)
    args$y <- c(y,rep(1,n2))
    args$x <- rbind(x,xp)
    args$ry <- c(rep(TRUE,n1),rep(FALSE,n2))
    return(args)
}
