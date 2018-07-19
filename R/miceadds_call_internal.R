## File Name: miceadds_call_internal.R
## File Version: 0.08

miceadds_call_internal <- function(pkg, fct, args, value)
{
    res <- paste0( value, " <- ", pkg, paste0(rep(":",3), collapse=""), fct )
    res <- paste0(res, args)
    Reval(res, print.string=FALSE, n.eval.parent=2)
    return(NULL)
}
