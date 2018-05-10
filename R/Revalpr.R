## File Name: Revalpr.R
## File Version: 1.03

Revalpr <- function( Rstring , print.string=TRUE)
{
    Rstring1 <- paste0( "print(", Rstring , ")" )
    Reval( Rstring=Rstring1, print.string=print.string, n.eval.parent=2)
}

