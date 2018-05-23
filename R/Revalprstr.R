## File Name: Revalprstr.R
## File Version: 1.04

Revalprstr <- function( Rstring, print.string=TRUE)
{
    Rstring1 <- paste0( "print( str(", Rstring, ") )" )
    Reval( Rstring=Rstring1, print.string=print.string, n.eval.parent=2)
}

