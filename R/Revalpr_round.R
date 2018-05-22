## File Name: Revalpr_round.R
## File Version: 0.05


Revalpr_round <- function( Rstring, digits=5, print.string=TRUE)
{
    Rstring1 <- paste0( "print( round(", Rstring, ",", digits, ") )" )
    Reval( Rstring=Rstring1, print.string=print.string, n.eval.parent=2)
}

