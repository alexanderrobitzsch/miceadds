## File Name: Revalpr_maxabs.R
## File Version: 0.04

Revalpr_maxabs <- function( Rstring_x, Rstring_y, print.string=TRUE, na.rm=FALSE)
{
    Rstring1 <- paste0( "print( max(abs(", Rstring_x ,"-", Rstring_y ,") , na.rm=",
                            na.rm, ") )" )
    Reval( Rstring=Rstring1, print.string=print.string, n.eval.parent=2)
}

