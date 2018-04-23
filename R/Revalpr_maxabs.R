## File Name: Revalpr_maxabs.R
## File Version: 0.02

Revalpr_maxabs <- function( Rstring_x, Rstring_y, print.string=TRUE)
{
	Rstring1 <- paste0( "print( max(abs(", Rstring_x ,"-", Rstring_y ,")) )" )
	Reval( Rstring=Rstring1, print.string=print.string, n.eval.parent=2)
}
		
