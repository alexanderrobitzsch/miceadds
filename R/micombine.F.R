## File Name: micombine.F.R
## File Version: 0.12

micombine.F <- function( Fvalues, df1, display=TRUE, version=1 )
{
    M <- length(Fvalues)        # number of imputations
    dk <- df1 * Fvalues         #
    micombine.chisquare( dk=df1*Fvalues, df=df1, display=display, version=version)
}
