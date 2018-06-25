## File Name: output.format1.R
## File Version: 1.07


output.format1 <- function( stringtype, label, rep.N=1, stringlength=70)
{
    h1 <- paste( rep("\n",  rep.N), collapse="")
    cat(h1)
    cat( paste( paste( rep( stringtype, stringlength ), collapse="") ) )
    cat(h1)
    cat(label)
    cat(h1)
    cat("\n")
}

