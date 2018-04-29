## File Name: Reval.R
## File Version: 1.05


# evaluates an R function
Reval <- function( Rstring , print.string=TRUE, n.eval.parent=1)
{ 
    if (print.string){ 
        cat( paste( "R>" , Rstring ) , "\n"  ) 
    }
    eval.parent( parse( text = paste( Rstring )) , n=n.eval.parent )
}

