## File Name: Reval.R
## File Version: 1.02
## File Last Change: 2017-02-06 11:05:50



#**************************************************
# evaluates an R function
Reval <- function( Rstring , print.string=TRUE){ 
    if (print.string){ cat( paste( "R>" , Rstring ) , "\n"  ) }
	eval.parent( parse( text = paste( Rstring )) , n=1 )	
	# .GlobalEnv
	# or eval.parent(expr, n = 1)
}
#**************************************************
Revalpr <- function( Rstring , print.string=TRUE){
    if (print.string){ cat( paste( "R> print(" , Rstring , ")") , "\n"  ) }
	eval.parent( parse( text = paste( "print(",Rstring,")" )) , n=1 )	
}
		
		
Revalprstr <- function( Rstring , print.string=TRUE){
    if (print.string){ cat( paste( "R> print(str(" , Rstring , "))") , "\n"  ) }
	eval.parent( parse( text = paste( "print(str(",Rstring,"))" )) , n=1 )	
}

