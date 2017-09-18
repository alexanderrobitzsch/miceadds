## File Name: NMIextract.R
## File Version: 0.02
## File Last Change: 2017-02-06 11:05:49


NMIextract <- function(results, expr, fun){
  pf<-parent.frame()
  results0 <- results
  if (!is.null(match.call()$expr)){
    expr<-substitute(expr)
	lapply( results0 , FUN = function(results){
        lapply(results, function(result) eval(expr, result,pf))
							} )
  } else {
    lapply( results0 , FUN = function( results){
				lapply(results, fun)
							} )
  }
  
}
