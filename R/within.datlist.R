## File Name: within.datlist.R
## File Version: 1.02
## File Last Change: 2017-02-06 11:05:50


#########################################################
within.datlist <- function (data, expr, ...){
   res <- data
   CALL <- attr(data , "call")
   imp <- res
   M <- length(imp)   
   for (ii in 1:M){
	   # this function is simply a copy of within.data.frame
		parent <- parent.frame()
		data <- imp[[ii]]
		e <- evalq(environment(), data, parent)
		eval(substitute(expr), e)
		l <- as.list(e)
		l <- l[!sapply(l, is.null)]
		nD <- length(del <- setdiff(names(data), (nl <- names(l))))
		data[nl] <- l
		if (nD) 
			data[del] <- if (nD == 1) 
				NULL
			else vector("list", nD)
		res[[ii]] <- data
			}
    res <- datlist_create(res)
	attr(res,"call") <- CALL
	return(res)
}
###############################################################
