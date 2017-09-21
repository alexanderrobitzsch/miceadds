## File Name: Rfunction_output_list_result_function.R
## File Version: 0.02
## File Last Change: 2017-09-19 20:58:31

Rfunction_output_list_result_function <- function(string, mid = " <- res$")
{
	a1 <- strsplit( string , split= "(" , fixed = TRUE )[[1]]
	s1 <- a1[2]
	s1 <- strsplit( s1 , split= ")" , fixed = TRUE )[[1]][1]
	s1 <- gsub("\n" , "" , s1 )
	s1 <- strsplit( s1 , split= "," , fixed = TRUE )[[1]]
	NS <- length(s1)
	s2 <- ""
	for (ss in 1:NS){
		s3 <- strsplit( s1[ss] , split="=")
		s3 <- gsub(" " , "" , s3[[1]][1] )
		s2 <- paste0( s2 , "\n" , s3 , mid , s3 )
	}
	s2 <- paste0( s2 , "\n" )
	return(s2)
}
