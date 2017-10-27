## File Name: Rfunction_include_argument_values.R
## File Version: 0.06

Rfunction_include_argument_values <- function(string, maxlen=70)
{

	a1 <- strsplit( string , split= "(" , fixed = TRUE )[[1]]
	s1 <- a1[2]
	s1 <- strsplit( s1 , split= ")" , fixed = TRUE )[[1]][1]
	s1 <- gsub("\n" , "" , s1 )
	s1 <- strsplit( s1 , split= "," , fixed = TRUE )[[1]]
	NS <- length(s1)
	s2 <- gsub( " " , "" , s1 )
	M0 <- 0
	for (ss in 1:NS){
		# ss <- 5
		s2[ss] <- gsub( " " , "" , s2[ss] )			
		if ( length( grep("=" , s1[ss] ) ) == 0 ){
			h1 <- gsub( " " , "" , s1[ss] )
			s2[ss] <- paste0( h1 , "=" , h1 )
		}
		nss <- nchar(s2[ss])
		M0 <- M0 + nss
		if (M0 > maxlen ){
			s2[ss] <- paste0("\n " , s2[ss] )
			M0 <- nss
		}
	}
	s2 <- paste0( a1[1] , "( " , paste0( s2 , collapse = ", " ) , " ) \n" )
	return(s2)
}
