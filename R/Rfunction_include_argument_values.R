## File Name: Rfunction_include_argument_values.R
## File Version: 0.25

Rfunction_include_argument_values <- function(string, maxlen=70)
{
    ind1 <- string_find_first(string=string, symbol="(" )
    a1 <- c( substring(string,1, ind1-1), substring(string, ind1+1, nchar(string) ) )
    s1 <- a1[2]

    ind1 <- string_find_last(string=s1, symbol=")" )
    s1 <- substring(s1,1, ind1-1)
    s1 <- gsub("\n", "", s1 )
    s1 <- strsplit( s1, split=",", fixed=TRUE )[[1]]
    NS <- length(s1)
    s2 <- gsub( " ", "", s1 )
    M0 <- nchar(a1[1])
    for (ss in 1:NS){
        s2[ss] <- gsub( " ", "", s2[ss] )
        if ( length( grep("=", s1[ss] ) )==0 ){
            h1 <- gsub( " ", "", s1[ss] )
            s2[ss] <- paste0( h1, "=", h1 )
        }
        nss <- nchar(s2[ss])
        M0 <- M0 + nss
        if (M0 > maxlen ){
            s2[ss] <- paste0("\n ", s2[ss] )
            M0 <- nss
        }
    }
    s2 <- paste0( a1[1], "( ", paste0( s2, collapse=", " ), " ) \n" )
    #--- handle dots
    s2 <- gsub("...=...", "...", s2, fixed=TRUE)
    #--- delete blanks at begin of lines
    s2 <- gsub("\n ", "\n", s2, fixed=TRUE)
    #--- output
    return(s2)
}
