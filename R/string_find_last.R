## File Name: string_find_last.R
## File Version: 0.03

string_find_last <- function(string, symbol )
{
    nc <- nchar(string)
    ind <- NA
    for (cc in seq(nc,1,-1) ){
        if( substring(string,cc,cc)==symbol ){
            ind <- cc
            break
        }
    }
    return(ind)
}
