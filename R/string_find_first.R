## File Name: string_find_first.R
## File Version: 0.01

string_find_first <- function(string, symbol )
{
    nc <- nchar(string)
    ind <- NA
    for (cc in 1:nc){
        if( substring(string,cc,cc) == symbol ){
            ind <- cc
            break
        }
    }
    return(ind)
}
