## File Name: grep_leading.R
## File Version: 0.03


grep_leading <- function( pattern, x, value=FALSE )
{
    ind <- which( substring( x, 1 , nchar(pattern) ) == pattern )
    if (value){
        ind <- x[ind]
    }
    return(ind)
}
