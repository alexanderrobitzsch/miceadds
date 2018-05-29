## File Name: ma_exists.R
## File Version: 0.01

ma_exists <- function( x, pos, n_index=1:8)
{
    n_index <- n_index + 1
    is_there <- exists(x, where=pos)
    obj <- NULL
    if (is_there){
        obj <- get(x, pos)
    }
    if (! is_there){
        for (nn in n_index){
            pos <- parent.frame(n=nn)
            is_there <- exists(x, where=pos)
            if (is_there){
                obj <- get(x, pos)
                break
            }        
        }
    }
    #--- output
    res <- list( is_there=is_there, obj=obj, pos=pos)
    return(res)
}
