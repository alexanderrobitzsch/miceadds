## File Name: string_extract_part.R
## File Version: 0.11

string_extract_part <- function( vec, part=1, sep="__", remove_empty=TRUE)
{
    v1 <- strsplit(paste(vec), split=sep)
    m1 <- lapply( v1, FUN=function(ll){
                if (remove_empty){
                    ll <- ll[ ll !=""]
                }
                ll[ part]
                } )
    m1 <- unlist(m1)
    return(m1)
}
