## File Name: cwc.R
## File Version: 0.01

cwc <- function(y, cluster)
{
    res <- y - gm(y=y, cluster=cluster)
    return(res)
}
