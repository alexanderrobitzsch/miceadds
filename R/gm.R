## File Name: gm.R
## File Version: 0.04

gm <- function(y, cluster, elim=FALSE)
{
    res <- GroupMean(data=y, group=cluster, extend=TRUE, elim=elim)
    return(res[,2])
}
