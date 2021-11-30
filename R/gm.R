## File Name: gm.R
## File Version: 0.03

gm <- function(y, cluster)
{
    res <- GroupMean(data=y, group=cluster, extend=TRUE)
    return(res[,2])
}
