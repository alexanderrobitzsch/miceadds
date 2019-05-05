## File Name: GroupMean_group_indices.R
## File Version: 0.02


GroupMean_group_indices <- function(group)
{
    groups <- sort( unique( group ) )
    index.group <- match( group, groups )
    #-- output
    res <- list(groups=groups, index.group=index.group)
    return(res)
}
