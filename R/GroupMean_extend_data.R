## File Name: GroupMean_extend_data.R
## File Version: 0.04

GroupMean_extend_data <- function(data, index.group, extend=FALSE)
{
    if (extend){
        data <- data[ index.group, ]
        rownames(data) <- NULL
    }
    return(data)
}
