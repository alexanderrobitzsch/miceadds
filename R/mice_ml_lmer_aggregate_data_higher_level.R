## File Name: mice_ml_lmer_aggregate_data_higher_level.R
## File Version: 0.16

mice_ml_lmer_aggregate_data_higher_level <- function(vname_level, y, ry, x, data,
        levels_id, vname=NULL )
{
    # if (ncol(x)==0){
    #    stop( paste0("\nNo predictor variables are selected for variable ", vname, "\n"))
    # }
    if (vname_level !=""){
        #--- cluster identifiers
        clus <- data[, vname_level]
        data1 <- GroupMean(data=data[, c(vname_level, levels_id), drop=FALSE ],
                            group=clus )
        data <- data1[, -1 ]
        #--- y
        y1 <- GroupMean(data=y, group=clus )
        y <- as.vector(y1[,-1])
        #--- ry
        ry1 <- GroupMean(data=1*ry, group=clus )
        ry <- as.vector( ry1[,-1] > .999 )
        #--- ry
        x1 <- GroupMean(data=x, group=clus )
        x <- x1[, -1, drop=FALSE ]
    }
    #--- output
    res <- list( data=data, y=y, ry=ry, x=x )
    return(res)
}
