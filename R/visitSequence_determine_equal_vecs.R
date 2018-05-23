## File Name: visitSequence_determine_equal_vecs.R
## File Version: 0.03


#*****************************************************
# equality of two vectors
visitSequence_determine_equal_vecs <- function(v1,v2)
{
    val <- FALSE
    if ( length(v1)==length(v2) ){
        if ( sum( v1 !=v2 )==0){
            val <- TRUE
        }
    }
    return(val)
}
#***************************************************

equal_vecs <- visitSequence_determine_equal_vecs
