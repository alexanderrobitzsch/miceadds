## File Name: syn_mice.R
## File Version: 0.16

syn_mice <- function(data, m=5, k=NULL, syn_check=TRUE, ...)
{
    dots <- list(...)
    if (is.null(k)){
        k <- nrow(data)
    }
    #- arrange data
    n <- nrow(data)
    data1 <- data[ c(1:n,rep(1,k)), ]
    syn_cases <- seq(n+1,n+k)
    data1[ syn_cases, ] <- NA

    #- checking functions
    if (syn_check){
        #* empty mice imputation
        imp0 <- mice::mice(data=data1, m=m, maxit=0, ...)
        for (vv in c("predictorMatrix", "visitSequence") ){
            if (! ( vv %in% names(dots) ) ){
                dots[[ vv ]] <- imp0[[ vv ]]
            }
        }
        #* empty synthpop imputation
        syn0 <- synthpop::syn(data=data, predictor.matrix=dots$predictorMatrix,
                    m=0, visit.sequence=dots$visitSequence)
        dots$predictorMatrix <- syn0$predictor.matrix
        dots$visitSequence <- syn0$visit.sequence
    }

    #- apply mice
    dots$m <- m
    dots$maxit <- 1
    dots$data <- data1
    imp <- do.call( what=mice::mice, args=dots)

    #- arrange output object
    imp$k <- k
    imp$syn <- syn_mice_completed_datasets(imp=imp, syn_cases=syn_cases)
    class(imp) <- c("synds")
    return(imp)
}
