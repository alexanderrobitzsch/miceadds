## File Name: string_to_matrix.R
## File Version: 0.17

string_to_matrix <- function(x, rownames=NULL, col_elim=NULL, as_numeric=FALSE,
        diag_val=NULL, extend=FALSE, split=" ")
{
    x <- unlist(strsplit(x, split="\n", fixed=TRUE ))
    x <- gsub("\t", " ", x, fixed=TRUE )
    x <- x[ x!=""]

    #-- split vectors
    v1 <- sapply( x, FUN=function(vv){
                g1 <- strsplit( vv, split=split )[[1]]
                g1 <- g1[ g1 !="" ]
                return(g1)
                }, simplify=FALSE)
    NV <- length(v1)

    #-- dimensions
    sizes <- rep(NA, NV)
    for (vv in 1:NV){
        sizes[vv] <- length(v1[[vv]])
    }
    NR <- length(x)
    NC <- max(sizes)
    mat <- matrix("", nrow=NR, ncol=NC)
    for (vv in 1:NV){
        mat[ vv, seq(1,sizes[vv]) ] <- v1[[vv]]
    }

    if ( ! is.null(rownames) ){
        rownames(mat) <- mat[,rownames]
    }
    if ( ! is.null(col_elim) ){
        mat <- mat[, - col_elim ]
    }
    if ( as_numeric){
        NC <- ncol(mat)
        mat0 <- mat
        mat <- matrix(NA, nrow=nrow(mat0), ncol=ncol(mat0) )
        for (cc in 1:NC){
            mat[,cc] <- as.numeric(mat0[,cc])
        }
        rownames(mat) <- rownames(mat0)
    }
    if ( ! is.null(diag_val) ){
        diag(mat) <- diag_val
    }
    if ( extend ){
        mat0 <- mat
        mat0[ is.na(mat0) ] <- 0
        mat <- ( mat0 + t(mat0) )
        diag(mat) <- diag(mat0)
        colnames(mat) <- rownames(mat)
    }
    #--- output
    return(mat)
}
