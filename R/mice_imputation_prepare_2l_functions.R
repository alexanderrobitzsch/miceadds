## File Name: mice_imputation_prepare_2l_functions.R
## File Version: 0.683

#############################################
# This preparation function is partly copied
# from the mice:::sampler function
mice_imputation_prepare_2l_functions <- function( vname, envir,
    use_formula=FALSE, frame=4, remove_lindep=TRUE, ... )
{
    x <- NULL
    keep <- NULL
    p_data <- ma_exists_get(x='data', pos=envir)
    p_predictorMatrix <- ma_exists_get(x='predictorMatrix', pos=envir)
    # newstate <- ma_exists_get('newstate', pos=envir)
    j <- ma_exists_get('j', pos=envir)
    # r <- get("r", envir=envir )
    r <- ma_exists_get('r', pos=envir)
    p_form <- NULL
    if ( use_formula ){
        p_form <- ma_exists_get('formulas', pos=envir)
    }
    calltype <- ma_exists_get('calltype', pos=envir)

    #*****************************************
    #****** start copy from mice
    # for a multilevel imputation method
    # predictors <- p_predictorMatrix[j, ] !=0

    # RB: formula-based specification
    if ( calltype=="formula" ) {
        myform <- paste(p_form[j], "0", sep="+")
        x <- stats::model.matrix( stats::formula(myform), p_data)
        ry <- r[, j]
        type <- 0
    }
    if (calltype=="type" ){
        # x <- p_data[, predictors, drop=FALSE]
        # y <- p_data[, j]
        # type <- p_predictorMatrix[j, predictors]
        # nam <- vname
        # ry <- r[, j]
        #***
        # copied and adapted from mice package
        # https://github.com/stefvanbuuren/mice/blob/master/R/sampler.R
        # function sampler.univ()
        data <- p_data
        type <- p_predictorMatrix[j, ]
        vars <- colnames(data)[type !=0]
        formula <- stats::reformulate(setdiff(vars, j), response=j)
        formula <- stats::update(formula, ". ~ . ")

        fcall <- paste0("x <- mice", paste0(rep(":",3), collapse=""),
                    "obtain.design(data=data, formula=formula)")
        eval(parse( text=fcall ))

        type <- type[labels(terms(formula))][attr(x, "assign")]
        x <- x[, -1L, drop=FALSE]
        names(type) <- colnames(x)
        # define y, ry and wy
        y <- data[, j]
        ry <- stats::complete.cases(x, y) & r[, j]
        wy <- NULL
        if (is.null(wy)) wy <- !ry
        # nothing to impute
        if (all(!wy)) return(numeric(0))
            # cc <- wy[where[, j]]
            # if (k==1L) check.df(x, y, ry)
        # remove linear dependencies
        if (remove_lindep){
            fcall <- miceadds_call_internal(pkg="mice", fct="remove.lindep",
                        args="(x=x, y=y, ry=ry, frame=frame, ...)", value="keep")
            eval(parse(text=fcall))
            x <- x[, keep, drop=FALSE]
            type <- type[keep]
        }
    }
    #****** END: copy from mice
    #*****************************************
    res <- list( y=y, x=x, ry=ry, type=type)
    return(res)
}
