## File Name: syn_mice_catch_arguments.R
## File Version: 0.160

syn_mice_catch_arguments <- function(arguments)
{
    pos <- parent.frame(n=2)
    #-- get vname
    vname <- ma_exists_get(x='vname', pos=pos, n_index=9 )
    #-- get function arguments
    dots <- ma_exists_get(x='dots', pos=pos, n_index=9 )
    arg_names <- names(dots)

    #-- create list
    AA <- length(arguments)
    mice_arg_list <- list()
    vals <- NULL
    for (aa in arguments){
        mice_arg_list[[aa]][[vname]] <- NULL
        if ( length(grep(aa,arg_names))>0){
            vals <- c(vals, aa)
        }
    }
    for (fun_name in vals){
        arg_name <- grep( fun_name, arg_names, value=TRUE)
        arg_split <- strsplit(arg_name, split=".", fixed=TRUE)[[1]]
        mth.args <- ma_exists_get(x='mth.args', pos=pos, n_index=9 )
        mth.args <- mth.args[[ arg_split[[1]] ]][[ fun_name ]]
        nn <- 2
        h1 <- NULL
        while (!is.null(h1)){
            h1 <- mice_arg_list[[ fun_name ]]
            mice_arg_list[[ fun_name ]] <- eval.parent(mth.args, n=nn)
            nn <- nn+1
        }
    }

    #- further search if mice_arg_list is empty
    if (length(mice_arg_list)==0){
        for (aa in arguments){
            aa_val <- ma_exists_get(x=aa, pos=pos, n_index=9 )
            mice_arg_list[[aa]] <- aa_val

        }
    }

    #--- output
    res <- list(vname=vname, mice_arg_list=mice_arg_list)
    return(res)
}
