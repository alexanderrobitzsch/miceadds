## File Name: syn_mice_completed_datasets.R
## File Version: 0.01

syn_mice_completed_datasets <- function(imp, syn_cases)
{
    m <- imp$m
    syn_data <- list()
    for (ii in 1:m){
        dat_ii <- mice::complete(imp, action=ii)
        syn_data[[ii]] <- dat_ii[ syn_cases, ]
    }
    if (m==1){
        syn_data <- syn_data[[1]]
    }
    return(syn_data)
}
