## File Name: ANSI_create_table_check_variable_values.R
## File Version: 0.041


ANSI_create_table_check_variable_values <- function(variable, values, dat)
{
    v1 <- setdiff( values, na.omit(unique(dat[,variable])))
    if (length(v1)>0){
        g1 <- paste0("The following values of variable '", variable,
                "' are missing: ")
        g2 <- paste0(v1, collapse=" ")
        warning( paste0(g1, g2, "\n"))
    }

}
