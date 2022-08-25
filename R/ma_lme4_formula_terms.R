## File Name: ma_lme4_formula_terms.R
## File Version: 0.157

ma_lme4_formula_terms <- function(formula)
{
    #** all variables
    all_vars <- all.vars(formula)
    t1 <- stats::terms(formula)
    term_labels <- attr( t1, "term.labels" )
    #** left side of formula
    formula_lhs <- all.vars( stats::update(formula, .~0))

    #** random effects
    ind_re <- grepl("\\|", term_labels)
    t1r <- term_labels[ ind_re ]
    NR <- length(t1r)
    formula_random <- list()
    random_effects_id <- list()
    for (rr in seq_len(NR)){
        s1 <- strsplit(t1r[[rr]], "\\|")[[1]]
        random_effects_id[[rr]] <- gsub( " ", "", s1[2])
        formula_random[[rr]] <- stats::as.formula( paste0( "~ ",
                                        paste0( s1[1], collapse=" + " ) ) )

    }
    names(formula_random) <- random_effects_id

    #** fixed effects
    t1f <- term_labels[ ! ind_re ]
    # formula fixed effects
    if (length(t1f)>0){
        rf1 <- paste0( t1f, collapse=" + " )
    } else {
        rf1 <- "1"
    }
    formula_fixed <- stats::as.formula( paste0( formula_lhs, " ~ ", rf1 ) )
    formula_fixed1 <- stats::as.formula( paste0( " ~ ", rf1 ) )

    #--- output
    res <- list( all_vars=all_vars, formula_fixed=formula_fixed,
                    formula_random=formula_random,
                    random_effects_id=random_effects_id, formula_lhs=formula_lhs,
                    terms_fe=t1f, terms_re=t1r, NR=NR, formula_fixed1=formula_fixed1)
    return(res)
}
