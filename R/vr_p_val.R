#' Adds p-values to tableone
#' 
#' @param x Vector for variable of interest
#' @param ... Additional arguments passed to other methods.
#' @noRd
vr_p_val <- function(x, ...) UseMethod("vr_p_val")

#' P value test for numeric variables
#'
#' @param x Vector for variable of interest
#' @param group Vector with grouping variable data
#' @noRd
vr_p_val.numeric <- function(x, group, p_val_test) {
    p_test <- match.fun(p_val_test["continuous"])
    
    if(p_val_test["continuous"] %in% c('t.test', 'wilcox.test')) {
        p_val <- p_test(x ~ as.factor(group))$p.value
    }
    if(p_val_test["continuous"] %in% c('kruskal.test')) {
        p_val <- p_test(x, as.factor(group))$p.value
    }
    
    format_p_value(p_val)
}

#' P value test for factors variables
#'
#' @param x Vector for variable of interest
#' @param group Vector with grouping variable data
#' @noRd
vr_p_val.factor <- function(x, group, p_val_test) {
    p_test <- match.fun(p_val_test["factor"])
    
    p_val <- p_test(x, as.factor(group))$p.value
    
    format_p_value(p_val)
}

#' P value default
#'
#' @param x Vector for variable of interest
#' @param group Vector with grouping variable data
#' @noRd
vr_p_val.default <- function(x, group, p_val_test) {
    "NA"
}


format_p_value <- function(p_val) {
    ifelse(p_val < 0.001, "<0.001", format(p_val, digits = 2))
}
