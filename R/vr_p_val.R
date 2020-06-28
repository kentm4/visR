#' Adds p-values to tableone
#' 
#' @param x Vector for variable of interest
#' @param ... Additional arguments passed to other methods.
vr_p_val <- function(x, ...) UseMethod("vr_p_val")

#' P value test for numeric variables
#'
#' @param x Vector for variable of interest
#' @param group Vector with grouping variable data

vr_p_val.numeric <- function(x, group) {
    p_val <- kruskal.test(x, group)$p.value
    
    format_p_value(p_val)
}

#' P value test for factors variables
#'
#' @param x Vector for variable of interest
#' @param group Vector with grouping variable data

vr_p_val.factor <- function(x, group) {
    p_val <- chisq.test(x, group)$p.value
    
    format_p_value(p_val)
}

#' P value default
#'
#' @param x Vector for variable of interest
#' @param group Vector with grouping variable data

vr_p_val.default <- function(x, group) {
    "NA"
}


format_p_value <- function(p_val) {
    ifelse(p_val < 0.001, "<0.001", sprintf(p_val, fmt = '%#.2f'))
}