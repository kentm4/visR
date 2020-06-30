#' Adds p-values to tableone
#' 
#' @param x Vector for variable of interest
#' @param ... Additional arguments passed to other methods.
#' @noRd
calc_p_val <- function(x, ...) UseMethod("calc_p_val")

#' P value test for numeric variables
#'
#' @param x Vector for variable of interest
#' @param group Vector with grouping variable data
#' @noRd
calc_p_val.numeric <- function(x, group, p_val_test) {
    p_test <- match.fun(paste0("p_val_test.", p_val_test["continuous"]))
    format_p_value(p_test(x, group))
}

#' P value test for factors variables
#'
#' @param x Vector for variable of interest
#' @param group Vector with grouping variable data
#' @noRd
calc_p_val.factor <- function(x, group, p_val_test) {
    p_test <- match.fun(paste0("p_val_test.", p_val_test["factor"]))
    format_p_value(p_test(x, group))
}

#' P value default
#'
#' @param x Vector for variable of interest
#' @param group Vector with grouping variable data
#' @noRd
calc_p_val.default <- function(x, group, p_val_test) {
    "NA"
}

# Individual methods for p value tests
p_val_test.t.test <- function(x, group) {
    t.test(x ~ as.factor(group))$p.value
}

p_val_test.wilcox.test <- function(x, group) {
    wilcox.test(x ~ as.factor(group))$p.value
}

p_val_test.kruskal.test <- function(x, group) {
    kruskal.test(x, as.factor(group))$p.value
}

p_val_test.chisq.test <- function(x, group) {
    chisq.test(x, as.factor(group))$p.value
}

p_val_test.fisher.test <- function(x, group) {
    fisher.test(x, as.factor(group))$p.value
}

# a small helper function to format p value results
format_p_value <- function(p_val) {
    ifelse(p_val < 0.001, "<0.001", format(p_val, digits = 2))
}
