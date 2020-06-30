#' Create Summary Table (also known as Table 1)
#' 
#' @description Create a summary table of descriptive statistics from a data frame or tibble. 
#' 
#' By default the following summary stats are calculated:
#' * Numeric variables: mean, min, 25th-percentile, median, 75th-percentile, maximum, standard deviation
#' * Factor variables: proportion of each factor level in the overall data set
#' * Default: number of unique values and number of missing values
#'
#' @param dat The data set to summarize as data frame or tibble
#' @param group_columns Stratifying/Grouping variable name(s) as character vector.
#' If NULL, only overall results are returned
#' @param overall If TRUE, the summary statistics for the overall data set are also calculated 
#' @param summary_function A function defining summary statistics for numeric and categorical values
#' @param add_p_val A logical value to add p values if a single grouping column is used.
#' @param p_val_test A named vector indicating which test to use for continuous variables
#' and for factor variables (e.g. c("continuous" = "t.test", "factor" = "chisq.test")).
#' The default is `"kruskal.test"` and `"chisq.test"` All options include:
#' * `"t.test"` for a t-test
#' * `"wilcox.test"` for a Wilcoxon rank-sum test
#' * `"kruskal.test"` for a Kruskal-Wallis rank-sum test
#' * `"chisq.test"` for a chi-squared test of independence
#' * `"fisher.test"` for a Fisher's exact test
#' @details It is possible to provide your own summary function. Please have a loot at vr_summary for inspiration.
#' 
#' @note All columns in the table will be summarized. 
#' If only some columns shall be used, please select only those
#' variables prior to creating the summary table by using dplyr::select()
#' 
#' @export
#' 
#' @examples
#' library(survival)
#' library(dplyr)
#' ovarian %>% 
#' select(-fustat) %>% 
#'   mutate(age_group = factor(case_when(age <= 50 ~ "<= 50 years",
#'                                       age <= 60 ~ "<= 60 years",
#'                                       age <= 70 ~ "<= 70 years",
#'                                       TRUE ~ "> 70 years")),
#'          rx = factor(rx),
#'          ecog.ps = factor(ecog.ps)) %>% 
#'   select(age, age_group, everything()) %>% 
#'   vr_create_tableone()

vr_create_tableone <- function(dat,
                               group_columns = NULL, overall = TRUE,
                               summary_function = vr_summarize_tab1,
                               add_p_val = FALSE,
                               p_val_test = NULL) {
  
  if (is.null(group_columns) & !overall) {
    stop("Overall and group columns can not both be missing/False")
  }
  
  summary_FUN <- match.fun(summary_function)
  
  if(overall) {
    table1_overall <- create_tableone_data(dat, summary_function = summary_FUN)
  }
  
  if(!is.null(group_columns)){
    table1_grouped <- create_tableone_data(dat,
                                           group_columns = group_columns,
                                           summary_function = summary_FUN)
  }
  
  # create final table based on user input
  if(overall & is.null(group_columns)) {
    table1 <- table1_overall
    
  } else if (!overall & !is.null(group_columns)) {
    table1 <- table1_grouped
    
  } else {
    table1 <- table1_overall %>%
      left_join(table1_grouped, by = c("variable", "statistic"))
    
  }
  
  # Add p-value calculations
  if (add_p_val & length(group_columns) == 1) {
    if(!is.null(p_val_test)) {
      valid_cont_test <- c("t.test", "wilcox.test", "kruskal.test")
      valid_factor_tests <- c("chisq.test", "fisher.test")
      
      if (!p_val_test["continuous"] %in% valid_cont_test) {
        stop(paste0("Continuous p-value test must be ",
                    paste0(valid_cont_test, collapse = ", ")))
      }
      
      if (!p_val_test["factor"] %in% valid_factor_tests) {
        stop(paste0("Factor p-value test must be ",
                    paste0(valid_factor_tests, collapse = ", ")))
      }
      
    } else {
      p_val_test <- c("continuous" = "kruskal.test",
                      "factor" = "chisq.test")
    }

    p_vals <- dat %>%
      dplyr::summarise_all(vr_p_val, dat[[group_columns]],
                           p_val_test) %>%
      tidyr::gather(
        key = "variable",
        value = "p.value"
      )
    
    table1 <- table1 %>%
      dplyr::left_join(p_vals, by = "variable") %>%
      # only keep first p.value record for each variable
      group_by(variable) %>%
      mutate(p.value = if_else(
        !is.na(p.value) & row_number() == 1, p.value, "")
      ) %>%
      ungroup()
  }
  
  # get and add variable labels if they have labels set
  var_labels <-  sapply(names(dat), function(x) {
    ifelse(
      is.null(attr(dat[[x]], "label")), 
      NA, attr(dat[[x]], "label"))
  })
  
  var_labels <- tibble::enframe(var_labels,
                                name = "variable", value = "Label")
  
  table1 <- table1 %>%
    dplyr::left_join(var_labels, by = "variable") %>%
    dplyr::mutate(Label = ifelse(is.na(Label), variable, Label)) %>%
    dplyr::select(Label, statistic, everything(), -variable)
  
  return(table1)
  
}

#' Create summary table one data
#'
#' @param dat The data set to summarize as data frame or tibble
#' @param group_columns Stratifying/Grouping variable name(s) as character vector.
#' If NULL, only overall results are returned
#' @param summary_function A function defining summary statistics for
#' numeric and categorical values
#' @noRd

create_tableone_data <- function(dat, group_columns = NULL,
                                 summary_function = vr_summarize_tab1) {
  
  if(is.null(group_columns)){
    dat <- dat %>% 
      dplyr::mutate(all = "Overall")
    group_columns <- c("all")
  }
  
  dat <- dat %>%
    dplyr::group_by(.dots = group_columns)
  
  dat_n <- dat %>%
    dplyr::summarise(summary = dplyr::n()) %>%
    tidyr::pivot_wider(
      names_from = tidyselect::any_of(group_columns),
      values_from = "summary"
    ) %>%
    dplyr::mutate(variable = "Sample", summary_id = "N")
  
  dat_summary <- dat %>%
    dplyr::summarise_all(summary_function) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = setdiff(names(.), group_columns),
      names_to = "variable",
      values_to = "summary"
    ) %>%
    tidyr::unnest_longer(summary) %>%
    tidyr::pivot_wider(
      names_from = tidyselect::any_of(group_columns),
      values_from = "summary"
    )
  
  dat_table1 <- rbind(dat_n, dat_summary) %>%
    dplyr::rename(statistic = summary_id) %>%
    dplyr::select(variable, statistic, everything())
  
  return(dat_table1)
}