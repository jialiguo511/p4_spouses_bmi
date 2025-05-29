# Fisher text
get_or_ci_fisher <- function(data, cond) {
  female_var <- paste0("female_", cond)
  male_var <- paste0("male_", cond)
  
  or_matrix <- data %>%
    dplyr::filter(!is.na(.data[[female_var]]), !is.na(.data[[male_var]])) %>%
    mutate(
      female = as.integer(.data[[female_var]]),
      male   = as.integer(.data[[male_var]])
    ) %>%
    count(male, female) %>%
    pivot_wider(
      names_from = female, values_from = n, values_fill = 0,
      names_prefix = "female_"
    ) %>%
    arrange(male)
  
  tab <- matrix(
    c(
      or_matrix$female_1[or_matrix$male == 1],
      or_matrix$female_0[or_matrix$male == 1],
      or_matrix$female_1[or_matrix$male == 0],
      or_matrix$female_0[or_matrix$male == 0]
    ),
    nrow = 2,
    byrow = TRUE
  )
  
  ft <- fisher.test(tab)
  c(
    OR = round(unname(ft$estimate), 2),
    lower_95CI = round(ft$conf.int[1], 2),
    upper_95CI = round(ft$conf.int[2], 2)
  )
}

# Wald test
# get_or_ci <- function(data, cond) {
#   female_var <- paste0("female_", cond)
#   male_var <- paste0("male_", cond)
#   
#   # Create contingency table
#   or_matrix <- data %>%
#     dplyr::filter(!is.na(.data[[female_var]]), !is.na(.data[[male_var]])) %>%
#     mutate(
#       female = as.integer(.data[[female_var]]),
#       male   = as.integer(.data[[male_var]])
#     ) %>%
#     count(male, female) %>%
#     pivot_wider(
#       names_from = female, values_from = n, values_fill = 0,
#       names_prefix = "female_"
#     ) %>%
#     arrange(male)
#   
#   # Matrix for oddsratio()
#   table_matrix <- matrix(
#     c(
#       or_matrix$female_1[or_matrix$male == 1],
#       or_matrix$female_0[or_matrix$male == 1],
#       or_matrix$female_1[or_matrix$male == 0],
#       or_matrix$female_0[or_matrix$male == 0]
#     ),
#     nrow = 2,
#     byrow = TRUE,
#     dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
#   )
#   
#   # Calculate odds ratio and CI
#   or_result <- oddsratio(table_matrix, method = "wald")$measure
#   c(
#     OR = round(or_result[2, 1], 2),
#     `lower_95CI` = round(or_result[2, 2], 2),
#     `upper_95CI` = round(or_result[2, 3], 2)
#   )
# }