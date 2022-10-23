

#' Get concordant and discordant pairs for two variables
#'
#' @param .data = data
#' @param id = id column
#' @param var_1 variable 1
#' @param var_2 variable 2
#' @return a data frame
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr mutate case_when count select
#' @importFrom stringr str_replace
#' @importFrom tibble deframe
#' @importFrom utils globalVariables
#' @export
#' @examples
#' mtcars$id <- seq.int(nrow(mtcars))
#' get_concordant_discordant_pairs(mtcars, id = id, var_1 = vs, var_2 = am)



get_concordant_discordant_pairs <- function(.data, id, var_1, var_2) {

  var_1_name <- deparse(substitute(var_1))
  var_2_name <- deparse(substitute(var_2))

  table <-   .data %>% pivot_longer(cols = -{{id}}) %>%
    mutate(value = as.numeric(factor(value))-1) %>%
    pivot_wider() %>%
    mutate(pairs = case_when(
      {{var_1}} > {{var_2}} ~ "b",
      {{var_1}} < {{var_2}} ~ "c",
      {{var_1}} + {{var_2}} == 2 ~ "a",
      {{var_1}} + {{var_2}} == 0 ~ "d")) %>%
    mutate(type = case_when(pairs == "a" ~ "concordant: var_1 = 1 & var_2 = 1",
                            pairs == "d" ~ "concordant: var_1 = 0 & var_2 = 0",
                            pairs == "b" ~ "discordant: var_1 = 1 & var_2 = 0",
                            pairs == "c" ~ "discordant: var_1 = 0 & var_2 = 1")) %>%
    mutate(type = str_replace(type, 'var_1', var_1_name)) %>%
    mutate(type = str_replace(type, 'var_2', var_2_name)) %>%
    count(pairs, type) %>% data.frame()


  selected <- table %>% select(-type) %>%
    deframe() %>% as.list()

  return(table)

}

