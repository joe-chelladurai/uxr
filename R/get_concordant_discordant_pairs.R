

#' Get concordant and discordant pairs for two variables
#'
#' @param .data = data
#' @param id = id column
#' @param var1 variable 1
#' @param var2 variable 2
#' @return a data frame
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr mutate case_when count select
#' @importFrom tibble deframe
#' @importFrom utils globalVariables
#' @export
#' @examples
#' mtcars$id <- seq.int(nrow(mtcars))
#' get_concordant_discordant_pairs(mtcars, id = id, var1 = vs, var2 = am)



get_concordant_discordant_pairs <- function(.data, id, var1, var2) {
  table <-   .data %>% pivot_longer(cols = -{{id}}) %>%
    mutate(value = as.numeric(factor(value))-1) %>%
    pivot_wider() %>%
    mutate(pairs = case_when(
      {{var1}} > {{var2}} ~ "b",
      {{var1}} < {{var2}} ~ "c",
      {{var1}} + {{var2}} == 2 ~ "a",
      {{var1}} + {{var2}} == 0 ~ "d")) %>%
    mutate(type = case_when(pairs == "a" ~ "concordant - var1=1; var2=1",
                            pairs == "d" ~ "concordant - var1=0; var2=0",
                            pairs == "b" ~ "discordant - var1=1; var2=0",
                            pairs == "c" ~ "discordant - var1=0; var2=1")) %>%
    count(pairs, type) %>% data.frame()


  selected <- table %>% select(-type) %>%
    deframe() %>% as.list()

  return(table)

}

