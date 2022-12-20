

## Get pairs from wide data



A <- c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1)
B <- c(0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0)

data <- data.frame(A, B)



check_two_levels <- function(data, x, y) {
  
  result <- data |> 
    summarise(x = n_distinct({{x}}, na.rm = TRUE),
              y = n_distinct({{y}}, na.rm = TRUE)) |> as.list()
  
  if(result$x > 2 | result$y > 2) {
    stop("The unique values in the column should not be more than 2")
  }
}



dichotomize <- function(data, x, y) {
  data |> 
    select({{x}}, {{y}}) |> 
    mutate(across(.fns = factor)) |> 
    mutate(across(.fns = as.numeric)) |> 
    mutate(across(.fns = ~ .x -1)) 
}



add_pairs_type <- function(data, x, y) {
  data |> 
    mutate(pairs = case_when(
      {{x}} > {{y}} ~ "b",
      {{x}} < {{y}} ~ "c",
      {{x}} + {{y}} == 2 ~ "a",
      {{x}} + {{y}} == 0 ~ "d")) |>
    mutate(type = case_when(pairs == "a" ~ "concordant: x = 1 & y = 1",
                            pairs == "d" ~ "concordant: x = 0 & y = 0",
                            pairs == "b" ~ "discordant: x = 1 & y = 0",
                            pairs == "c" ~ "discordant: x = 0 & y = 1"))
}




format_pairs_type <- function(data) {
  data |> 
    separate(type, into = c("type", "combination"), sep = ":") |> 
    separate(combination, into = c("x", "y"), sep = "&") |> 
    mutate(x = str_remove(x, "x = "),
           y = str_remove(y, "y = "))
}



get_pairs_from_wide <- function(data, x, y, output = "list") {
  
  check_two_levels(data, {{x}}, {{y}})
  
  data <- data |> 
    dichotomize({{x}}, {{y}}) |> 
    add_pairs_type({{x}}, {{y}}) |> 
    count(pairs, type) 
  
  if(output == "list") {
    output <- data |> 
      select(pairs, n) |> 
      deframe() |> as.list()
  } else if(output == "dataframe") {
    output <- data |> 
      format_pairs_type()
  }
  
  return(output)
}

# get_pairs_from_wide(data, A, B, "list")

#  TO DO: 
  
#  What if there are 0s in count(pairs, type)
#  Tested: It returns NA as a list item. Does not affect anything.
#  Also added dichotomize and check levels functions.


