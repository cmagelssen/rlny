library(mlr3verse)
library(mlr3)

sample_average <- function(data) {
  d <- data
  
  # Filter the sessions we want to estimate from 
  d_choices <- filter(d, session == "block1" | session == "block2" | session == "block3")
  
  # Then estimate the action values using the sampling averaging method described in Sutton and Barlo (1999)
  action_values <- d_choices |>
    group_by(experiment, skier, strategy) |> #We group be experiment (skigroup), skiers and strategy
    summarise(action_value = round( mean(finish, na.rm = TRUE), digits = 2)) |> #We calculate the mean
    pivot_wider(names_from = strategy, values_from = action_value) #We pivot wider to get the estimates per skier in long format
  
  return(action_values)
}




find_best_strategy <- function(data) {
  # This takes the grouped dataset as input
  best_strategy <- data
  
  
  best_strategy <- best_strategy |>
    mutate(
      best_action_value = min(c_across(a:d)), # Find the value with lowest (best) values
      best_strategy_first = which_min(c_across(a:d), ties_method = "first"), # Report which of these that has the lowest value (number)
      best_strategy_last = which_min(c_across(a:d), ties_method = "last"),
      best_strategy_first = case_when(
        best_strategy_first == 1 ~ "a",
        best_strategy_first == 2 ~ "b",
        best_strategy_first == 3 ~ "c",
        best_strategy_first == 4 ~ "d",
        TRUE ~ as.character(best_strategy_first)
        ),
      best_strategy_last = case_when(
        best_strategy_last == 1 ~ "a",
        best_strategy_last == 2 ~ "b",
        best_strategy_last == 3 ~ "c",
        best_strategy_last == 4 ~ "d",
        TRUE ~ as.character(best_strategy_last)
      )) |>
    #select(-c(a,b,c,d)) |>
    ungroup() 
  


  return(best_strategy)
}



calculate_regret <- function(data) {
  
  regret <- data
  
  regret <- regret |>
    mutate(regret_a = a - best_action_value,
           regret_b = b - best_action_value,
           regret_c = c - best_action_value,
           regret_d = d - best_action_value
           )
  
  
  return(regret)
}











# 
# 
# d |>
#   group_by(experiment, treatment, session, strategy) |>
#   summarise(n = n())
# 
# 
# best_strategy <- d |>
#   group_by(experiment, skier, strategy) |>
#   summarise(action_value = mean(finish, na.rm = TRUE)) |>
#   pivot_wider(names_from = strategy, values_from = action_value) |>
#   mutate(
#     best_action_value = min(c_across(a:d)),
#     best_strategy = which.min(c_across(a:d)),
#     best_strategy = case_when(
#       best_strategy == 1 ~ "a",
#       best_strategy == 2 ~ "b",
#       best_strategy == 3 ~ "c",
#       best_strategy == 4 ~ "d",
#       TRUE ~ as.character(best_strategy) 
#     )) |>
#   select(-c(a,b,c,d)) |>
#   ungroup()
