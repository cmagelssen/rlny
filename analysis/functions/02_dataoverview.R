set_highlow_values_to_na <- function(data) {
  
  d <- data
  
  d <- d |>
    mutate(
      finish = ifelse(finish > 20, NA, 
               ifelse(finish < 12, NA, finish))
      )
  

  
  return(d)
}



add_lostdata <- function(data) {
  
  d <- data
  
  relative_path <- "../../data/data_raw/information"
  
  # Combine the relative path with the file name to create the destination path
  path <- file.path(relative_path, "lostdata.xlsx")
  lostdata <- read_excel(path, skip = 2)
  lostdata <- lostdata |>
    mutate(skier = as.character(skier),
           experiment = as.character(experiment))
  
    
  for (i in 1:nrow(lostdata)) {
    new_row <- data.frame(
      sta = lostdata$sta[i],
      run = lostdata$run[i],
      skier = lostdata$skier[i],
      tod = lostdata$tod[i],
      comment = lostdata$comment[i],
      int1 = lostdata$int1[i],
      int2 = lostdata$int2[i],
      int3 = lostdata$int3[i],
      int4 = lostdata$int4[i],
      finish = lostdata$finish[i],
      strategy = lostdata$strategy[i],
      experiment = lostdata$experiment[i],
      group = lostdata$group[i],
      session = lostdata$session[i],
      treatment = lostdata$treatment[i],
      coach = lostdata$coach[i]
    )
    
    # Append the new row to the original dataframe d
    d <- rbind(d, new_row)
  }
  return(d)
  
}


remove_errordata <- function(data) {
  
  d <- data
  
  relative_path <- "../../data/data_raw/information"
  
  # Combine the relative path with the file name to create the destination path
  path <- file.path(relative_path, "data-errors.xlsx")
  
  errordata <- read_excel(path, skip = 2)
  errordata <- errordata |>
    mutate(
      skier = as.character(skier),
      experiment = as.character(experiment)
      )
  
  
  for (i in 1:nrow(errordata)) {
    condition <- d$experiment == errordata$experiment[i] &
      d$skier == errordata$skier[i] &
      d$session == errordata$session[i] &
      d$run == errordata$run[i]
    
    d <- d %>%
      filter(!condition)  # Remove rows that match the condition
  }
  
  return(d)
  
}


swap_strategies <- function(data, skier, experiment, session) {
  
  d <- data %>%
    filter(skier == skier, experiment == experiment, session == session) %>%
    mutate(strategy = ifelse(run == 8, "d", ifelse(run == 9, "b", strategy)))
  
  return(d)
  
}


change_session <- function(data) {
  d <- data
  
  d <- d |>
    mutate(session = case_when(
      experiment == "a" & session == "retention" & hour(tod) < 12 ~ "retention",
      experiment == "a" & session == "retention" & hour(tod) >= 12 ~ "transfer",
      experiment == "a" & session == "baseline" & hour(tod) < 16 ~ "baseline",
      experiment == "a" & session == "baseline" & hour(tod) >= 16 ~ "block1",
      
      TRUE ~ session  # Keep the existing values if conditions are not met
    ))
  
  return(d)
  
  
}

mark_dnf_as_na <- function(data) {
  d <- data
  
  relative_path <- "../../data/data_raw/information"
  
  # Combine the relative path with the file name to create the destination path
  path <- file.path(relative_path, "data-dnf.xlsx")
  
  dnfdata <- read_excel(path, skip = 2)
  dnfdata <- dnfdata |>
    mutate(
      skier = as.character(skier),
      experiment = as.character(experiment)
    )
  
  
  for (i in 1:nrow(dnfdata)) {
    condition <- d$experiment == dnfdata$experiment[i] & d$skier == dnfdata$skier[i] & d$session == dnfdata$session[i] & d$run == dnfdata$run[i]
    d$finish[condition] <- NA
    
    #d <- d %>%
     # filter(!condition)  # Remove rows that match the condition
  }
  

  
  return(d)
  
  
}

remove_screen_trials <- function(data) {
  d <- data
  
  relative_path <- "data"
  
  # Combine the relative path with the file name to create the destination path
  path <- file.path(relative_path, "screenedtrials.xlsx")
  
  screeneddata <- read_excel(path, skip = 2)
  screeneddata <- screeneddata |>
    mutate(
      skier = as.character(skier),
      experiment = as.character(experiment)
    )
  
  
  for (i in 1:nrow(screeneddata)) {
    condition <- d$experiment == screeneddata$experiment[i] & d$skier == screeneddata$skier[i] & d$session == screeneddata$session[i] & d$run == screeneddata$run[i]
    d$finish[condition] <- NA
    
    #d <- d %>%
    # filter(!condition)  # Remove rows that match the condition
  }
  
  
  
  return(d)
  
  
}



exclude_skiers <- function(data, experiment, skier) {

  d <- data
  experiment_exclude <- as.character(experiment)
  skier_exclude <- as.character(skier)
  
  d <- d |>
    filter(!(experiment == experiment_exclude & skier == skier_exclude))
  
  return(d)
  
  
}


change_sg_trial <- function(data, skier, strategy, session, experiment, replacesession) {
  d <- data
  
  skier_filtered <- d |>
    filter(skier == skier, strategy == strategy, session == session, experiment == experiment)
  
  d <- d %>%
    mutate(finish = ifelse(skier == skier & strategy == strategy & session == replacesession & experiment == experiment, skier_filtered$finish, finish))
  
  return(d)
}



replace_strategies_with_na <- function(data, experiment_cond, skier_cond, session_cond, runs_to_replace_cond) {
  
  d <- data %>%
    mutate(
      strategy = case_when(
        experiment == experiment_cond & 
          skier == skier_cond & 
          session == session_cond &
          run %in% runs_to_replace_cond ~ NA,
        TRUE ~ strategy  # Keep the original strategy if conditions don't match
      )
    )
  
  return(d)
}


replace_finish_with_na <- function(data, experiment_cond, skier_cond, session_cond, run_cond) {
  
  d <- data %>%
    mutate(
      finish = case_when(
        experiment == experiment_cond & 
          skier == skier_cond & 
          session == session_cond &
          run == run_cond ~ NA,
        TRUE ~ finish  # Keep the original strategy if conditions don't match
      )
    )
  
  return(d)
}
