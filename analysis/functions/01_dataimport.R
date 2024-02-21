


readcsv <- function(directory_path) {
  
  current_dir <- getwd()  # Get the current working directory
  
  on.exit(setwd(current_dir)) # Return to the current directory once finished
  
  
  setwd(directory_path)
  csv_files <- list.files(pattern = "*.csv") # Find all the csv files in the folder
  
  data_list <- list()  # Initialize data_list as an empty list
  
  
  for (file in csv_files) { # Iterate over the csv files
    print(file)
    data <- read_csv(file, skip = 2) # Skip 2 rows
    data <- rename(data, sta = `STA#`, run = `RUN#`, skier = `BIB#`, tod = TOD, comment = `SKI#`, int1 = `INTER 1`, int2 =  `INTER 2`, int3 = `INTER 3`, int4 =  `INTER 4`, finish =  FINISH, strategy = COMMENT) # rename column names from 
    
    file_info <- strsplit(file, "_")[[1]]
    experiment <- file_info[2]
    date <- file_info[5]
    group <- file_info[3]
    session <- file_info[4]
    
    data$experiment <- experiment
    data$group <- group
    data$session <- session
    
    data <- data %>%
      mutate(
        experiment = str_replace(experiment, "group-", ""),
        session = case_when(
          session == "session-1" ~ "block1",
          session == "session-2" ~ "block2",
          session == "session-3" ~ "block3",
          TRUE ~ session
        ),
        strategy = tolower(strategy)
      )
    
    data <- data %>%
      mutate(strategy = case_when(
        strategy == "nsns" ~ "ns",
        strategy == "sgsg" ~ "sg",
        strategy == "aa" ~ "a",
        strategy == "bb" ~ "b",
        strategy == "cc" ~ "c",
        strategy == "dd" ~ "d",
        TRUE ~ strategy
      ))
    
    experiment_name <- data$experiment[1]
    
    skier_info <- read.xlsx(paste0("experimentinfo_", experiment_name, ".xlsx")) # This finds the info of the skier in a excel file and attaches it to the tibble
    data <- data %>%
      left_join(skier_info, by = "skier")
    
    data_list[[file]] <- data
  }
  
  combined_data <- bind_rows(data_list)
  
  combined_data <- combined_data |>
    mutate(skier = as.character(skier))
  
  return(combined_data)
}
