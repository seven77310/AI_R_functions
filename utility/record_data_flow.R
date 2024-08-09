
record_data_flow <- function(dataset, patient_id, case_id, description) {
  # Ensure the required packages are loaded
  library(dplyr)
  
  # Count distinct patient ids and case ids
  patient_count <- dataset %>% distinct({{ patient_id }}) %>% nrow()
  case_count <- dataset %>% distinct({{ case_id }}) %>% nrow()
  
  # Define the flowchart data frame if it doesn't exist
  if (!exists("flowchart")) {
    flowchart <<- data.frame(
      patient_number = integer(),
      case_number = integer(),
      pop_crutial_desc = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # Check if the record already exists
  record_exists <- flowchart %>%
    filter(patient_number == patient_count,
           case_number == case_count,
           pop_crutial_desc == description) %>%
    nrow()
  
  # If the record does not exist, insert it
  if (record_exists == 0) {
    new_record <- data.frame(
      patient_number = patient_count,
      case_number = case_count,
      pop_crutial_desc = description,
      stringsAsFactors = FALSE
    )
    flowchart <<- bind_rows(flowchart, new_record)
  }
}


