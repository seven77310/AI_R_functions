library(dplyr)
library(lubridate)

## !! Waiting further test
calculate_twa <- function(data, id_col, time_col, value_col, start_time = NULL, end_time = NULL) {
  # Convert character column names to symbols
  id_sym <- rlang::ensym(id_col)
  time_sym <- rlang::ensym(time_col)
  value_sym <- rlang::ensym(value_col)
  start_sym <- rlang::ensym(start_time)
  end_sym <- rlang::ensym(end_time)
  
  # Ensure time columns are in POSIXct format
  data[[time_col]] <- as.POSIXct(data[[time_col]], origin = "1970-01-01")
  if (!is.null(start_time) && !is.null(data[[start_time]])) {
    data[[start_time]] <- as.POSIXct(data[[start_time]], origin = "1970-01-01")
  }
  if (!is.null(end_time) && !is.null(data[[end_time]])) {
    data[[end_time]] <- as.POSIXct(data[[end_time]], origin = "1970-01-01")
  }
  
  # Filter data within the time window if start_time and end_time are provided
  if (!is.null(start_time) && !is.null(end_time)) {
    data <- data %>%
      filter(between({{ time_sym }}, {{ start_sym }}, {{ end_sym }}))
  }
  
  # Prepare data by removing rows with NA in key columns
  data <- data %>%
    filter(!is.na({{ time_sym }}) & !is.na({{ value_sym }})) %>%
    arrange({{ id_sym }}, {{ time_sym }})
  
  # Calculate intervals and average values
  data <- data %>%
    group_by({{ id_sym }}) %>%
    mutate(
      next_time = lead({{ time_sym }}),
      next_value = lead({{ value_sym }}),
      interval = as.numeric(difftime(next_time, {{ time_sym }}, units = "mins")),
      interval_mean = (next_value + {{ value_sym }}) / 2,
      leftstep_mean = {{ value_sym }},
      rightstep_mean = next_value
    ) %>%
    ungroup()
  
  # Calculate TWA using different methods
  results <- data %>%
    group_by({{ id_sym }}) %>%
    summarise(
      length = max({{ time_sym }}) - min({{ time_sym }}),
      mean_value = mean({{ value_sym }}, na.rm = TRUE),
      TWA = sum(interval_mean * interval, na.rm = TRUE) / length,
      LeftStep_TWA = sum(leftstep_mean * interval, na.rm = TRUE) / length,
      RightStep_TWA = sum(rightstep_mean * interval, na.rm = TRUE) / length,
      .groups = 'drop'
    )
  
  return(results)
}

# Example usage might need the actual variable names as strings passed to the function.
# results <- calculate_twa(df, 'id', 'time', 'value', 'start_time', 'end_time')