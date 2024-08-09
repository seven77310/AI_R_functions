#' Calculate Event Lengths
#'
#' This function calculates the duration of specified events within a dataset, grouping by a unique identifier and filtering for a given event name.
#'
#' @param df A dataframe containing the event data.
#' @param uid A string representing the column name for unique identifiers (e.g., final_op_sys).
#' @param event_name A string representing the column name for event names.
#' @param event_time A string representing the column name for event timestamps.
#' @param filter_event A string representing the specific event name to filter and calculate the duration (e.g., 'CP Bypass Ended').
#'
#' @return Returns a dataframe with two columns: 
#' \describe{
#'   \item{uid}{The unique identifier for each group.}
#'   \item{cpb_length}{The total duration (in minutes) of the specified events for each unique identifier.}
#' }

event_length <- function(df, uid, event_name, event_time,filter_event) {
  uid <- sym(uid)
  event_name <- sym(event_name)
  event_time <- sym(event_time)
  
  df_1 <- df %>%
    group_by(!!uid) %>%
    arrange(!!event_time) %>%
    mutate(previous_event_name = lag(!!event_name)) %>%
    filter(is.na(!!event_name) | is.na(previous_event_name) | !!event_name != previous_event_name) %>%
    mutate(
      next_event_name = lead(!!event_name),
      next_event_time = lead(!!event_time),
      event_length = as.numeric(difftime(next_event_time, !!event_time, units = "mins"))
    ) %>% 
    select(-previous_event_name) %>% 
    filter(next_event_name == !!filter_event) %>%
    group_by(!!uid) %>% 
    summarise(event_length = sum(event_length, na.rm = TRUE)) %>% 
    clean_names()
  
  return(df_1)
}