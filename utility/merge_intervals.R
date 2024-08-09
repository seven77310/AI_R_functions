#' Merge Overlapping Intervals
#'
#' This function merges overlapping or close time intervals for each unique ID in a dataframe.
#' It ensures that any time intervals that are within a specified gap (in minutes) are combined into a single interval.
#'
#' @param df A dataframe containing the data.
#' @param uid A column name representing the unique ID for each group of intervals.
#' @param start_time A column name representing the start time of the intervals.
#' @param end_time A column name representing the end time of the intervals.
#' @param gap_minutes A numeric value specifying the gap (in minutes) within which intervals should be merged.
#' @return A dataframe with merged time intervals.
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 1, 2, 2),
#'   start_time = c('2023-01-01 10:00:00', '2023-01-01 10:30:00', '2023-01-01 11:00:00', '2023-01-01 12:00:00', '2023-01-01 12:30:00'),
#'   end_time = c('2023-01-01 10:15:00', '2023-01-01 10:45:00', '2023-01-01 11:30:00', '2023-01-01 12:15:00', '2023-01-01 12:45:00')
#' )
#' df <- df %>% mutate(across(c(start_time, end_time), lubridate::ymd_hms))
#' result <- merge_intervals(df, id, start_time, end_time, gap_minutes = 15)
#' print(result)
#' @author 
#' Xiaodan Liu <liux4@ccf.org>

merge_intervals <- function(df, uid, start_time, end_time, gap_minutes = 1) {
  uid <- enquo(uid)
  start_time <- enquo(start_time)
  end_time <- enquo(end_time)
  
  df <- df %>%
    # Select relevant columns and arrange by uid and start_time
    select(!!uid, !!start_time, !!end_time) %>%
    arrange(!!uid, !!start_time) %>%
    group_by(!!uid) %>%
    mutate(
      # Calculate the end time of the previous interval
      prev_end = lag(!!end_time, default = first(!!end_time)),
      # Calculate the gap between the current start time and the previous end time
      gap = as.numeric(difftime(!!start_time, prev_end, units = "mins")),
      # Determine groups: if gap is within the allowed range, keep the same group; otherwise, start a new group
      group = cumsum(ifelse(gap <= gap_minutes, 0, 1))
    ) %>%
    ungroup() %>%
    select(-prev_end, -gap)
  
  # Summarize the groups
  result <- df %>%
    group_by(!!uid, group) %>%
    summarise(
      # For each group, get the unique ID
      !!quo_name(uid) := first(!!uid),
      !!quo_name(start_time) := min(!!start_time),
      !!quo_name(end_time) := max(!!end_time),
      .groups = 'drop'
    )
  
  return(result)
}
