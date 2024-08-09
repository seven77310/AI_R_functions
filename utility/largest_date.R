#'
#' This function combines lists of days and hours, where hours are converted
#' to day-equivalents. It identifies the largest numeric day value and concatenates
#' non-numeric strings from the day list.
#'
#' @param days_values A list containing numeric day values and potentially non-numeric strings.
#' @param hours_values A list of hour values which will be converted to days.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{largest_days}: The largest numeric day value.
#'     \item \code{concatenated_string}: A single concatenated string of all non-numeric items.
#'   }
#'
#' @examples
#' days_example <- c(5, "discharge_date", 9)
#' hours_example <- c(48, 36, 72)
#' result <- largest_date(days_example, hours_example)
#' print(result)


largest_date <- function(days_values=NULL, hours_values=NULL) {
  # Convert hour values to days and append to day values
  days_values <- c(days_values, ceiling(hours_values / 24))
  
  # Initialize variables to store the largest day value and concatenated string
  largest_days <- NULL
  concatenated_string <- ""
  
  # Process the combined list of days
  for (value in days_values) {
    if (!is.na(as.numeric(value))) { 
      numeric_value <- as.numeric(value)
      if (is.null(largest_days) || numeric_value > largest_days) {
        largest_days <- numeric_value
      }
    }else {  # If value is not numeric, concatenate
      concatenated_string <- paste0(concatenated_string, value)
    }
  }
  
  # Output the results
  return(list(largest_days = largest_days, concatenated_string = concatenated_string))
}

# Example usage
days_example <- c(5, "discharge_date", 9)
hours_example <- c(48, 36, 72)

result <- largest_date(days_example,hours_example)
print(result)