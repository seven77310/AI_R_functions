
#' Check Lab Names Function
#' 
#' This function reads a SAS dataset of laboratory IDs and compares it with a provided list of lab names. 
#' It identifies which lab names are present in the SAS dataset and which are not. 
#' The function assumes case sensitivity could be an issue and converts all input lab names to uppercase.
#'
#' @param lab_list A character vector containing lab names to be checked.
#' @return A list containing two data frames: `lab_id_check` with lab names found in the SAS dataset,
#'         and `unmatched_lab_list` with lab names not found.
#' @examples
#' lab_list <- c("ALBUMIN", "ALP", "ALT", "AST", "ACT", "APTT", "BE")
#' results <- check_lab_names(lab_list)
#' print("Matched Lab Names:")
#' print(results$lab_id_check)
#' print("Unmatched Lab Names:")
#' print(results$unmatched_lab_list)
#'
#' @note This function reads data from a SAS file located at a network path.
#'       Ensure you have access to the path and necessary read permissions.
check_lab_names <- function(lab_list) {
  
  lab_id_list_df <- read_sas('//lrismb00.lerner.ccf.org/proj/anesthesiology/EDV/SAS/EDV_id_list/lab_id_list.sas7bdat')
  # Check if the lab_list is empty
  if (length(lab_list) == 0) {
    cat("WARNING: lab_list is empty, lab_id_check and unmatched_lab_list tables not created.\n")
    return(invisible(NULL))
  }
  
  lab_list <- toupper(lab_list)
  
  # Filter matching and non-matching lab names
  lab_id_check <- lab_id_list_df %>%
    filter(lab_name %in% lab_list)
  
  unmatched_lab_list <- data.frame(lab_name = setdiff(lab_list, lab_id_list_df$lab_name))
  
  list(lab_id_check = lab_id_check, unmatched_lab_list = unmatched_lab_list)
}