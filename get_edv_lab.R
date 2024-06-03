
# --------------------------------------------------
# Package Management
# --------------------------------------------------
#Ensuring Package Installation in R

install_package<- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  
}

# List of packages to be installed 
pkg_list <- c('tidyverse', 'lubridate', 'haven', 'readxl', 
              'DBI', 'RODBC', 'summarytools', 'snakecase', 
              'sqldf', 'reshape2', 'dplyr', 'janitor', 
              'anytime', 'questionr', 'fs', 'redcapAPI','Hmisc',
              'odbc','stringr')

# Run the function with the package list
install_package(pkg_list)

# Load library
library('tidyverse')
library('lubridate')
library('haven')
library('readxl')
library('DBI')
library('RODBC')
library('summarytools')
library('snakecase')
library('sqldf')
library('reshape2')
library('dplyr')
library('janitor')
library("anytime") 
library("questionr")
library("fs")
library("redcapAPI")     
library("Hmisc")
library("odbc")
library("stringr")

library(dplyr)
library(DBI)
library(odbc)
library(stringr)
library(RODBC)
library(rstudioapi)
library(devtools)
library(roxygen2)



inds <- 'DL_ANESTHESIA_RESEARCH.liux4_p333335_id'
lab_list <- "ALBUMIN ALP ALT AST"
time_var <- 'final_dos'
cutoff_start <-30
cutoff_end <-0
pulldata == 1
outds <- 'preop_edv_lab'



#————————————————————————————————————————————————————————————————————————————————

#' Check and Establish Connection to EDV Database
#'
#' This function checks if a connection to the EDV database already exists and is valid.
#' If no valid connection exists, it establishes a new one.
#'
#' @return Returns an ODBC connection object to the EDV database.
#' @examples
#' \dontrun{
#' edv <- check_and_connect_edv()
#' # Use the `edv` connection for your queries
#' }
check_and_connect_edv <- function() {

  # Check if the connection already exists and is valid
  if (!exists("edv") || is.null(edv)) {
    message("Establishing a new connection to the EDV database...")
    
    edv <- tryCatch({
      RODBC::odbcConnect(
        dsn = ifelse(Sys.info()['sysname'] == "Windows", "edv", "tdprod1"),
        uid = Sys.getenv("USERNAME"),
        pwd = askForPassword("Please enter your database password:")
      )
    }, error = function(e) {
      message("Failed to establish a connection to the EDV database: ", e$message)
      return(NULL)
    })
    
    if (!is.null(edv)) {
      message("Connection to the EDV database established successfully.")
    } else {
      stop("Failed to establish a connection to the EDV database.")
    }
  } else {
    # Check if the existing connection is still valid by running a simple query
    tryCatch({
      test_query <- sqlQuery(edv, "SELECT 1")
      if (inherits(test_query, "try-error")) {
        stop("The existing connection is no longer valid.")
      } else {
        message("Connection to the EDV database already exists and is valid.")
      }
    }, error = function(e) {
      message("The existing connection is no longer valid. Re-establishing...")
      edv <<- RODBC::odbcConnect(
        dsn = ifelse(Sys.info()['sysname'] == "Windows", "edv", "tdprod1"),
        uid = Sys.getenv("USERNAME"),
        pwd = askForPassword("Please enter your database password:")
      )
      if (inherits(edv, "try-error")) {
        stop("Failed to re-establish a connection to the EDV database.")
      } else {
        message("Re-established connection to the EDV database successfully.")
      }
    })
  }
  
  return(edv)
}


edv <- check_and_connect_edv()



# -----------------------------------------------------------------------------
# resize function
resize <- function(df) {
  df %>%
    mutate_if(is.character, ~ str_trim(.)) %>%
    mutate_if(is.character, ~ str_pad(., min(nchar(.)), side = "right"))
}


#' Format Input List
#'
#' This function takes a string of terms, possibly separated by spaces, commas, or semicolons,
#' normalizes it by removing unwanted characters and excess spaces, and then formats it into a standardized
#' string where each term is uppercase, enclosed in single quotes, and separated by commas.
#'
#' @param input_list A character string containing terms, which can be separated by spaces, commas, or semicolons.
#' @return Returns a character string with each term in uppercase, enclosed in single quotes and separated by commas.
#' @examples
#' input_list_1 <- "ALBUMIN ALP ALT AST"
#' input_list_2 <- "ALBUMIN,ALP,ALT,AST;"
#' input_list_3 <- "ALBUMIN  ALP,  ALT, AST;"
#' input_vector_4 <- c("ALBUMIN", "ALP", "ALT", "AST")
#' 
#' formatted_list_1 <- format_input_list(input_list_1)
#' formatted_list_2 <- format_input_list(input_list_2)
#' formatted_list_3 <- format_input_list(input_list_3)
#' formatted_list_4 <- format_input_list(input_vector_4)
#'
#' print(formatted_list_1)  # Output: "'ALBUMIN', 'ALP', 'ALT', 'AST'"
#' print(formatted_list_2)  # Output: "'ALBUMIN', 'ALP', 'ALT', 'AST'"
#' print(formatted_list_3)  # Output: "'ALBUMIN', 'ALP', 'ALT', 'AST'"
#' print(formatted_list_4)  # Output: "'ALBUMIN', 'ALP', 'ALT', 'AST'"
#' 
format_input_list <- function(input) {
  # Check if input is a single string or a vector of strings
  if (is.character(input) && length(input) == 1) {
    # If a single string, proceed with original cleaning and formatting
    input <- trimws(input)  # Trim any extra whitespace
    input <- gsub("\\s*[;,]+\\s*", ",", input)  # Normalize delimiters to single commas
    lab_list <- strsplit(input, ",")[[1]]
  } else if (is.character(input) && length(input) > 1) {
    # If already a vector of strings, just assign directly
    lab_list <- input
  } else {
    # If input is neither, return an error message
    stop("Invalid input: Please provide a character string or a vector of character strings.")
  }
  
  # Trim whitespace from each lab name
  lab_list <- trimws(lab_list)
  
  # Surround each lab name with single quotes and join into a single string
  formatted_list <- paste0("'", lab_list, "'", collapse = ", ")
  
  return(formatted_list)
}





#------------------------------------------------------------------------------
# !! maybe we don't need to use outds paramater, can just return outs table
# !! lack function description


get_edv_lab <- function(inds, lab_list, time_var, cutoff_start, 
                        cutoff_end, COMPONENT_ID=NULL) {
  
  # Step 1: Process the lab list
  name_list <- format_input_list(lab_list)
  
  # check or connect to edv
  edv <- check_and_connect_edv()
  
  # Step 2: Execute the SQL query and store the result
  # Construct the SQL query
  sql_query <- paste0("
    SELECT a.*, 
           d.ORD_PROC_DESC, d.ORD_PROC_TYPE_CD, d.ORD_PROC_TYPE_DESC,
           d.ORD_PROC_SPECIMEN_DTTM, d.SPECIMEN_TYPE_CD, d.SPECIMEN_TYPE_DESC, d.DISPLAY_NM,
           ord.ORD_RSLT_DTTM, ord.ORD_RSLT_COMP_ID, ord.ORD_RSLT_COMP_EXT_NM, ord.ORD_RSLT_VALUE, ord.ORD_RSLT_COMP_UNIT,
           f.*
    FROM ", inds, " AS a
    INNER JOIN IHAA_EDV.HR_ORD_PROC_PRIMARY AS c ON a.K_PAT_KEY = c.K_PAT_KEY
    INNER JOIN IHAA_EDV.BK_ORD_PROC AS d ON c.K_ORD_KEY = d.K_ORD_KEY
    INNER JOIN IHAA_EDV.BK_ORD_RESULTS AS ord ON d.K_ORD_KEY = ord.K_ORD_KEY
    INNER JOIN DL_ANESTHESIA_RESEARCH.lab_id_list AS f ON ord.ord_rslt_comp_id = f.ord_rslt_comp_id
    WHERE ")
  
  # Add conditions for lab_list and COMPONENT_ID if provided
  if (length(lab_list) > 0) {
    sql_query <- paste0(sql_query, "UPPER(f.lab_name) IN (", name_list, ") AND ")
  }
  
  if (!is.null(COMPONENT_ID) && nchar(COMPONENT_ID) > 0) {
    sql_query <- paste0(sql_query, "f.ord_rslt_comp_id IN (", COMPONENT_ID, ") AND ")
  }
  
  if (!is.na(as.numeric(cutoff_start))) {
    cutoff_start_1 <- as.numeric(cutoff_start)
    cutoff_end_1 <- as.numeric(cutoff_end)
    sql_query <- paste0(sql_query, "
      CAST(d.ORD_PROC_SPECIMEN_DTTM AS DATE) BETWEEN 
      (CAST(a.", time_var, " AS DATE) - INTERVAL '", cutoff_start_1, "' DAY) AND 
      (CAST(a.", time_var, " AS DATE) + INTERVAL '", cutoff_end_1, "' DAY) ")
  } else {
    sql_query <- paste0(sql_query, "
      CAST(d.ORD_PROC_SPECIMEN_DTTM AS DATE) BETWEEN 
      CAST(a.", cutoff_start, " AS DATE) AND CAST(a.", cutoff_end, " AS DATE) ")
  }
    edv_lab <- sqlQuery(edv,sql_query,as.is = TRUE)
  
  # Step 3: Resize character variables to their minimal lengths and output

  edv_lab <- resize(edv_lab)
  return(edv_lab)
  
}


#example use
get_edv_lab(inds = 'DL_ANESTHESIA_RESEARCH.liux4_p333335_id',
            lab_list ="ALBUMIN ALP ALT AST",
            time_var = 'final_dos',
            cutoff_start =30,
            cutoff_end =0
            )


inds = 'DL_ANESTHESIA_RESEARCH.liux4_p333335_id'
outds = 'preop_edv_lab'
lab_list ="ALBUMIN ALP ALT AST"
time_var = 'final_dos'
cutoff_start =30
cutoff_end =0
COMPONENT_ID=NULL


# Example usage:
# get_edv_lab(inds = "your_data_table", outds = "edv_lab_output", uid_var = "unique_id", 
#             lab_list = "lab1 lab2 lab3", time_var = "start_time", cutoff_start = 0, cutoff_end = 7)



# -----------------------------------------------------------------------------

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


# -----------------------------------------------------------------------------

combine_datasets<- function(datasets, outds) {
  # Filter out non-existing datasets first
  existing_datasets <- Filter(function(x) exists(x, where = .GlobalEnv), datasets)
  
  dataset_list <- lapply(existing_datasets, get)
  
  if (length(dataset_list) > 0) {

    combined_dataset <- do.call(rbind, dataset_list)
    
    # Remove duplicates
    combined_dataset <- combined_dataset[!duplicated(combined_dataset), ]

    assign(outds, combined_dataset, envir = .GlobalEnv)
    
    return(combined_dataset)
  } else {
    message("NOTE: No datasets to combine into ", outds)
  }
}

# Example usage as above

#----------------------------------------------------------------------------

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

#------------------------------------------------------------------------------
# function to generate arks lab link table 
io_lab_arks_linktb <- function(){
  # check or connect to edv
  edv <- check_and_connect_edv()
  
  sql_query <- paste0("
    select *
    from DL_ANES_ARKS_ARCHIVE.V_PHDS_var_master
  where var_MPOG_MappingLocation = 'IntraOpLabs' ")
  
  io_1 <- sqlQuery(edv,sql_query,as.is = TRUE)
  
  io_2 <- io_1 %>%
    mutate(new_1 = if_else(str_detect(var_desc, "030401"), 
                           substr(var_desc, 1, str_locate(var_desc, "030401")[,1] - 1), 
                           NA_character_),
           new_2 = if_else(str_detect(new_1,'BLoodGas'),
                           substr(new_1, str_locate(new_1, "BLoodGas")[, 1] + 8, nchar(new_1)),
                           NA_character_)) %>%
    mutate(new_2 = ifelse(is.na(new_2),var_desc,new_2),
           io_lab_type = case_when(
             str_detect(new_2, "AACL") ~ "AACL",
             substr(new_2, 1, 3) == "ABG" ~ "A",
             substr(new_2, 1, 3) == "VBG" ~ "V",
             substr(new_2, 1, 5) == "ACoag" ~ "ACoag",
             substr(new_2, 1, 5) == "VCoag" ~ "VCoag",
             substr(new_2, 1, 4) == "ALAB" ~ "A",
             substr(new_2, 1, 4) == "VLAB" ~ "V",
             substr(new_2, 1, 4) == "VACL" ~ "VACL",
             substr(new_2, 1, 4) == "AACL" ~ "AACL",
             substr(new_2, 1, 4) == "VLABP" ~ "VLABP",
             substr(new_2, 1, 4) == "VMET" ~ "VMET",
             substr(new_2, 1, 4) == "AMET" ~ "AMET",
             substr(new_2, 1, 3) == "VCO" ~ "VCO",
             substr(new_2, 1, 3) == "ACO" ~ "ACO",
             var_desc %in% c("AdditionalParameterACT", "CCFARKS |ACT030401") ~ " ",
             substr(new_2, 1, 1) == "V" ~ "V",
             substr(new_2, 1, 1) == "A" ~ "A",
             TRUE ~ NA_character_
           ),
           io_lab_name = case_when(
             str_detect(new_2, "AACL") ~ substr(new_2, 5, nchar(new_2)),
             substr(new_2, 1, 3) %in% c("ABG", "VBG", "VCO", "ACO") ~ substr(new_2, 4, nchar(new_2)),
             substr(new_2, 1, 5) %in% c("ACoag", "VCoag") ~ substr(new_2, 6, nchar(new_2)),
             substr(new_2, 1, 4) %in% c("ALAB", "VLAB", "VACL", "AACL", "VLABP", "VMET", "AMET") ~ substr(new_2, 5, nchar(new_2)),
             var_desc %in% c("AdditionalParameterACT", "CCFARKS |ACT030401") ~ "ACT",
             substr(new_2, 1, 1) %in% c("V", "A") ~ substr(new_2, 2, nchar(new_2)),
             TRUE ~ NA_character_
           )) %>%
    mutate(
      io_lab_name = str_trim(io_lab_name),
      
      io_lab_name = case_when(
        io_lab_name %in% c('IMg HL7', 'IMg Manual') ~ 'IMG',
        var_desc == 'ACO2 CT' ~ 'CO2 CT',
        var_desc == 'VCO2 CT' ~ 'CO2 CT',
        TRUE ~ io_lab_name
      ),
      
      io_lab_type = case_when(
        var_desc == 'ACO2 CT' ~ 'A',
        var_desc == 'VCO2 CT' ~ 'V',
        TRUE ~ io_lab_type
      ),
      
      # Create a compressed and uppercase match name
      match_name = toupper(gsub(" ", "", io_lab_name, fixed = TRUE))
    )%>%
    select(var_sys, var_desc, var_type, var_item, var_location, io_lab_type, io_lab_name, match_name, new_1, new_2)

  
}





#-------------------------------------------------------------------------------

io_lab_arks <- function(inds,io_lab_list){
  # check or connect to edv
  edv <- check_and_connect_edv()
  
  sql_query <- paste0("
    select *
    from DL_ANES_ARKS_ARCHIVE.V_PHDS_var_master
  where var_MPOG_MappingLocation = 'IntraOpLabs' ")
  
  io_1 <- sqlQuery(edv,sql_query,as.is = TRUE)
  
  io_2 <- io_1 %>%
    mutate(
      # Initialize the new columns
      new_1 = ifelse(str_detect(var_desc, '030401'), substr(var_desc, 1, str_locate(var_desc, '030401')[,1]-1), NA_character_),
      new_2 = ifelse(str_detect(new_1, 'BLoodGas'), substr(new_1, str_locate(new_1, 'BLoodGas')[,1] + 8, nchar(new_1)), var_desc),
      new_2 = ifelse(new_2 == ' ', var_desc, new_2),
      io_lab_type = case_when(
        str_detect(new_2, '^AACL') ~ 'AACL',
        substr(new_2, 1, 3) == 'ABG' ~ 'A',
        substr(new_2, 1, 3) == 'VBG' ~ 'V',
        substr(new_2, 1, 5) == 'ACoag' ~ 'ACoag',
        substr(new_2, 1, 5) == 'VCoag' ~ 'VCoag',
        substr(new_2, 1, 4) == 'ALAB' ~ 'A',
        substr(new_2, 1, 4) == 'VLAB' ~ 'V',
        substr(new_2, 1, 4) == 'VACL' ~ 'VACL',
        substr(new_2, 1, 4) == 'VLABP' ~ 'VLABP',
        substr(new_2, 1, 4) == 'VMET' ~ 'VMET',
        substr(new_2, 1, 4) == 'AMET' ~ 'AMET',
        substr(new_2, 1, 3) == 'VCO' ~ 'VCO',
        substr(new_2, 1, 3) == 'ACO' ~ 'ACO',
        var_desc %in% c('AdditionalParameterACT', 'CCFARKS |ACT030401') ~ ' ',
        TRUE ~ ifelse(substr(new_2, 1, 1) == 'V', 'V', ifelse(substr(new_2, 1, 1) == 'A', 'A', NA_character_))
      ),
      io_lab_name = case_when(
        io_lab_type != ' ' & io_lab_type != NA_character_ ~ substr(new_2, nchar(io_lab_type) + 1, nchar(new_2)),
        TRUE ~ NA_character_
      ),
      io_lab_name = ifelse(var_desc %in% c('ACO2 CT', 'VCO2 CT'), paste0(substr(var_desc, 1, 1), 'CO2 CT'), io_lab_name),
      io_lab_name = ifelse(io_lab_name %in% c('IMg HL7', 'IMg Manual'), 'IMG', io_lab_name),
      io_lab_name = str_trim(io_lab_name),
      match_name = toupper(str_replace_all(io_lab_name, "[[:punct:]]", ""))
    ) %>%
    select(var_sys, var_desc, var_type, var_item, var_location, io_lab_type, io_lab_name, match_name, new_1, new_2)
  
  
  
  
  
}



