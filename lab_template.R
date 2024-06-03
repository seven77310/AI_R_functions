
# --------------------------------------------------
# Environment Setup
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
              'anytime', 'questionr', 'fs', 'redcapAPI','Hmisc')

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


# --------------------------------------------------
# set up parameters 
# --------------------------------------------------

date ='20231120'
# Define data table names
id_data <- # local SAS ID data
datalab_data <- "DL_ANESTHESIA_RESEARCH.liux4_p333335_id"  # datalab data name
phds_data <-  # phds data
acg_data <-   #acg data

# Define patient id and unique ID variables
patient_id <- "pat_id"
uid_var <- "final_op_sys"

# Define time-window variables
time_var <- "final_dos"
in_or <- "in_or"
out_or <- "out_or"
hosp_admit_date <- "new_admit_date"
hosp_disch_date <- "new_disch_date"

# Define lab test lists and corresponding time windows
lab_preop_list <- c("ALBUMIN", "ALP", "ALT", "AST")
lab_preop_days <- 30

lab_intraop_list <- NULL

lab_postop_list <- c("ALBUMIN", "ALP", "ALT", "AST")
lab_postop_days <- c(7, 30, "new_disch_date")
lab_postop_hours <- c(30, 60, 72)

# --------------------------------------------------
#  main process
# --------------------------------------------------

# Combine lists
lab_list <- c(lab_preop_list, lab_intraop_list, lab_postop_list) %>%
  unique()

# Check if there are unmatched lab names 

if (nrow(check_lab_names(lab_list)$unmatched_lab_list)> 0) {
  print("please run 'lab_id_name()' function to update 'lab_id_list' form")
} else {
  print("No unmatched lab names found.")
}

# --------------------------------------------------
# pull lab data 
# --------------------------------------------------

# 1. preop lab

if (length(lab_preop_list) > 0) {
  
  preop_edv_lab <- get_edv_lab(inds = datalab_data,
                               lab_list =lab_preop_list,
                               time_var = time_var,
                               cutoff_start =lab_preop_days,
                               cutoff_end =0
  )
  
  # Adding a new column 'type' with all values set to 'pre_lab'
  preop_edv_lab$type <- 'pre_lab'
  
}

# 2. postop lab

# processing lab_postop_days and lab_postop_hours. 

largest_post_days <- largest_date(lab_postop_days,lab_postop_hours )$largest_days
lab_date <- largest_date(lab_postop_days,lab_postop_hours )$concatenated_string

if(length(lab_postop_list) >0 && length(largest_post_days) >0) {
  
  intra_postop_lab_days <- get_edv_lab(inds = datalab_data,
                               lab_list =lab_postop_list,
                               time_var = time_var,
                               cutoff_start =0,
                               cutoff_end =largest_post_days  )
  
  # Adding a new column 'type' with all values set to 'post_lab'
  intra_postop_lab_days$type <- 'post_lab'
  
}


if(length(lab_postop_list) >0 && length(lab_date) >0) {
  
  intra_postop_lab_date <- get_edv_lab(inds = datalab_data,
                                       lab_list =lab_postop_list,
                                       time_var = time_var,
                                       cutoff_start =in_or,
                                       cutoff_end =lab_date  )
  
  # Adding a new column 'type' with all values set to 'post_lab'
  intra_postop_lab_date$type <- 'post_lab'
  
}

# 3. intra-op



