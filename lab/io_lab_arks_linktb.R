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