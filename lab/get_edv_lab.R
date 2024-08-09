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