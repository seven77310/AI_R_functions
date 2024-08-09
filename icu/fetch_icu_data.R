# Example usage
# edv is your ODBC connection
# df = "DL_ANESTHESIA_RESEARCH.liux4_p301184_id_1"
# time_var = "final_dos"
#icu_0 <- fetch_icu_data(edv, "DL_ANESTHESIA_RESEARCH.liux4_p301184_id_1", "final_dos")
fetch_icu_data <- function(connection, df, time_var) {
  query <- paste("SELECT a.*, 
                         b.hsp_admit_dttm, 
                         b.hsp_disch_dttm, 
                         b.enc_csn_id AS hsp_enc_csn_id,
                         c.*
                  FROM", df, "AS a
                  INNER JOIN icu_dm_v.icu_hosp_enc_detail AS b ON a.pat_id = b.pat_id
                  INNER JOIN icu_dm_v.icu_enc_detail AS c ON b.k_pat_enc_key = c.k_pat_enc_key
                  WHERE a.", time_var, "BETWEEN 
                        CAST(b.hsp_admit_dttm AS DATE) AND 
                        CAST(b.hsp_disch_dttm AS DATE)")
  
  result <- sqlQuery(connection, query)
  return(result)
}