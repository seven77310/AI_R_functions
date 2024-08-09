## 1) ICU to OR
##   icu_in < in_or + 20 mins < icu_out OR -120 <= icu_out - in_or <= 0 mins
## 2) OR to ICU
##    ICU_in <out_or < ICU_out 
## OR out_or < ICU_in < new_disch_date

ICU_flag <- function(df) {
  df %>%
    mutate(
      ICU_to_OR = case_when(
        difftime(ICU_ADMIT_DTTM, in_or, units = "mins") <= 20 & ICU_DISCH_DTTM > in_or ~ 1,
        difftime(ICU_DISCH_DTTM, in_or, units = "mins") >= -120 & difftime(ICU_DISCH_DTTM, in_or, units = "mins") < 0 ~ 1,
        TRUE ~ 0
      ),
      OR_to_ICU = case_when(
        ICU_ADMIT_DTTM < out_or & out_or < ICU_DISCH_DTTM ~ 1,
        out_or < ICU_ADMIT_DTTM & ICU_ADMIT_DTTM < new_disch_date ~ 1,
        TRUE ~ 0 
      )
    ) %>% # deal with multiple value for the same final_op_sys
    distinct(final_op_sys,ICU_to_OR,OR_to_ICU) %>%
    group_by(final_op_sys) %>%
    summarise(ICU_to_OR = max(ICU_to_OR),
              OR_to_ICU = max(OR_to_ICU))
  
}
