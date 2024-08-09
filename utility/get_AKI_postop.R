

  
# Install libraries----------------------------------

if(!require(rlang)){
  install.packages("rlang")
  library(rlang)
}


if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}


if(!require(tibble)){
  install.packages("tibble")
  library(tibble)
}

if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}

# Global variables inside the functions ------------------
timewins = 48



get_AKI<- function(df,ids,dttm,post_val) {
  
  id_name <- ensym(ids)
  dttm_name<-ensym(dttm)
  val_name <- ensym(post_val)
  
  ids <- enquo(ids)
  dttm <- enquo(dttm)
  val <- enquo(post_val)
  

  
  df1 <- df %>% group_by_at(vars(!!ids)) %>%
                filter(n()>1)%>%
                ungroup()
  
  # For each op_sys, divide the records into N group. 
  # The time difference of each group greater than the timewins
   df2 <- df1 %>% group_by_at(vars(!!ids)) %>%
     arrange(!!id_name,!!dttm_name) %>%
     distinct()%>%
     mutate(groupID =cumsum(
       (!!dttm_name - lag(!!dttm_name, default = lubridate::as_date(0)))>=
         lubridate::duration(hours = timewins)
     ) )%>%
     drop_na() %>%
     group_by(groupID, .add = TRUE)  %>%
     rowid_to_column("uid") #update uid
  
   
   df_temp <- df2 %>% 
     count()%>%
     filter(n==1)
   # generate the Ids list which only contains one record in the same groupID
   rmids<- left_join(df2,df_temp,by=quo_name(ids))%>%
     group_by_at(vars(!!ids))%>%
     filter(groupID.x==groupID.y) %>%
     ungroup()%>%
     select(uid)
   
   df3 <- df2 %>% filter(!(uid %in% rmids$uid))
   
   # Get indexs: X1,X2 show the indexs of df3. 
   df4 <- df2 %>% 
     count()%>%
     filter(n >1) %>%
     ungroup() %>%
     # get start row number is n_1+1
     mutate(n_1 = cumsum(lag(.data$n, default = 0)))%>%
     rowwise() %>%
     do(data.frame(.data$n_1 + t(utils::combn(.data$n, 2)))) %>%
     arrange(.data$X2, desc(.data$X1))
  
   # generate two dataframe from df3 in the define sequence 
   T1 <- df3[df4 $X1, ] %>% 
        ungroup() %>% 
        select(!!!ids,!!!dttm,!!!post_val) %>%
        rename(dttm_1=!!dttm_name,crt_val_1=!!val_name)
   T2 <- df3[df4 $X2, ] %>% 
     ungroup() %>% 
     select(!!!dttm,!!!post_val) %>%
     rename(dttm_2=!!dttm_name,crt_val_2=!!val_name)
   
   crt_changes <- bind_cols(T1,T2) %>%
     mutate(D.val=crt_val_2-crt_val_1,
            D.dttm=as.numeric(difftime(dttm_2,dttm_1,units = 'hours')))
   
   df5 <- crt_changes %>%mutate(aki_flag =  
                                  ifelse(D.dttm <= 48 & D.val >= 0.3,1,0))
   
   aki_max_id <- df5 %>% filter(aki_flag==1) %>% 
     group_by_at(vars(!!ids))%>% 
     mutate(aki_max = max(D.val))%>%
     select(!!!ids,aki_max) %>%
     distinct()
   
   aki_max <- left_join(df5,aki_max_id,by=quo_name(ids)) %>%
     filter(D.val==aki_max & aki_flag ==1 )
   
   df6 <-df5 %>% select(!!!ids,aki_flag) %>% 
        group_by_at(vars(!!ids)) %>%
        arrange(desc(aki_flag)) %>%
        filter(row_number()==1)

   df6 <- left_join(df6,aki_max_id,by=quo_name(ids))
   df_id <- df %>% select(!!!ids) %>% distinct()
   df_aki <- left_join(df_id,df6,by=quo_name(ids)) %>%
     mutate_at(vars(aki_flag),~replace_na(., 0))
     
   
   output <- list("crt_changes"=crt_changes,"aki_max"=aki_max,"df_aki"=df_aki)
   return(output)
}



test = get_AKI(lab4,ids='op_sys',dttm='ORD_PROC_SPECIMEN_DTTM',post_val='ORD_RSLT_VALUE')

crt_changes <- test$crt_changes
aki_max <- test$aki_max
df_aki <- test$df_aki

test5 <- crt_changes %>% mutate(aki_flag =  
                               ifelse(D.dttm <= 48 & D.val >= 0.3,1,0)) %>%
  select(op_sys,aki_flag) %>%
  group_by(op_sys)%>%
  arrange(desc(aki_flag)) %>%
  filter(row_number()==1)



test1 <- get_AKI(crt_changes,ids='op_sys')
test1 [duplicated(test1 $op_sys),]

t<- test1 %>% filter(op_sys == '10140042')


tt9 <- test %>% filter(D.val >= 0.3) %>% select(op_sys) %>% distinct() #n=522