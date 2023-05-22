library(tidyverse)

#Question 1
admin_site1 <- read.csv('data/administrative_site1.csv')
admin_site2 <- read.csv('data/administrative_site2.csv')
lab <- read.csv('data/lab.csv')
names(admin_site1) <- names(admin_site2)
admin_site1 <- admin_site1 %>%
  mutate(
    admission_timestamp = paste0(admission.date, ' ', admission.time),
    discharge_timestamp = paste0(discharge.date, ' ', discharge.time),
    admission_timestamp = as.POSIXct(admission_timestamp,
                                     format = "%Y-%m-%d %H:%M"),
    discharge_timestamp = as.POSIXct(discharge_timestamp,
                                     format = "%m/%d/%Y %H:%M")
  )
admin_site2 <- admin_site2 %>%
  mutate(
    admission_timestamp = paste0(admission.date, ' ', admission.time),
    discharge_timestamp = paste0(discharge.date, ' ', discharge.time),
    admission_timestamp = as.POSIXct(admission_timestamp,
                                     format = "%Y/%m/%d %H:%M"),
    discharge_timestamp = as.POSIXct(discharge_timestamp,
                                     format = "%m/%d/%Y %H:%M")
  )

#admin<-bind_rows(admin_site1,admin_site2)
admin <- rbind(admin_site1, admin_site2)

#Question 2
admin <- admin %>%
  mutate(
    length_of_stay = discharge_timestamp - admission_timestamp,
    length_of_stay_days = as.numeric(length_of_stay) / 24
  )

question2 <- admin %>%
  group_by(hospital) %>%
  dplyr::summarise(average_stay = mean(length_of_stay_days, na.rm = TRUE))

#Question 3
question3 <- full_join(lab, admin, by = c('ID' = 'subjectid'))

#Question 5
lab_wide <- question3 %>%
  pivot_wider(
    names_from = test_name,
    values_from =  result_value,
    values_fn = ~ min(.x, na.rm = T)
  )
mean_urea_plasma <- mean(lab_wide$`Urea plasma`, na.rm = T)

#Question 6a.
adm_filtered <- admin[rowSums(is.na(admin)) > 1, ]

#Question 6b.
question6 <- colSums(is.na(adm_filtered)) / nrow(adm_filtered) * 100

#Question 7
lab_impute1 <- lab %>%
  mutate(
    result_year = lubridate::year(result_date),
    result_value = ifelse(result_year == 2002, NA, result_value)
  )
lab_impute2 <- lab %>%
  mutate(
    result_year = lubridate::year(result_date),
    result_value = ifelse(result_year == 2002, NA, result_value)
  ) %>%
  group_by(test_name) %>%
  dplyr::summarise(averaged_result_value = mean(as.numeric(result_value),
                                                na.omit = T))
lab_impute3 <- lab_impute1 %>%
  filter(!is.na(result_value))
lab_impute4 <- lab_impute1 %>%
  filter(is.na(result_value))

lab_impute5 <-
  inner_join(lab_impute4, lab_impute2, by = 'test_name') %>%
  mutate(result_value = averaged_result_value) %>%
  select(-c(averaged_result_value))
lab_impute <- rbind(lab_impute5, lab_impute3)
question7 <- lab_impute %>%
  select(c(test_name, result_value)) %>%
  filter(test_name == 'Bicarbonate plasma') %>%
  dplyr::summarise(overall_mean_value = mean(result_value, na.omit = T))

#Question 8 - part 1
question8 <- lab %>%
  filter(grepl('Jessica|Kelsey', provider_name, ignore.case = T))

#Question 8 - part 2
question8Kelsey <- question8 %>%
  filter(test_code == 'NAPL') %>%
  filter(grepl('Kelsey', provider_name, ignore.case = T)) %>%
  dplyr::summarise(averaged_result_value = mean(result_value, na.omit = T))
question8Jessica <- question8 %>%
  filter(test_code == 'NAPL') %>%
  filter(grepl('Jessica', provider_name, ignore.case = T)) %>%
  dplyr::summarise(averaged_result_value = mean(result_value, na.omit = T))

#Question 9
year_output <- function(inputted_year, output_file_name) {
  ds <- admin %>%
    mutate(discharged_year = lubridate::year(discharge_timestamp)) %>%
    filter(discharged_year == inputted_year)
  write.csv(ds,
            paste0('output/', output_file_name, '_', inputted_year, '.csv'))
  # return(ds)
}
year_output(2005, 'Discharge')


#Question 10
compute_hospital_mean <- function(test_code, hospital_id) {
  ds <- question3 %>%
    filter(`test_code` == test_code &
             hospital == hospital_id) %>%
    dplyr::summarise(averaged_result_value = mean(result_value, na.omit = T))
  return(ds)
}
question10 <- compute_hospital_mean('KPL', "St. Michael's Hospital")
