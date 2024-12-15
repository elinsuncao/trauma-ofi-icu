
#INSTALLING PACKAGES
#devtools::install_github("martingerdin/noacsr")
#devtools::install_github("martingerdin/rofi")
library(dotenv)
library(noacsr)
library(rofi)
#noacsr::source_all_functions()
data <- import_data()

merged.data <- merge_data(data)
merged.data$ofi <- create_ofi(merged.data)

#install.packages("dplyr")
library(dplyr)
#install.packages("gtsummary")
library(gtsummary)
library(tidyverse)
library(officer)


#FLOWCHART: included/excluded
#install.packages("Gmisc")
library(Gmisc, quietly = TRUE)
library(glue)
#install.packages("htmlTable")
library(htmlTable)
library(grid)
library(magrittr)

##CLEANING DATA
subdat <- merged.data %>%
  select(ofi, pt_Gender, pt_age_yrs,  ed_gcs_sum, ed_sbp_value, ed_rr_value, 
         res_survival, pre_intubated, ed_intubated, dt_ed_first_ct, ISS, DateTime_ArrivalAtHospital, FirstTraumaDT_NotDone,
         host_care_level, hosp_vent_days, pt_asa_preinjury, pre_gcs_sum, 
         pre_rr_value, pre_sbp_value, Fr1.12, ed_rr_rtscat, ed_sbp_rtscat, pre_rr_rtscat, pre_sbp_rtscat, iva_dagar_n, Problemomrade_.FMP, DeceasedDate, Deceased)

#Converting subdat$ofi to logical so subset can be used 
subdat$ofi <- ifelse(subdat$ofi == "Yes", TRUE, FALSE)

#Only those in IVA
iva <- subset(subdat, subset = (host_care_level == 5))

#Removing pt_yrs < 15
adult <- subset(iva, subset = (pt_age_yrs > 14))

#Deceased on arrival 
alive <- subset(adult, subset = (Fr1.12 == 2 | is.na(Fr1.12)))

#Removing ofi = NA            
ofi <- alive %>% subset(!is.na(ofi))


#DEFINING VARIABLES FOR TABLE 1 
#Gender
ofi$Sex <- ifelse(ofi$pt_Gender == 1, "Male", 
                  ifelse(ofi$pt_Gender == 2, "Female", 
                         ifelse(ofi$pt_Gender == 999, NA, NA)))

#Age
ofi$Age <- ofi$pt_age_yrs

#Intubation 
#ofi$Intubation1 <- ifelse(ofi$pre_intubated == 1, "Intubation",
#                          ifelse(ofi$pre_intubated == 2, "Not intubated",  
#                                 ifelse(ofi$pre_intubated == 999, "Unknown",
#                                        ifelse(ofi$ed_intubated == 1, "Intubation",
#                                               ifelse(ofi$ed_intubated == 2, "Not intubated",  
#                                                      ifelse(ofi$ed_intubated == 999, "Unknown", "Unknown"))))))

#Intubation combined with ventilator days 
#ofi$Intubation <- ifelse(ofi$Intubation1 == "Not intubated", "Not intubated",
#                         ifelse(ofi$Intubation1 == "Intubation" & ofi$hosp_vent_days %in% 0:2, "Mechanical ventilation 0-2 days",
##                                ifelse(ofi$Intubation1 == "Intubation" & ofi$hosp_vent_days %in% 3:7, "Mechanical ventilation 3-7 days",
#                                       ifelse(ofi$Intubation1 == "Intubation" & ofi$hosp_vent_days > 7, "Mechanical ventilation > 7 days",
#                                              ifelse(ofi$Intubation1 == "Unknown", "Unknown", "Unknown")))))

ofi$Intubation <-  ifelse(ofi$hosp_vent_days %in% 0:2, "Mechanical ventilation 0-2 days",
                        ifelse(ofi$hosp_vent_days %in% 3:7, "Mechanical ventilation 3-7 days",
                             ifelse(ofi$hosp_vent_days > 7, "Mechanical ventilation > 7 days", "Unknown")))


#Kontinuerlig variabel mechanical ventilation
ofi$mechanical.ventilation.cont <- ifelse(ofi$hosp_vent_days > 0, ofi$hosp_vent_days, "Unknown")


#Respiratory rate 
ofi$RespiratoryRate <- ifelse(is.na(ofi$ed_rr_value), ofi$pre_rr_value, ofi$ed_rr_value)

#Systolic blood pressure 
ofi$SystolicBloodPressure <- ifelse(is.na(ofi$ed_sbp_value), ofi$pre_sbp_value, ofi$ed_sbp_value)

#Glasgow Coma Scale
ofi$GlasgowComaScale <- ifelse(ofi$ed_gcs_sum == 99, 99,
                               ifelse(ofi$ed_gcs_sum == 999, NA,
                                      ifelse(ofi$ed_gcs_sum == 3, 3,
                                             ifelse(ofi$ed_gcs_sum == 4, 4,
                                                    ifelse(ofi$ed_gcs_sum == 5, 5,
                                                           ifelse(ofi$ed_gcs_sum == 6, 6,
                                                                  ifelse(ofi$ed_gcs_sum == 7, 7,
                                                                         ifelse(ofi$ed_gcs_sum == 8, 8,
                                                                                ifelse(ofi$ed_gcs_sum == 9, 9,
                                                                                       ifelse(ofi$ed_gcs_sum == 10, 10,
                                                                                              ifelse(ofi$ed_gcs_sum == 11, 11,
                                                                                                     ifelse(ofi$ed_gcs_sum == 12, 12,
                                                                                                            ifelse(ofi$ed_gcs_sum == 13, 13,
                                                                                                                   ifelse(ofi$ed_gcs_sum == 14, 14,
                                                                                                                          ifelse(ofi$ed_gcs_sum == 15, 15, NA)))))))))))))))


ofi$GlasgowComaScale <- ifelse(is.na(ofi$ed_gcs_sum), ofi$pre_gcs_sum, ofi$ed_gcs_sum)

#RTS score
ofi$RTSGCS <- ifelse(ofi$GlasgowComaScale %in% 13:15, 4,
                     ifelse(ofi$GlasgowComaScale %in% 9:12, 3,
                            ifelse(ofi$GlasgowComaScale %in% 6:8, 2,
                                   ifelse(ofi$GlasgowComaScale %in% 4:5, 1,
                                          ifelse(ofi$GlasgowComaScale == 3, 0,
                                                 ifelse(ofi$GlasgowComaScale == 99, 0, NA))))))

ofi$RTSSBP <- ifelse(ofi$SystolicBloodPressure > 89, 4,
                     ifelse(ofi$SystolicBloodPressure %in% 76:89, 3,
                            ifelse(ofi$SystolicBloodPressure %in% 50:75, 2,
                                   ifelse(ofi$SystolicBloodPressure %in% 1:49, 1,
                                          ifelse(ofi$SystolicBloodPressure == 0, 0,
                                                 ifelse(ofi$SystolicBloodPressure == 99, 0, NA))))))

ofi$RTSRR <- ifelse(ofi$RespiratoryRate %in% 10:29, 4,
                    ifelse(ofi$RespiratoryRate >29, 3,
                           ifelse(ofi$RespiratoryRate %in% 6:9, 2,
                                  ifelse(ofi$RespiratoryRate %in% 1:5, 1,
                                         ifelse(ofi$RespiratoryRate == 0, 0,
                                                ifelse(ofi$RespiratoryRate == 99, 0, NA)))))) 

ofi$RTS <- (0.9368*ofi$RTSGCS + 0.7326*ofi$RTSSBP + 0.2908*ofi$RTSRR)
#ofi$RTS <- (ofi$RTSGCS + ofi$RTSSBP + ofi$RTSRR)



#Working hours: arrived between 8 am and 5 pm 
ofi$hour <- format(ofi$DateTime_ArrivalAtHospital, "%H")
ofi$WorkingHoursTF <- ifelse(ofi$hour == "08" | ofi$hour == "09" | ofi$hour == "10" | ofi$hour == "11" | ofi$hour == "12" | ofi$hour == "13" | ofi$hour == "14" | ofi$hour == "15" | ofi$hour == "16", TRUE, FALSE)
ofi$WorkingHours <- ifelse(ofi$WorkingHoursTF == TRUE, "Yes", 
                           ifelse(ofi$WorkingHoursTF == FALSE, "No", NA))

#Weekend: arrived on Saturday or Sunday 
ofi$Weekdays <- weekdays(ofi$DateTime_ArrivalAtHospital)
ofi$WeekendTF <- ifelse(ofi$Weekdays == "Saturday" | ofi$Weekdays == "Sunday", TRUE, FALSE)
ofi$Weekend <- ifelse(ofi$WeekendTF == TRUE, "Yes",
                      ifelse(ofi$WeekendTF == FALSE, "No", NA))

#Duty shift
ofi$OnDuty <- ifelse(ofi$Weekend == "Yes", 1,
                     ifelse(ofi$WorkingHours == "No", 1, 0))

#Time to first CT
ofi$TimeFCT <- ofi$dt_ed_first_ct

#Days in the ICU 
ofi$daysinICU <- ifelse(ofi$iva_dagar_n < 7 | ofi$iva_dagar_n == 7, "≤ 7 days",
                        ifelse(ofi$iva_dagar_n > 7, "> 7 days", NA))

#Kontinuerlig variabel ICU LOS
ofi$icu.los.cont <- ofi$iva_dagar_n

#Pt ASA preinjury
ofi$ASApreinjury <- ifelse(ofi$pt_asa_preinjury == 1 | ofi$pt_asa_preinjury == 2, "ASA 1-2",
                           ifelse(ofi$pt_asa_preinjury %in% 3:5, "ASA 3-5",
                                  ifelse(ofi$pt_asa_preinjury == 999, NA, NA)))

#Survival after 30 days 
ofi$Survival <- ifelse(ofi$res_survival == 1, "Dead",
                       ifelse(ofi$res_survival == 2, "Alive",
                              ifelse(ofi$res_survival == 999, NA, NA)))

#Jonatans version 
ofi$Deceased <- as.logical(as.character(ofi$Deceased))
ofi$DiedWithin24Hours <- ifelse(ofi$Deceased == FALSE, FALSE, NA)
ofi$DateTime_ArrivalAtHospital <- as.Date(ofi$DateTime_ArrivalAtHospital)
ofi$DeceasedDate <- as.Date(ofi$DeceasedDate)
time_diff <- as.numeric(ofi$DeceasedDate - ofi$DateTime_ArrivalAtHospital)
ofi$DiedWithin24Hours <- ifelse(
  ofi$Deceased == TRUE & time_diff <= 1, TRUE, 
  ifelse(ofi$Deceased == TRUE & time_diff > 1, FALSE, 
         ofi$DiedWithin24Hours))

#24-h mortality
# Konvertera datumen till Date-format
#ofi$DateTime_ArrivalAtHospital <- as.Date(ofi$DateTime_ArrivalAtHospital)
#ofi$DeceasedDate <- as.Date(ofi$DeceasedDate)

# Beräkna skillnaden i dagar mellan ankomst och avliden datum
#ofi$days_to_deceased <- as.numeric(ofi$DeceasedDate - ofi$DateTime_ArrivalAtHospital)

# Klassificera patienter som "Dead" eller "Alive" baserat på villkoren
#ofi$Mortality <- ifelse(!is.na(ofi$days_to_deceased) & ofi$days_to_deceased <= 1, "Dead",
 #                       ifelse(!is.na(ofi$days_to_deceased) & ofi$days_to_deceased > 1, "Alive",
#                               ifelse(is.na(ofi$DeceasedDate) & ofi$Deceased == FALSE, "Alive", NA)))

                        

#OFI 
ofi$OpportunityForImprovement <- ifelse(ofi$ofi == TRUE, "Opportunity for improvement",
                                        ifelse(ofi$ofi == FALSE, "No opportunity for improvement", NA))

ofi$OpportunityForImprovement1 <- ifelse(ofi$OpportunityForImprovement == "Opportunity for improvement", 1,
                                         ifelse(ofi$OpportunityForImprovement == "No opportunity for improvement", 0, NA))


table1 <- ofi %>% 
  select(Sex, Age, Intubation, mechanical.ventilation.cont, RTS, ISS, TimeFCT, OnDuty, daysinICU, 
         icu.los.cont, ASApreinjury, Survival, DiedWithin24Hours, OpportunityForImprovement)


table1 <- table1 %>%
  filter(if_all(.cols = -c(DiedWithin24Hours, Intubation, mechanical.ventilation.cont, icu.los.cont), .fns = ~ !is.na(.)))


table2 <- table1 %>%
  mutate(Intubation = factor(Intubation, levels = c("Mechanical ventilation 0-2 days", "Mechanical ventilation 3-7 days", "Mechanical ventilation > 7 days"))) %>%
  tbl_summary(by = OpportunityForImprovement,
              type = list(OnDuty ~ "dichotomous"),
              label = list(RTS = "Revised Trauma Score",
                           ISS = "Injury Severity Score",
                           Intubation = "Mechanical ventilation",
                           mechanical.ventilation.cont = "Mechanical ventilation in days",
                           TimeFCT = "Time to first CT, in minutes", 
                           daysinICU = "ICU length of stay",
                           icu.los.cont = "ICU length of stay in days",
                           OnDuty = "On call hours",
                           ASApreinjury = "ASA preinjury",
                           DiedWithin24Hours = "24-hour mortality"),
              statistic = list(
             #   all_continuous() ~ "{mean} ({sd})",
                all_continuous() ~ c("{median} ({p25}, {p75})"),
                all_categorical() ~ "{n} ({p}%)"
               # daysinICU ~ "{mean} ({sd})"
          #      Intubation ~ "{mean} ({sd})"
          #Intubation och daysinICU måste göras till en numeric för att kunna visa medelvärde och standardavvikelse men blir då en ny rad i tabellen? 
              ),
              missing = "ifany",
              missing_text = "Missing",
              missing_stat = "{N_miss} ({p_miss}%)",
              digits = all_continuous() ~ 0
  )  %>%
  modify_table_styling(
    columns = label,
    rows = label == "On call hours",
    footnote = "Arrival at the hospital on Saturday or Sunday, or arrival at the hospital before 8 am or after 5 pm"
  ) %>%
  bold_labels() %>% 
  add_overall(last = TRUE) %>% 
  add_stat_label() %>%
  #add_p() %>%
  #bold_p(t=0.05) %>%
  modify_caption("<div style='text-align: left; font-weight: bold; color: black'>Table 1. Sample Characteristics</div>") %>% 
 # as_flex_table() %>%
  print()
