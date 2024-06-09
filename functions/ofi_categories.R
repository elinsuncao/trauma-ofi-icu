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
         pre_rr_value, pre_sbp_value, Fr1.12, ed_rr_rtscat, ed_sbp_rtscat, pre_rr_rtscat, pre_sbp_rtscat, iva_dagar_n, Problemomrade_.FMP)

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
ofi$Intubation1 <- ifelse(ofi$pre_intubated == 1, "Intubation",
                          ifelse(ofi$pre_intubated == 2, "Not intubated",  
                                 ifelse(ofi$pre_intubated == 999, "Unknown",
                                        ifelse(ofi$ed_intubated == 1, "Intubation",
                                               ifelse(ofi$ed_intubated == 2, "Not intubated",  
                                                      ifelse(ofi$ed_intubated == 999, "Unknown", "Unknown"))))))

#Intubation combined with ventilator days 
ofi$Intubation <- ifelse(ofi$Intubation1 == "Not intubated", "Not intubated",
                         ifelse(ofi$Intubation1 == "Intubation" & ofi$hosp_vent_days ==  0, "Mechnical ventilation 1-7 days",
                                ifelse(ofi$Intubation1 == "Intubation" & ofi$hosp_vent_days %in% 1:7, "Mechanical ventilation 1-7 days",
                                       ifelse(ofi$Intubation1 == "Intubation" & ofi$hosp_vent_days > 7, "Mechanical ventilation > 7 days", 
                                              ifelse(ofi$Intubation1 == "Unknown", "Unknown", NA)))))

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

#Pt ASA preinjury
ofi$ASApreinjury <- ifelse(ofi$pt_asa_preinjury == 1 | ofi$pt_asa_preinjury == 2, "ASA 1-2",
                           ifelse(ofi$pt_asa_preinjury %in% 3:6, "ASA 3-6",
                                  ifelse(ofi$pt_asa_preinjury == 999, NA, NA)))

#Survival after 30 days 
ofi$Survival <- ifelse(ofi$res_survival == 1, "Dead",
                       ifelse(ofi$res_survival == 2, "Alive",
                              ifelse(ofi$res_survival == 999, NA, NA)))


#OFI 
ofi$OpportunityForImprovement <- ifelse(ofi$ofi == TRUE, "Opportunity for improvement",
                                        ifelse(ofi$ofi == FALSE, "No opportunity for improvement", NA))

ofi$OpportunityForImprovement1 <- ifelse(ofi$OpportunityForImprovement == "Opportunity for improvement", 1,
                                         ifelse(ofi$OpportunityForImprovement == "No opportunity for improvement", 0, NA))


# Load necessary libraries
library(dplyr)
library(stringr)
library(gtsummary)

# Select different OFIs
ofi_categories <- ofi %>%
  select(Problemomrade_.FMP, Sex, Age, Intubation, RTS, ISS, TimeFCT, OnDuty, daysinICU, 
         ASApreinjury, Survival, OpportunityForImprovement)

# Translating OFIs and identifying NAs
ofi_categories$Problemomrade_.FMP <- str_replace_all(
  str_to_lower(ofi_categories$Problemomrade_.FMP),
  c(
    "ok" = NA_character_,
    "nej" = NA_character_,
    "inget problemområde" = NA_character_,
    "föredömligt handlagd" = NA_character_,
    "dokumetation" = "Documentation",
    "handläggning" = "Patient management",
    "logistik/teknik" = "Logistics/technical",
    "lång tid till op" = "Delay to surgery",
    "lång tid till dt" = "Delay to CT",
    "kompetens brist" = "Competence",
    "kommunikation" = "Communication",
    "kommunikation+missad skada" = "Communication + missed injury",
    "handläggning/logistik" = "Patient management/logistics",
    "handläggning+dokumentation" = "Patient management + documentation",
    "handläggning prehosp" = "Prehospital management",
    "traumakriterier/styrning" = "Trauma criteria/guidelines",
    "tertiär survey" = "Tertiary survey",
    "bristande rutin" = "Inadequate routine",
    "annat" = "Other",
    "missad skada" = "Missed injury",
    "resurs" = "Resources",
    "triage på akm" = "Triage in the ED",
    "triage på akutmottagningen" = "Triage in the ED",
    "vårdnivå" = "Level of care",
    "vårdnivå\\+\r\nmissade skador" = "Level of care + missed injury",
    "handläggning\r\ndokumentation" = "Patient management + documentation"
  )
)

ofi_categories$Problemomrade_.FMP <- ifelse(ofi_categories$Problemomrade_.FMP == "Patient management/logistik", "Patient management/logistics", ofi_categories$Problemomrade_.FMP)
ofi_categories$Problemomrade_.FMP <- ifelse(ofi_categories$Problemomrade_.FMP == "Level of care+ missade skador", "Level of care + missed injuries", ofi_categories$Problemomrade_.FMP)

# Assign broad categories based on translated OFIs
ofi_categories <- ofi_categories %>%
  mutate(
    BroadCategory = case_when(
      Problemomrade_.FMP %in% c("Missed injury", "Tertiary survey") ~ "Missed diagnosis",
      Problemomrade_.FMP %in% c("Delay to surgery", "Delay to CT") ~ "Delay in treatment",
      Problemomrade_.FMP %in% c("Triage in the ED", "Level of care", "Patient management", "Communication") ~ "Clinical judgement error",
      Problemomrade_.FMP %in% c("Documentation") ~ "Documentation Issues",
      Problemomrade_.FMP %in% c("Technical error") ~ "Technical error",
      Problemomrade_.FMP %in% c("Trauma criteria/guidelines", "Inadequate routine") ~ "Inadequate protocols",
      Problemomrade_.FMP %in% c("Competence", "Resources", "Logistics/technical") ~ "Inadequate resources",
      Problemomrade_.FMP %in% c("Other", "Patient management/logistics", "Prehospital management", "Level of care + missed injury") ~ "Other errors",
      TRUE ~ "Other errors"
    )
  )

# Handle NA values in the Intubation column
ofi_categories <- ofi_categories %>%
  mutate(Intubation = ifelse(is.na(Intubation), "Unknown", Intubation))

# Remove rows where any variable other than Problemomrade_.FMP is NA
clean <- ofi_categories %>%
  filter(if_all(c(Sex, Age, Intubation, RTS, ISS, TimeFCT, OnDuty, daysinICU, ASApreinjury, Survival, OpportunityForImprovement), ~ !is.na(.)))

# Create a combined column for BroadCategory and Problemomrade_.FMP
clean <- clean %>%
  mutate(Category = paste(BroadCategory, Problemomrade_.FMP, sep = ": ")) %>%
  select(Category, OpportunityForImprovement)

# Filter to include only rows with Opportunity for Improvement
clean <- clean %>%
  filter(OpportunityForImprovement == "Opportunity for improvement")

# Create a summary table
table_ofic <- clean %>%
  tbl_summary(by = OpportunityForImprovement,
              statistic = list(
                all_categorical() ~ "{n}"
              ),
              missing = "ifany",
              missing_text = "Missing",
              digits = all_continuous() ~ 0
  ) %>%
  bold_labels() %>%
  modify_caption("<div style='text-align: left; font-weight: bold; color: black'>Table 1. Opportunities for improvement categories on the patient cohort of the study</div>") %>%
  modify_table_styling(
    columns = label,
    rows = label == "Clinical judgement error: Communication",
    footnote = "[active] failures in communication with parties outside the treating team, e.g. consultants, receiving unit etc., did not consult neurosurgeon before transfer"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Clinical judgement error: Level of care",
    footnote = "pt transferred to inappropriate level of care given available information/protocols (not due to lack of resources), pt transferred to IMCU rather than ICU"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Clinical judgement error: Patient management",
    footnote = "[active] failure to perform the appropriate exams and interventions in appropriate order and time given available information/protocols (not due to lack of resources) sent pt to CT with inadequately secured airway"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Clinical judgement error: Triage in the ED",
    footnote = "[active] pt assigned inappropriate level given available information/protocols, failure to activate trauma team, ED = emergency department"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Delay in treatment: Delay to CT",
    footnote = "[active] failure to perform computed tomography when indicated/in protocols"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Delay in treatment: Delay to surgery",
    footnote = "[active] failure to move pt to OR within an appropriate time, let's monitor this pt further before deciding on surgery"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Inadequate protocols: Trauma criteria/guidelines",
    footnote = "[passive] maladapted protocols/guidelines, the trauma team activation protocol should assign this pt level 1"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Inadequate resources: Logistics/technical",
    footnote = "[passive] required equipment was unavailable/out of service, IT system was down making pt records unavailable"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Inadequate resources: Resources",
    footnote = "[passive] insufficient available resources (not active errors such as failure to activate backup team), no OR was available"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Missed diagnosis: Missed injury",
    footnote = "[active] failure to identify injury that should have been identified given available information, reasonable clinical judgment and protocols (includes missed injuries due to skipped exam"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Missed diagnosis: Tertiary survey",
    footnote = "[active] failure to perform tertiary survey within appropriate time when indicated/in protocols"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Other errors: Level of care+ missade skador",
    footnote = "[active] pt transferred to inappropriate level of care given available information/protocols (not due to lack of resources), pt transferred to IMCU rather than ICU"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Other errors: Patient management/logistics",
    footnote = "[other] name implies interaction with systems"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Category",
    footnote = "Definitions of categories of opportunities for improvements used"
  ) %>%
  print()





# lookup to categorize ofi
    # MISSED INJURY
    # Missed injury (active failure): failure to identify injury despite adequate information. Includes misinterpretation of imagery etc.
    # Examples: surveys, radiology, explorative procedures
    # DELAYED TREATMENT
    # Delay in treatment (active failure): inappropriate delay from arrival/assessment to treatment causing pt harm. Does **not** include delays due to clinical judgment errors, i.e. "I don't think a DT is needed".
    # Examples: failure to activate trauma team, delay in hemorrhage control, delay in moving to OR, failure to activate backup surgical team, ddelay in moving from ED to OR, OR unavailable, ddelay to angio, delay in performing intervention, delay in intubation, delay due to interhospital transfer, delay in interhospital transfer, initial transfer to wrong department
    # CLINICAL JUDGMENT ERROR
    # Clinical judgment error (active failure): inappropriate plan of actions/management, given available information, causing pt harm.
    # Examples: Inadequate monitoring, medication errors, wrong level of care/unit, wrong protocol/procedure applied, wrong treatment
    # TECHNICAL ERROR (SKILL)
    # Technical error (active failure): inadequate performance of intervention/procedure causing pt harm.
    # INADEQUATE PROTOCOLS
    # Inadequate protocols (passive failure): inadequate protocols/guidelines etc. leading to pt harm.
 #   "Inadequate protocols" = c(
 #     "bristande rutin" # [passive] no/maladapted formalized routine for type cases, "no guidelines for pediatric multitrauma"
    # INADEQUATE RESOURCES
    # Inadequate resources (passive failure): lack of resources leading to pt harm.
    # Examples: lack of trauma/backup team, lack of equipment/training etc.
 #   "Inadequate resources" = c(
 #     "kompetens brist", # [passive] required competence not available, "no neurosurgeon on site/available"
#    ),
    # OTHER
    # Anything else.
  #  "Other errors" = c(
  #    "annat", # [other]
  #    "handläggning prehosp", # [other] unclear
  #    "neurokirurg" # [other] unclear




