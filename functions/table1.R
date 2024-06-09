
#TABLE 1: Sample characteristics
#Creating new table with defined data 
library(dplyr)
library(gt)
#install.packages("flextable")
library(flextable)

table1 <- ofi %>% 
  select(Sex, Age, Intubation, RTS, ISS, TimeFCT, OnDuty, daysinICU, 
         ASApreinjury, Survival, OpportunityForImprovement)


table1$Intubation <- ifelse(is.na(table1$Intubation), "Unknown", table1$Intubation)
table1 <- na.omit(table1)

table2 <- table1 %>%
  mutate(Intubation = factor(Intubation, levels = c("Not intubated", "Mechanical ventilation 1-7 days", "Mechanical ventilation > 7 days", "Unknown"))) %>%
  tbl_summary(by = OpportunityForImprovement,
              type = list(OnDuty ~ "dichotomous"),
              label = list(RTS = "Revised Trauma Score",
                           ISS = "Injury Severity Score",
                           Intubation = "Mechanical ventilation",
                           TimeFCT = "Time to first CT", 
                           daysinICU = "Days in the ICU",
                           OnDuty = "On call hours",
                           ASApreinjury = "ASA preinjury"),
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              ),
              missing = "ifany",
              missing_text = "Missing",
              digits = all_continuous() ~ 0
  )  %>%
  modify_table_styling(
    columns = label,
    rows = label == "On call hours",
    footnote = "Arrival at the hospital on Saturday or Sunday, or arrival at the hospital before 8 am or after 5 pm"
  ) %>%
  bold_labels() %>% 
  add_overall(last = TRUE) %>% 
  #add_p() %>%
  #bold_p(t=0.05) %>%
  modify_caption("<div style='text-align: left; font-weight: bold; color: black'>Table 2. Sample Characteristics</div>") %>% 
 # as_flex_table() %>%
  print()
