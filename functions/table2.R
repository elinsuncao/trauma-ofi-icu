#TABLE 2: Adjusted and unadjusted logistic regression
# Data Preparation
tablereg <- ofi %>% 
  select(Sex, Age, Intubation, RTS, ISS,TimeFCT, OnDuty, daysinICU, TimeFCT, 
         ASApreinjury, Survival, OpportunityForImprovement1)

tablereg$Intubation <- ifelse(is.na(tablereg$Intubation), "Unknown", table1$Intubation)
tablereg$Intubation <- fct_relevel(tablereg$Intubation, "Not intubated", "Mechanical ventilation 0-2 days", "Mechanical ventilation 3-7 days", "Mechanical ventilation > 7 days", "Unknown")
tablereg$daysinICU <- fct_relevel(tablereg$daysinICU, "≤ 7 days", "> 7 days")
tablereg$Survival <- fct_relevel(tablereg$Survival, "Dead", "Alive")

tablereg <- na.omit(tablereg)

# Unadjusted Table
table3a <- tbl_uvregression(data = tablereg,
                            method = glm,
                            y = OpportunityForImprovement1,
                            method.args = list(family = binomial),
                            exponentiate = TRUE, 
                            label = list(
                              RTS = "Revised Trauma Score",
                              daysinICU = "Days in the ICU",
                              TimeFInt = "Time to first intervention",
                              Intubation = "Mechanical ventilation",
                              ASApreinjury = "ASA preinjury",
                              OnDuty = "On call hours",
                              TimeFCT = "Time to first CT, in minutes"
                            )) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

#print(table3a)

# Adjusted Table
#Creating linear regression 
adjusted_table <- glm(OpportunityForImprovement1 ~ Sex + Age + Intubation + RTS +  ISS + OnDuty + daysinICU + TimeFCT + ASApreinjury + Survival, family = binomial, data = tablereg) 

table3b <- tbl_regression(adjusted_table,
                          exponentiate = TRUE, 
                          label = list(RTS = "Revised Trauma Score",
                                       daysinICU = "Days in the ICU",
                                       Intubation = "Mechanical ventilation",
                                       OnDuty = "On call hours",
                                       ASApreinjury = "ASA preinjury",
                                       TimeFCT = "Time to first CT, in minutes")) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

# print(table3b)

# Merging Tables
table3_merge <- tbl_merge(tbls = list(table3a, table3b),
                          tab_spanner = c("**Unadjusted**", "**Adjusted**")) %>%
 # modify_column_merge(pattern = "{OR} ({ci}) {p.value}") %>%
  modify_caption("<div style='text-align: left; font-weight: bold; color: black'>Table 3. Unadjusted and adjusted logistic regression analyses of associations between patient level factors and opportunities for improvement (N = 1449).</div>")


print(table3_merge)

