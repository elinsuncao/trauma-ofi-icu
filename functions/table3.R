#TABLE 2: Adjusted and unadjusted logistic regression
# Data Preparation
ofi_alive <- subset(tablereg, subset = (Survival == "Alive"))

tablereg1 <- ofi_alive %>% 
  select(Sex, Age, Intubation, RTS, ISS,TimeFCT, OnDuty, daysinICU, TimeFCT, 
         ASApreinjury, OpportunityForImprovement1)

tablereg1$daysinICU <- fct_relevel(tablereg1$daysinICU, "≤ 7 days", "> 7 days")

# Unadjusted Table
#table3aalive <- tbl_uvregression(data = tablereg1,
#                            method = glm,
#                            y = OpportunityForImprovement1,
#                            method.args = list(family = binomial),
#                            exponentiate = TRUE, 
#                            label = list(
#                              RTS = "Revised Trauma Score",
#                              daysinICU = "Days in the ICU",
#                              TimeFInt = "Time to first intervention",
#                              ASApreinjury = "ASA preinjury",
#                              OnDuty = "On call hours",
#                              Intubation = "Mechanical ventilation",
#                              TimeFCT = "Time to first CT"
#                            )) %>%
#  bold_labels() %>%
#  bold_p(t = 0.05) %>%
#  hide_n = TRUE

# Adjusted Table
#Creating linear regression 
adjusted_table1 <- glm(OpportunityForImprovement1 ~ Sex + Age + Intubation + RTS +  ISS + OnDuty + daysinICU + TimeFCT + ASApreinjury, family = binomial, data = tablereg1) 

table3aalive <- tbl_regression(adjusted_table1,
                          exponentiate = TRUE, 
                          label = list(RTS = "Revised Trauma Score",
                                       daysinICU = "Days in the ICU",
                                       OnDuty = "On call hours",
                                       Intubation = "Mechanical ventilation",
                                       ASApreinjury = "ASA preinjury",
                                       TimeFCT = "Time to first CT, in minutes")) %>%
  bold_labels() %>%
  add_n() %>%
  bold_p(t = 0.05)

#Creating with patients who died
ofi_dead <- subset(tablereg, subset = (Survival == "Dead"))

tablereg2 <- ofi_dead %>% 
  select(Sex, Age, Intubation, RTS, ISS,TimeFCT, OnDuty, daysinICU, TimeFCT, 
         ASApreinjury, OpportunityForImprovement1)

tablereg2$daysinICU <- fct_relevel(tablereg2$daysinICU, "≤ 7 days", "> 7 days")

adjusted_table2 <- glm(OpportunityForImprovement1 ~ Sex + Age + Intubation + RTS +  ISS + OnDuty + daysinICU + TimeFCT + ASApreinjury, family = binomial, data = tablereg2) 

table3bdead <- tbl_regression(adjusted_table2,
                               exponentiate = TRUE, 
                               label = list(RTS = "Revised Trauma Score",
                                            daysinICU = "Days in the ICU",
                                            OnDuty = "On call hours",
                                            Intubation = "Mechanical ventilation",
                                            ASApreinjury = "ASA preinjury",
                                            TimeFCT = "Time to first CT, in minutes")) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

# Merging Tables
table3b_merge <- tbl_merge(tbls = list(table3aalive, table3bdead),
                          tab_spanner = c("**Alive**", "**Dead**")) %>%
 # modify_table_styling(table3b_merge, hide_n = TRUE) %>%
  modify_caption("<div style='text-align: left; font-weight: bold; color: black'>Table 4. Adjusted logistic regression analyses of associations between patient level factors and opportunities for improvement in patients alive and dead 30 days after hospitalization (N = 1163).</div>")

# Print the merged table
print(table3b_merge) 

