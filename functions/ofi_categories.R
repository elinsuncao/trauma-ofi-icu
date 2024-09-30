
# Load necessary libraries
library(dplyr)
library(stringr)
library(gtsummary)
library(gt)

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




