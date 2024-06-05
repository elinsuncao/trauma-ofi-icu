suppressPackageStartupMessages({
  library(rofi)
  library(dplyr)
  library(readr)
  library(stringr)
  library(hms)
  library(lubridate)
})

################################################################################
#' Prepare original dataframe
prepare_data <- function(df) {
  # drop irrelevant
  df <- df %>% 
    select(pt_age_yrs, pt_Gender, pt_asa_preinjury, inj_intention, inj_mechanism,
           inj_dominant, NISS, starts_with("AISCode_"), starts_with("ICD_"),
           DateTime_ArrivalAtHospital, Problemomrade_.FMP, ofi_review, arrival)
  
  # create x_sex factor of sex - `Male`/`Female`
  df$x_sex <- factor(df$pt_Gender, levels = c(1, 2), labels = c("Male", "Female"))
  attr(df$x_sex, "label") <- "Sex"
  
  #clean age into x_age
  df$x_age <- ifelse(df$pt_age_yrs > 150, NA, df$pt_age_yrs)
  attr(df$x_age, "label") <- "Age"
  
  # group pt_asa_preinjury into x_asa
  df$x_asa <- factor(
    case_when(
      df$pt_asa_preinjury %in% c(1, 2) ~ "a",
      df$pt_asa_preinjury == 3 ~ "b",
      df$pt_asa_preinjury %in% c(4, 5, 6) ~ "c",
      TRUE ~ NA
    ),
    levels = c("a", "b", "c"),
    labels = c("Minor", "Moderate", "Severe")
  )
  attr(df$x_asa, "label") <- "Preinjury comorbidities"
  
  # attempt to calculate exposure group PRN
  source("functions/icd.R")
  df$x_group <- as.factor(ifelse(is.na(df$inj_intention) | df$inj_intention > 3,
                                 calc_intent(na.omit(unname(select(df, starts_with("ICD_"))))) %||% df$inj_intention,
                                 df$inj_intention))
  attr(df$x_group, "label") <- "Injury intent"
  EXPOSED <- "Intentional"
  UNEXPOSED <- "Unintentional"
  
  # df$x_group <- as.factor(ifelse(is.na(df$inj_mechanism) | df$inj_mechanism > 12,
  #                       calc_moi(na.omit(unname(select(df, starts_with("ICD_"))))) %||% df$inj_mechanism,
  #                       df$inj_mechanism))
  # attr(df$x_group, "label") <- "Mechanism Of Injury"
  # EXPOSED <- "Gunshot wounds"
  # UNEXPOSED <- "Other wounds"
  
  
  
  # clean NISS into x_niss, try to calculate if missing
  source("functions/ais.R")
  df$x_niss <- ifelse(is.na(df$NISS) | df$NISS > 75,
                      calc_niss(na.omit(unname(select(df, starts_with("AISCode_"))))) %||% df$NISS, df$NISS)
  attr(df$x_niss, "label") <- "NISS"
  
  # clean arrival time into x_date, `sum(is.na(df$DateTime_ArrivalAtHospital) & is.na(df$arrival))` = 0
  df$x_date <- as.Date(ifelse(!is.na(df$DateTime_ArrivalAtHospital),
                              as.Date(df$DateTime_ArrivalAtHospital), as.Date(df$arrival)))
  attr(df$x_date, "label") <- "Hospital arrival date"
  
  # clean arrival time into x_time
  df$x_time <- as_hms(df$DateTime_ArrivalAtHospital)
  attr(df$x_time, "label") <- "Hospital arrival time"
  df$DateTime_ArrivalAtHospital <- NULL
  
  # categorize x_time into x_shift
  # https://victor.se/bjorn/holidays.php?lang=en&year=&from=2013&to=2023
  holiday <- c("2013-01-01", "2013-01-06", "2013-03-29", "2013-03-31", "2013-04-01", "2013-05-01", "2013-05-09", "2013-06-06", "2013-06-22", "2013-11-02", "2013-12-25", "2013-12-26", "2014-01-01", "2014-01-06", "2014-04-18", "2014-04-20", "2014-04-21", "2014-05-01", "2014-05-29", "2014-06-06", "2014-06-21", "2014-11-01", "2014-12-25", "2014-12-26", "2015-01-01", "2015-01-06", "2015-04-03", "2015-04-05", "2015-04-06", "2015-05-01", "2015-05-14", "2015-06-06", "2015-06-20", "2015-10-31", "2015-12-25", "2015-12-26", "2016-01-01", "2016-01-06", "2016-03-25", "2016-03-27", "2016-03-28", "2016-05-01", "2016-05-05", "2016-06-06", "2016-06-25", "2016-11-05", "2016-12-25", "2016-12-26", "2017-01-01", "2017-01-06", "2017-04-14", "2017-04-16", "2017-04-17", "2017-05-01", "2017-05-25", "2017-06-06", "2017-06-24", "2017-11-04", "2017-12-25", "2017-12-26", "2018-01-01", "2018-01-06", "2018-03-30", "2018-04-01", "2018-04-02", "2018-05-01", "2018-05-10", "2018-06-06", "2018-06-23", "2018-11-03", "2018-12-25", "2018-12-26", "2019-01-01", "2019-01-06", "2019-04-19", "2019-04-21", "2019-04-22", "2019-05-01", "2019-05-30", "2019-06-06", "2019-06-22", "2019-11-02", "2019-12-25", "2019-12-26", "2020-01-01", "2020-01-06", "2020-04-10", "2020-04-12", "2020-04-13", "2020-05-01", "2020-05-21", "2020-06-06", "2020-06-20", "2020-10-31", "2020-12-25", "2020-12-26", "2021-01-01", "2021-01-06", "2021-04-02", "2021-04-04", "2021-04-05", "2021-05-01", "2021-05-13", "2021-06-06", "2021-06-26", "2021-11-06", "2021-12-25", "2021-12-26", "2022-01-01", "2022-01-06", "2022-04-15", "2022-04-17", "2022-04-18", "2022-05-01", "2022-05-26", "2022-06-06", "2022-06-25", "2022-11-05", "2022-12-25", "2022-12-26", "2023-01-01", "2023-01-06", "2023-04-07", "2023-04-09", "2023-04-10", "2023-05-01", "2023-05-18", "2023-06-06", "2023-06-24", "2023-11-04", "2023-12-25", "2023-12-26")
  
  df$x_shift <- factor(
    ifelse(
      !df$x_date %in% holiday & !wday(df$x_date, week_start = 1) %in% c(6, 7) &
        df$x_time >= as_hms("07:30:00") & df$x_time <= as_hms("16:00:00"),
      "a", "b"),
    levels = c("a", "b"),
    labels = c("Day", "Off-hours")
  )
  attr(df$x_shift, "label") <- "Hospital arrival shift"
  
  # set x_mmc for reviewed records
  df$x_mmc <- !is.na(df$ofi_review)
  attr(df$x_mmc, "label") <- "MMC review"
  
  # clean up and translate OFIs (Problemomrade_.FMP) into x_ofi
  df$x_ofi <- str_replace_all(
    str_to_lower(df$Problemomrade_.FMP),
    c(
      "ok" = NA,
      "nej" = NA,
      "inget problemområde" = NA,
      "föredömligt handlagd" = NA,
      "dokumetation" = "dokumentation",
      "triage på akm" = "triage på akutmottagningen",
      "vårdnivå\\+\r\nmissade skador" = "vårdnivå+missade skador",
      "handläggning\r\ndokumentation" = "handläggning+dokumentation"
    )
  )
  attr(df$x_ofi, "label") <- "Opportunity For Improvement"
  
  # translate ofi and set up categories
  translate <- c(
    "triage på akm" = "Triage in the ED",
    "triage på akutmottagningen" = "Triage in the ED",
    "vårdnivå" = "Level of care",
    "vårdnivå+missade skador" = "Level of care + missed injury",
    "dokumetation" = "Documentation",
    "dokumentation" = "Documentation",
    "logistik/teknik" = "Logistics/technical",
    "resurs" = "Resources",
    "missad skada" = "Missed injury",
    "lång tid till op" = "Delay to surgery",
    "lång tid till dt" = "Delay to CT",
    "kompetens brist" = "Competence",
    "kommunikation" = "Communication",
    "kommunikation+missad skada" = "Communication + missed injury",
    "handläggning" = "Patient management",
    "handläggning/logistik" = "Patient management/logistics",
    "handläggning+dokumentation" = "Patient management + documentation",
    "handläggning prehosp" = "Prehospital management",
    "traumakriterier/styrning" = "Trauma criteria/guidelines",
    "tertiär survey" = "Tertiary survey",
    "bristande rutin" = "Inadequate routine",
    "neurokirurg" = "Neurosurgeon",
    "annat" = "Other"
  )
  df$x_ofi <- unname(translate[df$x_ofi])
  source("functions/categories.R")
  categories <- lapply(get_category_cfg(), function(x) unname(translate[x]))
  #categories <- setNames(unique(na.omit(df$x_ofi)), unique(na.omit(df$x_ofi)))
  
  # set up categorization lookup - reverse and flatten members
  categorize <- as.list(unlist(map(
    names(categories),
    ~setNames(rep(.x, length(categories[[.x]])), categories[[.x]])), use.names = TRUE
  ))
  #categorize <- as.list(categories)
  
  # categorize x_ofi into x_ofi_category
  df$x_ofi_category <- factor(ifelse(!is.na(df$x_ofi), categorize[df$x_ofi], NA),
                              levels = names(categories),
                              labels = names(categories))
  attr(df$x_ofi_category, "label") <- "OFI category"
  
  # categorize exposure into x_exposure
  # [x_group] 1: Accident, 2: Self-inflicted, 3: Assault, 4: Other, NA missing data
  # [inj_mechanism] 1. Traffic; MVA, 2. Traffic; MCA, 3. Traffic; bicycle, 4. Traffic; pedest,
  #                 5. Traffic; other, 6. GSW, 7. Stab, 8. Blunt, 9. Low fall, 10. High fall,
  #                 11. Blast, 12. Other, 999. Unknown
  df$x_exposure <- factor(case_when(
    df$x_group %in% c(1, 2) ~ UNEXPOSED,
    df$x_group == 3 ~ EXPOSED,
    #df$inj_mechanism %in% c(6) ~ EXPOSED,
    #df$inj_mechanism %in% c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12) ~ UNEXPOSED,
    TRUE ~ NA
  ), levels = c(UNEXPOSED, EXPOSED))
  attr(df$x_exposure, "label") <- "Trauma type"
  
  # "static" vars
  .exposed <<- EXPOSED
  .unexposed <<- UNEXPOSED
  .categories <<- categories
  .categorize <<- categorize
  .labels <<- c(
    list(
      x_shift_day = "Day",
      x_shift_off = "Off-hours",
      x_ofi_category = "OFI category"
    ),
    as.list(sapply(names(df)[grepl("^x_", names(df))], function(x) attr(df[[x]], "label")))
  )
  
  return(df[, grep("^x_", names(df), value = TRUE), drop = FALSE])
}

################################################################################
#' Trim categories to actual data
trim_categories <- function(df) {
  # drop unused categories
  .categories <<- .categories[names(.categories) %in% df$x_ofi_category] %>% 
    lapply(function(x) x[x %in% df$x_ofi])
  .categorize <<- .categorize[names(.categorize) %in% df$x_ofi]
  
  return(.categories)
}

################################################################################
#' Get exposed label
exposed <- function() {
  return(.exposed)
}

################################################################################
#' Get unexposed label
unexposed <- function() {
  return(.unexposed)
}

# lookup to categorize ofi
get_category_cfg <- function() {
  list(
    # MISSED INJURY
    # Missed injury (active failure): failure to identify injury despite adequate information. Includes misinterpretation of imagery etc.
    # Examples: surveys, radiology, explorative procedures
    "Missed injury" = c(
      "missad skada", # [active] failure to identify injury that should have been identified given available information, reasonable clinical judgment and protocols (includes missed injuries due to skipped exams)
      "tertiär survey" # [active] failure to perform tertiary survey within appropriate time when indicated/in protocols
    ),
    # DELAYED TREATMENT
    # Delay in treatment (active failure): inappropriate delay from arrival/assessment to treatment causing pt harm. Does **not** include delays due to clinical judgment errors, i.e. "I don't think a DT is needed".
    # Examples: failure to activate trauma team, delay in hemorrhage control, delay in moving to OR, failure to activate backup surgical team, ddelay in moving from ED to OR, OR unavailable, ddelay to angio, delay in performing intervention, delay in intubation, delay due to interhospital transfer, delay in interhospital transfer, initial transfer to wrong department
    "Delay in treatment" = c(
      "lång tid till op", # [active] failure to move pt to OR within an appropriate time, "let's monitor this pt further before deciding on surgery"
      "lång tid till dt" # [active] failure to perform CT when indicated/in protocols, "
    ),
    # CLINICAL JUDGMENT ERROR
    # Clinical judgment error (active failure): inappropriate plan of actions/management, given available information, causing pt harm.
    # Examples: Inadequate monitoring, medication errors, wrong level of care/unit, wrong protocol/procedure applied, wrong treatment
    "Clinical judgment error" = c(
      "triage på akutmottagningen", # [active] pt assigned inappropriate level given available information/protocols, "failure to activate trauma team"
      "vårdnivå", # pt transferred to inappropriate level of care given available information/protocols (not due to lack of resources), "pt transferred to IMCU rather than ICU"
      "vårdnivå+missade skador", # [active] pt transferred to inappropriate level of care given available information/protocols (not due to lack of resources), "pt transferred to IMCU rather than ICU"
      "handläggning", # [active] failure to perform the appropriate exams and interventions in appropriate order and time given available information/protocols (not due to lack of resources) "sent pt to CT with inadequately secured airway"
      "kommunikation", # [active] failures in communication with parties outside the treating team, e.g. consultants, receiving unit etc., "did not consult neurosurgeon before transfer"
      "kommunikation+missad skada" # [active] failures in communication with parties outside the treating team, e.g. consultants, receiving unit etc., "did not consult neurosurgeon before transfer"
    ),
    # TECHNICAL ERROR (SKILL)
    # Technical error (active failure): inadequate performance of intervention/procedure causing pt harm.
    "Technical error" = c(
    ),
    # INADEQUATE PROTOCOLS
    # Inadequate protocols (passive failure): inadequate protocols/guidelines etc. leading to pt harm.
    "Inadequate protocols" = c(
      "traumakriterier/styrning", # [passive] maladapted protocols/guidelines, "the trauma team activation protocol should assign this pt level 1"
      "bristande rutin" # [passive] no/maladapted formalized routine for type cases, "no guidelines for pediatric multitrauma"
    ),
    # INADEQUATE RESOURCES
    # Inadequate resources (passive failure): lack of resources leading to pt harm.
    # Examples: lack of trauma/backup team, lack of equipment/training etc.
    "Inadequate resources" = c(
      "kompetens brist", # [passive] required competence not available, "no neurosurgeon on site/available"
      "resurs", # [passive] insufficient available resources (not active errors such as failure to activate backup team), "no OR was available"
      "logistik/teknik" # [passive] required equipment was unavailable/out of service, "IT system was down making pt records unavailable"
    ),
    # OTHER
    # Anything else.
    "Other errors" = c(
      "annat", # [other]
      "handläggning/logistik", # [other] name implies interaction with systems?
      "handläggning prehosp", # [other] unclear
      "neurokirurg" # [other] unclear
    )
  )
}

# clean up and translate OFIs (Problemomrade_.FMP) into x_ofi
df$x_ofi <- str_replace_all(
  str_to_lower(df$Problemomrade_.FMP),
  c(
    "ok" = NA,
    "nej" = NA,
    "inget problemområde" = NA,
    "föredömligt handlagd" = NA,
    "dokumetation" = "dokumentation",
    "triage på akm" = "triage på akutmottagningen",
    "vårdnivå\\+\r\nmissade skador" = "vårdnivå+missade skador",
    "handläggning\r\ndokumentation" = "handläggning+dokumentation"
  )
)
attr(df$x_ofi, "label") <- "Opportunity For Improvement"



# translate ofi and set up categories
translate <- c(
  "triage på akm" = "Triage in the ED",
  "triage på akutmottagningen" = "Triage in the ED",
  "vårdnivå" = "Level of care",
  "vårdnivå+missade skador" = "Level of care + missed injury",
  "dokumetation" = "Documentation",
  "dokumentation" = "Documentation",
  "logistik/teknik" = "Logistics/technical",
  "resurs" = "Resources",
  "missad skada" = "Missed injury",
  "lång tid till op" = "Delay to surgery",
  "lång tid till dt" = "Delay to CT",
  "kompetens brist" = "Competence",
  "kommunikation" = "Communication",
  "kommunikation+missad skada" = "Communication + missed injury",
  "handläggning" = "Patient management",
  "handläggning/logistik" = "Patient management/logistics",
  "handläggning+dokumentation" = "Patient management + documentation",
  "handläggning prehosp" = "Prehospital management",
  "traumakriterier/styrning" = "Trauma criteria/guidelines",
  "tertiär survey" = "Tertiary survey",
  "bristande rutin" = "Inadequate routine",
  "neurokirurg" = "Neurosurgeon",
  "annat" = "Other"
)
df$x_ofi <- unname(translate[df$x_ofi])
source("functions/categories.R")
categories <- lapply(get_category_cfg(), function(x) unname(translate[x]))
#categories <- setNames(unique(na.omit(df$x_ofi)), unique(na.omit(df$x_ofi)))



# set up categorization lookup - reverse and flatten members
categorize <- as.list(unlist(map(
  names(categories),
  ~setNames(rep(.x, length(categories[[.x]])), categories[[.x]])), use.names = TRUE
))
#categorize <- as.list(categories)

# categorize x_ofi into x_ofi_category
df$x_ofi_category <- factor(ifelse(!is.na(df$x_ofi), categorize[df$x_ofi], NA),
                            levels = names(categories),
                            labels = names(categories))
attr(df$x_ofi_category, "label") <- "OFI category"

