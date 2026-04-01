## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Settings
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    
##
## Dependencies:      World Justice Project
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Load Packages                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(pacman)
p_load(char = c(
  #Load data
  "readxl", "openxlsx",
  # Visualizations
  "ggplot2", "scales",
  # Data managment
  "purrr", "rlang",
  # Good 'ol Tidyverse
  "tidyverse",
  # Terminal
  "optparse"
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (Sys.info()["user"] == "santiagopardo") {
  
  path2DA <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics/9. OECD/2. Data")
  path2EU <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/EU Subnational")
  
  
} else {
  "INSERT PATH"
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Functions                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### 
### Cleaning Process                                                           
###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

A2J_dataset <- function(data, 
                        countries = NULL, 
                        target_year = "latest", 
                        severity_threshold = 4) {
  
  legprob_bin <- paste0("q19_", legal_problems)
  legprob_sev <- paste0("q20_", legal_problems)
  
  # Filter countries
  df <- data
  if (!is.null(countries)) {
    df <- df %>% filter(country %in% countries)
  }
  
  # Select year
  if (target_year == "latest") {
    df <- df %>%
      group_by(country) %>%
      mutate(lastYear = max(year, na.rm = TRUE)) %>%
      mutate(lastYear = if_else(country %in% c("Dominican Republic", "United States"), 2018, 
                                if_else(country %in% c("Ireland"), 2021,
                                        lastYear)
      )
      ) %>%
      ungroup() %>%
      filter(year == lastYear)
  } else {
    df <- df %>% filter(year == target_year)
  }
  
  # Clean and derive variables
  df_clean <- df %>%
    
    mutate(across(
      c(q30, q34, q36a, q37b, q41b, q1g, q24, q26, q27, q28, q29, 
        starts_with("q19_"), starts_with("q25_"), starts_with("q32")),
      ~ as.numeric(haven::zap_labels(.))
    )) %>%
    
    # (rest of the function remains unchanged from here on)
    mutate(
      rp_outcome = case_when(
        q30 %in% c(1, 2) | q34 %in% c(1, 2) ~ NA_real_,
        q30 == 4 | q34 == 4 ~ 1,
        q30 == 3 | q34 == 3 ~ 0,
        TRUE ~ NA_real_
      ),
      rp_cost = case_when(
        # 1. If you incurred costs, but they were easy to pay --> not in gap
        (q30 == 3 | q30 == 4 | q34 == 3 | q34 == 4) & (q37d == 1 | q37d == 2) & q37c == 1 ~ 1,
        # 2. If you incurred costs that were difficult to pay --> in the justice gap
        (q30 == 3 | q30 == 4 | q34 == 3 | q34 == 4) & (q37d == 3 | q37d == 4) & q37c == 1 ~ 0,
        # 3. You did not incur costs -- not in justice gap
        q37c == 0 ~ 1,
        T ~ NA_real_
      ),
      rp_fair = case_when(
        q30 %in% c(1, 2) | q34 %in% c(1, 2) ~ NA_real_,
        q36a == 1 ~ 1,
        q36a == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      rp_time = case_when(
        (q37b >= 0 & q37b < 13) & (q30 %in% c(3, 4) | q34 %in% c(3, 4)) ~ 1,
        (q37b > 12) & (q30 %in% c(3, 4) | q34 %in% c(3, 4)) ~ 0,
        q37b == -8888 ~ NA_real_,
        q37b == -9999 ~ NA_real_,
        q30 %in% c(1, 2) | q34 %in% c(1, 2) ~ NA_real_,
        TRUE ~ NA_real_
      ),
      access2info = case_when(
        q41b %in% c(1, 2) ~ 1,
        q41b %in% c(3, 4) ~ 0,
        TRUE ~ NA_real_
      ),
      trust_in_judges = case_when(
        q1g %in% c(1, 2) ~ 1,
        q1g %in% c(3, 4) ~ 0,
        TRUE ~ NA_real_
      ),
      access2rep = case_when(
        q24 == 1 & (q25_2 == 1 | q25_3 == 1 | q25_4 == 1 | q25_5 == 1 | q25_6 == 1 | q25_7 == 1 | q25_8 == 1) ~ 1,
        q24 == 1 & q25_1 == 1 & q26 == 1 ~ 1,
        q24 == 2 & q27 %in% c(1, 2, 3) ~ 1,
        q24 == 1 & (q25_1 == 1 | q25_9 == 1) ~ 0,
        q24 == 2 & q27 %in% c(4:10) ~ 0,
        TRUE ~ NA_real_
      ),
      access2DRM = case_when(
        q28 == 1 ~ 1,
        q28 == 0 & q29 %in% c(3, 5, 6, 7, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      p_consumer = case_when(
        q19_A1 == 1 | q19_A2 == 1 | q19_A3 == 1 ~ 1,
        q19_A1 == 0 & q19_A2 == 0 & q19_A3 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      p_land = case_when(
        q19_B1 == 1 | q19_B2 == 1 | q19_B3 == 1 | q19_B4 == 1 ~ 1,
        q19_B1 == 0 & q19_B2 == 0 & q19_B3 == 0 & q19_B4 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      p_housing = case_when(
        q19_C1 == 1 | q19_C2 == 1 | q19_C4 == 1 ~ 1,
        q19_C1 == 0 & q19_C2 == 0 & q19_C4 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      p_family = case_when(
        q19_D1 == 1 | q19_D2 == 1 | q19_D3 == 1 | q19_D4 == 1 | q19_D5 == 1 | q19_D6 == 1 ~ 1,
        q19_D1 == 0 & q19_D2 == 0 & q19_D3 == 0 & q19_D4 == 0 & q19_D5 == 0 & q19_D6 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      p_education = case_when(
        q19_E1 == 1 | q19_E2 == 1 ~ 1,
        q19_E1 == 0 & q19_E2 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      p_community = case_when(
        q19_E3 == 1 | q19_C3 == 1 ~ 1,
        q19_E3 == 0 & q19_C3 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      p_accidental = case_when(
        q19_F1 == 1 | q19_F2 == 1 ~ 1,
        q19_F1 == 0 & q19_F2 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      p_employment = case_when(
        q19_G1 == 1 | q19_G2 == 1 | q19_G3 == 1 ~ 1,
        q19_G1 == 0 & q19_G2 == 0 & q19_G3 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      p_public = case_when(
        q19_H1 == 1 | q19_H2 == 1 | q19_H3 == 1 | q19_J4 == 1 ~ 1,
        q19_H1 == 0 & q19_H2 == 0 & q19_H3 == 0 & q19_J4 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      p_id = case_when(
        q19_J1 == 1 | q19_J2 == 1 | q19_J3 == 1 ~ 1,
        q19_J1 == 0 & q19_J2 == 0 & q19_J3 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      p_money = case_when(
        q19_L1 == 1 | q19_L2 == 1 | q19_K1 == 1 | q19_K2 == 1 | q19_K3 == 1 ~ 1,
        q19_L1 == 0 & q19_L2 == 0 & q19_K1 == 0 & q19_K2 == 0 & q19_K3 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      p_law_enforcement = case_when(
        q19_I1 == 1 ~ 1,
        q19_I1 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      category_problem_selected = case_when(
        q21 %in% c("A1", "A2", "A3")                   ~ "Consumer",
        q21 %in% c("B1", "B2", "B3", "B4")             ~ "Land",
        q21 %in% c("C1", "C2", "C4")                   ~ "Housing",
        q21 %in% c("D1", "D2", "D3", "D4", "D5", "D6") ~ "Family",
        q21 %in% c("E1", "E2")                         ~ "Education",
        q21 %in% c("E3", "C3")                         ~ "Community",
        q21 %in% c("F1", "F2")                         ~ "Accidental",
        q21 %in% c("G1", "G2", "G3")                   ~ "Employment",
        q21 %in% c("H1", "H2", "H3", "J4")             ~ "Public",
        q21 %in% c("J1", "J2", "J3")                   ~ "ID",
        q21 %in% c("L1", "L2", "K1", "K2", "K3")       ~ "Money",
        q21 %in% c("I1")                               ~ "Law Enforcement",
        T ~ NA_character_
      ),
      gender = case_when(
        gend == 1 ~ "Male",
        gend == 2 ~ "Female",
        T ~ NA_character_
      ),
       age_group = case_when(
        age >= 18 & age <= 29 ~ "18-29",
        age >= 30 & age <= 44 ~ "30-44",
        age >= 45 & age <= 59 ~ "45-59",
        age >= 60 ~ "60+",
        TRUE ~ NA_character_
      ),
      fin_status = case_when(
        fin == 1 | fin == 2            ~ "Low Economic Status",
        fin == 3 | fin == 4 | fin == 5 ~ "High Economic Status",
        TRUE ~ NA_character_
      ),
      skin_color = case_when(
        COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4 | COLOR == 5                ~ "White",
        COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10 | COLOR == 11 ~ "Non-White",
        T ~ NA_character_
      ),
      area = case_when(
        Urban == 1 ~ "Urban",
        Urban == 2 ~ "Rural",
        TRUE ~ NA_character_
      ),
      education_status = case_when(
        edu == 1 | edu == 2 | edu == 3 | edu == 4 ~ "Basic Education", 
        edu == 5 | edu == 6 | edu == 7 ~ "Higher Education",
      ),
      method = case_when(
        method == "F2F"          ~ "F2F",
        method == "Face-to-face" ~ "F2F",
        method == "Face-to-Face" ~ "F2F",
        method == "Online"       ~ "Online",
        T ~ NA_character_
      )
    ) %>%
    mutate(
      flw_app_assitance_bin = case_when(
        access2rep == 1 & (q25_3 == 1 | q25_5 == 1 | q25_6 == 1 | q25_7 == 1 | q25_8 == 1) & (q25_2 != 1 | q25_4 != 1)  ~ "Community justice workers",
        access2rep == 1 & (q25_2 == 1 | q25_4 == 1) & (q25_3 != 1 | q25_5 != 1 | q25_6 != 1 | q25_7 != 1 | q25_8 != 1)  ~ "No community justice workers",
        access2rep == 1 & (q25_1 == 1 & q26 == 1) & (q25_2 != 1 | q25_4 != 1)                                           ~ "Community justice workers",
        
        TRUE ~ NA_character_
      ),
      access2DRM_bin = case_when(
        access2DRM == 1 & (q32d == 1 | q32e == 1 | q32f == 1) & (q32a != 1 | q32b != 1 | q32c != 1 | q32g != 1)         ~ "Alternative Dispute Resolution Mechanism",
        access2DRM == 1 & (q32a == 1 | q32b == 1 | q32c == 1 | q32g == 1)  & (q32d != 1 | q32e != 1 | q32f != 1)        ~ "Formal Dispute Resolution Mechanism",
        access2DRM == 1 & (q32a == 1 | q32b == 1 | q32c == 1 | q32g == 1)  & (q32d != 1 | q32e != 1 | q32f != 1)        ~ "Formal Dispute Resolution Mechanism",
        TRUE ~ NA_character_
      ),
      flw_app_assistance_informal = case_when(
        access2rep == 1 & (q25_3 == 1 | q25_5 == 1 | q25_6 == 1 | q25_7 == 1 | q25_8 == 1) & (q25_2 != 1 | q25_4 != 1)  ~ "Community justice workers",
        access2rep == 1 & (q25_1 == 1 & q26 == 1) & (q25_2 != 1 | q25_4 != 1)                                           ~ "Community justice workers",
        access2rep == 1 ~ "No informal justice workers"
      ),
      flw_app_assistance_formal = case_when(
        access2rep == 1 & (q25_2 == 1 | q25_4 == 1) & (q25_3 != 1 | q25_5 != 1 | q25_6 != 1 | q25_7 != 1 | q25_8 != 1) ~ "Formal justice workers",
        access2rep == 1 ~ "No formal justice workers"
      ),
      flw_app_assistance_both = case_when(
        flw_app_assistance_informal == "Community justice workers" & flw_app_assistance_formal == "Formal justice workers" ~ "Both",
        access2rep == 1 ~ "No both"
      ),
      access2DRM_informal = case_when(
        access2DRM == 1 & (q32d == 1 | q32e == 1 | q32f == 1) & (q32a != 1 | q32b != 1 | q32c != 1 | q32g != 1)         ~ "Alternative Dispute Resolution Mechanism",
        access2DRM == 1 ~ "No informal"
      ),
      access2DRM_formal = case_when(
        access2DRM == 1 & (q32a == 1 | q32b == 1 | q32c == 1 | q32g == 1)  & (q32d != 1 | q32e != 1 | q32f != 1)        ~ "Formal Dispute Resolution Mechanism",
        access2DRM == 1 & (q32a == 1 | q32b == 1 | q32c == 1 | q32g == 1)  & (q32d != 1 | q32e != 1 | q32f != 1)        ~ "Formal Dispute Resolution Mechanism",
        access2DRM == 1 ~ "No formal"
      ),
      access2DRM_both = case_when(
        access2DRM_informal == "Alternative Dispute Resolution Mechanism" & access2DRM_formal == "Formal Dispute Resolution Mechanism" ~ "Both",
        access2DRM == 1 ~ "No both"
      ),
      assistance_group = case_when(
        flw_app_assistance_both     == "Both"                      ~ "Both",
        flw_app_assistance_formal   == "Formal justice workers"    ~ "Formal justice\nworkers",
        flw_app_assistance_informal == "Community justice workers" ~ "Community justice\nworkers",
        TRUE ~ NA_character_
      ),
      access2DRM_group = case_when(
        access2DRM_both     == "Both"                                       ~ "Both",
        access2DRM_formal   == "Formal Dispute Resolution Mechanism"        ~ "Formal Dispute \nResolution Mechanism",
        access2DRM_informal == "Alternative Dispute Resolution Mechanism"   ~ "Alternative Dispute \nResolution Mechanism",
        TRUE ~ NA_character_
      )
    ) 
  
  
  df_clean <- df_clean %>%
    mutate(across(all_of(c(legprob_bin, legprob_sev)), ~ as.numeric(haven::zap_labels(.)))) %>%
    mutate(across(all_of(legprob_bin), ~ case_when(as.numeric(.x) == 99 ~ NA_real_, TRUE ~ as.numeric(.x)))) %>%
    mutate(across(all_of(legprob_bin), ~ as.integer(. == 1))) %>%
    mutate(legprob = as.integer(rowSums(across(all_of(legprob_bin)), na.rm = TRUE) > 0)) %>%
    filter(q21 %in% legal_problems) %>%
    select(country, country_code, nuts_id, year, reweighted, income_group, method,
           all_of(legprob_bin), all_of(legprob_sev), q21, legprob, legprob_bin, legprob_sev, 
           q30, q34, q36a, q37b, q41b, q1g, q24, starts_with("q25_"),
           q27, q26, q28, q29, starts_with("q32"),
           rp_outcome, rp_fair, rp_time, rp_cost, access2info, trust_in_judges, access2rep, 
           access2DRM, starts_with("p_"), assistance_group, access2DRM_group, category_problem_selected,
           gender, age_group, fin_status, skin_color, area, education_status
           )

  df_clean <- df_clean %>%
    rowwise() %>%
    mutate(
      q21_selected = paste0("q20_", q21),
      q20_selected = cur_data()[[q21_selected]]
      ) %>%
    ungroup()
  
  df_clean <- df_clean %>%
    mutate(
      legprob_sev = 
        case_when(
          q20_selected >= severity_threshold & q20_selected < 98 ~ 1,
          q20_selected < severity_threshold ~ 0,
          TRUE ~ NA_real_        
          )
    )
    

  if(severity_threshold == 0) {
      
      df_clean <- df_clean %>%
        filter(legprob == 1)
      
    } else {
      
      df_clean <- df_clean %>%
        filter(legprob == 1, 
               legprob_sev == 1) 
    }
  
  return(df_clean)
}

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### 
### Estimation process                                                           
###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

avg_estimation <- function(data, var, weighted_countries = eu_countries, 
                           weight_col = "reweighted", region_col = "nuts_id") {
  
  var_sym <- rlang::ensym(var)                      # Capture the variable as symbol
  var_name <- rlang::as_name(var_sym)               # Get name as string
  var_nobs <- paste0(var_name, "_nobs")             # Name for observation count
  
  # STEP 1: Split weighted vs unweighted
  data_weighted <- data %>% filter(country %in% weighted_countries)
  data_unweighted <- data %>% filter(!country %in% weighted_countries)
  
  # STEP 2: Weighted estimation
  weighted_result <- data_weighted %>%
    group_by(country, .data[[region_col]], year) %>%
    summarise(
      reweighted = mean(.data[[weight_col]], na.rm = TRUE),
      indicator = mean(!!var_sym, na.rm = TRUE),
      n = sum(!is.na(!!var_sym)),
      .groups = "drop"
    ) %>%
    mutate(weighted_value = indicator * reweighted) %>%
    group_by(country, year) %>%
    summarise(
      !!var_name := sum(weighted_value, na.rm = TRUE),
      n = sum(n, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(!!var_nobs := n)
  
  # STEP 3: Unweighted estimation
  unweighted_result <- data_unweighted %>%
    group_by(country, year) %>%
    mutate(      
      nobs = sum(!is.na(.data[[var_name]]))
      ) %>%
    group_by(country, year) %>%
    summarise(
      !!var_name := mean(.data[[var_name]], na.rm = TRUE),
      n = mean(nobs, na.rm = T),
      .groups = "drop"
    ) %>%
    rename(!!var_nobs := n)
  
  # STEP 4: Combine and return
  bind_rows(weighted_result, unweighted_result) %>%
    arrange(country)
}


estimate_A2J_indicators <- function(problem_codes, 
                                    severity_threshold, 
                                    data, 
                                    countries = eu_countries, 
                                    indicators) {
  assign("legal_problems", problem_codes, envir = .GlobalEnv)
  
  # Step 1: Estimate prevalence BEFORE filtering
  prevalence_df <- estimate_legprob_sev(data, 
                                        weighted_countries = eu_countries, 
                                        severity_threshold = severity_threshold)
  
  # Step 2: Filter and compute indicators
  A2J.df <- A2J_dataset(data = data, severity_threshold = severity_threshold)
  
  # Step 3: Estimate each indicator and number of observations
  results_list <- lapply(indicators, function(varname) {
    avg_estimation(data = A2J.df, var = !!rlang::sym(varname), weighted_countries = eu_countries)
  })
  
  # Step 4: Merge all indicator tables (including *_nobs columns)
  indicator_df <- Reduce(function(x, y) full_join(x, y, by = c("country", "year")), results_list)
  
  # Step 5: Join prevalence estimates
  full_join(indicator_df, prevalence_df, by = c("country", "year"))
}



estimate_legprob_sev <- function(data, 
                                 countries = NULL,
                                 weighted_countries = eu_countries, 
                                 target_year = "latest",
                                 weight_col = "reweighted", 
                                 region_col = "nuts_id",
                                 severity_threshold = severity_threshold) {
  
  legprob_bin <- paste0("q19_", legal_problems)
  legprob_sev <- paste0("q20_", legal_problems)
  
  # Filter countries
  df <- data
  if (!is.null(countries)) {
    df <- df %>% filter(country %in% countries)
  }
  
  # Select year
  if (target_year == "latest") {
    df <- df %>%
      group_by(country) %>%
      mutate(lastYear = max(year, na.rm = TRUE)) %>%
      mutate(lastYear = if_else(country %in% c("Dominican Republic", "United States"), 2018, 
                                if_else(country %in% c("Ireland"), 2021, lastYear))) %>%
      ungroup() %>%
      filter(year == lastYear)
  } else {
    df <- df %>% filter(year == target_year)
  }
  
  # Clean and derive variables
  df_clean <- df %>%
    mutate(across(all_of(c(legprob_bin, legprob_sev)), ~ as.numeric(haven::zap_labels(.)))) %>%
    mutate(across(all_of(legprob_bin), ~ case_when(as.numeric(.x) == 99 ~ NA_real_, TRUE ~ as.numeric(.x)))) %>%
    mutate(across(all_of(legprob_bin), ~ as.integer(. == 1))) %>%
    mutate(legprob = as.integer(rowSums(across(all_of(legprob_bin)), na.rm = TRUE) > 0)) %>%
    mutate(across(all_of(legprob_sev), ~ case_when(
      . >= severity_threshold & . < 98 ~ 1,
      . < severity_threshold ~ 0,
      TRUE ~ NA_real_
    ))) %>%
    mutate(legprob_sev = as.integer(rowSums(across(all_of(legprob_sev)), na.rm = TRUE) > 0))
  
  # Split
  data_weighted <- df_clean %>% filter(country %in% weighted_countries)
  data_unweighted <- df_clean %>% filter(!country %in% weighted_countries)
  
  # Weighted
  if (severity_threshold == 0) {
    weighted <- data_weighted %>%
      group_by(country, .data[[region_col]], year) %>%
      summarise(
        reweighted = mean(.data[[weight_col]], na.rm = TRUE),
        prevalence = mean(legprob, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(weighted_value = prevalence * reweighted) %>%
      group_by(country, year) %>%
      summarise(
        legprob_sev = sum(weighted_value, na.rm = TRUE),
        legprob_sev_nobs = sum(n, na.rm = TRUE),
        .groups = "drop"
      )
    
    unweighted <- data_unweighted %>%
      group_by(country, year) %>%
      summarise(
        legprob_sev = mean(legprob, na.rm = TRUE),
        legprob_sev_nobs = n(),
        .groups = "drop"
      )
    
    bind_rows(weighted, unweighted) %>%
      arrange(country)
    
  } else {
    weighted <- data_weighted %>%
      group_by(country, .data[[region_col]], year) %>%
      summarise(
        reweighted = mean(.data[[weight_col]], na.rm = TRUE),
        prevalence = mean(legprob_sev, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(weighted_value = prevalence * reweighted) %>%
      group_by(country, year) %>%
      summarise(
        legprob_sev = sum(weighted_value, na.rm = TRUE),
        legprob_sev_nobs = sum(n, na.rm = TRUE),
        .groups = "drop"
      )
    
    unweighted <- data_unweighted %>%
      group_by(country, year) %>%
      summarise(
        legprob_sev = mean(legprob_sev, na.rm = TRUE),
        legprob_sev_nobs = n(),
        .groups = "drop"
      )
    
    bind_rows(weighted, unweighted) %>%
      arrange(country)
  }
}

nobs_criteria <- function(df, threshold = 50) {
  # Encuentra columnas de conteo que terminan en _nobs
  ncols <- grep("_nobs$", names(df), value = TRUE, ignore.case = TRUE)
  if (length(ncols) == 0) return(df)

  for (nc in ncols) {
    # El prefijo es todo antes de "_nobs"
    prefix <- sub("_nobs$", "", nc)

    # Busca la columna del indicador con ese prefijo exacto
    if (!prefix %in% names(df)) next

    # Filtra valores donde nobs <= threshold o NA
    idx_mask <- is.na(df[[nc]]) | df[[nc]] <= threshold

    # Pone NA (o vacío) en el indicador
    df[[prefix]][idx_mask] <- NA
  }

  df
}

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###
### Sociodemographic Data Bank Functions
###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

avg_estimation_by_demo <- function(data, var, demo_var,
                                   weighted_countries = eu_countries,
                                   weight_col = "reweighted",
                                   region_col = "nuts_id") {


  var_sym  <- rlang::ensym(var)
  var_name <- rlang::as_name(var_sym)
  demo_sym <- rlang::ensym(demo_var)
  demo_name <- rlang::as_name(demo_sym)

  # Split weighted vs unweighted
  data_weighted   <- data %>% filter(country %in% weighted_countries)
  data_unweighted <- data %>% filter(!country %in% weighted_countries)

  # Weighted estimation by demographic group
  weighted_result <- data_weighted %>%
    group_by(country, .data[[region_col]], year, !!demo_sym) %>%
    summarise(
      reweighted = mean(.data[[weight_col]], na.rm = TRUE),
      indicator  = mean(!!var_sym, na.rm = TRUE),
      n = sum(!is.na(!!var_sym)),
      .groups = "drop"
    ) %>%
    mutate(weighted_value = indicator * reweighted) %>%
    group_by(country, year, !!demo_sym) %>%
    summarise(
      value = sum(weighted_value, na.rm = TRUE),
      nobs  = sum(n, na.rm = TRUE),
      .groups = "drop"
    )

  # Unweighted estimation
  unweighted_result <- data_unweighted %>%
    group_by(country, year, !!demo_sym) %>%
    summarise(
      value = mean(!!var_sym, na.rm = TRUE),
      nobs  = sum(!is.na(!!var_sym)),
      .groups = "drop"
    )

  bind_rows(weighted_result, unweighted_result) %>%
    mutate(
      indicator   = var_name,
      demographic = demo_name
    ) %>%
    rename(group = !!demo_sym) %>%
    filter(!is.na(group)) %>%
    select(country, year, demographic, group, indicator, value, nobs) %>%
    arrange(country, group)
}

generate_databank <- function(data, indicators, demo_vars,
                              weighted_countries = eu_countries,
                              include_national = TRUE) {

  results_list <- list()

  # Add national estimates first (no demographic disaggregation) for validation
  if (include_national) {
    message("   Processing: National estimates...")
    for (ind in indicators) {
      nat_result <- avg_estimation(
        data = data,
        var = !!rlang::sym(ind),
        weighted_countries = weighted_countries
      ) %>%
        mutate(
          demographic = "National",
          group = "National",
          indicator = ind
        ) %>%
        rename(value = !!rlang::sym(ind), nobs = !!rlang::sym(paste0(ind, "_nobs"))) %>%
        select(country, year, demographic, group, indicator, value, nobs)

      results_list[[paste0("National_", ind)]] <- nat_result
    }
    gc()
  }

  # Generate estimates for each demographic variable (process one at a time)
  for (demo in demo_vars) {
    message(paste0("   Processing: ", demo, "..."))
    for (ind in indicators) {
      demo_result <- avg_estimation_by_demo(
        data = data,
        var = !!rlang::sym(ind),
        demo_var = !!rlang::sym(demo),
        weighted_countries = weighted_countries
      )
      results_list[[paste0(demo, "_", ind)]] <- demo_result
    }
    gc()  # Free memory after each demographic variable
  }

  # Combine all results
  results <- bind_rows(results_list)
  rm(results_list)
  gc()

  # Apply nobs criteria (mask values with insufficient observations)
  results <- results %>%
    mutate(value = if_else(nobs <= 50, NA_real_, value))

  results %>%
    arrange(indicator, demographic, country, group)
}

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### 
### Save charts                                                          
###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Crea una funcion que guarde todos los graficos creados en assistance
save_all_charts <- function(analysis_results,
                            base_width_per_facet = 3.6,
                            height_in = 4,
                            filename_prefix = "assistance_") {
  for (var_name in names(analysis_results)) {
    result <- analysis_results[[var_name]]
    plot <- result$plot
    filename <- paste0("Outputs/data_viz/",filename_prefix, var_name, "_donut.svg")
    
    save_donut_svg(
      plot = plot,
      filename = filename,
      base_width_per_facet = base_width_per_facet,
      height_in = height_in
    )
  }
}

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### 
### Formal and informal analysis                                                         
###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

formal_informal_analysis <- function(data,
                                     var_name,
                                     titles_map,
                                     group_var = c("assistance_group", "access2DRM_group")) {
  group_var <- rlang::arg_match(group_var)
  group_sym <- rlang::sym(group_var)
  
  # 1) Título del gráfico
  if (!var_name %in% names(titles_map)) {
    stop(glue::glue("'{var_name}' no está en 'titles_map'."))
  }
  plot_title <- titles_map[[var_name]]
  
  # 2) Configuración según el tipo de grupo -------------------------------
  if (group_var == "assistance_group") {
    formal_label    <- "Formal justice\nworkers"
    community_label <- "Community justice\nworkers"
    levels_vec      <- c(formal_label, community_label, "Both")
    
    fill_values  <- c("Value" = "#A90099", "Remainder" = "#E4DFDA")
    caption_text <- "* The difference between formal and informal assistance groups is statistically significant (p < 0.05)."
    main_color   <- "#A90099"

      } else if (group_var == "access2DRM_group") {
    formal_label    <- "Formal Dispute \nResolution Mechanism"
    community_label <- "Alternative Dispute \nResolution Mechanism"
    levels_vec      <- c(formal_label, community_label, "Both")
    
    fill_values  <- c("Value" = "#2a2a9A", "Remainder" = "#E4DFDA")
    caption_text <- "* The difference between formal and alternative dispute resolution mechanisms is statistically significant (p < 0.05)."
    main_color   <- "#2a2a9A"
    
  }
  
  # 3) t-test (solo entre "formal" y "community/informal") ----------------
  data_ttest <- data %>%
    dplyr::filter(.data[[group_var]] %in% c(formal_label, community_label)) %>%
    tidyr::drop_na(dplyr::all_of(c(group_var, var_name)))
  
  if (dplyr::n_distinct(data_ttest[[group_var]]) < 2L) {
    warning(glue::glue(
      "La variable '{group_var}' no tiene las dos categorías necesarias ",
      "para el t-test en '{var_name}'."
    ))
    p_val <- NA_real_
    sig   <- FALSE
  } else {
    tt <- t.test(
      reformulate(group_var, var_name),
      data = data_ttest
    )
    p_val <- broom::tidy(tt)$p.value
    sig   <- p_val < 0.05
  }
  
  # 4) Promedios por grupo (incluye Both si existe) -----------------------
  means_df <- data %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(
      value_raw = mean(.data[[var_name]], na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    tidyr::drop_na() %>%
    dplyr::rename(category = !!group_sym)
  
  access_clean <- means_df %>%
    dplyr::mutate(
      category_label = category,
      value          = pmin(pmax(value_raw, 0), 1)  # acotar 0–1
    ) %>%
    dplyr::filter(category_label %in% levels_vec) %>%
    dplyr::mutate(
      category_label = factor(category_label, levels = levels_vec)
    )
  
  # 5) Data para las donas (valor + resto) --------------------------------
  donut_df <- dplyr::bind_rows(
    access_clean %>%
      dplyr::transmute(category_label, part = "Value", amount = value),
    access_clean %>%
      dplyr::transmute(category_label, part = "Remainder", amount = 1 - value)
  )
  
  # 6) Etiquetas centrales (con asterisco si p < 0.05) --------------------
  labels_df <- access_clean %>%
    dplyr::transmute(
      category_label,
      label = ifelse(
        isTRUE(sig) & category_label %in% c(formal_label, community_label),
        paste0(scales::percent(value, accuracy = 0.1), "*"),
        scales::percent(value, accuracy = 0.1)
      )
    )
  
  # 7) Gráfico ------------------------------------------------------------
  p <- ggplot(donut_df, aes(x = 2, y = amount, fill = part)) +
    geom_col(width = 1, color = NA) +
    coord_polar(theta = "y") +
    xlim(-1, 3) +
    facet_wrap(~ category_label, nrow = 1) +
    geom_text(
      data = labels_df,
      aes(x = -0.75, y = 0, label = label),
      inherit.aes = FALSE,
      fontface = "bold",
      size = 5,
      color = main_color  # el color del texto central se mantiene
    ) +
    scale_fill_manual(values = fill_values) +
    theme_void() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 11),
      plot.title = element_text(
        face   = "bold",
        size   = 15,
        hjust  = 0.5,
        margin = margin(b = 40)
      ),
      plot.title.position = "plot",
      plot.margin = margin(t = 30, r = 10, b = 20, l = 10)
    ) +
    labs(
      title   = plot_title,
      caption = if (!is.na(p_val) && isTRUE(sig)) caption_text else NULL
    )
  
  # 8) Salida -------------------------------------------------------------
  list(
    plot      = p,
    p_val     = p_val,
    signif    = sig,
    var       = var_name,
    group_var = group_var
  )
}

run_analysis.fn <- function(data,
                            titles_map,
                            group_var = c("assistance_group", "access2DRM_group")) {
  group_var <- rlang::arg_match(group_var)
  
  res <- lapply(names(titles_map), function(v) {
    formal_informal_analysis(
      data       = data,
      var_name   = v,
      titles_map = titles_map,
      group_var  = group_var
    )
  })
  
  names(res) <- names(titles_map)
  res
}

save_donut_svg <- function(plot,
                           filename = "donut.svg",
                           base_width_per_facet = 3.6,  # ancho (in) por dona
                           height_in = 4,               # alto fijo (in)
                           min_width = 3) {             # ancho mínimo (in)
  
  # Intentar inferir el número de facetas usando la variable category_label
  n_facets <- tryCatch({
    # ggplot_build devuelve una lista de data.frames (uno por capa)
    built <- ggplot_build(plot)
    all_data <- dplyr::bind_rows(built$data)
    
    if ("category_label" %in% names(all_data)) {
      dplyr::n_distinct(all_data$category_label)
    } else if ("PANEL" %in% names(all_data)) {
      dplyr::n_distinct(all_data$PANEL)
    } else {
      1L
    }
  }, error = function(e) {
    # fallback razonable: asumimos 3 facetas (Formal, Community, Both)
    3L
  })
  
  # Calcular ancho final
  width_in <- max(min_width, n_facets * base_width_per_facet)
  
  # Guardar el SVG
  ggsave(
    filename = filename,
    plot     = plot,
    device   = svglite::svglite,
    width    = width_in,
    height   = height_in,
    units    = "in"
    # dpi no es relevante para SVG
  )
  
  message(sprintf(
    "SVG guardado en '%s' (%.1f x %.1f in, %d facetas).",
    filename, width_in, height_in, n_facets
  ))
}


run_logit_regressions <- function(data,
                                  titles_map,
                                  assistance_var = c("assistance_group", "access2DRM_group")) {
  
  assistance_var <- rlang::arg_match(assistance_var)
  
  results <- lapply(names(titles_map), function(dep_var) {
    
    # Selección de variables
    df <- data %>%
      dplyr::select(
        dplyr::all_of(dep_var),
        dplyr::all_of(assistance_var),
        income_group,
        category_problem_selected,
        year,
        country
      ) %>%
      tidyr::drop_na()
    
    # Dependiente binaria (por si acaso)
    if (!all(df[[dep_var]] %in% c(0, 1))) {
      df[[dep_var]] <- ifelse(df[[dep_var]] > 0, 1, 0)
    }
    
    # ===============================
    # DEFINIR REFERENCIAS FORMAL
    # ===============================
    
    if (assistance_var == "assistance_group") {
      
      # Formal justice workers VA A SER LA REFERENCIA
      df[[assistance_var]] <- factor(
        df[[assistance_var]],
        levels = c(
          "Formal justice\nworkers",
          "Community justice\nworkers",
          "Both"
        )
      )
      
    } else if (assistance_var == "access2DRM_group") {
      
      # Formal DRM VA A SER LA REFERENCIA
      df[[assistance_var]] <- factor(
        df[[assistance_var]],
        levels = c(
          "Formal Dispute Resolution \nMechanism",
          "Alternative Dispute Resolution \nMechanism",
          "Both"
        )
      )
    }
    
    # ===============================
    # Fórmula del modelo
    # ===============================
    fml <- reformulate(
      c(
        assistance_var,
        "factor(income_group)",
        "factor(category_problem_selected)",
        "factor(year)",
        "factor(country)"
      ),
      dep_var
    )
    
    # Logit
    model <- glm(fml, data = df, family = binomial(link = "logit"))
    
    # Output limpio
    tidy_out <- broom::tidy(model)
    
    list(
      dep_var = dep_var,
      formula = fml,
      model   = model,
      tidy    = tidy_out
    )
  })
  
  names(results) <- names(titles_map)
  return(results)
}
