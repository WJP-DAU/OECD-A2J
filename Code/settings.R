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
  "ggplot2",
  # Data managment
  "purrr",
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
        starts_with("q19_"), starts_with("q25_")),
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
      )
    ) 
  
  df_clean <- df_clean %>%
    mutate(across(all_of(c(legprob_bin, legprob_sev)), ~ as.numeric(haven::zap_labels(.)))) %>%
    mutate(across(all_of(legprob_bin), ~ case_when(as.numeric(.x) == 99 ~ NA_real_, TRUE ~ as.numeric(.x)))) %>%
    mutate(across(all_of(legprob_bin), ~ as.integer(. == 1))) %>%
    mutate(legprob = as.integer(rowSums(across(all_of(legprob_bin)), na.rm = TRUE) > 0)) %>%
    filter(q21 %in% legal_problems) %>%
    select(country, country_code, nuts_id, year, reweighted,
           all_of(legprob_bin), all_of(legprob_sev), q21, legprob, legprob_bin, legprob_sev, 
           q30, q34, q36a, q37b, q41b, q1g, q24, starts_with("q25_"),
           q27, q26, q28, q29, 
           rp_outcome, rp_fair, rp_time, access2info, trust_in_judges, access2rep, access2DRM,
           starts_with("p_")
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

nobs_criteria <- function(df, threshold = 30) {
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