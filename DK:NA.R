library(haven)
companies <- final_diagnostic_A2J %>%
  group_by(`Polling company`) %>%
  summarise(diff_proportion_rp_time       = mean(diff_proportion_rp_time, na.rm = T),
            diff_proportion_rp_fair       = mean(diff_proportion_rp_fair, na.rm = T), 
            diff_proportion_access2rep    = mean(diff_proportion_access2rep, na.rm. = T)) %>%
  drop_na()

method <- final_diagnostic_A2J %>%
  group_by(Method) %>%
  summarise(diff_proportion_rp_time = mean(diff_proportion_rp_time, na.rm = T),
            diff_proportion_rp_fair = mean(diff_proportion_rp_fair, na.rm = T),
            diff_proportion_access2rep = mean(diff_proportion_access2rep, na.rm = T)) %>%
  drop_na()



eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Czechia", "Denmark",
                  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                  "Italy", "Latvia", "Lithuania", "Luxembourg", "Netherlands",
                  "Poland", "Portugal", "Romania", "Slovak Republic", "Slovenia",
                  "Spain", "Sweden")

EU_GPP_2024 <- EU_GPP_2024 %>%
  mutate(
    country_name_ltn =
      case_when(
        country_name_ltn == "Slovakia" ~ "Slovak Republic",
        T ~ country_name_ltn
      )
  ) %>%
  filter(country_name_ltn %in% eu_countries) %>%
  rename(country = country_name_ltn)


master_data <- master_data.df %>%
  filter(country %in% eu_countries) %>%
  group_by(country) %>%
  mutate(lastYear = max(year, na.rm = TRUE)) %>%
  filter(year == lastYear) %>%
  mutate(
    rp_outcome = case_when(
      as.numeric(q30) == 4 | as.numeric(q34) == 4 ~ 1,
      as.numeric(q30) == 3 | as.numeric(q34) == 3 ~ 0,
      as.numeric(q30) %in% c(1, 2) | as.numeric(q34) %in% c(1, 2) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    rp_fair = case_when(
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
    DK_timeliness = case_when(
      q37b == -8888 ~ 1,
      q37b >= 0 ~ 0,
      T ~ NA_real_
    )
    ) %>%
  select(id, country, "rp_outcome", "rp_fair", "rp_time","access2info", "trust_in_judges", "access2rep", "access2DRM",
         "p_consumer", "p_land", "p_housing", "p_family", "p_education", "p_community", "p_accidental", 
         "p_employment", "p_public", "p_id", "p_money", "p_law_enforcement", "DK_timeliness", "method", "nuts_id","q36a")

master_data_final <- master_data %>%
  left_join(EU_GPP_2024 %>% select(id, country, AJR_fair, AJD_inst_advice), by = c("id","country")) %>%
  mutate(
    DK_fair = case_when(
      AJR_fair == 98 ~ 1,
      AJR_fair %in% c(1,2) ~0,
      T ~ NA_real_
    ),
    DK_representation = case_when(
      AJD_inst_advice == 98 ~ 1,
      AJD_inst_advice %in% c(1,2) ~0,
      T~ NA_real_
    )
  )

# Timeliness

independent_vars <- c("rp_outcome", "rp_fair", "access2info", "trust_in_judges", "access2rep", "access2DRM",
                       "p_consumer", "p_land", "p_housing", "p_family", "p_education", "p_community", "p_accidental", 
                       "p_employment", "p_public", "p_id", "p_money", "p_law_enforcement", "method")
# Crear la fórmula para la regresión
reg_formula <- as.formula(
  paste(" DK_timeliness ~", paste(c(independent_vars), collapse = " + "))
)
modelo_logit <- glm(reg_formula, data = master_data_final, family = binomial)
summary(modelo_logit)

# Crear la fórmula para la regresión
reg_formula_ridge <- as.formula(
  paste(" DK_timeliness ~", paste(c(independent_vars,  "factor(nuts_id)"), collapse = " + "))
)
modelo_br <- glm(reg_formula_ridge, data = master_data_final, family = binomial(link = "logit"), method = "brglmFit")

summary(modelo_br)

# Fairness

independent_vars <- c("rp_outcome", "rp_time", "access2info", "trust_in_judges", "access2rep", "access2DRM",
                      "p_consumer", "p_land", "p_housing", "p_family", "p_education", "p_community", "p_accidental", 
                      "p_employment", "p_public", "p_id", "p_money", "p_law_enforcement", "method")
# Crear la fórmula para la regresión
reg_formula <- as.formula(
  paste("DK_fair ~", paste(c(independent_vars), collapse = " + "))
)
modelo_logit <- glm(reg_formula, data = master_data_final, family = binomial)
summary(modelo_logit)

# Crear la fórmula para la regresión
reg_formula_ridge <- as.formula(
  paste("DK_fair ~", paste(c(independent_vars,  "factor(nuts_id)"), collapse = " + "))
)
modelo_br <- glm(reg_formula_ridge, data = master_data_final, family = binomial(link = "logit"), method = "brglmFit")

summary(modelo_br)

# Representation

independent_vars <- c("rp_outcome", "rp_time", "rp_fair","access2info", "trust_in_judges", "access2DRM",
                      "p_consumer", "p_land", "p_housing", "p_family", "p_education", "p_community", "p_accidental", 
                      "p_employment", "p_public", "p_id", "p_money", "p_law_enforcement", "method")
# Crear la fórmula para la regresión
reg_formula <- as.formula(
  paste("DK_representation ~", paste(c(independent_vars), collapse = " + "))
)
modelo_logit <- glm(reg_formula, data = master_data_final, family = binomial)
summary(modelo_logit)

# Crear la fórmula para la regresión
reg_formula_ridge <- as.formula(
  paste("DK_representation ~", paste(c(independent_vars,  "factor(nuts_id)"), collapse = " + "))
)
modelo_br <- glm(reg_formula_ridge, data = master_data_final, family = binomial(link = "logit"), method = "brglmFit")

summary(modelo_br)
