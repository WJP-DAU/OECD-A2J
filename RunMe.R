## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            RunMe QRQ experts system
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
## 1.  Load sources                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading additional code module
modules <- c(
  "settings" 
)
for (mod in modules){
  source(
    paste0("Code/",mod,".R")
  )
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Load data                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### OECD and JAC data ------------------------------------------------------------------------------------------

master_data.df <- readRDS(file.path(path2DA,"Data/OECD_JAC.RDS")) %>%
  mutate(
    nuts_id = 
      case_when(
        nuts_id == "CZ01" ~ "CZ01",
        nuts_id == "CZ02" ~ "CZ020304",
        nuts_id == "CZ03" ~ "CZ020304",
        nuts_id == "CZ04" ~ "CZ020304",
        nuts_id == "CZ05" ~ "CZ0506",
        nuts_id == "CZ06" ~ "CZ0506",
        nuts_id == "CZ07" ~ "CZ0708",
        nuts_id == "CZ08" ~ "CZ0708",
        nuts_id == "FI1C" ~ "FI1C20",
        T ~ nuts_id
      ),
    q21 =
      case_when(
        q21 == "c3" ~ "C3",
        T ~ q21
      )
  )

# Loading region names

region_names <- read_xlsx(
  file.path(path2EU, 
            "EU-S Data/reports/eu-thematic-reports/data-viz/inputs/region_labels.xlsx")
  ) %>%
  select(country, 
         nuts_id, 
         nameSHORT, 
         pop_weight    = regionpoppct, 
         unique_border = border, 
         unique_label  = label) %>%
  mutate(
    country =
      case_when(
        country == "Slovakia" ~ "Slovak Republic",
        T ~ country
      )
  ) %>% 
  group_by(country) %>%
  mutate(
    total_pop_weight = sum(pop_weight, na.rm = T),
    reweighted = pop_weight / total_pop_weight  # Adjusting pop weight for the remaining regions
  ) 

master_data.df <- master_data.df %>%
  left_join(region_names, by = c("country", "nuts_id"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Estimation process                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Indicators to estimate

indicators <- c("rp_outcome", "rp_fair", "trust_in_judges", "access2info", "access2rep", "access2DRM")

# Define countries with weights 

eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Czechia", "Denmark",
                  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                  "Italy", "Latvia", "Lithuania", "Luxembourg", "Netherlands",
                  "Poland", "Portugal", "Romania", "Slovak Republic", "Slovenia",
                  "Spain", "Sweden")
configs <- list(
  
  "Specific non trivial lp" = list(
    problem_codes = c("C1", "C2", "C4", "D1", "D2", "D3", "D4", "D5", "D6",
                      "G1", "G2", "G3", "H1", "H2", "H3", "J4", "K1", "K2", "K3", "L1", "L2"),
    severity_threshold = 4
  ),
  "All non trivial lp" = list(
    problem_codes = c("A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C4", "C3", "E3",
                      "D1", "D2", "D3", "D4", "D5", "D6", "E1", "E2", "F1", "F2", "G1", "G2", "G3",
                      "H1", "H2", "H3", "J4", "I1", "J1", "J2", "J3", "K1", "K2", "K3", "L1", "L2"),
    severity_threshold = 4
  ),
  "All lp" = list(
    problem_codes = c("A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C4", "C3", "E3",
                      "D1", "D2", "D3", "D4", "D5", "D6", "E1", "E2", "F1", "F2", "G1", "G2", "G3",
                      "H1", "H2", "H3", "J4", "I1", "J1", "J2", "J3", "K1", "K2", "K3", "L1", "L2"),
    severity_threshold = 0
  ),
  "Employment" = list(
    problem_codes = c("G1", "G2", "G3"),
    severity_threshold = 4
  ),
  "Family" = list(
    problem_codes = c("D1", "D2", "D3", "D4", "D5", "D6"),
    severity_threshold = 4
  ),
  "Housing" = list(
    problem_codes = c("C1", "C2", "C4"),
    severity_threshold = 4
  ),
  "Money and Debt" = list(
    problem_codes = c("L1", "L2", "K1", "K2", "K3"),
    severity_threshold = 4
  ),
  "Public Services" = list(
    problem_codes = c("H1", "H2", "H3", "J4"),
    severity_threshold = 4
  )
)

results <- list()
gc()
for (label in names(configs)) {
  config <- configs[[label]]
  results[[label]] <- estimate_A2J_indicators(
    problem_codes      = config$problem_codes,
    severity_threshold = config$severity_threshold,
    data               = master_data.df,
    countries          = eu_countries,
    indicators         = indicators
  ) %>%
    select(country, year, starts_with("rp_outcome"), starts_with("rp_fair"), starts_with("trust"), 
           starts_with("access2info"), starts_with("access2rep"), starts_with("access2DRM"), 
           starts_with("legprob_sev")) %>%
    nobs_criteria(.)
}

sample_information <- master_data.df %>%
  group_by(country) %>%
  mutate(lastYear = max(year, na.rm = TRUE)) %>%
  mutate(lastYear = if_else(country %in% c("Dominican Republic", "United States"), 2018, 
                            if_else(country %in% c("Ireland"), 2021, lastYear))) %>%
  ungroup() %>%
  filter(year == lastYear) %>%
  mutate(
    counter = 1
  ) %>%
  group_by(country) %>%
  summarise(total_sample = sum(counter, na.rm = T))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Saving process                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

openxlsx::write.xlsx(results, 
                     file = file.path(
                       path2DA,
                       "/Outcomes/OECD_input_indicators.xlsx")
                     )


