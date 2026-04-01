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
options(warn = -1)

if(!interactive()){
  message("--- Loading Packages...")
}

# Loading additional code module
modules <- c(
  "settings" 
)
for (mod in modules){
  source(
    paste0("Code/",mod,".R")
  )
}

#renv::init()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Load data                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  message("--- Loading Data...")
}

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
## 3. Estimation main indicators                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  message("--- Set de parameters")
}

# Indicators to estimate

if(!interactive()){
  message("--- Indicators")
}

indicators <- c("rp_outcome", "rp_fair", "rp_time", "rp_cost",
                "trust_in_judges", "access2info", "access2rep", "access2DRM")

print(indicators)

# Define countries with weights 

eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Czechia", "Denmark",
                  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                  "Italy", "Latvia", "Lithuania", "Luxembourg", "Netherlands",
                  "Poland", "Portugal", "Romania", "Slovak Republic", "Slovenia",
                  "Spain", "Sweden")
if(!interactive()){
  message("--- Set de problems and severity")
}


if(!interactive()){
  message("--- Running the analysis")
}

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

print(names(configs))

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
    select(country, year, starts_with("rp_outcome"), starts_with("rp_fair"), 
           starts_with("rp_time"), starts_with("rp_cost"),  starts_with("trust"), 
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

results <- append(
  list("Sample Information" = sample_information), 
  results, 
  after = 1)

if(!interactive()){
  message("--- Saving final results...")
}
openxlsx::write.xlsx(results, 
                     file = file.path(
                       path2DA,
                       "/Outputs/tables/OECD_input_indicators.xlsx")
                     )

if(!interactive()){
  message("--- DONE with main indicators!✅")
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4. Sociodemographic Data Bank                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  message("--- Generating Sociodemographic Data Bank...")
}

# Define demographic variables for disaggregation
demo_vars <- c("gender", "age_group", "fin_status", "skin_color",
               "area", "education_status", "method")

# Define indicators (excluding access2DRM for this analysis)
databank_indicators <- c("rp_outcome", "rp_fair", "rp_time", "rp_cost",
                         "trust_in_judges", "access2info", "access2rep")

# Prepare clean dataset for databank estimation
legal_problems <- c("A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C4", "C3", "E3",
                    "D1", "D2", "D3", "D4", "D5", "D6", "E1", "E2", "F1", "F2", "G1", "G2", "G3",
                    "H1", "H2", "H3", "J4", "I1", "J1", "J2", "J3", "K1", "K2", "K3", "L1", "L2")

databank_clean.df <- A2J_dataset(
  data = master_data.df,
  countries = NULL,
  target_year = "latest",
  severity_threshold = 4
) %>%
  # Select only columns needed for databank to reduce memory
  select(country, nuts_id, year, reweighted,
         all_of(databank_indicators),
         all_of(demo_vars))

gc()  # Free memory before databank generation

# Generate the Data Bank with National estimates for validation
databank <- generate_databank(
  data = databank_clean.df,
  indicators = databank_indicators,
  demo_vars = demo_vars,
  weighted_countries = eu_countries,
  include_national = TRUE
)

rm(databank_clean.df)  # Free memory
gc()

# Create separate sheets by demographic variable for easier navigation
databank_by_demo <- split(databank, databank$demographic)

# Also create sheets by indicator
databank_by_indicator <- split(databank, databank$indicator)

# Export Data Bank - by demographic variable
openxlsx::write.xlsx(
  databank_by_demo,
  file = file.path(path2DA, "Outputs/tables/OECD_databank_by_demographic.xlsx")
)

# Export Data Bank - by indicator
openxlsx::write.xlsx(
  databank_by_indicator,
  file = file.path(path2DA, "Outputs/tables/OECD_databank_by_indicator.xlsx")
)

# Export Data Bank - long format (single sheet)
openxlsx::write.xlsx(
  list("Data Bank" = databank),
  file = file.path(path2DA, "Outputs/tables/OECD_databank_long.xlsx")
)

if(!interactive()){
  message("--- Data Bank generated successfully!✅")
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. Informal vs formal analysis                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# if(!interactive()){
#   message("--- Informal vs formal analysis")
# }
# 
# # Subset de datos ----------------------------------------------------------
# 
# legal_problems <- c("A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C4", "C3", "E3",
#                     "D1", "D2", "D3", "D4", "D5", "D6", "E1", "E2", "F1", "F2", "G1", "G2", "G3",
#                     "H1", "H2", "H3", "J4", "I1", "J1", "J2", "J3", "K1", "K2", "K3", "L1", "L2")
# 
# data_subset.df <- A2J_dataset(
#   master_data.df,
#   countries = NULL, 
#   target_year = "latest", 
#   severity_threshold = 4
# ) 
# 
# eu_subset.df <- data_subset.df %>%
#   filter(country %in% eu_countries)
# 
# # Vector de títulos ---------------------------------------------------------
# titles_map <- c(
#   "access2info" = "Access to Information by Type of Assistance",
#   "access2DRM"  = "Access to Dispute Resolution Mechanism by Type of Assistance",
#   "rp_outcome"  = "Resolution Outcome by Type of Assistance",
#   "rp_fair"     = "Perceived Fairness by Type of Assistance",
#   "rp_time"     = "Resolution Timeliness by Type of Assistance",
#   "rp_cost"     = "Affordability of Problem Resolution by Type of Assistance",
#   "trust_in_judges" = "Trust in Judges by Type of Assistance"
# )
# 
# # Assistance group analysis -------------------------------------------------------
# 
# # Para asistencia legal (representación)
# results_assistance <- run_analysis.fn(
#   data       = data_subset.df,
#   titles_map = titles_map,
#   group_var  = "assistance_group"
# )
# 
# 
# # Guarda todos los gráficos de assistance
# save_all_charts(results_assistance,
#                 base_width_per_facet = 3.6,
#                 height_in = 4,
#                 filename_prefix = "assistance_")
# 
# # Para asistencia legal (representación) - EU only
# eu_assistance <- run_analysis.fn(
#   data       = eu_subset.df,
#   titles_map = titles_map,
#   group_var  = "assistance_group"
# )
# 
# 
# # Guarda todos los gráficos de assistance - EU only
# save_all_charts(eu_assistance,
#                 base_width_per_facet = 3.6,
#                 height_in = 4,
#                 filename_prefix = "eu_assistance_")
# 
# 
# logits_assistance <- run_logit_regressions(
#   data         = data_subset.df,
#   titles_map   = titles_map,
#   assistance_var = "assistance_group"
# )
# 
# # ADRM group analysis -------------------------------------------------------
# 
# titles_map <- c(
#   "access2info" = "Access to Information by Type of Assistance",
#   "access2rep"  = "Access to Adequate Representation by Type of Assistance",
#   "rp_outcome"  = "Resolution Outcome by Type of Assistance",
#   "rp_fair"     = "Perceived Fairness by Type of Assistance",
#   "rp_time"     = "Resolution Timeliness by Type of Assistance",
#   "rp_cost"     = "Affordability of Problem Resolution by Type of Assistance",
#   "trust_in_judges" = "Trust in Judges by Type of Assistance"
# )
# 
# results_drm <- run_analysis.fn(
#   data       = data_subset.df,
#   titles_map = titles_map,
#   group_var  = "access2DRM_group"
# )
# 
# # Guarda todos los gráficos de assistance
# save_all_charts(results_drm,
#                 base_width_per_facet = 3.6,
#                 height_in = 4,
#                 filename_prefix = "access2ADRM_group_")
# 
# ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
