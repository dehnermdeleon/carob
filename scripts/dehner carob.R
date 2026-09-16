# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
  path <- "C:/Users/DDeLeon/Documents/GitHub/carob dehner"

"Area Based Farm Household Survey in Indonesia for the Metrics and Indicators for Tracking in RICE project

   An Area-Based Farm Household Survey is a structured field methodology used within international agricultural initiatives like the Metrics and Indicators for Tracking in RICE project. Spearheaded by the International Rice Research Institute (IRRI), this dual approach integrates ground-level microeconomic data with macro spatial tracking to measure the sustainability, productivity, and socio-economic transformation of smallholder rice systems. In the CGIAR Research Program on Rice (formerly known as the Global Rice Science Partnership, or GRiSP), monitoring and evaluation (M&E) relies on an integrated framework of metrics to track progress. This structure was refined under the MISTIG project (Metrics and Indicators for Tracking in GRiSP) to map directly to CGIAR’s Intermediate Development Outcomes (IDOs). The primary M&E tracking metrics and indicators are categorized below by their targeted developmental impact areas: 1. Productivity and Yield Indicators 2. Economic and Livelihood Indicators 3. Resource-Use Efficiency and Environmental Sustainability 4. Technology Adoption and Scale Metrics 5. Institutional Capacity and Science Quality Indicators The area of focus is in 7 provinces of Indonesia (see below) with a total sampling size of 800 conducted during the crop calendar of 2016 (2 season data capture). 1. West Java 2. Yogyakarta 3. South Kalimantan 4. South Sulawesi 5. South Sumatera 6. Lampung 7. Papua Trained Partner Enumerators to perform Data Collection: Indonesian Center for Rice Research"

uri <- "doi.org/10.7910/DVN/F6R3HR"
group <- "survey"
ff <- carobiner::get_data(uri, path, group)

meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
                                data_organization = "IRRI",
                                publication = NA,
                                project = MISTIR,
                                data_type = "survey",
                                treatment_vars = "adoption machinery",
                                response_vars = "new machine adopted for the ", 
                                carob_completion = 0,
                                carob_contributor = "Dehner De Leon",
                                carob_date = "2026-09-15",
                                carob_effort = NA,
                                notes = NA, 
                                design = NA


















# Script: standardize_farm_machinery.R
# Standardization script based on the CAROB / CGIAR ETL standard conventions.

library(haven) #for SPSS conversion
library(dplyr)
library(tidyr)
library(stringr)

# 1. Read Raw SPSS File
# Replace 'path/to/raw_data.sav' with your local filepath.
c:/Users/DDeLeon/Documents/GitHub/Carob/raw_data <- haven::read_sav("path/to/raw_data.sav")

# 2. Extract Variable Labels and Value Labels (Haven attributes)
df <- raw_data %>%
  # Convert labelled foreign vectors to standard R factors/characters
  haven::as_factor(only_labelled = TRUE) %>%
  as.data.frame()

# Normalize column headers to lowercase
names(df) <- tolower(names(df))

# 3. Standardization / Transformation Pipeline
clean_data <- df %>%
  # Map variables to CAROB standard naming conventions
  rename(
    country             = country_,
    household_id        = hhid,
    machinery_type      = adopt_fa,
    is_adopted          = mach_ado,
    area_tech_raw       = mach_are,
    area_unit_raw       = v6_a,
    info_source         = v7_a,
    adoption_year       = v8_a,
    c_adoption_year     = mach_c_a,
    decision_gender     = v10_a,
    training_gender     = mach_gen,
    disadoption_status  = mach_ric,
    adoption_notes      = v13_a
  ) %>%
  
  # Standardize Country & IDs
  mutate(
    country = str_squish(as.character(country)),
    household_id = as.character(household_id),
    
    # Standardize Boolean / Binary Adoption flags (1 = Yes, 0 = No)
    is_adopted = case_when(
      str_detect(tolower(is_adopted), "^yes|1") ~ 1L,
      str_detect(tolower(is_adopted), "^no|0")  ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Standardize Gender Categories
    decision_gender = case_when(
      str_detect(tolower(decision_gender), "both")   ~ "both",
      str_detect(tolower(decision_gender), "female") ~ "female",
      str_detect(tolower(decision_gender), "male")   ~ "male",
      TRUE ~ NA_character_
    ),
    training_gender = case_when(
      str_detect(tolower(training_gender), "both")   ~ "both",
      str_detect(tolower(training_gender), "female") ~ "female",
      str_detect(tolower(training_gender), "male")   ~ "male",
      TRUE ~ NA_character_
    ),
    
    # Clean text notes: convert SPSS empty markers and blanks to NA
    adoption_notes = na_if(str_squish(as.character(adoption_notes)), "None"),
    disadoption_status = na_if(str_squish(as.character(disadoption_status)), "None")
  ) %>%
  
  # Standardize Land Area to Hectares (CAROB standard: area_ha)
  mutate(
    area_tech_raw = as.numeric(area_tech_raw),
    area_unit_norm = tolower(str_squish(as.character(area_unit_raw))),
    area_ha = case_when(
      str_detect(area_unit_norm, "hectare")       ~ area_tech_raw,
      str_detect(area_unit_norm, "acre")          ~ area_tech_raw * 0.404686,
      str_detect(area_unit_norm, "sqm")           ~ area_tech_raw * 0.0001,
      str_detect(area_unit_norm, "bigha")         ~ area_tech_raw * 0.1338,      # Standard conversion; can vary regionally
      str_detect(area_unit_norm, "katha")         ~ area_tech_raw * 0.0067,
      str_detect(area_unit_norm, "decimal")       ~ area_tech_raw * 0.00404686,
      str_detect(area_unit_norm, "are")           ~ area_tech_raw * 0.01,
      str_detect(area_unit_norm, "bata|ubin")     ~ area_tech_raw * 0.0014,
      str_detect(area_unit_norm, "bahu")          ~ area_tech_raw * 0.7096,
      str_detect(area_unit_norm, "rante")         ~ area_tech_raw * 0.04,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # Drop redundant raw unit columns
  select(
    country,
    household_id,
    machinery_type,
    is_adopted,
    area_ha,
    area_tech_raw,
    area_unit_raw,
    adoption_year,
    c_adoption_year,
    info_source,
    decision_gender,
    training_gender,
    disadoption_status,
    adoption_notes
  )

# 4. Export Processed Harmonized Data
write.csv(clean_data, "standardized_farm_machinery.csv", row.names = FALSE, na = "")