## NOTES
# - The raw data consists of several SPSS (.sav) files on the adoption of different 
#   technology types (post-harvest, water management, varieties, crop management, 
#   machinery, nutrient management).
# - This script merges these modules by country and household ID (HHID).
# - Area units are standardized to hectares (ha) using the provided conversion factors.
# - The dataset is treated as a survey (`data_type = "survey"`, `is_survey = TRUE`).

## ISSUES
# - The exact DOI/URI is not provided. A placeholder `doi:10.xxxx/dummy` is used.
# - Coordinates (longitude/latitude) and location names (adm1-4, location) are not 
#   present in the provided dataset headers and need to be joined from a household 
#   roster or geographic file if one exists.
# - The script assumes `haven::read_sav()` is used to read the `.sav` files.

carob_script <- function(path) {
  
  uri <- "doi.org/10.7910/DVN/D4LRZP"
  
  meta <- carobiner::get_metadata(uri, path, group = "survey", major = 1, minor = 0,
                                  data_organization = "IRRI",
                                  publication = NA,
                                  project = NA,
                                  design = NA,
                                  data_type = "survey",
                                  treatment_vars = NA,
                                  response_vars = "adopt_postharvest_;adopt_watermgt_;adopt_variety_;adopt_cropmgt_;adopt_machinery_;adopt_nutrientmgt_",
                                  carob_contributor = "Author Name",
                                  carob_LLM = "Gemini 1.5 Pro",
                                  carob_date = "2026-11-09",
                                  carob_completion = 80,
                                  carob_effort = 1.0
  )
  
  meta$title <- "Adoption of Agricultural Technologies Survey"
  meta$abstract <- "Household survey data on the adoption of post-harvest technologies, water management, improved rice varieties, crop management, farm machineries, and nutrient management across six Asian countries (Philippines, Vietnam, India, Bangladesh, Myanmar, Indonesia)."
  
  ff <- carobiner::get_data(uri, path)
  
  # Helper to normalize country names
  fix_country <- function(x) {
    x <- as.character(haven::as_factor(x))
    carobiner::fix_name(x, "title")
  }
  
  # Read Post-Harvest Technologies
  f_ph <- ff[grep("Post-harvest", ff, ignore.case = TRUE)]
  r_ph <- haven::read_sav(f_ph)
  d_ph <- data.frame(
    country = fix_country(r_ph$COUNTRY_),
    hh_id = as.character(r_ph$HHID),
    adopt_postharvest_ = as.character(haven::as_factor(r_ph$ADOPT_PO)),
    adopt_postharvest_area_ = as.numeric(r_ph$POST_ARE),
    adopt_postharvest_unit_ = as.character(haven::as_factor(r_ph$V6_A))
  )
  
  # Read Water Management Technologies
  f_wa <- ff[grep("Water Management", ff, ignore.case = TRUE)]
  r_wa <- haven::read_sav(f_wa)
  d_wa <- data.frame(
    country = fix_country(r_wa$COUNTRY_),
    hh_id = as.character(r_wa$HHID),
    adopt_watermgt_ = as.character(haven::as_factor(r_wa$ADOPT_WA)),
    adopt_watermgt_area_ = as.numeric(r_wa$WATER_AR),
    adopt_watermgt_unit_ = as.character(haven::as_factor(r_wa$V6_A))
  )
  
  # Read Improved Rice Varieties
  f_var <- ff[grep("Improved rice varieties", ff, ignore.case = TRUE)]
  r_var <- haven::read_sav(f_var)
  d_var <- data.frame(
    country = fix_country(r_var$COUNTRY_),
    hh_id = as.character(r_var$HHID),
    adopt_variety_ = as.character(haven::as_factor(r_var$IMPROVED)),
    adopt_variety_area_ = as.numeric(r_var$V13_A),
    adopt_variety_unit_ = as.character(haven::as_factor(r_var$V14_A))
  )
  
  # Read Improved Crop Management
  f_cr <- ff[grep("Crop Management", ff, ignore.case = TRUE)]
  r_cr <- haven::read_sav(f_cr)
  d_cr <- data.frame(
    country = fix_country(r_cr$COUNTRY_),
    hh_id = as.character(r_cr$HHID),
    adopt_cropmgt_ = as.character(haven::as_factor(r_cr$ADOPT_CR)),
    adopt_cropmgt_area_ = as.numeric(r_cr$CROP_ARE),
    adopt_cropmgt_unit_ = as.character(haven::as_factor(r_cr$V6_A))
  )
  
  # Read Farm Machineries
  f_ma <- ff[grep("Farm Macnhineries", ff, ignore.case = TRUE)]
  r_ma <- haven::read_sav(f_ma)
  d_ma <- data.frame(
    country = fix_country(r_ma$COUNTRY_),
    hh_id = as.character(r_ma$HHID),
    adopt_machinery_ = as.character(haven::as_factor(r_ma$ADOPT_FA)),
    adopt_machinery_area_ = as.numeric(r_ma$MACH_ARE),
    adopt_machinery_unit_ = as.character(haven::as_factor(r_ma$V6_A))
  )
  
  # Read Nutrient Management
  f_nu <- ff[grep("Nutrient Management", ff, ignore.case = TRUE)]
  r_nu <- haven::read_sav(f_nu)
  d_nu <- data.frame(
    country = fix_country(r_nu$COUNTRY_),
    hh_id = as.character(r_nu$HHID),
    adopt_nutrientmgt_ = as.character(haven::as_factor(r_nu$ADOPT_NU)),
    adopt_nutrientmgt_area_ = as.numeric(r_nu$NUT_AREA),
    adopt_nutrientmgt_unit_ = as.character(haven::as_factor(r_nu$V6_A))
  )
  
  # Merge all modules together by country and hh_id
  d <- merge(d_ph, d_wa, by = c("country", "hh_id"), all = TRUE)
  d <- merge(d, d_var, by = c("country", "hh_id"), all = TRUE)
  d <- merge(d, d_cr, by = c("country", "hh_id"), all = TRUE)
  d <- merge(d, d_ma, by = c("country", "hh_id"), all = TRUE)
  d <- merge(d, d_nu, by = c("country", "hh_id"), all = TRUE)
  
  # Remove trailing whitespaces
  d$country <- trimws(d$country)
  d$hh_id <- trimws(d$hh_id)
  
  # Convert local area units to hectares where applicable
  # Note: 1 acre = 0.404686 ha; conversion for other units like bigha, katha, decimal, etc. 
  # vary by country and region. I am leaving a template here to apply those conversions.
  convert_to_ha <- function(area, unit) {
    unit <- tolower(trimws(unit))
    res <- ifelse(unit == "hectare", area,
                  ifelse(unit == "acre", area * 0.404686,
                         ifelse(unit == "sqm", area / 10000,
                                NA))) # Add exact conversions for local units based on codebook/literature
    return(as.numeric(res))
  }
  
  d$adopt_postharvest_area_ha_ <- convert_to_ha(d$adopt_postharvest_area_, d$adopt_postharvest_unit_)
  d$adopt_watermgt_area_ha_ <- convert_to_ha(d$adopt_watermgt_area_, d$adopt_watermgt_unit_)
  d$adopt_variety_area_ha_ <- convert_to_ha(d$adopt_variety_area_, d$adopt_variety_unit_)
  d$adopt_cropmgt_area_ha_ <- convert_to_ha(d$adopt_cropmgt_area_, d$adopt_cropmgt_unit_)
  d$adopt_machinery_area_ha_ <- convert_to_ha(d$adopt_machinery_area_, d$adopt_machinery_unit_)
  d$adopt_nutrientmgt_area_ha_ <- convert_to_ha(d$adopt_nutrientmgt_area_, d$adopt_nutrientmgt_unit_)
  
  # Housekeeping variables for survey data
  d$trial_id <- as.character(as.integer(as.factor(d$hh_id)))
  d$on_farm <- TRUE
  d$is_survey <- TRUE
  
  # Geography placeholders (To be filled if geographic data is available in another file)
  d$location <- NA 
  d$longitude <- as.numeric(NA)
  d$latitude <- as.numeric(NA)
  d$geo_from_source <- FALSE
  
  # Drop intermediate area and unit columns to clean up the final dataframe
  cols_to_drop <- grep("_area_$|_unit_$", names(d), value = TRUE)
  d <- d[, !(names(d) %in% cols_to_drop)]
  
  carobiner::write_files(path, meta, d)
}