
## ISSUES
## - The coding manual ('Mega data_Pie Moya coding manual.doc') could not be read. As a result, the vast majority of columns relating to labor, operations, and agrochemicals (e.g., CRABAIHA, CBFAMHA, HERAIHA, FUNGAIHA) remain unprocessed.
## - `currency` is required when `crop_price` is provided, but it is not specified in the raw data. Currently set to NA.
## - Geography: `IDSITE` contains location codes (e.g., "AD"), but exact coordinates are missing and need to be looked up or provided by the human reviewer.
## - Crop is assumed to be "rice" based on the yield ranges (~5000 kg/ha) and IRRI context, but this should be verified.
##
## NOTES
## - `PRODHA` is mapped to `yield` (production in kg/ha).
## - `FSIZE` is mapped to `plot_area` assuming it represents hectares.

carob_script <- function(path) {
  
  uri <- "REPLACE_WITH_DATASET_URI"
  
  meta <- carobiner::get_metadata(
    uri = uri,
    path = path,
    group = "survey",
    major = 1,
    minor = 0,
    data_organization = "IRRI",
    publication = NA,
    project = "Mega Project",
    design = NA,
    data_type = "survey",
    treatment_vars = NA,
    response_vars = "yield",
    carob_contributor = "Dehner De Leon",
    carob_LLM = "Gemini 1.5 Pro",
    carob_date = as.character(Sys.Date()),
    carob_completion = 15,
    carob_effort = 0.5
  )
  
  # Read the raw data
  # Note: The .doc coding manual is required to interpret the remaining ~120 variables.
  f <- carobiner::find_data(uri, path, "Mega project process data_Pie Moya.xls")
  r <- carobiner::read.excel(f, na = "NA")
  
  d <- data.frame(
    plot_id = r$IDFARM,
    location = r$IDSITE,
    season = r$IDSEASON,
    hh_id = r$IDHHCODE,
    plot_area = as.numeric(r$FSIZE),
    N_fertilizer = as.numeric(r$NHA),
    P_fertilizer = as.numeric(r$PHA),
    K_fertilizer = as.numeric(r$KHA),
    seed_rate = as.numeric(r$SEEDKGHA),
    yield = as.numeric(r$PRODHA),
    crop_price = as.numeric(r$PRICE)
  )
  
  # Set housekeeping and required variables
  d$crop <- "rice"  # Assumed, verify against documentation
  d$currency <- as.character(NA) # Required because crop_price is present
  
  d$trial_id <- as.character(as.integer(as.factor(paste(d$location, d$season))))
  
  d$on_farm <- TRUE
  d$is_survey <- TRUE
  d$irrigated <- as.logical(NA)
  
  # Geography - requires manual resolution based on location/site codes
  d$longitude <- as.numeric(NA)
  d$latitude  <- as.numeric(NA)
  d$geo_from_source <- FALSE
  d$geo_uncertainty <- as.numeric(NA)
  
  d$yield_part <- "grain"
  d$yield_moisture <- as.numeric(NA)
  
  carobiner::write_files(path, meta, d)
}
