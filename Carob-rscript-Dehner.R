remotes::install_github("carob-data/carobiner")
library(carobiner)
remotes:install_github("carob-data/yuri")
library(yuri)

my_doi <- "doi:10.7910/DVN/YOUR_DATASET_ID"
raw_files <- yuri::dataURI(my_doi, path = "raw_data")
print(raw_files)