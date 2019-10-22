
library(readr)

# load utility function ----

source("utility_functions.R")

# read in data ----

responses <- read_csv("Data/20191017_Export_Results_FlyawareSurvey.csv")

# export raw column names (only first 100 chars)
write_csv(data.frame(raw_cols = unlist(lapply(colnames(responses), 
                                              function(x) substr(x,1,100)))),
          path = "Data/colnames_raw.csv", 
          col_names = TRUE)

# load codebook for manually recoded column names
var_codebook <- read_csv("Data/colnames_codebook.csv", 
                         col_names = TRUE,
                         trim_ws = FALSE)

# rename colnames
colnames(responses) <- 
  rename_based_on_codebook(unlist(lapply(colnames(responses), 
                                               function(x) substr(x,1,100))),
                                 codebook = var_codebook,
                                 rawvar = "raw_cols",
                                 codevar = "var_code")


