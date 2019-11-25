
# setup ----

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
# check
colnames(responses)
# remove explanatory text cols
responses <- responses[,colnames(responses) != "expl"]
# remove meta info and comment fields
responses <- responses[,!(colnames(responses) %in% c("response_id",
                                                   "start_date","start_time",
                                                   "complete_time"))]
responses_with_comments <- responses
responses <- responses[,!(grepl("comment",colnames(responses)))]

# get an overview over complete vs incomplete
table(responses$status)
# for the moment remove incompletes and associated var
responses <- responses[responses$status == "Complete", 
                       colnames(responses) != "status"]
responses_with_comments <- responses_with_comments[responses_with_comments$status == "Complete", 
                                                  colnames(responses_with_comments) != "status"]


# apply for all likert vars
responses[,!(colnames(responses) %in% c("position","ever_avoid"))] <- 
  lapply(responses[,!(colnames(responses) %in% c("position","ever_avoid"))],
         factor_likert_statements)

# compute maximum number of responses in one category in a variable (for viz later)
max_n <- max(unlist(lapply(responses,function(x) max(table(x)))))

# internal assessment of cost of measures by flyaware group

internal_assessment <- read.csv2(file = "Data/internal_assessment_measures.csv")

internal_assessment$acceptance <- factor(internal_assessment$acceptance,
                                         levels =  c("high","disputed","very disputed"),
                                         ordered = TRUE)