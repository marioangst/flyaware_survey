# flyaware_survey

- Data and scripts needed to replicate analysis of survey responses to proposed flight reduction measures at Eawag.
- Repository of static files (generated through RMarkdown) as backend to homepage hosting report to the Eawag directorate (www.flyaware.ch, not active anymore)

## Contents

### Top level

- read_in_clean_data.R: reads in raw data and cleans it for further analysis
- utility_functions.R: higher-level plotting and processing functions used in the scripts
- descriptive_stats.R: generation of descriptive statistics and visualizations
 - descriptives_app.R: shiny app to interactively explore data
- cluster_respondents.R: Clustering of respondents based on patterns in answer profiles

- index.Rmd/ index.html/ german_index.Rmd/ german_index.html: RMarkdown files to generate static html for homepage in german and english versions

### /Data

- 20191017_Export_Results_FlyawareSurvey.csv: raw survey response data (IP addresses and timestamps removed)
- colnames_codebook.csv: codebook used to recode variable names from original raw text
- colnames_raw.csv: raw variable names
- internal_assessment_measures.csv: internal assessment of measures as done by the flyaware team

### /Viz_outputs

Folder containing visualization outputs

### /static

Static files for homepage downloads (pdfs)
