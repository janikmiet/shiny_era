## RUN FILE WHERE YOU CAN INIT THE PROJECT
## Use this file to control project and see R-scripts for more detailed information. You may need to change user etc. in source scripts.

## Install Libraries -----
if(FALSE){
  source("R/libraries.R")
}

## Generate data ------
if(FALSE){
  # Datas should be in _data/ -folder. Names for the data: population, diagnoses, codes, questionnaires
  ## This is a example of common data model ETL scripts, which saves data from src location to _data
  source("R/create_data.R") 
}


## Run app in R instance -----
if(FALSE){
  rmarkdown::run(file = "index.Rmd")
}

## Upload Shiny to Server -----
if(FALSE){
  source("R/deploy.R")
}

