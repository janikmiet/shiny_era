## Shiny App: ERA Explorer global.R
library(shiny)
library(bslib)
library(qs)
library(tidyverse)
library(dplyr)
library(lubridate)
library(DT)
library(fmsb)
library(hrbrthemes)
library(ggVennDiagram)
library(plotly)
library(tidyr)
library(cmprsk)
library(survminer)
library(survival)
library(datasets)
library(shinymanager)

# Settings ------
eval_credentials <- FALSE   # TRUE for having a login page
eval_ostpre <- FALSE        # TRUE for ostpre specific outputs / FALSE for test the program

## Credentials ----
if(file.exists("credentials.txt")) credentials <- read.table("credentials.txt", header = TRUE)

## Load Data ----
if(TRUE){
  if(eval_ostpre == TRUE){
    loc = "_data/" # for OSTPRE
    population <- arrow::read_parquet(file = paste0(loc, "population.parquet"))
    diagnoses <- arrow::read_parquet(file = paste0(loc, "diagnoses.parquet"))
    data_codes <- arrow::read_parquet(file = paste0(loc, "data_codes.parquet"))
  }else{
    loc = "data/"    # for synthetic data
    population <- arrow::read_parquet(file = paste0(loc, "population_synth.parquet"))
    diagnoses <- arrow::read_parquet(file = paste0(loc, "diagnoses_synth.parquet"))
    data_codes <- arrow::read_parquet(file = paste0(loc, "data_codes.parquet"))
  }
  
}
## Colors for App ------
## colors for exposure and response groups
colors_border=c(rgb(0.8,0.2,0.5,0.9), rgb(0.2,0.5,0.5,0.9))
colors_in=c( rgb(0.8,0.2,0.5,0.4) , rgb(0.2,0.5,0.5,0.4))
## Diagnose Data Sources -----
src_choices <- unique(diagnoses$SRC) # Select globally available registry sources 

# OSTPREFUN Functions Copy Script ------

## This works in tuma
if(FALSE){
  funclist <- list.files("./functions/", full.names = T) ## Where OSTPREFUN functions are located
  sapply(funclist, source)
  # or this
  # library(ostprefun)
}

## Because sourcing functions does not work in ostpre.uef.fi, this is a shortcut
## writing all the functions/ file codes to one big R-file. Then copy-paste it to end of this global.R scripts.
if(FALSE){
  fils <- list.files("functions/", full.names = T) # Where OSTPREFUN R files are located
  long_file <- ""
  for (i in fils) {
    fil <- readr::read_file(i)
    long_file <- paste0(long_file, "", fil)
  }
  writeLines(long_file, "R/functions_ostprefun.R") ## Creates one R file
}


if(TRUE){
  
  ## Idea to run function in shiny with messages. Sometimes this does not work. Needs dev.
  progress_message <- function(fn, msg = "Please wait"){
    withProgress(message = msg, value = 0, {
      fn
    })
  }
  
  ## Population which has certain diagnoses
  pop_dg <- function(regex_icd10= "",
                     regex_icd9= "",
                     regex_icd8= "",
                     registry_source=c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"),
                     groups = c("exposure", "no exposure"),
                     data_diagnoses = diagnoses,
                     data_population = population,
                     runtime_shiny = FALSE){
    
    ## Testing
    if(FALSE){
      regex_icd10= "^E11"
      regex_icd9= "^250A"
      regex_icd8= "^250"
      registry_source=c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa")
      groups = c("exposure", "no exposure")
      data_diagnoses = diagnoses
      data_population = population
    }
    all <- function(){
      ## Phase 1
      dat <- search_diagnoses(regex_icd10=regex_clean(regex_icd10),
                              regex_icd9=regex_clean(regex_icd9),
                              regex_icd8=regex_clean(regex_icd8),
                              registry_source=registry_source,
                              data_diagnoses=data_diagnoses
      )
      if(shiny::isRunning()) incProgress(1/4)
      ## Phase 2
      d1 <- dat %>%
        arrange(ID, DATE) %>%
        group_by(ID) %>%
        summarise(
          DATE = first(DATE),
          SRC = first(SRC),
          DGREG = first(DGREG),
        ) %>%
        left_join(population, by = "ID") %>%
        mutate(GROUP = groups[1],  #"exposure" , # "no exposure" "exposure"
               AGE_DG = trunc((DATE_BIRTH %--% DATE) / years(1)))
      if(shiny::isRunning()) incProgress(2/4)
      ## Phase 3
      ## no exposure
      d2 <- data_population %>%
        filter(!ID %in% d1$ID) %>%
        mutate(GROUP = groups[2],
               AGE_DG = NA, ## added bc of union_all() did not work.
               DATE = NA,
               SRC = NA,
               DGREG = NA
        )
      if(shiny::isRunning()) incProgress(3/4)
      ## Phase 4
      d <- rbind(d1,d2)
      if(shiny::isRunning()) incProgress(4/4)
      ## Aineiston rajaus, jos liian vahan henkiloita
      if(nrow(d %>% filter(GROUP == groups[1])) > 5){
        return(d)
      }else{
        return(NULL)
      }
    }
    if(shiny::isRunning()){
      withProgress(message = "Creating population data",  value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
  
  
  #' Exposure and Response Population
  #'
  #' Finds population which have selected ICD diagnoses as exposure and response
  #'
  #' @param exposure_icd10 character.
  #' @param exposure_icd9 character.
  #' @param exposure_icd8 character.
  #' @param exposure_src vector of sources. Available sources are c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa")
  #' @param response_icd10 character.
  #' @param response_icd9 character.
  #' @param response_icd8 character.
  #' @param response_src vector of sources. Available sources are c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa")
  #'
  #' @return data frame
  #'
  #' @examples
  #'  \dontrun{
  #'  pop_dg(exposure_icd10="^E11", exposure_icd9="^250A", exposure_icd8="^250", exposure_src=c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"), response_icd10 = "^I2")
  #' }
  #'
  #' @import dplyr
  #' @importFrom magrittr %>%
  #' @export
  
  
  ## Population exposure and response
  pop_exp_resp <- function(exposure_icd10 = "",
                           exposure_icd9 = "",
                           exposure_icd8 = "",
                           exposure_src = c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"),
                           response_icd10 = "",
                           response_icd9 = "",
                           response_icd8 = "",
                           response_src = c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"),
                           data_population = population,
                           data_diagnoses = diagnoses,
                           runtime_shiny = FALSE){
    
    all <- function(){
      if(shiny::isRunning()) incProgress(1/4)
      ## Phase 1
      ## Exposure populaatio
      exp <- pop_dg(regex_icd10 = regex_clean(exposure_icd10),
                    regex_icd9 = regex_clean(exposure_icd9),
                    regex_icd8 = regex_clean(exposure_icd8),
                    registry_source = exposure_src,
                    groups = c("exposure", "no exposure"),
                    data_population = population,
                    data_diagnoses = diagnoses)
      if(shiny::isRunning()) incProgress(2/4)
      
      ## Phase 2
      ## Response Populaatio
      resp <- pop_dg(regex_icd10 = regex_clean(response_icd10),
                     regex_icd9 = regex_clean(response_icd9),
                     regex_icd8 = regex_clean(response_icd8),
                     registry_source = response_src,
                     groups = c("response", "no response"),
                     data_population = population,
                     data_diagnoses = diagnoses)  %>%
        rename_with(.fn = ~ paste0("resp.", .x, "")) %>%
        rename(ID = resp.ID)
      if(shiny::isRunning()) incProgress(3/4)
      
      ## Phase 3: Combine
      d <- exp %>%
        left_join(resp, by = "ID") %>%
        mutate(
          exposure = ifelse(!is.na(DATE), 1, 0),
          response = ifelse(!is.na(resp.DATE), 1, 0)
        )
      if(shiny::isRunning()) incProgress(4/4)
      return(d)
    }
    if(shiny::isRunning()){
      withProgress(message = "Creating exposure and response data", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
  #' Regex Cleaner
  #'
  #' Cleans spaces and style from regex code
  #'
  #' @param dglist character.
  #'
  #' @return character
  #'
  #' @examples
  #'  \dontrun{
  #'  regex_clean("^f10 | f09")
  #'
  #'  }
  #'
  #' @export
  regex_clean <- function(dglist){
    rgx <- toupper(gsub(pattern = " ", replacement = "", x = dglist))
    return(rgx)
  }
  #' All Diagnoses found with certain ICD criterias
  #'
  #' Finds all the cases with regex code. Useful as finding all the exposure/response diagnoses.
  #'
  #' @importFrom magrittr %>%
  #' @import dplyr
  #' @return data frame
  #'
  #' @examples
  #'  \dontrun{
  #'  search_diagnoses(regex_icd10="^E11", regex_icd9="^250A", regex_icd8="^250", registry_source=c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"))
  #' }
  #' @export
  search_diagnoses <- function(regex_icd10="",
                               regex_icd9="",
                               regex_icd8="",
                               registry_source=c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"),
                               data_diagnoses=diagnoses
  ){
    all <- function(){
      if(shiny::isRunning()) incProgress(1/5)
      if(regex_icd10 != ""){
        d1 <- data_diagnoses %>%
          filter(DGREG == "ICD10") %>%
          filter(SRC %in% registry_source) %>%
          filter(grepl(pattern = regex_icd10, x = DG)) %>%
          select(ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE)
      }
      if(shiny::isRunning()) incProgress(2/5)
      if(regex_icd9 != ""){
        d2 <- data_diagnoses %>%
          filter(DGREG == "ICD9") %>%
          filter(SRC %in% registry_source) %>%
          filter(grepl(pattern = regex_icd9, x = DG))%>%
          select(ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE)
      }
      if(shiny::isRunning()) incProgress(3/5)
      if(regex_icd8 != ""){
        d3 <- data_diagnoses %>%
          filter(DGREG == "ICD8") %>%
          filter(SRC %in% registry_source) %>%
          filter(grepl(pattern = regex_icd8, x = DG))%>%
          select(ID, DGREG, SRC, DATE, DG, ICD10_CLASS, ICD10_3LETTERS, AGE)
      }
      if(shiny::isRunning()) incProgress(4/5)
      ## Kaikki ICD rekisterit yhdessa.
      d <- d1 %>%
        rbind(if(exists("d2") & nrow(d2)>0) d2) %>%
        rbind(if(exists("d3") & nrow(d3)>0) d3)
      d <- as_tibble(d) %>% arrange(ID, DGREG, DATE)
      if(shiny::isRunning()) incProgress(5/5)
      return(d)
    }
    
    if(shiny::isRunning()){
      withProgress(message = "Creating diagnoses data", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
    
    # return(d)
  }
  #' Venndiagram of registry sources
  #'
  #' Creates vendiagram
  #'
  #'
  #' @return list
  #'
  #' @examples
  #'  \dontrun{
  #'  venn_plot1(search_diagnoses(regex_icd10 = "^E11"))
  #'  }
  #'
  #' @import dplyr
  #' @import ggplot2
  #' @importFrom magrittr %>%
  #' @export
  venn_plot1 <- function(data = exposure_diagnoses){
    all <- function(){
      if(shiny::isRunning()) incProgress(1/4)
      dvenn <- data %>%
        arrange(ID, DATE) %>%
        group_by(ID) %>%
        summarise(
          SRC = first(SRC),
          DATE = first(DATE)) %>%
        select(ID, SRC)
      if(shiny::isRunning()) incProgress(2/4)
      ## Function to split
      split_tibble <- function(tibble, column = 'col') {
        temp <- tibble %>% split(., .[,column]) %>% lapply(., function(x) x[,setdiff(names(x),column)]) %>% unlist(.,recursive = F)
        names(temp) <- gsub(pattern = ".ID", replacement = "", x = names(temp))
        return(temp)
      }
      x <- split_tibble(dvenn, 'SRC')
      if(shiny::isRunning()) incProgress(3/4)
      plt <- ggVennDiagram(x) +
        scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")
      if(shiny::isRunning()) incProgress(4/4)
      return(plt)
    }
    if(shiny::isRunning()){
      ## Vain yksi diagnosi per ID eli saadan ensimmaine sorsa
      withProgress(message = "Plotting Venn #1", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
  
  venn_plot2 <- function(data = exposure_diagnoses){
    all <- function(){
      if(shiny::isRunning()) incProgress(1/4)
      ## Otetaan yksi per src per lomno
      dvenn <- data %>%
        arrange(ID, DATE) %>%
        group_by(ID,SRC) %>%
        summarise(
          SRC = first(SRC),
          DATE = first(DATE)) %>%
        select(ID, SRC)
      if(shiny::isRunning()) incProgress(2/4)
      ## Function to split
      split_tibble <- function(tibble, column = 'col') {
        temp <- tibble %>% split(., .[,column]) %>% lapply(., function(x) x[,setdiff(names(x),column)]) %>% unlist(.,recursive = F)
        names(temp) <- gsub(pattern = ".ID", replacement = "", x = names(temp))
        return(temp)
      }
      x <- split_tibble(dvenn, 'SRC')
      if(shiny::isRunning()) incProgress(3/4) 
      plt <- ggVennDiagram(x) +
        scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")
      if(shiny::isRunning()) incProgress(4/4)
      return(plt)
    }
    if(shiny::isRunning()){
      withProgress(message = "Plotting Venn #2", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
}

# APPLICATION FUNCTIONS -----

if(TRUE){
  ## Functions for Shiny App and Report
  ## 20241030 - JKM
  
  ## EXPOSURE ----- 
  plot_exposure_agedist <- function(dat){
    all <- function(){
      if(shiny::isRunning()) incProgress(1/3)
      d <- dat %>% 
        filter(GROUP == "exposure") %>% 
        group_by(AGE_DG) %>% 
        summarise(
          freq = n()
        )
      if(shiny::isRunning()) incProgress(2/3)
      title = paste("Exposure Population size", sum(d$freq))
      plot <- ggplot(data = d) +
        geom_bar(aes(x=AGE_DG, y= freq), color=colors_border[1] , fill = colors_in[1], stat = "identity") +
        hrbrthemes::theme_ipsum_rc() +
        labs(title = title, subtitle = "Age on first exposure diagnose date")
      if(shiny::isRunning()) incProgress(3/3)
      return(plot)
    }
    if(shiny::isRunning()){
      withProgress(message = "Plotting Age Distribution", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
  
  table_exposure_agedist <- function(dat){
    dat %>% 
      filter(GROUP == "exposure") %>% 
      group_by(GROUP) %>% 
      summarise(
        pop_n = n(),
        age_min = min(AGE_DG, na.rm = T),
        age_median = median(AGE_DG, na.rm = T),
        age_mean = mean(AGE_DG, na.rm = T),
        age_max = max(AGE_DG, na.rm = T)
      )
  }
  
  # SUMMARIES ----
  
  table_summary_diagnoses <- function(data_pop, data_diagnoses, group = "exposure"){
    ## Creates a summary of selected diagnoses on the group (exposure/response)
    ## and gives summary information (Cases, Patients, PCT, Desc)
    # ## DEBUG
    # if(FALSE){
    #   data_pop <- dpop
    #   data_diagnoses <- exposure_diagnoses
    # }
    all <- function(){
      if(shiny::isRunning()) incProgress(1/3)
      koehenkiloita <- nrow(data_pop[data_pop$GROUP == group,]) ## Group joko exposure tai response
      d <- data_diagnoses %>% 
        group_by(DG, DGREG) %>%
        summarise(
          patients = length(unique(ID)),
          cases = n()
        ) %>%
        mutate(
          group_pct = round(100 * patients / koehenkiloita, 1)
        ) %>%
        left_join(data_codes) %>%
        select(DG, DGREG, patients, cases, group_pct, DESC)
      if(shiny::isRunning()) incProgress(2/3)
      ## Tietosuoja alle 6 tapaukset
      d <- d %>%
        filter(patients >= 6) %>%
        rbind(
          d %>%
            filter(patients < 6) %>%
            summarise(
              DG = "XX",
              DGREG = "XX",
              patients = sum(patients),
              cases = sum(cases),
              group_pct = NA,#sum(group_pct),
              # Diagnose = "Rest of the diagnoses",
              DESC = "Rest of the diagnoses"
            ) %>% 
            group_by(DG, DGREG) %>% 
            summarise(
              patients = sum(patients),
              cases = sum(cases)
            ) %>% 
            mutate(
              group_pct = round(100 * patients / koehenkiloita, 1)
            )
        )
      if(shiny::isRunning()) incProgress(3/3)
      return(d)
    }
    if(shiny::isRunning()){
      withProgress(message = "Table of Exposure Diagnoses", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
  
  
  ## RESPONSE -------
  
  plot_response_agedist <- function(dat){
    all <- function(){
      if(shiny::isRunning()) incProgress(1/3)
      d <- dat %>% 
        filter(resp.GROUP == "response") %>% 
        select(ID, resp.AGE_DG, GROUP) %>% 
        group_by(GROUP, resp.AGE_DG) %>%
        summarise(freq = n())
      if(shiny::isRunning()) incProgress(2/3)
      title = paste("Total Response Population size", sum(d$freq ))
      plt <- ggplot(data = d) +
        geom_bar(aes(x=resp.AGE_DG, y= freq, fill = GROUP, group=GROUP), 
                 stat = "identity") +
        scale_fill_manual(values = colors_in) +
        hrbrthemes::theme_ipsum_rc() +
        labs(title = title, subtitle = "Age on first response diagnose date")
      if(shiny::isRunning()) incProgress(3/3)
      return(plt)
    }
    if(shiny::isRunning()){
      withProgress(message = "Plotting Response Age Distribution", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
  
  table_response_agedist <- function(dat){
    all <- function(){
      ## Response population
      ## Exposure / No Exposure / All
      
      incProgress(1/3)
      d1 <- dat %>% 
        filter(resp.GROUP == "response") %>% 
        group_by(GROUP) %>% 
        summarise(
          pop_n = n(),
          age_min = min(resp.AGE_DG, na.rm = T),
          age_median = median(resp.AGE_DG, na.rm = T),
          age_mean = mean(resp.AGE_DG, na.rm = T),
          age_max = max(resp.AGE_DG, na.rm = T)
        )
      incProgress(2/3)
      d2 <- dat %>%
        filter(resp.GROUP == "response") %>%
        mutate(GROUP = "all") %>%
        group_by(GROUP) %>%
        summarise(
          pop_n = n(),
          age_min = min(resp.AGE_DG, na.rm = T),
          age_median = median(resp.AGE_DG, na.rm = T),
          age_mean = mean(resp.AGE_DG, na.rm = T),
          age_max = max(resp.AGE_DG, na.rm = T)
        )
      incProgress(3/3)
      d <- d1 %>% rbind(d2)
      return(d)
    }
    if(shiny::isRunning()){
      withProgress(message = "Table Response Age Distribution", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
  
  
  
  plot_crosstabulation <- function(data_population){
    ## TODO checkkaa html
    ## TODO voisiko excel parempi? openxlsx
    # data_population <- dpop # debug
    sjPlot::tab_xtab(var.row = data_population$exposure, var.col = data_population$response, title = "Population exposure and response diagnoses", show.row.prc = TRUE) 
  }
  
  
  tab_exp_resp <- function(dpop){
    all <- function(){
      if(shiny::isRunning()) incProgress(1/2)
      d <- dpop %>% 
        filter(exposure == 1 & response == 1) %>% 
        mutate(
          exp_resp = ifelse(DATE < resp.DATE, 1, ifelse(DATE == resp.DATE, 0, -1))
          # exp_resp = ifelse(exposure_date < response_date, 1, 0)
        ) %>% 
        group_by(exp_resp) %>% 
        summarise(
          n = n()
        ) %>% 
        mutate(
          percentage = round(100 * n / nrow(dpop %>% filter(exposure == 1 & response == 1)), 1),
          exp_resp = factor(case_when(
            exp_resp == 1 ~ "Exposure < Response",
            exp_resp == 0 ~ "Exposure == Response",
            exp_resp == -1 ~ "Exposure > Response"
          ), levels = c("Exposure < Response", "Exposure == Response", "Exposure > Response"))
        )
      if(shiny::isRunning()) incProgress(2/2)
      return(d)
    }
    if(shiny::isRunning()){
      withProgress(message = "Cross Tabulation", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
  
  ## HEALTH -----
  
  health_profile <- function(data_population, data_diagnoses, exposure_icd10, exposure_src){
    all <- function(){
      # data_population <- dpop
      # data_diagnoses <- diagnoses
      # exposure_icd10 <- regex_clean("^E11")
      # exposure_src <- c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa")
      
      ## Summat ja patient prossat
      d <- data_population
      # pop sizes
      popn <- d %>%
        select(ID, GROUP) %>%
        group_by(GROUP) %>%
        summarise(n_group=length(unique(ID)))
      ## add total group pop sizes
      d <- d %>%
        left_join(popn)
      if(shiny::isRunning()) incProgress(1/4) 
      # Groups / group & no exposure
      data_diagnoses %>%
        filter(DGREG == "ICD10") %>% 
        filter(SRC %in% exposure_src) %>% 
        filter(!grepl(pattern = regex_clean(exposure_icd10), x = DG)) %>% ## Ei oteta exposure diagnooseja analyysiin
        left_join(d %>% select(ID, GROUP, n_group)) %>%
        group_by(GROUP, ICD10_CLASS) %>%
        summarise(
          cases=n(),
          patients=length(unique(ID)),
          n_group = first(n_group)
        ) %>%
        mutate(
          per100=cases/100 * n_group,
          pct = 100 * patients / n_group,
        )-> icd10_recoded_summary
      if(shiny::isRunning()) incProgress(2/4) 
      # create 'data'
      data <- icd10_recoded_summary %>% 
        pivot_wider(id_cols = GROUP, values_from = pct, names_from = ICD10_CLASS) %>%
        arrange(GROUP)
      data <- as.data.frame(data)
      rownames(data) <- data$GROUP  # new
      data <- data[, 2:ncol(data)]            # TODO tämä vähän epäselvä, miksi aiemmin olin laittanut tähän ncol ja seuraavaan ncol-1 (23 ja 22)
      data <- rbind(rep(100,ncol(data)) , rep(0,ncol(data)) , data) # new
      if(shiny::isRunning()) incProgress(3/4)
      return(data)
    }
    if(shiny::isRunning()){
      withProgress(message = "Creating Health Profile", value = 0, {
        data <- all()
        # Custom the radarChart !
        radarchart(data, 
                   axistype=1,
                   pcol=colors_border, 
                   pfcol=colors_in, 
                   plwd=4,
                   plty=1,#custom polygon
                   cglcol="grey", 
                   cglty=1, 
                   axislabcol="grey", 
                   caxislabels=seq(0,200,20), 
                   cglwd=0.9,
                   vlcex=0.8
        )
        # Add a legend
        legend(x=1, 
               y=1.4, 
               legend = rownames(data[-c(1,2),]), 
               bty = "n", 
               pch=20, 
               col=colors_in, 
               text.col = "grey",
               cex=1.2, 
               pt.cex=3)
      })
    }else{
      data <- all()
      # Custom the radarChart !
      radarchart(data, 
                 axistype=1,
                 pcol=colors_border, 
                 pfcol=colors_in, 
                 plwd=4,
                 plty=1,#custom polygon
                 cglcol="grey", 
                 cglty=1, 
                 axislabcol="grey", 
                 caxislabels=seq(0,200,20), 
                 cglwd=0.9,
                 vlcex=0.8
      )
      # Add a legend
      legend(x=1, 
             y=1.4, 
             legend = rownames(data[-c(1,2),]), 
             bty = "n", 
             pch=20, 
             col=colors_in, 
             text.col = "grey",
             cex=1.2, 
             pt.cex=3)
    }
  }
  
  
  
  
  tbl_icd10_comparison <- function(data_population_grouped, data_diagnoses, exposure_icd10, exposure_src){
    all <- function(){
      dpop <- data_population_grouped
      if(shiny::isRunning()) incProgress(1/4)
      # Tarkastellaan TOP diagnoosit populaatiolla
      data_diagnoses %>%
        filter(DGREG == "ICD10") %>% 
        filter(SRC  %in% exposure_src) %>% 
        filter(ID %in% dpop$ID[dpop$GROUP == "exposure"] & !(grepl(pattern = regex_clean(exposure_icd10), DG))) %>% # for regex
        group_by(ICD10_3LETTERS) %>%
        summarise(
          patients = length(unique(ID))
        ) %>%
        mutate(
          exposure_group_pct = round(100 * patients / nrow(dpop[dpop$GROUP == "exposure",]), 1)
        ) -> d1
      if(shiny::isRunning()) incProgress(2/4)
      # Tarkastellaan TOP diagnoosit anti populaatiolla
      data_diagnoses %>%
        filter(DGREG == "ICD10") %>% 
        filter(SRC  %in% exposure_src) %>% 
        filter(ID %in% dpop$ID[dpop$GROUP == "no exposure"]) %>%
        group_by(ICD10_3LETTERS) %>%
        summarise(
          no_exposure_patients = length(unique(ID))
        ) %>%
        mutate(
          no_exposure_pct = round(100 * no_exposure_patients / nrow(dpop[dpop$GROUP == "no exposure",]), 1)
        ) -> d2
      if(shiny::isRunning()) incProgress(3/4)
      d <- left_join(d1,d2) %>%
        mutate(diff_pct = exposure_group_pct - no_exposure_pct) %>%
        left_join(
          data_codes %>% filter(CODECLASS == "ICD10") %>% select(DG, DESC), 
          by = c("ICD10_3LETTERS" = "DG"))
      if(shiny::isRunning()) incProgress(4/4)
      return(d)
    }
    if(shiny::isRunning()){
      withProgress(message = "Table ICD-10 Comparison", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
  
  
  plot_icd10_comparison <- function(tbl){
    all <- function(){
      dplot <- tbl %>%
        filter(diff_pct > 10 ) %>%
        pivot_longer(cols = c(exposure_group_pct, no_exposure_pct))
      if(shiny::isRunning()) incProgress(1/2)
      plt <- ggplot(dplot ) +
        geom_bar(aes(x=reorder(ICD10_3LETTERS, -value), y=value, fill=name, group=name), stat = "identity", position = "dodge") +
        coord_flip() +
        scale_fill_manual(values = colors_in) +
        labs(x="diagnose", y="percentage", title = "Exposure group top diagnoses") +
        hrbrthemes::theme_ipsum_rc()
      if(shiny::isRunning()) incProgress(2/2)
      return(plt)
    }
    if(shiny::isRunning()){
      withProgress(message = "Plotting ICD-10 Comparison", value = 0, {
       return(all())
      })
    }else{
      return(all())
    }
  }
  
  
  
  
  ## SURVIVAL ----
  create_dsurv <- function(dpop, data_response_diagnoses, censoring_date = as.Date("2023-12-21"), newdiag_before = TRUE){
    all <- function(){
      #censoring_date <- as.Date("2023-12-21") ## TODO: ajankohta mihin asti on kuolleiden tiedot / Paivita
      exposure_to_response <- dpop %>%
        filter(GROUP == "exposure") %>%
        rename(exposure_date = DATE) %>%
        left_join(data_response_diagnoses %>%
                    arrange(ID, DATE) %>%
                    group_by(ID) %>%
                    summarise(response_date = first(DATE)) %>%
                    select(ID, response_date),
                  by = "ID") %>%
        mutate(
          epvm=pmin(DATE_MIGRATION, censoring_date, na.rm = TRUE)
        ) %>%
        mutate(diagnose =  trunc((exposure_date %--% response_date) / days(1) ),
               dead = ifelse(!is.na(DATE_DEATH), trunc((exposure_date %--% DATE_DEATH) / days(1) ) , NA),
               censoring = trunc((exposure_date %--% epvm) / days(1))
        ) %>%
        mutate(censoring = ifelse(is.na(dead), censoring, NA))
      if(shiny::isRunning()) incProgress(1/2)
      
      d <- exposure_to_response %>%
        select(ID, response_date, diagnose, dead, censoring) %>%
        arrange(ID, response_date) %>%
        ## FILTER , take cases before 0 timepoint, yes/no
        filter(if(newdiag_before){diagnose >= 0 }else{is.numeric(diagnose)}) %>%
        arrange(ID, response_date) %>%
        group_by(ID) %>%
        summarise(diagnose = first(diagnose),
                  dead = first(dead),
                  censoring = first(censoring)) %>%
        mutate(diagnose = ifelse(diagnose < 0, 0 , diagnose)) %>%
        pivot_longer(cols = c(diagnose, dead, censoring)) %>%
        filter(!is.na(value))
      if(shiny::isRunning()) incProgress(2/2)
      return(d)
    }
    if(shiny::isRunning()){
      withProgress(message = "Creating Survival Data", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
  
  
  
  plot_kaplan_meier <- function(dsurv){
    all <- function(){
      dsurv <- dsurv %>%
        # TODO jos uusi data, tähän filtteri vain group
        mutate(
          event = ifelse(name == "diagnose", 1, 0) ## Event: 0 = censoring/kuollut, 1 = dementia
        )
      if(shiny::isRunning()) incProgress(1/3)
      ## Mallinnetaan elinaika-analyysi
      # library(survival)
      surv_object <- survival::Surv(time = dsurv$value, event = dsurv$event)
      if(shiny::isRunning()) incProgress(2/3)
      fit1 <- survfit(surv_object ~ 1, data = dsurv, id = ID)
      if(shiny::isRunning()) incProgress(3/3)
      plot <- plot(fit1)
      return(plot)
    }
    if(shiny::isRunning()){
      withProgress(message = "Plotting Kaplan Meier", value = 0, {
        return(all()) 
      })
    }else{
      return(all())
    }
  }
  
  plot_competing_risk <- function(dsurv){
    all <- function(){
      ## Data
      dsurv <- dsurv %>%
        mutate(
          event = ifelse(name == "diagnose", 1, ifelse(name == "dead", 2, 3))
        )
      if(shiny::isRunning()) incProgress(1/3)
      ## fitting a competing risks model
      CR <- cuminc(ftime = dsurv$value,
                   fstatus = dsurv$event,
                   cencode = 3)
      if(shiny::isRunning()) incProgress(2/3)
      plt <- ggcompetingrisks(fit = CR, multiple_panels = F, xlab = "Days", ylab = "Cumulative incidence of event",title = "Competing Risks Analysis") +
        scale_color_manual(name="", values=c("blue","red"), labels=c("Response Diagnose", "Dead"))
      if(shiny::isRunning()) incProgress(3/3)
      return(plt)
    }
    if(shiny::isRunning()){
      withProgress(message = "Plotting Competing Risk", value = 0, {
        return(all())
      })
    }else{
      return(all())
    }
  }
}

## POISSON REGRESSION -----

pirr <- function(
    exposure_icd10 = "",
    exposure_icd9 = "",
    exposure_icd8 = "",
    exposure_src = c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"),
    response_icd10 = "",
    response_icd9 = "",
    response_icd8 = "",
    response_src = c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa"),
    response_extra = c("Fractures"),
    limits=c(0.3,3) ) {
  
  library(dplyr)
  library(lubridate)
  library(ggeffects)
  library(lmtest)
  library(ggplot2)
  ## Käyttää aineistoja: ostpre_ic10, population, murtumat
  if(FALSE){
    ## Input Data and Vars // FOR TESTING
    exposure_icd10 <- "^E11"
    response_icd10 <- "^I10"
    exposure_src <- c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa")
    response_src <- c("avohilmo", "erko", "hilmo", "local", "ksyy", "soshilmo", "syopa")
    response_extra = c("murtumat")
    limits <-  c(0.3,3) ## TODO onko nämä vakiot, vai pitäisikö pystyä muuttamaan?
  }
  all <- function(){
    ### Diagnoses ----
    ## Altiste
    dalt <- search_diagnoses(regex_icd10 = exposure_icd10,
                             regex_icd9 = exposure_icd9,
                             regex_icd8 = exposure_icd8,
                             registry_source = exposure_src) %>%
      arrange(ID, DATE) %>%
      group_by(ID) %>%
      summarise(DATE_EXPOSURE = first(DATE),
                DG = first(DG) ## TODO vaihda nimi kauttaaltaan
      ) %>%
      left_join(population, by = "ID") %>%
      mutate(ika_altiste = trunc((DATE_BIRTH %--% DATE_EXPOSURE) / years(1))) %>%
      select(ID, DATE_EXPOSURE, ika_altiste, DG)
    if(shiny::isRunning()) incProgress(1/10)
    
    
    ## Vaste
    dvast <- search_diagnoses(regex_icd10 = response_icd10,
                              regex_icd9 = response_icd9,
                              regex_icd8 = response_icd8,
                              registry_source = response_src) %>%
      arrange(ID, DATE) %>%
      group_by(ID) %>%
      summarise(DATE_RESPONSE = first(DATE),
                DG = first(DG) ## TODO vaihda nimi kauttaaltaan
      ) %>%
      left_join(population, by = "ID") %>%
      mutate(AGE_RESPONSE = trunc((DATE_BIRTH %--% DATE_RESPONSE) / years(1))) %>%
      select(ID, DATE_RESPONSE, AGE_RESPONSE, DG)
    if(shiny::isRunning()) incProgress(2/10)
    
    
    ### Further data wrangling ----
    
    ## Altisteen ja vasteen yhdistys
    d1 <- dvast |>
      left_join(dalt, by=c("ID")) |>
      mutate(
        ## vasteen ja altisteen pvm ero
        ero=as.numeric(lubridate::decimal_date(DATE_RESPONSE)-lubridate::decimal_date(DATE_EXPOSURE)),
        ## vasteen ja altisteen aika faktoroitu, käytetään jatkossa luokittelevana tekijänä
        caika=factor(case_when(
          is.na(ero) | ero<0 ~ "0 No exposure",
          ero < 1 ~ "1 exposure < 1y",
          ero < 5 ~ "2 exposure 1-4y",
          ero < 10 ~ "3 exposure 5-9y",
          ero < 15 ~ "4 exposure 10-14y",
          TRUE ~ "5 exposure 15+y",
        ))
      )
    if(shiny::isRunning()) incProgress(3/10) 
    
    ## Nyt vain aggrekoidaan ajan ja iän mukaan. Aiemmin ollut aliryhmät.
    d1 <- d1 %>%
      count(caika, AGE_RESPONSE)
    
    ## Seurannan aikarajojen muodostaminen
    dat1 <- population |>
      left_join(dvast %>% select(ID, DATE_RESPONSE), by = "ID") %>%
      mutate(
        kuol=as.integer(!is.na(DATE_DEATH)),
        apvm=pmax(DATE_BIRTH,as.Date("1953-01-01"),na.rm="TRUE"),
        epvm=pmin(DATE_DEATH,DATE_MIGRATION,as.Date("2022-12-31"),DATE_RESPONSE,na.rm = TRUE) ## TODO end date
      )
    ages <- c(20,50:90)*365.25
    ## Perustiedot (päivämäärät) ja altisteen päivämäärät
    cdat <- data.table::as.data.table(population) |>
      left_join(dalt |> select(ID, c00pvm=DATE_EXPOSURE), by=c("ID")) |> # c00pvm on altistediagnoosi aika
      mutate(
        c01pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(1)),
        c05pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(5)),
        c10pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(10)),
        c15pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(15))
      )
    ## split functions
    dat2 <- dat1 |> heaven::lexisSeq(invars=c("ID","apvm","epvm","kuol"), varname="DATE_BIRTH", splitvector=ages, format="vector", value="agec")
    dat3 <- dat2 |> heaven::lexisTwo(cdat, c("ID","apvm","epvm","kuol"),c("ID","c00pvm","c01pvm","c05pvm","c10pvm","c15pvm")) %>%
      mutate(
        ika =  round( as.numeric((apvm - DATE_BIRTH) / 365.25), 0)
      )
    if(shiny::isRunning()) incProgress(4/10) 
    
    adat <- dat3 |>
      mutate(
        caika=factor(case_when(
          c15pvm==1 ~ "5 exposure 15+y",
          c10pvm==1 ~ "4 exposure 10-14y",
          c05pvm==1 ~ "3 exposure 5-9y",
          c01pvm==1 ~ "2 exposure 1-4y",
          c00pvm==1 ~ "1 exposure < 1y",
          TRUE ~ "0 No exposure"
        )),
        ikar=case_when(
          agec<2 ~ 20,
          TRUE ~ agec+48
        ),
        kesto=as.numeric(lubridate::decimal_date(epvm)-lubridate::decimal_date(apvm))
      ) |>
      filter(ika>=50) |> ### seuranta alkaa kun henkilö täyttää 50v (cancer-/murtuma-aineisto) /// OSTPRE Pelkkä ICD10 filt >= 65
      group_by(ika,caika) |>
      summarise(
        pyrs=sum(kesto),
        kuol=sum(kuol)
      ) |>
      left_join(d1, by=c("ika"="AGE_RESPONSE","caika")) |>
      mutate(
        across(everything(), ~ tidyr::replace_na(.,0)),
        cever=factor(case_when(
          caika=="0 No exposure" ~ "0 No exposure",
          TRUE ~ "1 exposure"
        ))
      ) |>
      filter(pyrs>0.01) ## altistusajan filtteri
    if(shiny::isRunning()) incProgress(5/10) 
    
    ### Create diagnoses results ----
    ## Tuloslistaus nimetään muuttujat
    dglist <- c("kuol",names(d1)[3:ncol(d1)])
    names(dglist) <- c("Mortality","DG")
    tulos <- list()
    if(shiny::isRunning()) incProgress(6/10) 
    
    ### Tehdään Poisson analyysit tulos listaan
    # i <- "kuol"
    for (i in dglist) {
      
      #    adat$vaste <- adat[,dglist[i]][[1]]
      adat$vaste <- adat[,i][[1]]
      nimi <- names(dglist[dglist==i])
      
      n1 <- adat |>
        group_by(caika) |>
        summarise(
          n=sum(vaste),
          pyrs=sum(pyrs)
        ) |>
        mutate(rn=row_number())
      m1 <- glm(vaste ~ splines::bs(ika) + caika, offset = log(pyrs), family = poisson(link = "log"), data = adat)
      pval1 <- lmtest::coeftest(m1,vcov.=sandwich::vcovHC(m1,type="HC0"))
      pres <- tibble(pval=c(NA,pval1[,"Pr(>|z|)"][grep("exposure",rownames(pval1))])) |> mutate(rn=row_number())
      mdi <- tibble(ggeffects::predict_response(m1, terms=c("caika"), condition=(c(ika=70, pyrs=10000)))) |> mutate(rn=row_number())
      mdp <- ggeffects::predict_response(m1, terms=c("caika"), vcov_fun="vcovHC", vcov_type="HC0", condition=(c(ika=70, pyrs=10000/mdi$predicted[1])))
      
      n2 <- adat |>
        group_by(cever) |>
        summarise(
          n=sum(vaste),
          pyrs=sum(pyrs)
        ) |>
        mutate(rn=row_number())
      m2 <- glm(vaste ~ splines::bs(ika) + cever, offset = log(pyrs), family = poisson(link = "log"), data = adat)
      pval2 <- lmtest::coeftest(m2,vcov.=sandwich::vcovHC(m2,type="HC0"))
      pres2 <- tibble(pval=c(NA,pval2[,"Pr(>|z|)"][grep("exposure",rownames(pval2))])) |> mutate(rn=row_number())
      mdi2 <- tibble(ggeffects::predict_response(m2, terms=c("cever"), condition=(c(ika=70, pyrs=10000)))) |> mutate(rn=row_number())
      mdp2 <- ggeffects::predict_response(m2, terms=c("cever"), vcov_fun="vcovHC", vcov_type="HC0", condition=(c(ika=70, pyrs=10000/mdi2$predicted[1])))
      #    mda2 <- ggeffects::predict_response(m2, terms=c("ikar","cever"), vcov_fun="vcovHC", vcov_type="HC0", condition=(c(pyrs=10000)))
      mda2 <- ggeffects::predict_response(m2, terms=c("ika","cever"), condition=(c(pyrs=10000)))
      
      res <- NULL |>
        bind_rows(mdp |> as.data.frame() |> mutate(rn=row_number()) |> filter(!is.na(x)) |> left_join(pres,by="rn") |> left_join(n1,by="rn") |> left_join(as.data.frame(mdi) |> select(rn,adj=predicted),by="rn")) |>
        bind_rows(mdp2 |> as.data.frame() |> mutate(rn=row_number()) |> filter(!is.na(x)) |> left_join(pres2,by="rn") |> left_join(n2,by="rn") |> left_join(as.data.frame(mdi2) |> select(rn,adj=predicted),by="rn")) |>
        mutate(crude=n/pyrs*10000) |>
        select(factor=x,n,pyrs,crude,adj,SIR=predicted,conf.low,conf.high,pval)
      
      p1 <- plot(mdp) + ggplot2::scale_y_continuous(trans="log10",limits=limits) + ggplot2::labs(y="Standardized Incidence Ratio (SIR), log scale",x="",title=nimi)
      p2 <- plot(mda2) + ggplot2::labs(y="Exposure per 10000 person years",x="Age",title=nimi,color="Response status") + ggplot2::theme(legend.position.inside=c(0.11,0.83))
      
      resl <- list(table=res,plot1=p1,plot2=p2)
      
      tulos[[nimi]] <- resl
    }
    if(shiny::isRunning()) incProgress(7/10) 
    
    ## Fractures ------
    if("Fractures" %in% response_extra & "FRACTURES" %in% unique(diagnoses$DGREG)){
      ### Vasteaineisto - murtumat
      ## Altisteen ja vasteen yhdistys
      pmur <- diagnoses |>
        filter(DGREG == "FRACTURES") |>
        # filter(ID %in% unique(dalt$ID)) |> ## Tämä oli virhe ja aiheutti ongelmat
        left_join(dalt %>% rename(DG_ALT = DG), by=c("ID")) |>
        mutate(
          ## vasteen ja altisteen pvm ero
          ero=as.numeric(lubridate::decimal_date(DATE)-lubridate::decimal_date(DATE_EXPOSURE)),
          ## vasteen ja altisteen aika faktoroitu, käytetään jatkossa luokittelevana tekijänä
          caika=factor(case_when(
            is.na(ero) | ero<0 ~ "0 No exposure",
            ero < 1 ~ "1 exposure < 1y",
            ero < 5 ~ "2 exposure 1-4y",
            ero < 10 ~ "3 exposure 5-9y",
            ero < 15 ~ "4 exposure 10-14y",
            TRUE ~ "5 exposure 15+y",
          ))
        )
      
      ## aggrekoidaan aineisto murtuma_ikä ja altisteen_aika_murtumaan_nähden - mikä murtuma kyseessä
      pmura <- pmur |>
        rename(AGE_RESPONSE = AGE) |>
        count(DG,AGE_RESPONSE,caika) |>
        tidyr::pivot_wider(names_from=DG, values_from=n,values_fill=0) |>
        mutate(
          any_fracture=ankle+forearm+hip+humerus+vertebral,
          typ=forearm+hip+humerus+vertebral
        )
      
      ## Seurannan aikarajojen muodostaminen
      dat1 <- population |>
        mutate(
          kuol=as.integer(!is.na(DATE_DEATH)),
          apvm=pmax(DATE_BIRTH,as.Date("1953-01-01"),na.rm="TRUE"),
          epvm=pmin(DATE_DEATH,DATE_MIGRATION,as.Date("2022-12-31"),na.rm = TRUE)
        )
      ages <- c(20,50:90)*365.25 ## TODO mikä tarkoitus tällä?
      ## Perustiedot (päivämäärät) ja altisteen päivämäärät
      cdat <- data.table::as.data.table(population) |>
        left_join(dalt |> select(ID, c00pvm=DATE_EXPOSURE), by=c("ID")) |> # c00pvm on altistediagnoosi aika
        mutate(
          c01pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(1)),
          c05pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(5)),
          c10pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(10)),
          c15pvm=lubridate::add_with_rollback(c00pvm,lubridate::years(15))
        )
      ## split functions
      dat2 <- dat1 |> heaven::lexisSeq(invars=c("ID","apvm","epvm","kuol"), varname="DATE_BIRTH", splitvector=ages, format="vector", value="agec")
      dat3 <- dat2 |> heaven::lexisTwo(cdat, c("ID","apvm","epvm","kuol"),c("ID","c00pvm","c01pvm","c05pvm","c10pvm","c15pvm")) %>%
        mutate(
          ika =  round( as.numeric((apvm - DATE_BIRTH) / 365.25), 0)
        )
      
      adat <- dat3 |>
        mutate(
          caika=factor(case_when(
            c15pvm==1 ~ "5 exposure 15+y",
            c10pvm==1 ~ "4 exposure 10-14y",
            c05pvm==1 ~ "3 exposure 5-9y",
            c01pvm==1 ~ "2 exposure 1-4y",
            c00pvm==1 ~ "1 exposure < 1y",
            TRUE ~ "0 No exposure"
          )),
          ikar=case_when(
            agec<2 ~ 20,
            TRUE ~ agec+48
          ),
          kesto=as.numeric(lubridate::decimal_date(epvm)-lubridate::decimal_date(apvm))
        ) |>
        filter(ika>=50) |>
        group_by(ika,caika) |>
        summarise(
          pyrs=sum(kesto),
          kuol=sum(kuol)
        ) |>
        left_join(pmura, by=c("ika"="AGE_RESPONSE","caika")) |>
        mutate(
          across(everything(), ~ tidyr::replace_na(.,0)),
          cever=factor(case_when(
            caika=="0 No exposure" ~ "0 No exposure",
            TRUE ~ "1 exposure"
          ))
        ) |>
        filter(pyrs>0.01) ## altistusajan filtteri
      
      
      if(shiny::isRunning()) incProgress(8/10) 
      ### Create fractures results ----
      
      ## nimetään muuttujat
      dglist <- c(names(pmura)[3:ncol(pmura)])
      names(dglist) <- c("Ankle","Forearm","Hip","Humerus","Vertebral","Any Fracture","Osteoporotic")
      tulos2 <- list()
      
      ### Tehdään Poisson analyysit tulos listaan
      # i <- "kuol"
      for (i in dglist) {
        
        #    adat$vaste <- adat[,dglist[i]][[1]]
        adat$vaste <- adat[,i][[1]]
        nimi <- names(dglist[dglist==i])
        
        n1 <- adat |>
          group_by(caika) |>
          summarise(
            n=sum(vaste),
            pyrs=sum(pyrs)
          ) |>
          mutate(rn=row_number())
        m1 <- glm(vaste ~ splines::bs(ika) + caika, offset = log(pyrs), family = poisson(link = "log"), data = adat)
        pval1 <- lmtest::coeftest(m1,vcov.=sandwich::vcovHC(m1,type="HC0"))
        pres <- tibble(pval=c(NA,pval1[,"Pr(>|z|)"][grep("exposure",rownames(pval1))])) |> mutate(rn=row_number())
        mdi <- tibble(ggeffects::predict_response(m1, terms=c("caika"), condition=(c(ika=70, pyrs=10000)))) |> mutate(rn=row_number())
        mdp <- ggeffects::predict_response(m1, terms=c("caika"), vcov_fun="vcovHC", vcov_type="HC0", condition=(c(ika=70, pyrs=10000/mdi$predicted[1])))
        
        n2 <- adat |>
          group_by(cever) |>
          summarise(
            n=sum(vaste),
            pyrs=sum(pyrs)
          ) |>
          mutate(rn=row_number())
        m2 <- glm(vaste ~ splines::bs(ika) + cever, offset = log(pyrs), family = poisson(link = "log"), data = adat)
        pval2 <- lmtest::coeftest(m2,vcov.=sandwich::vcovHC(m2,type="HC0"))
        pres2 <- tibble(pval=c(NA,pval2[,"Pr(>|z|)"][grep("exposure",rownames(pval2))])) |> mutate(rn=row_number())
        mdi2 <- tibble(ggeffects::predict_response(m2, terms=c("cever"), condition=(c(ika=70, pyrs=10000)))) |> mutate(rn=row_number())
        mdp2 <- ggeffects::predict_response(m2, terms=c("cever"), vcov_fun="vcovHC", vcov_type="HC0", condition=(c(ika=70, pyrs=10000/mdi2$predicted[1])))
        #    mda2 <- ggeffects::predict_response(m2, terms=c("ika","cever"), vcov_fun="vcovHC", vcov_type="HC0", condition=(c(pyrs=10000)))
        mda2 <- ggeffects::predict_response(m2, terms=c("ika","cever"), condition=(c(pyrs=10000)))
        
        res <- NULL |>
          bind_rows(mdp |> as.data.frame() |> mutate(rn=row_number()) |> filter(!is.na(x)) |> left_join(pres,by="rn") |> left_join(n1,by="rn") |> left_join(as.data.frame(mdi) |> select(rn,adj=predicted),by="rn")) |>
          bind_rows(mdp2 |> as.data.frame() |> mutate(rn=row_number()) |> filter(!is.na(x)) |> left_join(pres2,by="rn") |> left_join(n2,by="rn") |> left_join(as.data.frame(mdi2) |> select(rn,adj=predicted),by="rn")) |>
          mutate(crude=n/pyrs*10000) |>
          select(factor=x,n,pyrs,crude,adj,SIR=predicted,conf.low,conf.high,pval)
        
        ## TODO kokeile ilman limits-maaritysta && automaattinen limits haistelu?
        # p1 <- plot(mdp) + ggplot2::scale_y_continuous(trans="log10") + ggplot2::labs(y="Standardized Incidence Ratio (SIR), log scale",x="",title=nimi)
        p1 <- plot(mdp) + ggplot2::scale_y_continuous(trans="log10",limits=limits) + ggplot2::labs(y="Standardized Incidence Ratio (SIR), log scale",x="",title=nimi)
        p2 <- plot(mda2) + ggplot2::labs(y="Exposure per 10000 person years",x="Age",title=nimi,color="Response status") + ggplot2::theme(legend.position.inside=c(0.11,0.83))
        resl <- list(table=res,plot1=p1,plot2=p2)
        tulos2[[nimi]] <- resl
      }
      if(shiny::isRunning()) incProgress(9/10) 
      
      ## Diagnose and Extra class
      tulos <- c(tulos, tulos2)
      if(shiny::isRunning()) incProgress(10/10) 
    }
    ## Final result out
    return(tulos)
  }
  if(shiny::isRunning()){
    withProgress(message = "Calculating SIR", value = 0, {
      return(all())
    })
  }else{
    return(all())
  }
  
  
}

