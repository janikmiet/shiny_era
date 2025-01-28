# Common Data Model
## OSTPRE DATA CLEANING AND CDM
## paivitetty versio, more cdm
## Source data files are in: ostpre_projects/miettinen/data/
## 2024-11-14 / JKM

# Settings ----
library(qs)
library(dplyr)
library(arrow)
library(lubridate)


## Data Source and Out ---------
## SRC is where data without cleaning is stored
## OUT is where data is preprocessed VIA this script

## Create Symbolic links
if(dir.exists("/research/groups/ostpre_projects/miettinen/explorer/data/src/")) system("ln -s /research/groups/ostpre_projects/miettinen/explorer/data/src/ _datasrc")
if(dir.exists("/research/groups/ostpre_projects/miettinen/explorer/data/app_shiny_era/")) system("ln -s /research/groups/ostpre_projects/miettinen/explorer/data/app_shiny_era/ _data")
loc_src <- "_datasrc/"
loc_out <- "_data/"
WRITE <- TRUE ## Variable to determine whether process src data to out folder.

# Population ----
if(TRUE){
  population <- arrow::read_parquet(file = paste0(loc_src, "perustiedot.parquet"))
  population <- population %>% 
    rename(DATE_BIRTH = spvm, 
           DATE_DEATH = kpvm,
           DATE_MIGRATION = ulkpvm)
  if(WRITE) arrow::write_parquet(x = population, sink = paste0(loc_out,  "population.parquet"))
}

# Diagnoses ----
if(TRUE){
  ## ICD8  ----
  icd8 <- arrow::read_parquet(paste0(loc_src, "poisto_icd8.parquet"))
  icd8 <- icd8 %>% 
    rename(DG=dg, DATE = TULOPV) %>% 
    mutate(
      DGREG = "ICD8",
      DG=gsub("[[:punct:]]","",DG),
      SRC="hilmo",
      ICD10_3LETTERS = NA,
      ICD10_CLASS = NA) %>% 
    select(LOMNO1, DGREG, DATE, DG, SRC, ICD10_3LETTERS, ICD10_CLASS)
  ## ICD9 ----- 
  icd9 <- arrow::read_parquet(paste0(loc_src, "icd9.parquet"))
  icd9 <- icd9 %>% 
    rename(DATE = TUPVA) %>% 
    mutate(
      DGREG = "ICD9",
      SRC = case_when(
        SRC == "poisto" ~ "hilmo",
        SRC == "shilmo" ~ "soshilmo",
        SRC == "ksyy" ~ "ksyy",
        SRC == "erko" ~ "erko"
      ),
      ICD10_3LETTERS = NA,
      ICD10_CLASS = NA
    ) %>% 
    select(LOMNO1, DGREG, DATE, DG, SRC, ICD10_3LETTERS, ICD10_CLASS)
  ## ICD10  ----
  aicd10_raw <- arrow::read_parquet(file = paste0(loc_src, "icd10_raw.parquet"))
  # aicd10_raw %>% count(src) 
  ## avohu_pitkadg otetaan vain ensimm.
  aicd10_raw %>% 
    filter(src == "avohu_pitkadg") %>% 
    arrange(lomno1, dgdate, dg) %>% 
    group_by(lomno1, dgdate, dg) %>% 
    summarise(
      dgdate = first(dgdate)
    ) %>% 
    mutate(
      src = "avohu_pitkadg"
    ) -> tmp
  aicd10_raw <- aicd10_raw %>% 
    filter(src != "avohu_pitkadg") %>% 
    rbind(tmp)
  ## muuttujien nimet ja SRC. kpo_laake omaksi ja kpo
  aicd10_raw <- aicd10_raw %>% 
    rename(LOMNO1 = lomno1,
           DATE = dgdate,
           DG = dg) %>% 
    mutate( 
      DGREG = "ICD10",
      SRC = case_when(
        src == "avohilmo" ~ "avohilmo",
        src == "avohu_icd10" ~ "avohilmo",
        src == "avohu_pitkadg" ~ "avohilmo",
        src == "avohu_taptyyp" ~ "avohilmo",
        src == "avohu_ulksyy" ~ "avohilmo",
        src == "erko" ~ "erko",
        src == "hilmo" ~ "hilmo",
        src == "hilmou" ~ "hilmo",
        src == "kpo_icd10" ~  "local",#"kpo",
        src == "kpo_laake" ~  "local",#"kpo_laake",
        src == "kpo_riski" ~  "local",#"kpo",
        src == "ksyy" ~ "ksyy",
        src == "kys" ~ "local", #"kys",
        src == "soshilmo" ~ "soshilmo",
        src == "syopa"  ~ "syopa"
      )
    ) %>% 
    mutate(
      ICD10_3LETTERS = substr(DG, 1, 3)
    ) %>% 
    select(LOMNO1, DGREG, SRC, DATE, DG, ICD10_3LETTERS)
  ## ICD10 pääluokitus
  icd10 <- aicd10_raw %>% 
    mutate(
      ICD10_CLASS = case_when(
        substr(ICD10_3LETTERS, 1,1 ) %in% c("A", "B")  ~ "A00–B99 ",
        substr(ICD10_3LETTERS, 1,1 ) == "C" | (substr(ICD10_3LETTERS, 1,1 ) == "D" & substr(ICD10_3LETTERS, 2,3 ) < 49 ) ~ "C00–D48",
        substr(ICD10_3LETTERS, 1,1 ) == "D" & substr(ICD10_3LETTERS, 2,3 ) > 49   ~ "D50–D89",
        substr(ICD10_3LETTERS, 1,1 ) == "E"  ~ "E00–E90",
        substr(ICD10_3LETTERS, 1,1 ) == "F"  ~ "F00–F99",
        substr(ICD10_3LETTERS, 1,1 ) == "G"  ~ "G00–G99",
        substr(ICD10_3LETTERS, 1,1 ) == "H" & substr(ICD10_3LETTERS, 2,3 ) < 60 ~ "H00–H59",
        substr(ICD10_3LETTERS, 1,1 ) == "H" & substr(ICD10_3LETTERS, 2,3 ) > 59 ~ "H60–H95",
        substr(ICD10_3LETTERS, 1,1 ) == "I"  ~ "I00–I99",
        substr(ICD10_3LETTERS, 1,1 ) == "J"  ~ "J00–J99",
        substr(ICD10_3LETTERS, 1,1 ) == "K"  ~ "K00–K93",
        substr(ICD10_3LETTERS, 1,1 ) == "L"  ~ "L00–L99",
        substr(ICD10_3LETTERS, 1,1 ) == "M"  ~ "M00–M99",
        substr(ICD10_3LETTERS, 1,1 ) == "N"  ~ "N00–N99",
        substr(ICD10_3LETTERS, 1,1 ) == "O"  ~ "O00–O99",
        substr(ICD10_3LETTERS, 1,1 ) == "P"  ~ "P00–P96",
        substr(ICD10_3LETTERS, 1,1 ) == "Q"  ~ "Q00–Q99",
        substr(ICD10_3LETTERS, 1,1 ) == "R"  ~ "R00–R99",
        substr(ICD10_3LETTERS, 1,1 ) %in% c("S", "T") ~ "S00–T98",
        substr(ICD10_3LETTERS, 1,1 ) %in% c("V", "Y")   ~ "V01–Y98",
        substr(ICD10_3LETTERS, 1,1 ) == "Z"  ~ "Z00–ZZB"
      ))
  ## Fractures -----
  ## Murtumat aineiston käsittely ja muuttujien yhtenäistäminen
  dmu <- arrow::read_parquet(paste0(loc_src, "dmu.parquet"))
  ostpre_murtumat <- dmu %>% 
    rename(
      DATE = pvm,
      DG = murt,
      AGE = muika
    ) %>% 
    mutate(
      DGREG = "FRACTURES",
      SRC = "murt",
      ICD10_3LETTERS = NA, 
      ICD10_CLASS = NA
    ) %>% 
    select(LOMNO1,DGREG, DATE, DG, SRC, ICD10_3LETTERS, ICD10_CLASS)
  ## Merge diagnoses ----
  diagnoses <- icd10 %>% 
    rbind(icd9) %>% 
    rbind(icd8) %>% 
    rbind(ostpre_murtumat) %>%
    left_join(population %>% select(LOMNO1, DATE_BIRTH), by = "LOMNO1") %>%
    mutate(AGE = floor(lubridate::time_length(difftime(DATE, DATE_BIRTH), "years")))
  
  
  if(WRITE) arrow::write_parquet(diagnoses, paste0(loc_out, "diagnoses.parquet"))
  
}


# Codes ----
if(TRUE){
  # TODO MERGE ALL DIAGNOSES TO ONE DATASET
  ## ICD8 Codes -----
  icd8_codes <- read.csv(paste0(loc_src, "icd8.csv"))
  icd8_codes <- icd8_codes %>% 
    mutate(
      DG = gsub("[[:punct:]]", "", Code),
      CODECLASS = "ICD8",
      DESC = Disease,
      CATEGORY = Category
    ) %>% 
    select(DG, CODECLASS, CATEGORY, DESC)
    # select(DG, Code, CATEGORY, DESC)
  
  ## ICD9 Codes (Finnish Version) -----
  fil <- paste0(loc_src, "icd9_nimet.xlsx")
  if(file.exists(fil)){
    icd9_codes <- readxl::read_xlsx(fil)
    icd9_codes <- icd9_codes %>% 
      rename(DESC = SELITE) %>% 
      mutate(
        CODECLASS = "ICD9",
        CATEGORY = "") %>% 
      select(DG, CODECLASS, CATEGORY, DESC)
  }
  ## ICD10 Codes ----
  # icd10_codes <- readxl::read_xlsx("_clinical/ICD-10_MIT_2021_Excel_16-March_2021.xlsx") ## ICD-10 labels
  icd10_codes <- readxl::read_xlsx(paste0(loc_src,"icd10codes.xlsx")) ## ICD-10 labels
  icd10_codes <- icd10_codes %>% 
    # rename(DESC = LongName) %>%
    mutate(CATEGORY = "") %>% 
    mutate(CODECLASS = "ICD10") %>% 
    select(DG, CODECLASS, CATEGORY, DESC)
  
  data_codes <- icd10_codes %>% 
    rbind(icd9_codes) %>% 
    rbind(icd8_codes)
  
  if(WRITE) arrow::write_parquet(data_codes, paste0(loc_out, "data_codes.parquet"))
}



# Others -----
## TODO miten otetaan mukaan tallaiset extra datat ja nakymat?


##  Questionares Vastpaivat ----
if(TRUE){
  qs::qload("_datasrc/vastpvm.qs")
  vastpaiv <- vastpaiv %>% 
    rename(
      LOMNO1 = lomno1
    )
  if(WRITE) arrow::write_parquet(x = vastpaiv, sink = paste0(loc_out,  "ostpre_vastpaiv.parquet"))
}




# Save all in one object -----
if(FALSE){
  # qs::qsavem()
}

# Clean -----
if(FALSE){
  ## remove symbolic link
  system("unlink _datasrc")
  
}
