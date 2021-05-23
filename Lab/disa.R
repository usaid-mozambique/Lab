
#------LOAD CORE TIDYVERSE & OTHER PACKAGES-------------------------------------------

library(readr)
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(purrr)

rm(list = ls())

#---- DEFINE MONTH AND PATHS -------------------------------------------------------

file_monthly <- "data_source/monthly/Relatorio Mensal de Carga Viral (Abril).xlsx"
month <- "2021-04-20"
month_output <- "C:/Users/jlara/Documents/GitHub/Lab/Lab/data_source/monthly_processsed/2021_04.tsv"
final_output <- "C:/Users/jlara/Documents/GitHub/Lab/Lab/output/disa.tsv"


historic_files_path <- "C:/Users/jlara/Documents/GitHub/Lab/Lab/data_source/monthly_processsed/"  # PATH USED TO CREATE A LIST OF ALL .CSV FILES PREVIOUSLY CREATED
compile_path <- "C:/Users/jlara/Documents/GitHub/Lab/Lab/data_source/monthly_processsed/"

#---- LOAD DATASETS AND UNION -------------------------------------------------------

xAge <- read_excel({file_monthly}, 
                     sheet = "S. Viral (Idade)", skip = 2) %>% 
  dplyr::mutate(group = "Age") %>%
  dplyr::select(-c(`SISMA ID`)) %>% 
  glimpse()

xSex <- read_excel({file_monthly}, 
                     sheet = "S. Viral (Genero)", skip = 2) %>% 
  dplyr::mutate(group = "Sex") %>% 
  dplyr::select(-c(`SISMA ID`)) %>% 
  glimpse

xPW <- read_excel({file_monthly}, 
                   sheet = "S. Viral (M. Gravidas)", skip = 2) %>% 
  dplyr::mutate(group = "PW") %>% 
  dplyr::select(-c(`SISMA ID`)) %>% 
  dplyr::rename(US = HF,
                PROVINCIA = PROVINCE,
                DISTRITO = DISTRICT) %>% 
  glimpse

xLW <- read_excel({file_monthly}, 
                   sheet = "S. Viral (M. Lactantes)", skip = 2) %>% 
  dplyr::mutate(group = "LW") %>%
  dplyr::select(-c(`SISMA ID`)) %>% 
  dplyr::rename(US = HF,
                PROVINCIA = PROVINCE,
                DISTRITO = DISTRICT) %>% 
  glimpse

df <- dplyr::bind_rows(xAge, xSex, xPW, xLW)

#---- PROCESS DATAFRAME -------------------------------------------------------

df_1 <- df %>% 
  dplyr::select(-c(`CV < 1000`, `CV > 1000`, TOTAL)) %>%
  dplyr::rename(province = PROVINCIA,
                district = DISTRITO,
                site = US,
                age = Idade,
                sex = Genero) %>% 
  tidyr::pivot_longer(`Rotina (<1000)`:`Motivo de Teste não especificado (>1000)`, names_to = "indicator", values_to = "value") %>% 
  dplyr::mutate(motive = dplyr::case_when(grepl("Rotina", indicator) ~ "Routine",
                                          grepl("Fal", indicator) ~ "Theraputic Failure",
                                          grepl("Repetir", indicator) ~ "Post Breastfeeding",
                                          grepl("Motivo de Teste n", indicator) ~ "Not Specified"),
                result = dplyr::case_when(grepl("<1000", indicator) ~ "<1000",
                                          grepl(">1000", indicator) ~ ">1000")) %>% 
  dplyr::select(-c(indicator)) %>% 
  glimpse()

#---- RECODE AGE/SEX VALUES -----------------------------------------------

df_2 <- df_1 %>% 
  dplyr::mutate(age = dplyr::na_if(age, "Idade não especificada"),
                age = dplyr::na_if(age, "No Age Specified"),
                age = dplyr::na_if(age, "Não especificada"),
                
                sex = dplyr::na_if(sex, "UNKNOWN"),
                sex = dplyr::na_if(sex, "Not Specified"),
                sex = dplyr::na_if(sex, "Não especificado"))

#---- FILTER LINES ONLY >0 -----------------------------------------------

df_3 <- df_2 %>% 
  dplyr::filter(value > 0) %>% 
  dplyr::mutate(indicator = "VL",
                month = {month})

#------ WRITE FILE TO DISK -------------------------------------------

readr::write_tsv(
  df_3,
  {month_output},
  na ="")

#---- DEFINE PATH AND SURVEY ALL MONTHLY TPT DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------

historic_files <- dir({historic_files_path}, pattern = "*.tsv")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

#---- ROW BIND ALL MONTHS -----------------------

disa <- historic_files %>%
  map(~ read_tsv(file.path(compile_path, .))) %>% 
  reduce(rbind)

#------ WRITE FILE TO DISK -------------------------------------------

readr::write_tsv(
  disa,
  {final_output},
  na ="")

