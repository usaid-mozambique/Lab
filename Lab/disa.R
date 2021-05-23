
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
  dplyr::mutate(age = dplyr::recode(age, "Idade não especificada" = "Unknown"),
                age = dplyr::recode(age, "No Age Specified" = "Unknown"),
                age = dplyr::recode(age, "Não especificada" = "Unknown"),
                age = tidyr::replace_na(age, "Unknown"),
                
                sex = dplyr::recode(sex, "UNKNOWN" = "Unknown"),
                sex = dplyr::recode(sex, "Not Specified" = "Unknown"),
                sex = dplyr::recode(sex, "Não especificado" = "Unknown"),
                sex = dplyr::recode(sex, "F" = "Female"),
                sex = dplyr::recode(sex, "M" = "Male"),
                sex = tidyr::replace_na(sex, "Unknown"))

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

rm(df, df_1, df_2, df_3, xAge, xLW, xPW, xSex)

#---- DEFINE PATH AND SURVEY ALL MONTHLY TPT DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------

historic_files <- dir({historic_files_path}, pattern = "*.tsv")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

#---- ROW BIND ALL MONTHS -----------------------

disa_vl <- historic_files %>%
  map(~ read_tsv(file.path(compile_path, .))) %>% 
  reduce(rbind)

#---- SUBSET VLS DATASET AND MAKE INDEPENDENT INDICATOR -------------

disa_vls <- disa_vl %>% 
  dplyr::filter(result == "<1000") %>% 
  dplyr::mutate(indicator = "VLS")

#---- UNION VL & VLS DATAFRAMES, PIVOT WIDER AND GROUP ----------------

disa <- dplyr::bind_rows(disa_vl, disa_vls) %>% 
  dplyr::mutate(row = row_number()) %>% 
  tidyr::pivot_wider(names_from = indicator, values_from = value, values_fill = NULL) %>% 
  dplyr::group_by(month, province, district, site, age, group, sex, motive) %>%
  summarise(VL = sum(VL, na.rm = T),
            VLS = sum(VLS, na.rm = T)) %>%
  ungroup()

#------ WRITE FILE TO DISK -------------------------------------------

readr::write_tsv(
  disa,
  {final_output},
  na ="")





