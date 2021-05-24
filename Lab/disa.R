
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

#---- DEFINE PATHS AND VALUES THAT REQUIRE UPDATING EACH MONTH! -------------------------------------------------------

file_monthly <- "data_source/monthly/Relatorio Mensal de Carga Viral (Abril).xlsx"
month <- "2021-04-20"
month_output <- "C:/Users/jlara/Documents/GitHub/Lab/Lab/data_source/monthly_processsed/2021_04.tsv"

#---- DEFINE PATHS AND VALUES THE DO NOT REQUIRE UPDATING -------------------------------------------------------

final_output <- "C:/Users/jlara/Documents/GitHub/Lab/Lab/output/disa.tsv"
final_ajuda_output <- "C:/Users/jlara/Documents/GitHub/Lab/Lab/output/disa_ajuda.tsv"
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

df_vl <- dplyr::bind_rows(xAge, xSex, xPW, xLW)
rm(xAge, xSex, xPW, xLW)

df_tat <- read_excel("data_source/monthly/Relatorio Mensal de Carga Viral (Abril).xlsx", 
                                                     sheet = "TRL", skip = 2)

ajuda_site_map <- read_excel("~/GitHub/AjudaSiteMap/AJUDA Site Map.xlsx")
disa_site_map <- read_excel("~/GitHub/_GeneralJoins/DISA Datim Mapping.xlsx") %>% 
  dplyr::select(Datim_ID, disa_site)

#---- PROCESS VL DATAFRAME -------------------------------------------------------

df_vl_1 <- df_vl %>% 
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
                                          grepl(">1000", indicator) ~ ">1000"),
                tat_step = "temp") %>% 
  dplyr::select(-c(indicator)) %>% 
  glimpse()

#---- RECODE VL AGE/SEX VALUES -----------------------------------------------

df_vl_2 <- df_vl_1 %>% 
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

#---- FILTER VL LINES ONLY >0 -----------------------------------------------

df_vl_3 <- df_vl_2 %>% 
  dplyr::filter(value > 0) %>% 
  dplyr::mutate(indicator = "VL",
                month = {month})

#---- PROCESS TAT DATAFRAME -----------------------------------------------

df_tat_1 <- df_tat %>% 
  dplyr::select(9:15) %>%
  dplyr::rename(province = PROVINCIA,
                district = DISTRITO,
                site = US) %>% 
  tidyr::pivot_longer((`COLHEITA À RECEPÇÃO...12`:`ANÁLISE À VALIDAÇÃO...15`), names_to = "tat_step", values_to = "value") %>% 
  dplyr::mutate(tat_step = dplyr::recode(tat_step, 
                                         "COLHEITA À RECEPÇÃO...12" = "S1: Collection to Receipt",
                                         "RECEPÇÃO AO REGISTO...13" = "S2: Receipt to Registration",
                                         "REGISTO À ANÁLISE...14" = "S3: Registration to Analysis",
                                         "ANÁLISE À VALIDAÇÃO...15" = "S4: Analysis to Validation"),
                indicator = "TAT",
                month = {month})

df_final <- dplyr::bind_rows(df_vl_3, df_tat_1)


#------ WRITE FILE TO DISK -------------------------------------------

readr::write_tsv(
  df_final,
  {month_output},
  na ="")

rm(df_final, df_tat, df_tat_1, df_vl, df_vl_1, df_vl_2, df_vl_3)

#---- DEFINE PATH AND SURVEY ALL MONTHLY TPT DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------

historic_files <- dir({historic_files_path}, pattern = "*.tsv")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

#---- ROW BIND ALL MONTHS -----------------------

disa_vl <- historic_files %>%
  map(~ read_tsv(file.path(compile_path, .))) %>% 
  reduce(rbind) %>% 
  glimpse()

#---- SUBSET VLS DATASET AND MAKE INDEPENDENT INDICATOR -------------

disa_vls <- disa_vl %>% 
  dplyr::filter(result == "<1000") %>% 
  dplyr::mutate(indicator = "VLS")

#---- UNION VL & VLS DATAFRAMES, PIVOT WIDER AND GROUP ----------------

disa <- dplyr::bind_rows(disa_vl, disa_vls) %>% 
  dplyr::mutate(row = row_number(),
                tat_step = na_if(tat_step, "temp")) %>% 
  tidyr::pivot_wider(names_from = indicator, values_from = value, values_fill = NULL) %>% 
  dplyr::group_by(month, province, district, site, age, group, sex, motive, tat_step) %>%
  summarise(VL = sum(VL, na.rm = T),
            VLS = sum(VLS, na.rm = T),
            TAT = sum(TAT, na.rm = T)) %>%
  ungroup() %>% 
  glimpse()

#---- CREATE AJUDA SUBSET OF DISA DATA -------------------------------

disa_ajuda <- disa %>% 
  dplyr::left_join(disa_site_map, c("site" = "disa_site")) %>% 
  dplyr::left_join(ajuda_site_map, c("Datim_ID" = "orgunituid")) %>% 
  tidyr::drop_na(Datim_ID) %>% 
  dplyr::select(-c(SNU,
                   Psnu,
                   Sitename)) %>% 
  dplyr::rename(orgunituid = Datim_ID,
                partner = `IP FY20`,
                lat = Lat,
                long = Long) %>% 
  dplyr::relocate(c(orgunituid, sisma_id, lat, long, partner), .after = site)
  
#------ WRITE FILE TO DISK -------------------------------------------

readr::write_tsv(
  disa,
  {final_output},
  na ="")

readr::write_tsv(
  disa_ajuda,
  {final_ajuda_output},
  na ="")








