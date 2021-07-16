
#------LOAD CORE TIDYVERSE & OTHER PACKAGES-------------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(readxl)
library(janitor)
library(glue)


rm(list = ls())

#---- DEFINE PATHS AND VALUES THAT REQUIRE UPDATING EACH MONTH! -------------------------------------------------------

file_monthly <- "Data/monthly/HIV VL May 2021 Data.xlsx"
month <- "2021-05-20"
month_output <- "Data/monthly_processsed/2021_05.tsv"

#---- DEFINE PATHS AND VALUES THE DO NOT REQUIRE UPDATING -------------------------------------------------------

historic_files_path <- "Data/monthly_processsed/"  # PATH USED TO CREATE A LIST OF ALL .CSV FILES PREVIOUSLY CREATED
compile_path <- "Data/monthly_processsed/"
final_output <- "Dataout/disa.tsv"
final_ajuda_output <- "Dataout/disa_ajuda.tsv"
final_misau_output <- "Dataout/disa_misau.tsv"


#---- LOAD DATASETS AND UNION -------------------------------------------------------

xAge <- read_excel({file_monthly},
                   sheet = "S. Viral (Idade)", 
                   col_types = c("text", "text", "text", 
                                 "text", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"), 
                   skip = 2) %>% 
  dplyr::mutate(group = "Age") %>%
  glimpse()

xSex <- read_excel({file_monthly}, 
                   sheet = "S. Viral (Genero)",
                   col_types = c("text", "text", "text", 
                                 "text", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"), 
                   skip = 2) %>% 
  dplyr::mutate(group = "Sex") %>% 
  glimpse

xPW <- read_excel({file_monthly}, 
                  sheet = "S. Viral (M. Gravidas)",
                  col_types = c("text", 
                                "text", "text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"), 
                  skip = 2) %>% 
  dplyr::mutate(group = "PW") %>% 
  dplyr::rename(US = HF,
                PROVINCIA = PROVINCE,
                DISTRITO = DISTRICT) %>% 
  glimpse

xLW <- read_excel({file_monthly}, 
                  sheet = "S. Viral (M. Lactantes)",
                  col_types = c("text", 
                                "text", "text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"),
                  skip = 2) %>% 
  dplyr::mutate(group = "LW") %>%
  dplyr::rename(US = HF,
                PROVINCIA = PROVINCE,
                DISTRITO = DISTRICT) %>% 
  glimpse

df_tat <- read_excel({file_monthly}, 
                      sheet = "TRL", 
                      col_types = c("text", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "text", 
                                    "text", "text", "text", "numeric",
                                    "numeric", "numeric", "numeric", "numeric"), 
                      skip = 2)

df_vl <- dplyr::bind_rows(xAge, xSex, xPW, xLW)

rm(xAge, xSex, xPW, xLW)

ajuda_site_map <- read_excel("~/GitHub/AJUDA_Site_Map/AJUDA Site Map.xlsx")
disa_site_map <- read_excel("~/GitHub/_GeneralJoins/DISA Datim Mapping.xlsx")

#---- PROCESS VL DATAFRAME -------------------------------------------------------

df_vl_1 <- df_vl %>% 
  dplyr::select(-c(`CV < 1000`, `CV > 1000`, TOTAL)) %>%
  dplyr::rename(sisma_id = `SISMA ID`,
                province = PROVINCIA,
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
  dplyr::select(8:15) %>%
  dplyr::rename(sisma_id = `SISMA ID`,
                province = PROVINCIA,
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


#------ WRITE MONTHLY FILE TO DISK -------------------------------------------

readr::write_tsv(
  df_final,
  {month_output},
  na ="")

rm(df_final, df_tat, df_tat_1, df_vl, df_vl_1, df_vl_2, df_vl_3)


#---- DEFINE PATH AND SURVEY ALL MONTHLY TPT DATASETS THAT NEED TO BE COMBINED TO CREATE HISTORIC DATASET ---------------------------------

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
  dplyr::group_by(month, province, district, site, sisma_id, age, group, sex, motive, tat_step) %>%
  summarise(VL = sum(VL, na.rm = T),
            VLS = sum(VLS, na.rm = T),
            TAT = sum(TAT, na.rm = T)) %>%
  ungroup() %>% 
  glimpse()

#---- CREATE AJUDA SUBSET OF DISA DATA -------------------------------

disa_ajuda <- disa %>% 
  dplyr::left_join(disa_site_map, c("site" = "disa_site")) %>% 
  dplyr::left_join(ajuda_site_map, c("orgunituid" = "orgunituid")) %>% 
  tidyr::drop_na(orgunituid) %>% 
  dplyr::select(-c(SNU,
                   Psnu,
                   Sitename,
                   sisma_id.x,
                   sisma_id.y)) %>% 
  dplyr::rename(partner = `IP FY20`,
                lat = Lat,
                long = Long) %>% 
  dplyr::relocate(c(orgunituid, lat, long, partner), .after = site)

#---- CREATE MISAU DISA DATA -------------------------------

disa_misau <- disa %>%
  dplyr::rename(periodo = month,
                provincia = province,
                distrito = district,
                us = site,
                idade = age,
                grupo = group,
                sexo = sex,
                motivo = motive,
                trl_etapa = tat_step,
                CV = VL,
                CVS = VLS,
                TRL = TAT) %>% 
  dplyr::mutate(grupo = dplyr::recode(grupo, "Age" = "Idade"),
                grupo = dplyr::recode(grupo, "Sex" = "Sexo"),
                grupo = dplyr::recode(grupo, "PW" = "MG"),
                grupo = dplyr::recode(grupo, "LW" = "ML"),
                
                sexo = dplyr::recode(sexo, "Male" = "Masculino"),
                sexo = dplyr::recode(sexo, "Female" = "Feminino"),
                sexo = dplyr::recode(sexo, "Unknown" = "Desconhecido"),
                
                motivo = dplyr::recode(motivo, "Routine" = "Rotineiro"),
                motivo = dplyr::recode(motivo, "Theraputic Failure" = "Falência Terapêutica"),
                motivo = dplyr::recode(motivo, "Post Breastfeeding" = "Pós-amamentação"),
                motivo = dplyr::recode(motivo, "Not Specified" = "Desconhecido"),
                
                trl_etapa = dplyr::recode(trl_etapa, "S1: Collection to Receipt" = "E1: Colheita a Chegada"),
                trl_etapa = dplyr::recode(trl_etapa, "S2: Receipt to Registration" = "E2: Chegada a Registo"),
                trl_etapa = dplyr::recode(trl_etapa, "S3: Registration to Analysis" = "E3: Registo a Analise"),
                trl_etapa = dplyr::recode(trl_etapa, "S4: Analysis to Validation" = "E4: Analise a Validacao"))
                                 
rm(ajuda_site_map, disa_site_map, disa_vl, disa_vls)

#------ WRITE FILE TO DISK -------------------------------------------

readr::write_tsv(
  disa,
  {final_output},
  na ="")

readr::write_tsv(
  disa_ajuda,
  {final_ajuda_output},
  na ="")

readr::write_tsv(
  disa_misau,
  {final_misau_output},
  na ="")








