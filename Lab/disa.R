#------LOAD CORE TIDYVERSE & OTHER PACKAGES-------------------------------------------
##

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

file_monthly <- "data_source/monthly/Relatorio Mensal de Carga Viral (Abril).xlsx"

#---- LOAD DATASETS AND UNION -------------------------------------------------------

xAge <- read_excel({file_monthly}, 
                     sheet = "S. Viral (Idade)", skip = 2) %>% 
  dplyr::mutate(group = "age") %>%
  dplyr::select(-c(`SISMA ID`)) %>% 
  glimpse()

xSex <- read_excel({file_monthly}, 
                     sheet = "S. Viral (Genero)", skip = 2) %>% 
  dplyr::mutate(group = "sex") %>% 
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

#---- RENAME VARIABLES -------------------------------------------------------

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
  dplyr::mutate(result = dplyr::recode(result, "<1000" = "suppressed", ">1000" = "non_suppressed")) %>%
  dplyr::mutate(row = row_number()) %>% 
  tidyr::pivot_wider(names_from = result, values_from = value, values_fill = NULL) %>%
  dplyr::mutate(suppressed = replace_na(suppressed, 0),
                non_suppressed = replace_na(non_suppressed, 0),
                total = suppressed + non_suppressed) %>% 
  glimpse
