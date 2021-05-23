
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

file_monthly <- "data_source/monthly/Lab Monthly VL Archive.xlsx"
output <- "C:/Users/jlara/Documents/GitHub/Lab/Lab/data_source/monthly_processsed/2020_2021.tsv"

#---- LOAD DATASETS AND UNION -------------------------------------------------------

xAge <- read_excel({file_monthly}, 
                   sheet = "SV (Idade)") %>% 
  dplyr::mutate(group = "Age") %>%
  glimpse()

xSex <- read_excel({file_monthly}, 
                   sheet = "SV (Genero)") %>% 
  dplyr::mutate(group = "Sex") %>% 
  glimpse

xPW <- read_excel({file_monthly}, 
                  sheet = "SV (M. Gravidas)") %>% 
  dplyr::mutate(group = "PW") %>% 
  glimpse

xLW <- read_excel({file_monthly}, 
                  sheet = "SV (M. Lactantes)") %>% 
  dplyr::mutate(group = "LW") %>%
  glimpse


df <- dplyr::bind_rows(xAge, xSex, xPW, xLW)

#---- MONTHLY RECODE -------------------------------------------------------

df <- df %>%  
  dplyr::mutate(Data = dplyr::recode(Data, 
                                     "January, 2020" = "2020-01-20",
                                     "February, 2020" = "2020-02-20",
                                     "March, 2020" = "2020-03-20",
                                     "April, 2020" = "2020-04-20",
                                     "May, 2020" = "2020-05-20",
                                     "June, 2020" = "2020-06-20",
                                     "July, 2020" = "2020-07-20",
                                     "August, 2020" = "2020-08-20",
                                     "September, 2020" = "2020-09-20",
                                     "October, 2020" = "2020-10-20",
                                     "November, 2020" = "2020-11-20",
                                     "December, 2020" = "2020-12-20",
                                     "January, 2021" = "2021-01-20",
                                     "February, 2021" = "2021-02-20",
                                     "March, 2021" = "2021-03-20",
                                     "April, 2021" = "2021-04-20")
                )

#---- PROCESS DATAFRAME -------------------------------------------------------

df_1 <- df %>% 
  dplyr::select(-c(`CV < 1000`, `CV > 1000`, TOTAL)) %>%
  dplyr::rename(month = Data,
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
  dplyr::mutate(indicator = "VL")

#------ WRITE FILE TO DISK -------------------------------------------

readr::write_tsv(
  df_3,
  {output},
  na ="")
