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

df_1 <- df %>% 
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

df_2 <- df_1 %>% 
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
  dplyr::mutate(result = dplyr::recode(result, "<1000" = "suppressed", ">1000" = "non_suppressed")) %>%
  dplyr::mutate(row = row_number()) %>% 
  tidyr::pivot_wider(names_from = result, values_from = value, values_fill = NULL) %>% 
  glimpse()

#---- GROUP DATA -------------------------------------------------------

df_3 <- df_2 %>% 
  group_by(month, province, district, site, age, group, sex, motive) %>%
  summarize(suppressed = sum(suppressed, na.rm = TRUE),
            non_suppressed = sum(non_suppressed, na.rm = TRUE)) %>%
  ungroup()

#---- CALCULATE TOTAL VL TESTS ------------------------------------------
  
df_4 <- df_3 %>%
  dplyr::mutate(suppressed = replace_na(suppressed, 0),
                non_suppressed = replace_na(non_suppressed, 0),
                total = suppressed + non_suppressed) %>% 
  glimpse

#---- LAST RECODE -------------------------------------------------------

df_5 <- df_4 %>% 
  dplyr::mutate(age = dplyr::na_if(age, "Idade não especificada"),
                age = dplyr::na_if(age, "No Age Specified"),
                age = dplyr::na_if(age, "Não especificada"),
                
                sex = dplyr::na_if(sex, "UNKNOWN"),
                sex = dplyr::na_if(sex, "Not Specified"),
                sex = dplyr::na_if(sex, "Não especificado"))

#---- FILTER LINES ONLY >0 -----------------------------------------------

df_6 <- df_5 %>% 
  dplyr::filter(total > 0)

#------ WRITE FILE TO DISK -------------------------------------------

readr::write_tsv(
  df_6,
  {output},
  na ="")
