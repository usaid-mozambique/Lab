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

#---- LOAD DATASETS AND UNION -------------------------------------------------------

xIdade <- read_excel("data_source/Lab Monthly VL Archive.xlsx", 
                     sheet = "SV (Idade)") %>% 
  dplyr::mutate(analysis_group = "age")

xGenero <- read_excel("data_source/Lab Monthly VL Archive.xlsx", 
                                     sheet = "SV (Genero)") %>% 
  dplyr::mutate(analysis_group = "sex")

xGravidas <- read_excel("data_source/Lab Monthly VL Archive.xlsx", 
                      sheet = "SV (M. Gravidas)") %>% 
  dplyr::mutate(analysis_group = "pw")

xLactantes <- read_excel("data_source/Lab Monthly VL Archive.xlsx", 
                      sheet = "SV (M. Lactantes)") %>% 
  dplyr::mutate(analysis_group = "lw")

TRL <- read_excel("data_source/Lab Monthly VL Archive.xlsx", 
                      sheet = "TRL")

df <- dplyr::bind_rows(xIdade, xGenero, xGravidas, xLactantes)

#---- RENAME VARIABLES -------------------------------------------------------

df_1 <- df %>% 
  dplyr::select(-c(`CV < 1000`, `CV > 1000`, TOTAL)) %>%
  tidyr::pivot_longer(`Rotina (<1000)`:`Motivo de Teste não especificado (>1000)`, names_to = "indicator", values_to = "value")
  glimpse
