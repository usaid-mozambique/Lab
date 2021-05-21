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

#---- LOAD DATASET  -------------------------------------------------------

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

