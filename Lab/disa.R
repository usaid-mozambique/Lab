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

Lab_Monthly_VL_Archive <- read_excel("~/USAID/00. SI/00. HIV/01. M&E/00. Data/10. Lab/00. VL/Lab Monthly VL Archive.xlsx", 
                                     sheet = "SV (Genero)")