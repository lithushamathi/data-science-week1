# Week 1: Initial Data Exploration ====
# Author: [Lithusha Mathiyalaian]
# Date: [30/01/2026]

# Load packages ====
library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)
# Load data ====
mosquito_egg_raw <- read_csv(here("data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview ====
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw)
skim(mosquito_egg_raw)

# React table====
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment====

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())

# Observations ====
# This dataset describes individual female mosquitoes.
# Each row represents one female measured at a given site and date under a treatment.
#
# Measurements include age, body mass, eggs laid (fecundity), and eggs hatched
# (reproductive success), along with site, treatment, and collection information.
#
# There are 205 observations (rows) and 9 variables (columns).
#
# Some issues are apparent:
# body_mass_mg contains biologically impossible negative values.
# Treatment labels are inconsistent (e.g. High_dose, HIGH_DOSE, high_dose).
# Site names are inconsistent (e.g. Site A, Site_A, Site-A).
# Several variables contain missing values, including body mass and egg counts.
# Some site-by-treatment combinations have very small sample sizes.
