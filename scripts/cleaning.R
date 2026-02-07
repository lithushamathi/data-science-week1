library(tidyverse)
str_trim(" Adelie Penguin (Pygoscelis adeliae) ")
penguins_clean_names <- readRDS(url("https://github.com/UEABIO/5023B/raw/refs/heads/2026/files/penguins.RDS"))
str_squish("  Adelie    Penguin   (Pygoscelis   adeliae)  ")
str_trunc("Adelie Penguin (Pygoscelis adeliae)", width = 18, side = "right")
str_split("Adelie Penguin (Pygoscelis adeliae)", " ")
str_c("Adelie", "Penguin", sep = "_")
# Print only unique character strings in this variable
penguins_clean_names |>  
  distinct(sex)

# use mutate and case_when 
# for a statement that conditionally changes 
# the names of the values in a variable
penguins_clean_names |> 
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
    .default = as.character(species)
  )
  )

# rename columns
penguins_clean_names <- penguins_clean_names %>%
  rename(
    delta_15n = delta_15_n_o_oo,
    delta_13c = delta_13_c_o_oo
  )

# remove match for Genus (followed by a whitespace)
str_remove("Genus specificus", pattern = "Genus ")

penguins_clean_names |> 
  separate(
    species,
    into = c("species", "full_latin_name"),
    sep = "(?=\\()" # regex pattern: split before the '('
  ) |> 
  mutate(full_latin_name = str_remove_all(full_latin_name, "[\\(\\)]"))

library(tidyverse)
# check for whole duplicate 
# rows in the data
penguins_clean_names |> 
  filter(duplicated(across(everything())))
sum() 

penguins_demo <- penguins_clean_names |> 
  slice(1:50) |> 
  bind_rows(slice(penguins_clean_names, c(1,5,10,15,30)))

# Keep only unduplicated data with !
penguins_demo |> 
  filter(!duplicated(across(everything())))

penguins_clean_names |> 
  summarise(
    n = n(),
    n_distinct(individual_id)
  )


library(tidyverse)
penguins_clean_names |> 
  group_by(species) |> 
  summarise(mean = mean(body_mass_g))

summary(penguins_clean_names)

penguins_clean_names |> 
  filter(if_any(everything(), is.na)) |>
  select(culmen_length_mm, culmen_depth_mm, flipper_length_mm, 
         sex, delta_15n, delta_13c,comments,
         everything()) # reorder columns

penguins_clean_names |> 
  filter(if_any(culmen_length_mm, is.na))  # reorder columns

penguins_clean_names |> 
  drop_na()

penguins_clean_names |> 
  group_by(species) |> 
  summarise(
    mean_body_mass = mean(body_mass_g, na.rm = T)
  )

library(lubridate)
# library(tidyverse)

date("2017-10-11T14:02:00")
dmy("11 October 2020")
mdy("10/11/2020")

df <- tibble(
  date = c("X2020.01.22",
           "X2020.01.22",
           "X2020.01.22",
           "X2020.01.22")
)

df |> 
  mutate(
    date = as_date(date)
  )

df |> 
  mutate(
    date = as_date(date, format = "X%Y.%m.%d")
  )

year("2017-11-28T14:02:00")
month("2017-11-28T14:02:00")
week("2017-11-28T14:02:00")
day("2017-11-28T14:02:00")
library(janitor)

excel_numeric_to_date(42370)

penguins_clean_names |> 
  summarise(min_date=min(date_egg),
            max_date=max(date_egg))
penguins_clean_names <- penguins_clean_names |> 
  mutate(year = lubridate::year(date_egg))

# return records after 2008
penguins_clean_names |>
  filter(date_egg >= ymd("2008-01-01"))

# week7
# Check ranges of all numeric variables at once
penguins_clean_names |> 
  summarise(across(where(is.numeric), 
                   list(min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE))))
# Check body mass range
penguins_clean_names |> 
  summarise(
    min_mass = min(body_mass_g, na.rm = TRUE),
    max_mass = max(body_mass_g, na.rm = TRUE)
  )
# Check for negative values (impossible for mass, length measurements)
penguins_clean_names |> 
  filter(if_any(c(body_mass_g, flipper_length_mm, 
                  culmen_length_mm, culmen_depth_mm), 
                ~ . < 0))
# Check for zero or negative values where zero doesn't make biological sense
penguins_clean_names |> 
  filter(body_mass_g <= 0)

# Body mass ranges by species
penguins_clean_names |> 
  group_by(species) |> 
  summarise(
    min_mass = min(body_mass_g, na.rm = TRUE),
    max_mass = max(body_mass_g, na.rm = TRUE),
    mean_mass = mean(body_mass_g, na.rm = TRUE)
  )
# Find Adelie penguins with Gentoo-sized body mass
penguins_clean_names |> 
  filter(species == "Adelie Penguin (Pygoscelis adeliae)", body_mass_g > 4750)

penguins_clean_names |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(
    x = "",
    y = "Body mass (g)"
  ) +
  theme_minimal()+
  coord_flip()

#| label: fig-mass-flipper
#| fig-cap: "Body mass should generally increase with flipper length within species. Points far from the trend may indicate measurement errors."

penguins_clean_names |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "Flipper length (mm)",
    y = "Body mass (g)",
  ) +
  theme_minimal()
# Find penguins with large flippers but low body mass
penguins_clean_names |> 
  filter(flipper_length_mm > 210, body_mass_g < 3500) |> 
  select(species, sex, flipper_length_mm, body_mass_g, island)

# Find penguins with small flippers but high body mass
penguins_clean_names |> 
  filter(flipper_length_mm < 185, body_mass_g > 4500) |> 
  select(species, sex, flipper_length_mm, body_mass_g, island)
# Males cannot lay eggs
penguins_clean_names |> 
  filter(sex == "MALE", !is.na(date_egg)) |> 
  select(species, sex, date_egg, body_mass_g, island)
# Check which species appear on which islands
penguins_clean_names |> 
  count(species, island) |> 
  pivot_wider(names_from = island, values_from = n, values_fill = 0)
# Create flags for different types of potential issues
penguins_flagged <- penguins_clean_names |> 
  mutate(
    # Single-variable flags
    flag_impossible = case_when(
      body_mass_g <= 0 ~ "negative_or_zero_mass",
      flipper_length_mm <= 0 ~ "negative_or_zero_flipper",
      TRUE ~ NA_character_
    ),
    flag_implausible = case_when(
      body_mass_g < 2000 ~ "suspiciously_light",
      body_mass_g > 7000 ~ "suspiciously_heavy",
      TRUE ~ NA_character_
    ),
    
    # Cross-variable flags
    flag_species_size = case_when(
      species == "Adelie" & body_mass_g > 5000 ~ "Adelie_too_heavy",
      species == "Gentoo" & body_mass_g < 4000 ~ "Gentoo_too_light",
      TRUE ~ NA_character_
    ),
    # Any flag present?
    any_flag = !is.na(flag_impossible) | !is.na(flag_implausible) | 
      !is.na(flag_species_size) 
  )

# Summarize flagged observations
penguins_flagged |> 
  summarise(
    n_impossible = sum(!is.na(flag_impossible)),
    n_implausible = sum(!is.na(flag_implausible)),
    n_species_size = sum(!is.na(flag_species_size)),
    total_flagged = sum(any_flag)
  )




# Week 2: Cleaning fixes ====

# IMPORTANT: Pull before starting (Git pane -> Pull)

# FIX 1: Standardise site and treatment labels ====

# Show the problem:
mosquito_egg_raw |>
  count(site, sort = TRUE)

mosquito_egg_raw |>
  count(treatment, sort = TRUE)

# Fix it:
mosquito_egg_data_step1 <- mosquito_egg_raw |>
  mutate(
    # Standardise site: replace "_" and "-" with space, trim, consistent case
    site = site |>
      str_replace_all("_", " ") |>
      str_replace_all("-", " ") |>
      str_squish() |>
      str_to_title(),
    
    # Standardise treatment: lower case, trim, convert to consistent style
    treatment = treatment |>
      str_squish() |>
      str_to_lower()
  ) |>
  mutate(
    # Optional: make treatment labels consistent (control/low_dose/medium_dose/high_dose)
    treatment = case_when(
      str_detect(treatment, "control") ~ "control",
      str_detect(treatment, "low") ~ "low_dose",
      str_detect(treatment, "medium") ~ "medium_dose",
      str_detect(treatment, "high") ~ "high_dose",
      TRUE ~ treatment
    )
  )

# Verify it worked:
mosquito_egg_data_step1 |>
  count(site, sort = TRUE)

mosquito_egg_data_step1 |>
  count(treatment, sort = TRUE)

mosquito_egg_data_step1 |>
  count(site, treatment, sort = TRUE)

# What changed and why it matters:
# I standardised site and treatment labels so the same groups are not split into
# multiple categories (e.g. "Site_A" vs "Site A", "HIGH_DOSE" vs "high_dose").
# This matters because grouping, summaries, and models would otherwise treat identical
# treatments/sites as different levels and give misleading results.


# FIX 2: Fix impossible body mass values (negative values) ====

# Show the problem:
summary(mosquito_egg_data_step1$body_mass_mg)
mosquito_egg_data_step1 |>
  filter(body_mass_mg < 0) |>
  select(female_id, body_mass_mg, site, treatment, collection_date) |>
  head()

# Fix it:
mosquito_egg_data_step2 <- mosquito_egg_data_step1

