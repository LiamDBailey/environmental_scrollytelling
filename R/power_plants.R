# Set up ------------------------------------------------------------------
library(tidyverse)
library(hrbrthemes)
library(treemap)

theme_set(theme_ipsum())

# Data cleaning & wrangling -----------------------------------------------
# power_raw <- readr::read_csv("https://raw.githubusercontent.com/wri/global-power-plant-database/master/output_database/global_power_plant_database.csv")

# Save data for when there is no internet connection
# readr::write_csv(power_raw, "data/power_plants.csv")

power_raw <- readr::read_csv("data/power_plants.csv")

# Filter for germany, select variables without too many missing values
power <- power_raw %>% 
  filter(country_long == "Germany") %>% 
  select(-contains("country"), -contains("other_fuel"), -url, -gppd_idnr, -source, -geolocation_source, -wepp_id,
         -year_of_capacity_data, -matches("generation_gwh_[0-9]"), -generation_data_source) %>% 
  mutate(name = str_remove(name, "(power station)|KW|Kraftwerk"))

# by fuel type
power_by_fuel <- power %>%
  mutate(total_generation = sum(estimated_generation_gwh)) %>% 
  group_by(primary_fuel) %>%
  summarise(generation = sum(estimated_generation_gwh),
            pct_generation = generation / mean(total_generation))
# check
power_by_fuel

# Map ---------------------------------------------------------------------
# Load map data
library(sf)
library(maptools)
library(rnaturalearth)

# better quality (scale = 10) can't be downloaded
europe <- ne_countries(scale = 50, returnclass = "sf", continent = "europe")

# map with state (bundeslaender) boundaries
# rnaturalearth::ne_states(country = "Germany")

# Restrict df to coal
coal <- power %>% 
  filter(primary_fuel == "Coal")

ggplot(data = europe) +
  geom_sf(fill = "antiquewhite") +
  geom_jitter(data = coal, aes(x = longitude, y = latitude, size = estimated_generation_gwh), alpha = 0.5) +
  labs(title = "Coal power plants in Germany",
       size = "Estimated power generation \n in gigawatt-hours (2014)", 
       caption = "Source: Globl Power Planet Database") +
  scale_color_ipsum() +
  xlim(5, 15) +
  ylim(47.5, 55) +
  # coord_sf(xlim = c(0, 20), ylim = c(40, 60)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue")) 

# Treemap -----------------------------------------------------------------
# tree map of energy mix
# by date commisioned
treemap(power,
        index = c("primary_fuel","name"),
        vSize = "estimated_generation_gwh",
        type = "index",
        palette = "Set3",
        title = "German power generation by source in in 2014") 

# Interactive treemap (doesn't work)
# library(d3treeR)
# devtools::install_github("d3treeR/d3treeR") 
# d3tree2(t,  rootname = "xyz")
# d3tree(t, rootname = "xyz")

# NOT DONE: ---------------------------------------------------------------
# Area chart --------------------------------------------------------------
# by year and fuel
power_by_year_fuel <- power %>% 
  arrange(commissioning_year) %>% 
  group_by(commissioning_year, primary_fuel) %>% 
  summarise(generation_added = sum(estimated_generation_gwh),
            generation_cum = cumsum(generation_added))

# commisioning year is missing for all solar
power_by_year_fuel %>% 
  arrange(primary_fuel, commissioning_year)

power %>% 
  mutate(na_year = is.na(commissioning_year)) %>% 
  count(primary_fuel, na_year)

power_by_year_fuel %>% 
  ggplot(aes(x = commissioning_year, y = generation_cum, fill = primary_fuel)) +
  geom_area()

# Bar chart biggest power plants for each category ------------------------
power %>% 
  group_by(primary_fuel) %>% 
  arrange(primary_fuel, desc(capacity_mw)) %>% 
  top_n(5, wt = 1)

## by decade
# power_by_decade %>% 
# power %>%
#   mutate(decade = floor(commissioning_year / 10) * 10,
#          decade = case_when(decade < 1950 ~ "Before 1950",
#                             TRUE ~ as.character(decade))) %>% 
#   filter(primary_fuel != "Solar") %>% 
#   # skimr::skim()
#   count(decade)
#   # group_by()


# Ownership ---------------------------------------------------------------
power %>% 
  filter(primary_fuel != "Solar") %>% 
  count(owner, sort = TRUE)


# NOT WORKING: ------------------------------------------------------------
# Waffle chart with pictograms (fontawesome) ------------------------------
# map
# waffle chart of 1306 power plants by type of primary_fuel
# latest version of waffle is not availabe on CRAN
# devtools::install_github("hrbrmstr/waffle")
library(waffle)
# waffle chart
power %>% 
  mutate(primary_fuel = case_when(primary_fuel %in% c("Biomass", "Oil", "Waste") ~ "Other",
                                  TRUE ~ primary_fuel)) %>% 
  count(primary_fuel) %>% 
  ggplot(aes(fill = primary_fuel, values = n)) +
  geom_waffle(n_rows = 50, size = 0.33, colour = "white", flip = TRUE) +
  coord_equal() +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle()

# pictogram chart
power %>% 
  mutate(primary_fuel = case_when(primary_fuel %in% c("Biomass", "Oil", "Waste") ~ "Other",
                                  TRUE ~ primary_fuel)) %>% 
  count(primary_fuel) %>% 
  ggplot(aes(label = primary_fuel, values = n)) +
  geom_pictogram(n_rows = 50, size = 0.33, flip = TRUE) +
  # scale_label_pictogram(
  #   name = NULL,
  #   # values = c("industry", "burn", "water", "radiation", "square", "sun",  "wind-turbine"),
  #   # labels = c("Coal", "Gas", "Hydro", "Nuclear", "Other", "Solar", "Wind")
  # )  +
  coord_equal()
# theme_ipsum_rc(grid = "") +
# theme_enhance_waffle() +
# theme(legend.key.height = unit(2.25, "line")) +
# theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75))


# ("industry", "burn", "water", "radiation", "square", "sun",  "wind-turbine" )

# scale_color_manual(
#   name = NULL,
#   values = c("#a40000", "#c68958", "#ae6056"),
#   labels = c("Fruit", "Sammich", "Pizza")
# ) +

# Interactive Treemap -----------------------------------------------------
# Treemap -----------------------------------------------------------------
# tree map of energy mix
# by date commisioned
treemap(power,
        index = c("primary_fuel","name"),
        vSize = "estimated_generation_gwh",
        type = "index",
        palette = "Set3",
        title = "German power generation by source in in 2014") 

# Interactive treemap (doesn't work)
# library(d3treeR)
# devtools::install_github("d3treeR/d3treeR") 
# d3tree2(t,  rootname = "xyz")
# d3tree(t, rootname = "xyz")

