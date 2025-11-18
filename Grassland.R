# ---- 1. Read data ----

library(dplyr)

# Cleaning enviromental from empty rows and columns
env <- read.csv("Data/environmentaldata.csv", header = TRUE) %>%
  mutate(across(where(is.character), ~ na_if(trimws(.), ""))) %>%
  select(where(~ any(!is.na(.)))) %>%
  filter(if_any(everything(), ~ !is.na(.)))
dim(env)
which(rowSums(!is.na(env)) == 0)
which(colSums(!is.na(env)) == 0)

# cleaning vasplants from empty rows and columns
readr::guess_encoding("Data/speciesdata.csv")
vas.plants <- read.csv(
  "Data/speciesdata.csv",
  header = T,
  sep = ",",
  fileEncoding = "ISO-8859-1"
) %>%
  mutate(across(where(is.character), ~ na_if(trimws(.), ""))) %>%
  select(where(~ any(!is.na(.)))) %>%
  filter(if_any(everything(), ~ !is.na(.)))

dim(vas.plants)
summary(vas.plants)
any(is.na(vas.plants)) #TRUE

# Data is in text format. Convert species to numeric
vas.plants <- vas.plants %>%
  mutate(
    across(
      -c(Site.number, Quadrat, Country, Year),
      as.numeric
    )
  )

#Control whether full na rows and columns are removed.
which(rowSums(!is.na(vas.plants)) == 0) #integer(0)
which(colSums(!is.na(vas.plants)) == 0) #named integer(0)

# ---- 3. Getting to knew the data ----

plot(
  sort(colSums(!is.na(vas.plants)), decreasing = T),
  ylab = "Species richness",
  xlab = "Rank"
)

# ---- 3. Simple map with sample points ----

library(ggplot2)
install.packages("sf") # for countries map, ne_countries
install.packages("rnaturalearth") # state borders
library(rnaturalearth)
install.packages("rnaturalearthdata") # metadata
library(rnaturalearthdata)

europe <- ne_countries(continent = "Europe", returnclass = "sf")

ggplot() +
  geom_sf(data = europe, fill = "grey80", color = "white") +
  geom_point(
    data = env,
    aes(x = Latitude, y = Longitude),
    color = "red",
    size = 1,
    alpha = 0.8,
    inherit.aes = FALSE
  ) +
  coord_sf(xlim = c(-12, 15), ylim = c(44, 61), expand = FALSE) +
  theme_gray() +
  labs(
    title = "Location of sampling sites",
    x = "Longitude (°E)",
    y = "Latitude (°N)"
  )
