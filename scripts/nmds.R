
library(readxl)
library(purrr)
library(dplyr)

# creating a list of file names (one for each plot)
files <- list.files(
  path = "/Users/jennifercribbs/Documents/SEKI_beetles/Data/UnderstoryListData",
  pattern = "^[^~].*\\.xlsx$",
  full.names = TRUE
)
  
# check class
map(files, ~ class(read_excel(.x)$percentCover))

# read in and combine files
veg <- map_dfr(files, \(f) {
  
  dat <- read_excel(f)
  
  dat %>%
    dplyr::select (plotID, species, percentCover) %>% 
    mutate(
      percentCover = as.character(percentCover)
    )
  
})

#unique_vals <- sort(unique(unlist(veg)))

#unique_vals
#sort(unique(unlist(vals)))

# look at unique values
#sort(unique(veg_values$percentCover))

# recode non-numeric entries 
veg <- veg %>% mutate(
  percentCover = case_when(
    percentCover == "<1" ~ "0.5",
    percentCover == "<2" ~ "1",
    percentCover == "<3" ~ "1.5",
    percentCover == "<5" ~ "2.5",
    percentCover == ">1" ~ "0.5",  # investigate later
    TRUE ~ percentCover
  ), 
  percentCover = as.numeric(percentCover)
)


# plots with duplicate entries PIMO8a (7) and PIMO6a (1)
# Ultimately need to fix this issue not remove

# check that each plot has only one entry per species
veg %>%
  count(plotID, species) %>%
  filter(n > 1) #PIMO8a and PIMO6a have 2-3 entries per species
# remove plots PIMO8a and PIMO6a for now
veg_clean <- veg %>% dplyr::filter(plotID != "PIMO8a", plotID != "PIMO6a_SummitLake")

# remove NAs for NMDS
veg_clean <- veg_clean %>% 
  filter(!is.na(percentCover))
# collapse duplicates
veg_clean <- veg_clean %>%
  group_by(plotID, species) %>%
  summarise(percentCover = max(percentCover), .groups = "drop")

# build species matrix
comm <- veg_clean %>%
  tidyr::pivot_wider(
    names_from = species,
    values_from = percentCover,
    values_fill = 0
  )


# NMDS
library(vegan)
# converting plotID column to row name
comm_mat <- comm %>%
  tibble::column_to_rownames("plotID")

nmds <- metaMDS(comm_mat, distance = "bray", k = 2, plot = TRUE)

plot(nmds)

ordiplot(nmds, type = "text", display = "sites")
ordiplot(nmds, type = "text", display = "species")

# run after creating environmental data matrix
ef <- envfit(nmds, env_data, permutations = 999)
plot(ef)
