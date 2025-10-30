# load packages
library("dplyr")
library("readr")
library("ggplot2")

# load data
penguins = read_csv("data/penguins.csv")

# pipes and looking at a portion of the data |> is the same as %>% (control + shift + m)
penguins |>  # interpreted as one line of code because of the pipe
  subset(island == "Biscoe")
# pipe version is the same as
subset(penguins, island == "Biscoe")

# single equals sign creates a new variable named island and sets it equal to the string. While the subset as the question is the island equal to Biscoe? then subsets those values
island = "Biscoe"

# remove NAs
penguins_clean = subset(penguins, !is.na(bill_length_mm) & !is.na(body_mass_g))

# create a plot in ggplot of beak length vs body mass, colored by species
# workshop chapter on ggplot 
ggplot(penguins_clean) + # provides the data layer
  aes(x = bill_length_mm, y = body_mass_g, color = species) + # maps the aesthetics to the data
  geom_point() # what is going to get drawn on the plot


