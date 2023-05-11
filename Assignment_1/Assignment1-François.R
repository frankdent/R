#setwd(choose.dir())
getwd()
# load packages
library(tidyverse)
library(sf)
library(tmap)
library(sp)

# load world data using csv file
world_data <- read_csv("world_data.csv")
world_data

# subset the dataset so that only countries are located in Eastern Europe,
#male life expectancy is greater than 68 years, and negative population growth
world_data_subset <- select(
  filter(world_data, Region == "East Europe", 
         `Life Expectancy (Male)` > 68, 
         `Population Increase (% per year)` < 0),
  Country, Region, `Life Expectancy (Male)`, `Population Increase (% per year)`
)

View(world_data_subset)

# now using piping syntax

world_data_subset_piping <- world_data %>% select(Country, Region, `Life Expectancy (Male)`, `Population Increase (% per year)`) %>% 
                        filter(Region == "East Europe", `Life Expectancy (Male)` > 68, 
                      `Population Increase (% per year)` < 0)  

View(world_data_subset_piping)
str(world_data_subset_piping)

# now calculating the male to female life expectancy ratio and output a grouped summary of the average male to
# female life expectancy at the regional scale (i.e, using the Region variable).

world_data_m2fer <- world_data %>% group_by(Region) %>% summarise(
  `Male to Female Life Expectancy Ratio` = mean(`Life Expectancy (Male)` / `Life Expectancy (Female)`)) 

View(world_data_m2fer)

world <- st_read("world.shp")
world_data <- read_csv("world_data.csv")

# checking if the columns are identical to prevent errors
world$CNTRY_NAME == world_data$Country

# We must give the columns the same name before we join them
names(world)[1] = "Country"
names(world)

class(world_data) #it's a tibble so we can do dplyr on it and tidy it etc
class(world) #we have to use the sf package, we can't really modify or use functions on the geo column
# world_data is a tibble and world is a data.frame

# let's join these two data sets together using an inner join
world_join <- inner_join(world_data, world)

# the resulting table is a Tibble
class(world_join)

world_join_filter <- filter(world_join, `Literacy Rate for Males (%)` != 0, `Literacy Rate for Females (%)` != 0,
                            `Daily Calorie Intake` != 0)
View(world_join_filter)

# Adding a column for Population Density
# This column will store values in persons per square kilometre
world_join_filter$`Population Density` <- world_join_filter$Population / world_join_filter$Area

# Adding a column for Male to Female Literacy Ratio
world_join_filter$`Literacy Ratio` <- world_join_filter$`Literacy Rate for Males (%)` / world_join_filter$`Literacy Rate for Females (%)`

# Let's select the important columns to better visualize the data
View(select(world_join_filter, Country, `Literacy Rate for Males (%)`, `Literacy Rate for Females (%)`, `Literacy Ratio`))

# Showing the top 5 countries in terms of population density
world_join_top5density <- select(head(arrange(world_join_filter, desc(`Population Density`)),5), Country, `Population Density`)
View(world_join_top5density)

# showing the bottom 5 countries in terms of population density
world_join_bot5m2flr <- select(head(arrange(world_join_filter, `Literacy Ratio`),5), Country, `Literacy Ratio`)
View(world_join_bot5m2flr)

class(world_join_filter)

M2F_Literacy_Ratio_Graph <- tm_shape(st_as_sf(world_join_filter)) +
  tm_polygons("Literacy Ratio") + tm_layout("World Map")
M2F_Literacy_Ratio_Graph


Population_Density_Graph <- tm_shape(st_as_sf(world_join_filter)) +
  tm_fill("Population Density", title = "Population Density",
          breaks = c(0, 50, 200, 800, 6000),
          palette = c("grey", "yellow", "orange", "red")) +
  tm_borders() + tm_layout("World Map")
Population_Density_Graph