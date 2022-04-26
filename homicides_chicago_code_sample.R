library(tidyverse)
library(sf)
library(cowplot)
library(shiny)
library(tidyr)
library(ggsn)

# The idea of this exercise is to compare educational outcomes to several other population metrics in the city of Chicago. 
# I will first extract homicides from the crimes data frame and perform a spatial join using the community areas shape file to 
# determine the rate of homicides in each area. I will compare this measure visually to the rate of college 
# educated adults, median income, unemployment rate and school performance to observe tendencies. To measure school performance 
# by community area, I will join the schools progress report to the schools shape file, perform a further spatial join of that 
# data frame with the boundaries data frame, and calculate average student attainment rating per community area. I will also overlay 
# maps with the locations of homicides and the top schools to observe a more granular pattern.


## Data in github repository

## Load all Files

setwd("")

#load data files
crimes <- read_csv("Crimes_-_One_year_prior_to_present.csv")
schools_progress <- read_csv("Chicago_Public_Schools_-_School_Progress_Reports_SY1819.csv")
population <- read_csv("ReferenceCCAProfiles20152019.csv")

#load shapefiles
schools_shape <- st_read("geo_export_8777e0e0-1fb5-4999-b483-21b9d56ab6d4.shp")
boundaries_shape <- st_read("geo_export_f87ed019-07a6-4e69-8165-e7f8140123c4.shp")


### Prepare data and build choropleth maps for homicide, income, college and unemployment  by community areas ----

df_homicides <- crimes %>% 
  filter(`PRIMARY DESCRIPTION` == "HOMICIDE") %>% 
  st_as_sf(coords = c(16, 15), crs = st_crs(boundaries_shape))

st_crs(df_homicides) == st_crs(boundaries_shape)#TRUE

# count homicides in each community area and merge to boundaries data ----
hom_area_matrix <- as.data.frame(st_within(df_homicides, boundaries_shape, sparse = FALSE))

hom_area <- hom_area_matrix %>% 
  summarize(across(everything(), sum)) %>%
  pivot_longer(cols = everything(), names_to = "community", values_to = "homicides")

hom_area_shp <- boundaries_shape %>% 
  bind_cols(hom_area) %>% 
  select(-10)

# merge in population data and remove useless columns  ----
hom_area_shp$area_num_1 <- as.numeric(hom_area_shp$area_num_1)#Geoid(population) and area_num_1(boundaries_shape) match and can be used as key

population <- population %>% 
  select(GEOID, TOT_POP, UNEMP, BACH, MEDINC, GRAD_PROF, POP_16OV, POP_25OV)

hom_area_pop <- hom_area_shp %>% 
  rename(GEOID = area_num_1) %>% 
  select(-1, -c(3:5), -7) %>% 
  left_join(population, by = "GEOID")
  
hom_area_pop <- hom_area_pop %>%
  mutate(hom_per_100 = homicides/TOT_POP*100000,
         unemp_rate = round(UNEMP/POP_16OV*100, digits = 2),
         coll_rate = round((BACH+GRAD_PROF)/POP_25OV*100, digits = 2))


# loop map generation to observe patterns ----
map_vars <- c("hom_per_100", "MEDINC", "unemp_rate", "coll_rate")
map_list <- vector("list", 4)

for (i in seq_along(map_vars)){
  map_list[[i]] <- hom_area_pop %>% 
    ggplot() +
    geom_sf(aes_string(fill = map_vars[[i]])) 
}

plot_grid(plotlist = map_list, nrow = 2)


# Edit Individual Map and Export ----

ggplot() +
  geom_sf(data = hom_area_pop, aes(fill = hom_per_100), color = "gray26", size = 0.5) +
  geom_sf(data = df_homicides, color = "gray6", size = 0.5, shape = 19) + 
  scale_fill_gradient("Homicides per 100,000 inhabitants", low = "orange", high = "orangered4") +
  ggtitle("Most violent Community Areas and Homicide Locations") +
  theme_map()


### Measure and map average school performance by community area ----
# Student Attainment variable in schools progress df measures how well the school performed on 
# standardized tests at a single point in time

# Merge Schools progress dataframe to schools shapefile ----
schools_shape <-  schools_shape %>% 
  mutate(School_ID = as.numeric(school_id)) %>% #school ids match
  select(-school_id)

schools_full <- schools_shape %>% 
  left_join(schools_progress, by = "School_ID")

# Mutate student attainment to numeric rating and filter missing information to gauge average per community area ----
# We can use numeric values to represent student attainment because ratings are ordinal and thus numerically meaningful
unique(schools_full$Student_Attainment_Rating)

schools_full <- schools_full %>%
  filter(is.na(Student_Attainment_Rating) == FALSE) %>% 
  filter(Student_Attainment_Rating != "NO DATA AVAILABLE") %>% 
  mutate(student_attainment_rank = case_when(
                                   Student_Attainment_Rating == "FAR ABOVE AVERAGE" |
                                   Student_Attainment_Rating == "FAR ABOVE EXPECTATIONS" ~ 5,
                                   Student_Attainment_Rating == "ABOVE AVERAGE" |
                                   Student_Attainment_Rating == "ABOVE EXPECTATIONS" ~ 4,
                                   Student_Attainment_Rating == "AVERAGE" |
                                   Student_Attainment_Rating == "MET EXPECTATIONS" ~ 3,
                                   Student_Attainment_Rating == "BELOW AVERAGE" |
                                   Student_Attainment_Rating == "BELOW EXPECTATIONS" ~ 2,
                                   Student_Attainment_Rating == "FAR BELOW AVERAGE" |
                                   Student_Attainment_Rating == "FAR BELOW EXPECTATIONS" ~ 1)) 

## Calculate and map average school attainment rank for each community ----

# merge by location and calculate community average ----
schools_full_community  <- schools_full %>% 
  st_join(boundaries_shape, join = st_within, left = TRUE) 

comm_rank <- schools_full_community %>%
  group_by(community) %>% 
  mutate(avg_rank = mean(student_attainment_rank)) %>% 
  select(community, avg_rank) %>% 
  st_set_geometry(value = NULL) #remove geometry to merge average rank to boundaries file using name

# merge shape file and school attainment data frame ----
bound_school <- boundaries_shape %>% 
  left_join(comm_rank, by = "community") %>% 
  select(-1, -c(3:5), 7, 8) %>% 
  group_by(community) %>% 
  mutate(n = row_number()) %>% 
  filter(n == 1)

schools_rank <- schools_full %>% 
  select(student_attainment_rank, commarea, short_name)

# Edit Individual Map and Export
top_schools <- schools_rank %>% 
  filter(student_attainment_rank == 5)












