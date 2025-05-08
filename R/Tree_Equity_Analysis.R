# TREE EQUITY ANALYSIS FOR CITY OF KITCHENER ###################################

# Trees play a crucial role in enhancing the health and resilience of our city's
# neighborhoods. In this demonstration, we will show how spatial data and 
# modelling can be leveraged to support tree planting strategies that focus on 
# equitable progress towards having our neighborhoods all across the city 
# enjoying the benefits of trees.

# BY: Jason Goetz, jgoetz@wlu.ca

# DATE: May 8, 2025

# Method
# 1. Load 2021 Census Data & Fill Missing Values
# 2. Calculate % Tree Canopy Coverage (2019)
# 3. Calculate Heat Disparity
# 4. Calculate Priority Index
# 5. Calculate Tree Equity Score
# 6. Create Plots and Export Data


# Load Libraries ###############################################################

library(sf) # work with vector spatial data
library(dplyr) # work with tables
library(data.table) # faster working with tables
library(mapview) # view spatial data
library(terra) # work with gridded/raster spatial data

source("R/census_da_functions.R")

setwd("C:\\teaching\\TreeEquity")

# Load Kitchener Data ##########################################################

# Load urban area boundary in 2023
urban_bnd <- st_read("Kitchener_2023_Urban_Boundary.shp")

# Load Land cover data
lc <- rast("Land_Cover_2019_Kitchener.tif")
plot(lc, main = "Land cover")



# Load Landsat derived land surface temperatures (°C)
lst <- rast("Landsat9_RegionWaterloo_LandSurfaceTemp_20220716_30m_NAD83_UTM17N.tif")
# ^ Note this is for conditions on a hot day in July 2022 - changes in urban
# landscape can change these values, could be updated for new analysis


# Load Census DA Data ##########################################################

# https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=E
# > Canada, provinces, territories, census divisions (CDs), census subdivisions (CSDs) and dissemination areas (DAs) - Ontario only
d <- fread("98-401-X2021006_English_CSV_data_Ontario.csv") # from data.table library to read large data quickly

# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
# > Statistical Boundaries > Dissemination areas > Shapefile
# ^ above was edited in GIS to select by location only DA's within Kitchener Municipality boundary
da <- st_read("Kitchener_2021_CensusDA_Boundary.shp") # Open a GIS and manually edit this to keep only DA's of interest.

# Filter Census DA table data to select only the ones in within loaded DA's
d_sel <- d[DGUID %in% da$DGUID,]

rm(d)


## Explore custom census DA functions ##########################################

# View the category/characteristics data tables  for each DA
categories <- listCensusTables(d_sel)
View(categories) # An RStudio function to look at tables nicely 

# Here's an example of getting the total income statistics for a household
# for a single DA: dguid = "2021S051235300381"

income_table <- getCensusTable(d_sel, 
                               characteristic = "Total - Income statistics for private households - 100% data",
                               dguid = "2021S051235300381")
income_table
View(income_table)

# Above we pulled by the characteristic / table name. We can also use an
# the index from categories table. This makes it easier to work with when the 
# characteristic names are very long.

income_table <- getCensusTable(d_sel, 
                               characteristic = 34,
                               dguid = "2021S051235300381")



## Prepare a data frame to pull the census DA data for our analysis ####

df <- data.frame(DGUID = da$DGUID,
                 population = NA,
                 population_density_sqkm = NA,
                 population_over64years = NA,
                 population_under5years = NA,
                 population_under15years = NA,
                 population_over64years_per = NA,
                 population_under5years_per = NA,
                 population_under15years_per = NA,
                 unemploy_rate = NA,
                 depend_work_ratio = NA,
                 median_house_income = NA,
                 p90p10_ratio = NA,
                 low_income_rate = NA,
                 count_renters = NA,
                 count_owners = NA,
                 percent_renters = NA,
                 child_nochild_ratio = NA,
                 no_engfr_language_rate = NA,
                 visible_minority_rate = NA)



#  Loop through each DA and pull the required Census data values ###############

for(i in 1:nrow(da)){
  
  print(paste(i, "of", nrow(da), "DA's"))
  
  # Get an individual DA
  dguid = da$DGUID[i]
  
  ## Population data ####
  df$population[i] <- getCensusTable(d_sel, "Population, 2021", dguid = dguid)$C1_COUNT_TOTAL
  
  df$population_density_sqkm[i] <- getCensusTable(d_sel, "Population density per square kilometre", dguid = dguid)$C1_COUNT_TOTAL
  
  ## Unemployment rate ####
  df$unemploy_rate[i] <- getCensusTable(d_sel, "Unemployment rate", dguid = dguid)$C10_RATE_TOTAL 
  
  ## Age dependency ####
  # Dependency ratio of seniors (65+) and children (0 - 17 or 19 for canada) to working-age adults.
  
  age <- getCensusTable(d_sel, "Total - Age groups of the population - 100% data", dguid = dguid)
  age_depend <- sum(age$C1_COUNT_TOTAL[c(2,17)]) # 0-14, 65+
  age_work <- sum(age$C1_COUNT_TOTAL[c(6)]) # 15-64
  df$depend_work_ratio[i] <- age_depend / age_work
  df$population_over64years[i] <- age$C1_COUNT_TOTAL[c(17)]
  df$population_under5years[i] <- age$C1_COUNT_TOTAL[c(3)]
  df$population_under15years[i] <- age$C1_COUNT_TOTAL[c(2)]
  df$population_over64years_per[i] <- age$C10_RATE_TOTAL[c(17)]
  df$population_under5years_per[i] <- age$C10_RATE_TOTAL[c(3)]
  df$population_under15years_per[i] <- age$C10_RATE_TOTAL[c(2)]
  
  ## Income ####
  income <- getCensusTable(d_sel, "Total - Income statistics for private households - 100% data", dguid = dguid)
  
  df$median_house_income[i] <- income$C1_COUNT_TOTAL[2]
  
  inequality <- getCensusTable(d_sel, 55, dguid = dguid)
  df$p90p10_ratio[i] <- inequality$C10_RATE_TOTAL[5]
  
  low_income <- getCensusTable(d_sel, 50, dguid = dguid)
  df$low_income_rate[i] <- low_income$C10_RATE_TOTAL[1]
  
  ## Ownership ####
  home_own <- getCensusTable(d_sel, 65, dguid = dguid)
  df$percent_renters[i] <- home_own$C1_COUNT_TOTAL[3]/home_own$C1_COUNT_TOTAL[1]*100
  df$count_renters[i] <- home_own$C1_COUNT_TOTAL[3]
  df$count_owners[i] <- home_own$C1_COUNT_TOTAL[2]
  
  ## Houses with children ####
  house_type <-getCensusTable(d_sel, 22, dguid = dguid)
  df$child_nochild_ratio[i] <- house_type$C1_COUNT_TOTAL[4] / house_type$C1_COUNT_TOTAL[5]
  
  ## Linguistic ####
  language <- getCensusTable(d_sel, 56, dguid = dguid)
  df$no_engfr_language_rate[i] <- language$C10_RATE_TOTAL[5]
  
  ## Race ####
  # % households wi visibile minority
  race <- getCensusTable(d_sel, 91, dguid = dguid)
  df$visible_minority_rate[i] <- race$C10_RATE_TOTAL[2]
  
}

# Join to the spatial DA's
df$DGUID <- NULL
da_join <- cbind(da, df)
#d_da[table_index[[51]],]

## Vulnerable ages ####
# Define vulnerable ages
da_join$vulnerable_ages <- round((da_join$population_under5years + da_join$population_over64years)/da_join$population * 100,1)


# Check results
mapview(da_join, zcol = "median_house_income")


# Imputation ###################################################################

# We will census tract data to fill in NAs in DAs

# Create a field for comments
da_join$comments <- ""

# Get census tract boundaries # can also do manually like DA's
# This is just a demo
library(arcgisbinding)
arc.check_product()
ct <- arc.open("https://geo.statcan.gc.ca/geo_wa/rest/services/2021/Cartographic_boundary_files/MapServer/11")
ct <- arc.select(ct)
ct <- arc.data2sf(ct)

mapview(ct)

# https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=E
# > Census metropolitan areas (CMAs), tracted census agglomerations (CAs) and census tracts (CTs)
d_ct <- fread("98-401-X2021007_English_CSV_data.csv") # from data.table library to read large data quickly

# To make more manageable to work with, we will reduce to CT data within the area
# of interest

kitchener_bnd <- st_read("https://services1.arcgis.com/qAo1OsXi67t7XgmS/arcgis/rest/services/Municipal_Boundary/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
kitchener_bnd <- kitchener_bnd[kitchener_bnd$MUNICIPALITY =="KITCHENER",]

# Transform to match census CRS
kitchener_bnd <- st_transform(kitchener_bnd, crs = st_crs(ct))

# Spatial selection of census tracts (CTs) that intersect with the municipal boundary
ct_sel <- ct[unlist(st_intersects(kitchener_bnd, ct)),]
d_ct_sel <-  d_ct[DGUID %in% ct_sel$DGUID,]

# Select census da records that are missing income data
na_income <- da_join[is.na(da_join$median_house_income),]

# Run a loop that goes through each missing income record, matches
# with a spatial join to the corresponding CT, and extracts the income
# value at that level

for(i in 1:nrow(na_income)){
  
  na_da <- na_income[i,]
  ct_contains_da <- ct[st_within(na_da, ct)[[1]],]

  income <- getCensusTable(d_ct_sel, "Total - Income statistics for private households - 100% data", 
                              dguid = ct_contains_da$DGUID)
  
  da_join$median_house_income[da_join$DGUID == na_da$DGUID] <- income$C1_COUNT_TOTAL[2] # Median total income of household in 2020 ($) 
  da_join$comments[da_join$DGUID == na_da$DGUID] <- "Missing DA median house income filled with CT data"
}


mapview(da_join, zcol = "median_house_income")


na_unemployment <- da_join[is.na(da_join$unemploy_rate),]

# Just one, we will do it manually

# Find DGUID from ct_sel
mapview(ct_sel) + mapview(na_unemployment)

rate <- getCensusTable(d_ct_sel, "Unemployment rate", 
                           dguid = 	"2021S05075410002.15")
  
da_join$unemploy_rate[da_join$DGUID == na_unemployment$DGUID] <- rate$C10_RATE_TOTAL
da_join$comments[da_join$DGUID == na_unemployment$DGUID] <- "Missing DA unemployment rate filled with CT data"



mapview(da_join, zcol = "unemploy_rate")

# Append comments
da_join$comments[is.na(da_join$low_income_rate)] <- paste0(da_join$comments[is.na(da_join$low_income_rate)], "Low-income filled with CT data")
da_join$low_income_rate [is.na(da_join$low_income_rate )] <- mean(da_join$low_income_rate , na.rm=TRUE)

na_lowincome <- da_join[is.na(da_join$low_income_rate ),]

for(i in 1:nrow(na_lowincome)){
  
  na_da <- na_lowincome[i,]
  ct_contains_da <- ct[st_within(na_da, ct)[[1]],]
  
  low_income <- getCensusTable(d_ct_sel, 50, 
                           dguid = ct_contains_da$DGUID)
  
  da_join$low_income_rate[da_join$DGUID == na_da$DGUID] <- low_income$C10_RATE_TOTAL[1] # Median total income of household in 2020 ($) 
  da_join$comments[da_join$DGUID == na_da$DGUID] <- "Missing DA median house income filled with CT data"
}


# Calc. Tree Canopy Cover (2019) per DA ########################################

library(exactextractr) # for fast extracting
# Land cover data (2019; including canopy coverage)
# https://open-kitchenergis.opendata.arcgis.com/documents/d408f24a5ae848f2bdaea569044f42a0/explore

# Reclassify land cover data to tree canopy cover (1: tree, 0: not tree)
tc <- lc
tc[tc > 1] <- 0
plot(tc, main = "Tree canopy cover")

#tc[is.na(tc)] <- 0

da_join <- st_transform(da_join, crs= st_crs(tc))

# Calculate Tree Cover area and % Cover per DA 
da_join$treecover_sqkm <- NA
da_join$treecover_percent <- NA

for(i in 1:nrow(da_join)){
  print(paste(i, "of", nrow(da_join)))
  
  da_i <- da_join[i,]
  dguid <- da_i$DGUID
  
  #tc_da <- terra::mask(tc, da_i)
  tc_da_extract <- exact_extract(tc, da_i)
  tc_da_km2 <- sum(tc_da_extract[[1]]$value, na.rm = TRUE) * prod(terra::res(tc)) / 1000000
  tc_da_per <- tc_da_km2 / (as.numeric(st_area(da_i)) / 1000000) * 100
  
  da_join$treecover_sqkm[i] <- round(tc_da_km2, 2)
  da_join$treecover_percent[i] <- round(tc_da_per, 2)
  
}

mapview(da_join, zcol = "treecover_percent")


# Calc. Land Surface Temperature ###############################################


bnd_buff <- st_buffer(kitchener_bnd, dist = 2000, endCapStyle = "FLAT", joinStyle = "MITRE")
# Make sure data is in same CRS
bnd_buff <- st_transform(bnd_buff, crs = st_crs(lst))

LST_kit <- crop(lst, bnd_buff)

## Mask out water bodies ####

# Water bodies will be masked out to keep temperature relative to 
# land surface (not water surface)
# Using GRCA data https://data.grandriver.ca/downloads-geospatial.html

waterbody <- st_read("GRCA_Waterbody.shp")
waterbody <- st_transform(waterbody, crs = st_crs(lst))

lst_mask <- mask(LST_kit, waterbody, inverse=TRUE)


da_join$mean_surfacetemp <- NA
da_join$median_surfacetemp <- NA
da_join$max_surfacetemp <- NA

for(i in 1:nrow(da_join)){
  print(paste(i, "of", nrow(da_join)))
  
  da_i <- da_join[i,]
  dguid <- da_i$DGUID
  
  #tc_da <- terra::mask(tc, da_i)
  lst_da_extract <- exact_extract(lst_mask, da_i)
  
  da_join$median_surfacetemp[i] <- round(median(lst_da_extract[[1]]$value, na.rm = TRUE),2)
  da_join$mean_surfacetemp[i] <- round(mean(lst_da_extract[[1]]$value, na.rm = TRUE),2)
  da_join$max_surfacetemp[i] <- round(max(lst_da_extract[[1]]$value, na.rm = TRUE),2)
  
}


mapview(LST_kit, maxpixels =  3371940) + mapview(da_join, zcol = "mean_surfacetemp")

# Create a LST heat disparity index ############################################

# An index indicating the difference in temperature to the average urban
# temperature in the City

urban_bnd <- st_transform(urban_bnd, crs = st_crs(lst))

# Extract LST within the urban boundary
lst_ua_extract <- exact_extract(lst, urban_bnd)

# Find mean LST within urban boundary
mean_urban_bnd_surface_temp <- mean(lst_ua_extract[[1]]$value, na.rm = TRUE)

kitchener_bnd <- st_transform(kitchener_bnd, st_crs(lst))

# Create a map of heat disparity at pixel resolution of LST data [30m]
heat_disparity <- LST_kit - mean(lst_ua_extract[[1]]$value, na.rm = TRUE)
heat_disparity <- mask(heat_disparity, kitchener_bnd)
heat_disparity <- crop(heat_disparity, kitchener_bnd)

# Can export raster
writeRaster(heat_disparity, filename = "Kitchener_HeatDisparity_LandsatSurfaceTemp_20220716_30m.tif", overwrite = TRUE)

# Calculate heat disparity index for each DA
da_join$heat_disparity <- da_join$mean_surfacetemp - mean_urban_bnd_surface_temp


# Calc. Priority index score ###################################################

age <- da_join$vulnerable_ages
employment <- da_join$unemploy_rate
income <- da_join$low_income_rate
language <- da_join$no_engfr_language_rate
race <- da_join$visible_minority_rate
heat <- da_join$mean_surfacetemp

#normalize
normInd <- function(x){
  (x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm =TRUE))
}

nAge <- normInd(age)
nEmployment <- normInd(employment)
nIncome <- normInd(income)
nLanguage<- normInd(language)
nRace <- normInd(race)
nHeat <- normInd(heat)


#priority index E
E <- 0.1 + (1-0.1)*(nAge+nEmployment+nIncome+nLanguage+nRace+nHeat)/6
da_join$priority_index <- round(E,2)

mapview(da_join, zcol = "priority_index")

# This is an un-weighted score, but we can also add weights [site suitability]

E_wgt <- nAge * .3 +
  nEmployment * .1 + 
  nIncome * .1  + 
  nLanguage * .1 + 
  nRace *.1 + 
  nHeat *.3

# ^ Above is weighted for heat and vulnerable ages (priorities) first in addition
# to the other socio-economic / environmental factors

# ! For simplicity, the original analysis was not weighted. The weighting process
# should involve consultation with decision makers.

da_join$priority_index_weighted <- round(E_wgt,2)

mapview(da_join, zcol = "priority_index_weighted")

# Calc. Tree Equity Score ######################################################

# Goal of
# - 3 trees visible
# - 30% tree cover in each neighborhood
# - 300 m to min 1 ha of green space

da_join$has_treecover_30per <- NA
da_join$has_treecover_30per[da_join$treecover_percent < 30] <- 1
#da_join$treecover_to_30per <- da_join$treecover_percent * da_join$has_treecover_30per

# percent diff to goal
da_join$treecover_to_30per <- round(30 - da_join$treecover_percent,2)
da_join$treecover_to_30per[da_join$treecover_to_30per <= 0] <- 0
mapview(da_join, zcol = 'treecover_to_30per', )

nTreeCover <- normInd(da_join$treecover_to_30per/100)

da_join$tree_equity_score <- round(100- (E*nTreeCover) * 100,1)
mapview(da_join, zcol = "tree_equity_score", col.regions = viridis::viridis(10, direction = -1))

da_join$tree_equity_rank <- as.integer(rank(da_join$tree_equity_score))



# Create Radar plots ###########################################################
library(fmsb)
library(ggplot2)
library(stringr)
library(Cairo)  # for exporting anti-aliased plots)

setwd("Plots")

dc <- data.frame( age = nAge, 
                  employment = nEmployment,
                  income = nIncome,
                  language = nLanguage,
                  race = nRace,
                  heat = nHeat)

dc_raw<- data.frame( age = age, 
                     employment = employment,
                     income = income,
                     language = language,
                     race = race,
                     heat = round(da_join$heat_disparity,1))

dc_units <- c("%", "%", "%", "%", "%", "\u00B0C")
dc_labs <- c("Children & seniors",
             "Unemployment",
             "Low income",
             "Linguistic isolation",
             "Visible minority",
             "Heat disparity")

mean_data <- c(mean(nAge, na.rm = TRUE),
               mean(nEmployment, na.rm = TRUE),
               mean(nIncome, na.rm = TRUE),
               mean(nLanguage, na.rm = TRUE),
               mean(nRace, na.rm = TRUE),
               mean(nHeat, na.rm = TRUE))

#par(mfrow = c(2,2))
for(i in 1:nrow(da_join)){
  da_join$weblink
  
  plot_name <- paste0("priority_score_", da_join$DGUID[i], ".png")
  #data <- rbind(rep(1, ncol(dc)),  rep(0, ncol(dc)), dc[i,])
  da_join$radarchart[i] <- plot_name
  CairoPNG(file = plot_name, width = 480, height = 400)
  par(mar = c(4,3.5,4,3.5))
  data <- rbind(rep(1, ncol(dc)),  rep(0, ncol(dc)), mean_data, dc[i,])
  
  colors_border=c( rgb(0.75,0.75,0.75,0.3), rgb(0.2,0.5,0.5,0.7) )
  colors_in=c( rgb(0.75,0.75,0.75,0.3), rgb(0.2,0.5,0.5,0.4) )
  
  labs <- paste0(dc_labs, " (", round(dc_raw[i,],2), dc_units, ")")
  
  radarchart(data, pcol = colors_border, axistype = 1, seg = 2,
             pfcol = colors_in,
             plwd = 3,
             cglcol = "#c9c9c9", cglty = 1, cglwd = 1,
             caxislabels=seq(0,1,0.5),
             axislabcol="#c9c9c9", vlcex = 1.1, calcex = 1.1,
             vlabels = labs,
             centerzero = TRUE)
  
  # Add a legend
  legend(x=0.9, y=0.1, legend = c("City average", "DA (local)"), 
         bty = "n", pch=20 , col=colors_in, 
         text.col = "#616161", cex=1, pt.cex=2)  
  dev.off()
  
}

# 
ggplot(da_join, aes(x = treecover_percent, y = mean_surfacetemp)) +
  geom_point(size = 5, alpha = 0.7, pch = 20) +
  xlab("Tree-canopy coverage (%)") +
  ylab("Land-surface temperature (°C)") +
  theme_classic()



setwd("../..")


# Export Analysis on DA's ######################################################

da_join$DGUID.1 <- NULL
da_join$depend_work_ratio <- round(da_join$depend_work_ratio*100,0)
da_join$median_surfacetemp  <- round(da_join$median_surfacetemp,2)
da_join$max_surfacetemp  <- round(da_join$max_surfacetemp,2)
da_join$mean_surfacetemp  <- round(da_join$mean_surfacetemp,2)
da_join$heat_disparity   <- round(da_join$heat_disparity,2)
da_join$treecover_percent   <- round(da_join$treecover_percent,2)
da_join$child_nochild_ratio   <- round(da_join$child_nochild_ratio,2)
da_join$percent_renters <- round(da_join$percent_renters,2)
da_join$priority_index <- round(da_join$priority_index,2)
da_join <- st_transform(da_join, st_crs(urban_bnd))

da_join <- dplyr::select(da_join, names(da_join))
names(da_join)[1:length(names(da_join))-1] <- toupper(names(da_join))[1:length(names(da_join))-1]
st_write(da_join, "UrbanForests.gdb", "TREE_COVER_EQUITY_CENSUS2021_DA", append = FALSE)



