## 
## Joining Census 2021 DA table data to boundaries
##
##        
## Author: Dr. Jason Goetz (jgoetz@wlu.ca)
##
## Date Created: 03-14-2025
##
## This R script provides tools and an example to retrieve Census 2021 data
## for dissemination areas (DA) and join to the DA boundary file.
##


# Load Libraries ###############################################################

library(sf) # work with vector spatial data
library(data.table) # faster working with tables
library(mapview) # view spatial data


# Set working directory
setwd("C:\\teaching\\GG_369")

# Load custom functions ########################################################

getCensusTable <- function(x, characteristic, dguid = "2021S051235300381"){
  # Get's census table based on characteristic name or index from characteristic list
  # Works for CD and CT data
  
  x <- x[DGUID == dguid,]
  x$CHARACTERISTIC_NAME <- iconv(x$CHARACTERISTIC_NAME, from = "latin1", to = "UTF-8", sub = "")
  categories <- grep("^[^ ]", x$CHARACTERISTIC_NAME, value = TRUE)
  
  census_tables<- iconv(x$CHARACTERISTIC_NAME, from = "latin1", to = "UTF-8", sub = "")
  table_index <- list()
  
  # Initialize a variable to track the current group
  census_topic <- c()
  
  # Loop through each string
  for (i in seq_along(census_tables)) {
    if (grepl("^[^ ]", census_tables[i])) {
      # If the string doesn't start with a space, record the current group and reset
      table_index <- append(table_index, list(census_topic))
      census_topic <- c(i)
    } else {
      # If the string starts with a space, add the index to the current group
      census_topic <- append(census_topic, i)
    }
  }
  
  # Append the last group to the list
  table_index <- append(table_index, list(census_topic))
  
  # Remove the empty first element in the list (caused by the initial empty census_topic)
  table_index <- table_index[-1]
  
  if(is.numeric(characteristic)){
    result <- x[table_index[[characteristic]],]
  } else {
    table_ind <- which(categories == characteristic)
    result <- x[table_index[[table_ind]],]
  }
  
  # if(is.numeric(characteristic)){
  #   print(paste("DGUID:", dguid, listCategories(x)[characteristic]))
  #   
  # } else {
  #   print(paste("DGUID:", dguid, characteristic))
  # }
  
  return(result)
}


listCensusTables <- function(x){
  # Using 
  #da_sel <- x[1,]
  # Works for CT and CD data
  id <- x$DGUID[1]
  d_da <- x[DGUID == id,]
  d_da$CHARACTERISTIC_NAME <- iconv(d_da$CHARACTERISTIC_NAME, from = "latin1", to = "UTF-8", sub = "")
  categories <- grep("^[^ ]", d_da$CHARACTERISTIC_NAME, value = TRUE)
  result <- data.frame(index = 1:length(categories), characteristic_name = categories)
  return(result)
}


listCensusTables_CT <- function(x){
  # Using 
  
  char_names <- names(table(x$CHARACTERISTIC_NAME))
  char_names <- iconv(char_names, from = "latin1", to = "UTF-8", sub = "")
  categories <- grep("^[^ ]", char_names, value = TRUE)
  
  matched_ids <- x[CHARACTERISTIC_NAME %in% categories, .(CHARACTERISTIC_ID, CHARACTERISTIC_NAME)]
  #print(matched_ids)
  unique_ids <- matched_ids[!duplicated(CHARACTERISTIC_ID)]
  
  return(unique_ids)
}



# Load Census DA Data ##########################################################

# https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=E
# > Canada, provinces, territories, census divisions (CDs), census subdivisions (CSDs) and dissemination areas (DAs) - Ontario only
d <- fread("98-401-X2021006_Ontario_eng_CSV\\98-401-X2021006_English_CSV_data_Ontario.csv")

# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
# > Statistical Boundaries > Dissemination areas > Shapefile
# ^ above was edited in GIS to select by location only DA's within Kitchener Municipality boundary
da <- st_read("kw_census_da.shp") # Open a GIS and manually edit this to keep only DA's of interest.

# Filter Census DA table data to select only the ones in within loaded DA's
d_sel <- d[DGUID %in% da$DGUID,]
#write.csv(d_sel, file = "Census2021_Kitchener_DA.csv")
rm(d)

## Summarize the Census Data Categories Indices ####
# Select just one DA


categories <- listCensusTables(d_sel)
View(categories)

income_table <- getCensusTable(d_sel, 
                               characteristic = "Total - Income statistics for private households - 100% data",
                               dguid = "2021S051235300381")
income_table

# or by index
income_table <- getCensusTable(d_sel, 
                               characteristic = 34,
                               dguid = "2021S051235300381")

# Get Census Data Values for Analysis ##########################################

# Define our data frame to fill
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

# Loop through each DA and pull the required Census data values
for(i in 1:nrow(da)){
  print(paste(i, "of", nrow(da), "DA's"))
  dguid = da$DGUID[i]
  
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

# Export results
st_write(da_join, "census_2021_da_join.gpkg")
