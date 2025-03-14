## 
## LANDSAT 9 LAND SURFACE TEMPERATURE
##
## Purpose: Calculate land surface temperature (LST) from Landsat 9 Data in R  
##        
## Author: Dr. Jason Goetz (jgoetz@wlu.ca)
##
## Date Created: 11-16-2023
##
## Notes: This R script provides an example of calculating LST from Landsat 9
##        data. The process is fully automated: parameters for LST retrieval 
##        are pulled from the sensors metadata file (xml)
##  
## References: 
##
##  https://www.usgs.gov/landsat-missions/using-usgs-landsat-level-1-data-product


# Load required packages and data ##############################################

library(terra) # For handling raster data
library(xml2) # For handling xml files (metadata)

# Set working directory to the location of the Landsat satellite data
setwd("V:\\GIS_Users\\Jason\\Landsat\\Landsat9_20220716")

# Load the metadata file
md <- as_list(read_xml("LC09_L1TP_018030_20220716_20230407_02_T1_MTL.xml"))

# Load only the required bands
b4 <- rast('LC09_L1TP_018030_20220716_20230407_02_T1_B4.TIF')
b5 <- rast('LC09_L1TP_018030_20220716_20230407_02_T1_B5.TIF')
b10 <- rast('LC09_L1TP_018030_20220716_20230407_02_T1_B10.TIF')

# Visualize the thermal infrared band (wavelength = 10.60-11.19 nm)
plot(b10)


# Convert to Top of Atmosphere (TOA) reflectance ###############################

# Get the multiplicative rescaling factor (Ml) from the metadata (MTL file) 
# for band 10
Ml <-  as.numeric(unlist(md$LANDSAT_METADATA_FILE$LEVEL1_RADIOMETRIC_RESCALING$RADIANCE_MULT_BAND_10))

# Get the additive rescaling factor (Ml) from the metadata (MTL file) for band 10
Al <- as.numeric(unlist(md$LANDSAT_METADATA_FILE$LEVEL1_RADIOMETRIC_RESCALING$RADIANCE_ADD_BAND_10))

# Combine the radiance rescaling factors to find TOA spectral radiance (Ll)
Ll <- Ml*b10+Al

# Convert to (TOA) brightness temperature ######################################

# Get the thermal conversion constants (K1 and K2) from the metadata for band 10
K1 <- as.numeric(unlist(md$LANDSAT_METADATA_FILE$LEVEL1_THERMAL_CONSTANTS$K1_CONSTANT_BAND_10))
K2 <- as.numeric(unlist(md$LANDSAT_METADATA_FILE$LEVEL1_THERMAL_CONSTANTS$K2_CONSTANT_BAND_10))

# Calculate TOA brightness temperature (BT; in Celsius) 
BT <- (K2/log((K1/Ll)+1)) - 273.15


# Determine NDVI, proportion of vegetation and emissivity ######################

# Calculate the nomralized difference vegetation index (NDVI)
NDVI <- (b5 - b4)/(b5 + b4)

# Find minimum and maximum NDVI
maxNDVI <- max(values(NDVI), na.rm = TRUE)
minNDVI <- min(values(NDVI), na.rm = TRUE)

# Caclulate proportion of vegetation (Pv) and emissivity (em)
Pv <- ((NDVI - minNDVI) / (maxNDVI - minNDVI))^2
em = 0.004*Pv+0.986

# Convert to land surface temperature (LST) ####################################

LST <- (BT/(1+(0.00115*BT/1.4388)*log(em)))
plot(LST)

# Export Landsat9 derived LST as a Geotiff
writeRaster(LST, filename = "landsat9_lst_20220716.tif")
