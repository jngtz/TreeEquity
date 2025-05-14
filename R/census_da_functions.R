
getCensusTable <- function(x, characteristic, dguid = "2021S051235300381"){
  # Get's census table based on characteristic name or index from characteristic list
  # Works for CD and CT data
  
  # x: a dataframe containing the imported census data
  # characteristic: either ID or characteristic name of a table within the census data
  # dguid: a geographic id to requiest a census table for
  
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

  # Works for CT and CD data
  # x: a dataframe containing the imported census data

  id <- x$DGUID[1]
  d_da <- x[DGUID == id,]
  d_da$CHARACTERISTIC_NAME <- iconv(d_da$CHARACTERISTIC_NAME, from = "latin1", to = "UTF-8", sub = "")
  categories <- grep("^[^ ]", d_da$CHARACTERISTIC_NAME, value = TRUE)
  result <- data.frame(index = 1:length(categories), characteristic_name = categories)
  return(result)
}


