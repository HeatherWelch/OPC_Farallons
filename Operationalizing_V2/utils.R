

# Function to update a template NCDF file for epac model predictions to host on ERDDAP

export_nc = function(x, template, get_date, out_dir, filename) {
  ## x = a raster of the model prediction
  ## template = the file path to the NCDF template file on which to copy metadata and dims
  ## get_date = the date of model prediction
  ## out_dir = the file path to store the new NCDF file
  ## filename = the name (w/ .nc extension) to save the new NCDF file
  
  
  # Make copy template for new file w/ metadata
  file.copy(from = template, to = glue("{out_dir}/{filename}"))
  
  # Open NCDF to edit values and attribs
  epac.nc <- nc_open(glue("{out_dir}/{filename}"), write = TRUE)
  
  # Store vector of predicted values from raster
  vals <- values(x) |> 
    as.vector() |> 
    na_if(NaN)  #change values from NaN to NA
  
  # Define time for date of file creation
  tm <- as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")
  tm2 <- gsub(x = strftime(tm, "%Y-%m-%dT%H:%M:%S%Z"), pattern = "UTC", replacement = "Z")
  
  # Define centered date-time for date of model prediction
  c_tm <- as.POSIXct(paste(get_date, "12:00:00"), tz = "UTC") |> 
    as.numeric()
  
  
  ### Update values in file ###
  
  # Add values for model prediction
  ncvar_put(epac.nc,
            "euphausia_pacifica",
            vals,
            na_replace = "fast")
  
  # Replace time with date of model prediction
  ncvar_put(epac.nc,
            "time",
            c_tm)
  
  # Change time of file creation
  ncatt_put(epac.nc,
            0,
            "date_created",
            tm2)
  
  # Close NCDF to ensure all changes are saved
  nc_close(epac.nc)

  }

#--------------------------------

# Function to access time metadata from CMEMS products

cmems_time = function(productID, path_copernicus_marine_toolbox) {
  
  # Define command for querying metadata
  command <- glue("{path_copernicus_marine_toolbox} describe -i {productID} -r coordinate_id,coordinate_unit,minimum_value,maximum_value")
  meta <- system(command, intern = TRUE)
  meta.json <- jsonlite::parse_json(meta)
  
  
  #Access time data
  dims <- meta.json$products[[1]]$datasets[[1]]$versions[[1]]$parts[[1]]$services[[3]]$variables[[1]]$coordinates
  
  # Check to see where time is stored; varies by product
  if (dims[[1]]$coordinate_id == "time" & grepl("^milliseconds", dims[[1]]$coordinate_unit)) {
    
    min_date <- as_datetime(dims[[1]]$minimum_value / 1000)
    max_date <- as_datetime(dims[[1]]$maximum_value / 1000)
    
  } else if (dims[[4]]$coordinate_id == "time" & grepl("^milliseconds", dims[[4]]$coordinate_unit)) {
    
    min_date <- as_datetime(dims[[4]]$minimum_value / 1000)
    max_date <- as_datetime(dims[[4]]$maximum_value / 1000)
    
  } else {
    stop("Need to fix bug in parsing of time")
  }
  
  time_range <- c(min = min_date, max = max_date) |> 
    as_date()
  
  return(time_range)
}


