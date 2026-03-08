

#function to read in all sheets in a workbook
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

#Calling in the data
full_data<-read_excel_allsheets("data.xlsx")


# Selecting the national data for all the ten regions 
nat_data <-full_data$National
national_data<-data.frame(nat_data)

##Reading in Shape file of Former Ghana ten regions
#Reference for the shape file: https://data.gov.gh/dataset/shapefiles-all-regions-ghana-2010-10-regions
gh_shape <- st_read("gh_shape/Map_of_Regions_in_Ghana.shp")


# Calculate the centroid of each region's polygon
region_centroids <- st_centroid(gh_shape)

# Extract the latitude and longitude coordinates from the centroid points
region_coords <- st_coordinates(region_centroids)

# Convert the coordinates to a data frame
region_coords_df <- data.frame(
  region = gh_shape$REGION,
  longitude = region_coords[, 1],
  latitude = region_coords[, 2]
)

# Alternative Conversion of region names to proper case (from Upper Cases to Proper Cases)
#region_coords_df$region <- str_to_title(region_coords_df$region)
region_coords_df$region <- gsub("\\b(\\w)(\\w*)", "\\U\\1\\L\\2", region_coords_df$region, perl = TRUE)

# Merge region coordinates with data based on region
merge_data <- left_join(national_data, region_coords_df, by = "region")

# Create date range
date_range <- seq(from = as.Date("2012-01-01"), to = as.Date("2023-12-01"), by = "month")

# Convert date range to data frame
date_df <- data.frame(date = date_range)

# Add date_df to the existing data merged frame and calling it data
data <- cbind(merge_data, date_df)    

#Ensure that id is a factor
data$id <- as.factor(data$id)
str(data)


