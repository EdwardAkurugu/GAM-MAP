

# Function to read all sheets
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


# Read Excel workbook
full_data<-read_excel_allsheets("data.xlsx")
national_data <-full_data$National


##Reading shapefile 
#Reference for the shape file: https://data.gov.gh/dataset/shapefiles-all-regions-ghana-2010-10-regions
gh_shape <- st_read("gh_shape/Map_of_Regions_in_Ghana.shp")

# Centroids
region_centroids <- st_centroid(gh_shape)
region_coords <- st_coordinates(region_centroids)

region_coords_df <- data.frame(
  region = gh_shape$REGION,
  longitude = region_coords[, 1],
  latitude = region_coords[, 2]
)

# Proper case for region names
region_coords_df$region <- gsub("\\b(\\w)(\\w*)", "\\U\\1\\L\\2", region_coords_df$region, perl = TRUE)
#region_coords_df$region <- str_to_title(region_coords_df$region)  

# Merge 
merge_data <- left_join(national_data, region_coords_df, by = "region")

# Data sequence
date_range <- seq(from = as.Date("2012-01-01"), to = as.Date("2023-12-01"), by = "month")
date_df <- data.frame(date = date_range)

# Final data
data <- cbind(merge_data, date_df)
data$id <- as.factor(data$id)

