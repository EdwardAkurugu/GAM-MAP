
# ==========================================================================================
# GAM(M)-MAP DATA PIPELINE
# ==========================================================================================

# -----------------------------------------------------------------------
# 1. READ WORKBOOK ONCE
# -----------------------------------------------------------------------
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) suppressMessages(readxl::read_excel(filename, sheet = X)))
  if (!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

full_data     <- read_excel_allsheets("data.xlsx")
national_data <- full_data$National
pop_data      <- full_data$census_data

# -----------------------------------------------------------------------
# 2. SHAPEFILE -> REGION CENTROIDS (lng/lat)
# -----------------------------------------------------------------------
gh_shape <- suppressMessages(st_read("gh_shape/Map_of_Regions_in_Ghana.shp"))

region_centroids    <- suppressWarnings(st_centroid(gh_shape))
region_centroids_ll <- st_transform(region_centroids, crs = 4326)
region_coords       <- st_coordinates(region_centroids_ll)

region_coords_df <- data.frame(
  region = gh_shape$REGION,
  lng    = region_coords[, 1],
  lat    = region_coords[, 2]
)

region_coords_df$region <- gsub("\\b(\\w)(\\w*)", "\\U\\1\\L\\2",
                                region_coords_df$region, perl = TRUE)

# -----------------------------------------------------------------------
# 3. BUILD BASE MONTHLY MALARIA/CLIMATE `data`
# -----------------------------------------------------------------------
merge_data <- left_join(national_data, region_coords_df, by = "region")

date_range <- seq(from = as.Date("2012-01-01"),
                  to   = as.Date("2023-12-01"),
                  by   = "month")
date_df <- data.frame(date = date_range)

data <- cbind(merge_data, date_df)
data$id <- as.factor(data$id)

# -----------------------------------------------------------------------
# 4. POPULATION OFFSET AND INTERPOLATION
# -----------------------------------------------------------------------
pop_long <- pop_data %>%
  tidyr::pivot_longer(cols = -year, names_to = "region", values_to = "population")

pop_long$region <- gsub("_", " ", pop_long$region)
pop_long$region <- gsub("\\b(\\w)(\\w*)", "\\U\\1\\L\\2", pop_long$region, perl = TRUE)

mismatch <- setdiff(unique(data$region), unique(pop_long$region))
if (length(mismatch) > 0) {
  warning("Regions in `data` with no match in census pop_long: ",
          paste(mismatch, collapse = ", "))
}

interpolate_region_annual <- function(df, years_needed) {
  df <- df %>% arrange(year)
  log_pop_interp <- approx(
    x = df$year, y = log(df$population),
    xout = years_needed, rule = 2
  )$y
  data.frame(year = years_needed, population = exp(log_pop_interp))
}

years_needed <- 2012:2023

pop_annual_full <- pop_long %>%
  group_by(region) %>%
  group_modify(~ interpolate_region_annual(.x, years_needed)) %>%
  ungroup()

disaggregate_to_monthly <- function(df) {
  df <- df %>% arrange(year)
  anchor_dates   <- as.Date(paste0(df$year, "-07-01"))  
  anchor_log_pop <- log(df$population)
  
  target_dates <- seq(as.Date("2012-01-01"), as.Date("2023-12-01"), by = "month")
  
  monthly_log_pop <- approx(
    x = as.numeric(anchor_dates), y = anchor_log_pop,
    xout = as.numeric(target_dates), rule = 2
  )$y
  
  data.frame(date = target_dates, population = exp(monthly_log_pop))
}

pop_monthly <- pop_annual_full %>%
  group_by(region) %>%
  group_modify(~ disaggregate_to_monthly(.x)) %>%
  ungroup() %>%
  rename(pop_join = population)

# -----------------------------------------------------------------------
# 5. MERGE POPULATION OFFSET ONTO `data`
# -----------------------------------------------------------------------
data <- data %>%
  left_join(pop_monthly, by = c("region", "date")) %>%
  mutate(log_pop_offset = log(pop_join))

n_missing <- sum(is.na(data$log_pop_offset))
if (n_missing > 0) {
  warning(sprintf(
    "%d rows have no matched population value - check region name spelling/consolidation.",
    n_missing))
}

