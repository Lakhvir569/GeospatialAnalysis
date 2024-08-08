# The libraries
library(sf)
library(dplyr)
library(tmap)
library(writexl)

neighborhoods <- st_read("Neighborhoods.shp")
zipcode <- st_read("zip_codes.shp")
Grocerystores <- st_read("Grocery_Stores_in_City_of_Detroit_Public_View.shp")
Crime <- st_read("RMS_Crime_Incidents.shp")

# Filter the Crime incidents that occurred in 2021
Crime_2021 <- Crime %>% 
  filter(year == 2021)

# Print the first few rows of the filtered dataset
head(Crime_2021)

# check out their crs
st_crs(neighborhoods)
st_crs(zipcode)
st_crs(Grocerystores)
st_crs(Crime_2021)

# Let' project them
zipcode_p <- st_transform(zipcode, crs = 5623)
neighborhoods_p <- st_transform(neighborhoods, crs = 5623)
grocery_p <- st_transform(Grocerystores, crs = 5623)
Crime_2021_p <- st_transform(Crime_2021, crs = 5623)

#Number of Crimes in each neighborhood
crime_neighborhood <- st_within(Crime_2021_p, neighborhoods_p, sparse = FALSE)
crime_neighborhood1 <- apply(X = crime_neighborhood, MARGIN = 2, FUN=sum)
crime_neighborhood2 <- cbind(neighborhoods_p, crime_neighborhood1)


# Create a tmap object with 'neighborhoods_p' data
NeighborhoodCrimeMap <- tm_shape(crime_neighborhood2) +
  # Add polygons for neighborhoods
  tm_polygons(col = "crime_neighborhood1", palette = c("green", "white", "red"),
              breaks = c(0, 100, 1200, Inf),
              title = "Crime Incidents in 2021") +
  # Remove frame
  tm_layout(frame = FALSE, main.title = "Crime Incidents Within Detroit Neighborhoods in 2021")

# Display the map
NeighborhoodCrimeMap

#Number of Crimes in each zipcode
crime_zipcode4 <- st_within(Crime_2021_p, zipcode_p, sparse = FALSE)
crime_zipcode5 <- apply(X = crime_zipcode4, MARGIN = 2, FUN=sum)
crime_zipcpde6 <- cbind(zipcode_p, crime_zipcode5)

# Create a tmap object with 'zipcode_p' data
ZipcodeCrimeMap <- tm_shape(crime_zipcpde6) +
  # Add polygons for zip code areas
  tm_polygons(col = "crime_zipcode5", palette = c("green", "white", "red"), 
              breaks = c(0, 1000, 5000, Inf),
              title = "Crime Incidents in 2021") +
  # Add zip code labels
  tm_text("zipcode", size = 0.5, root = 2, 
          bg.color = NA, bg.alpha = 0, col = "black") +
  # Remove frame
  tm_layout(frame = FALSE, main.title = "Crime Incidents within Different Detroit Zipcode Areas in 2021")

# Display the map
ZipcodeCrimeMap

# Plot neighborhoods and grocery stores
tm_shape(neighborhoods_p['nhood_name']) + tm_polygons(alpha = 0.8) +
  tm_shape(grocery_p) + tm_dots(col = 'blue', size = 0.1)

# Create a buffer of 1 mile radius around each grocery store
gro_1m_buff <- st_buffer(grocery_p, dist = 1609)

# Visualize buffers
tm_shape(neighborhoods_p) + tm_polygons(alpha = 0.4) +
  tm_shape(grocery_p) + tm_dots(col = "blue", size = 0.1) +
  tm_shape(gro_1m_buff) + tm_borders(col = "red") + tm_fill(col = "red", alpha = 0.2)

# Create another buffer of 2 miles around the grocery stores and call it 'gro_2m_buff'.
# Add this layer to the existing map plot. Let the borders and fill colors be red. Set the fill alpha value to 0.5.

gro_2m_buff <- st_buffer(grocery_p, dist = 3218)

tm_shape(neighborhoods_p) + tm_polygons(alpha = 0.4) +
  tm_shape(grocery_p) + tm_dots(col = "blue", size = 0.1) +
  tm_shape(gro_1m_buff) + tm_borders(col = "black") + tm_fill(col = "black", alpha = 0.2) +
  tm_shape(gro_2m_buff) + tm_borders(col = 'red') + tm_fill(col = 'red', alpha = 0.5)

# Create another buffer of 3 miles around grocery stores
gro_3m_buff <- st_buffer(grocery_p, dist = 4827)

tm_shape(neighborhoods_p) + tm_polygons(alpha = 0.4) +
  tm_shape(grocery_p) + tm_dots(col = "blue", size = 0.1) +
  tm_shape(gro_1m_buff) + tm_borders(col = "black") + tm_fill(col = "black", alpha = 0.2) +
  tm_shape(gro_2m_buff) + tm_borders(col = 'red') + tm_fill(col = 'red', alpha = 0.5) +
  tm_shape(gro_3m_buff) + tm_borders(col = 'green') + tm_fill(col = 'green', alpha = 0.5)

# Titling your plot and adding text

tm_shape(neighborhoods_p) + tm_polygons(alpha = 0.4) + tm_text('nhood_name', size = 0.4) +
  tm_shape(grocery_p) + tm_dots(col = "blue", size = 0.1) +
  tm_shape(gro_1m_buff) + tm_borders(col = "black") + tm_fill(col = "black", alpha = 0.2) +
  tm_shape(gro_2m_buff) + tm_borders(col = 'red') + tm_fill(col = 'red', alpha = 0.5) +
  tm_shape(gro_3m_buff) + tm_borders(col = 'green') + tm_fill(col = 'green', alpha = 0.5)
tm_layout(main.title = "Map of Detroit Neighborhood and Grocery Stores",
          main.title.position = "center",
          main.title.size = 1,
          frame = FALSE)

#Filter out the Crime offenses to only show the Crimes which were Robbery
Crime_2021_robbery <- Crime_2021_p %>% 
  filter(offense_de == "ROBBERY")

#Project the updated dataset
Crime_2021_robbery_p <- st_transform(Crime_2021_robbery, crs = 5623)

# Create buffers around grocery stores
buffer_1mile <- st_buffer(grocery_p, dist = 1609.34) # 1 mile in meters
buffer_2miles <- st_buffer(grocery_p, dist = 3218.69) # 2 miles in meters
buffer_3miles <- st_buffer(grocery_p, dist = 4828.03) # 3 miles in meters

# Count robberies within buffers
grocery_p$robberies_1mile <- sapply(st_intersects(buffer_1mile, Crime_2021_robbery_p), length)
grocery_p$robberies_2miles <- sapply(st_intersects(buffer_2miles, Crime_2021_robbery_p), length)
grocery_p$robberies_3miles <- sapply(st_intersects(buffer_3miles, Crime_2021_robbery_p), length)

#Descriptive Statistics for each of the robbery variables
RobberiesWithin1MileSummary <- summary(grocery_p$robberies_1mile)
RobberiesWithin2MilesSummary <- summary(grocery_p$robberies_2miles)
RobberiesWithin3MilesSummary <- summary(grocery_p$robberies_3miles)
RobberiesWithin1MileSummary
RobberiesWithin2MilesSummary
RobberiesWithin3MilesSummary

# Remove geometry column
grocery_p1 <- st_drop_geometry(grocery_p)

# Select relevant columns
grocery_p1 <- grocery_p1 %>% 
  select(Store_Name, Address, robberies_1mile, robberies_2miles, robberies_3miles)

# Write the data to an Excel spreadsheet - Try this again to ensure grocery got dropped
write_xlsx(grocery_p1, "GroceryRobberyStatsPerMile.xlsx")
