rm(list = ls()) # Clear the environment 
graphics.off() # Remove plots 

# install.packages('pacman')
# install.packages('tidyverse')
# install.packages('rnaturalearth')
# install.packages('rnaturalearthdata')
# install.packages('sf')
# install.packages('ggthemes')
# install.packages('ggmap')
# install.packages('dplyr')
# install.packages('viridis')
# install.packages('tinytex')
# install.packages('ggrepel')
# install.packages('httr')
# install.packages('jsonlite')
# install.packages('stats')
# install.packages('writexl')
# install.packages('gridExtra')
# install.packages('forcats')


library(pacman) # Load pacman package
library(tidyverse) # Load tidyverse package
library(rnaturalearth) # Load rnaturalearth package
library(rnaturalearthdata) # Load rnaturalearthdata package
library(sf) # To read and plot simple features dataframe
library(ggthemes) # For standard themes
library(ggmap)
library(dplyr)
library(viridis)
library(tinytex) # To use LaTeX syntax
library(ggrepel) # For nice positioning of the label aesthetic
library(httr) # To implement a GET() call for an API
library(jsonlite) # For parsing a json file into text
library(stats) # For implementing logistic regression via glm()
library(gridExtra) # For stitching plots together using grid.arrange()
library(forcats)

# Google Maps Platform API_Key
register_google(key = "AIzaSyAqaV9tLWSuhuWiIDhUJH1_1FdXkPe6LtI")
options(timeout = 300) # Increase timeout to 300 seconds

# Cambodia Map Overview 
cambodia <- ggmap::geocode("Cambodia") # Obtaining Cambodia's location
cambodia

cambodia.map <- get_googlemap(center = c(lon = cambodia$lon, lat = cambodia$lat), zoom = 6)
ggmap(cambodia.map) + # cambodia.map is the base map that is fetched from get_googlemap()
  ggtitle("Cambodia Map Overview") + 
  theme(plot.title = element_text(size = 16, face = "bold"))


cam.map <-
  ne_countries(scale = 'medium',
               type = "countries", 
               country = "Cambodia",
               returnclass = "sf")

View(cam.map)

setwd("/Users/joeychua/Desktop/SUSS/Y1S1/ANL501 - Data Visualisation & Storytelling/ECAPROJ") # Directory path where my knitted output file should appear

# Importing raw data files
mine.df <- sf::read_sf(dsn = "./fear/mine.shp")
fear.df <- sf::read_sf(dsn = "./mine/khm_blscontaminationp_gov_cmaa.shp")
View(mine.df)
View(fear.df)

####################### MINE DATASET #######################
# Data Analysis
str(mine.df) # Shows the number of observations(rows) and variables (columns) in mine dataframe
head(mine.df) # Shows the first 6 rows of the data
tail(mine.df) # Shows the bottom 6 rows of the data
summary(mine.df) # Indicates some descriptive statistics of mine dataframe

# Data Management
is.data.frame(mine.df) # To check if mine.df is a dataframe
colnames(mine.df)

# Checking for any NA values for mine.df Dataset
is.na(mine.df) # to check if there is any missing values
colSums(is.na(mine.df)) # count the no of missing values F=0; T=1

blank_ERW_TYPE <- mine.df %>%
  filter(DEVICES == "ERW") %>% # Filter rows where DEVICES is "ERW"
  filter(is.na(ERW_TYPE)) # Further filter to extract rows where ERW_TYPE is NA
View(blank_ERW_TYPE) # 61 fields under ERW_TYPE are omitted

blank_MINE_TYPE <- mine.df %>%
  filter(DEVICES == "Mine") %>% # Filter rows where DEVICES is "Mine"
  filter(is.na(MINE_TYPE)) # Further filter to extract rows where MINE_TYPE is NA
View(blank_MINE_TYPE) # No missing fields in MINE_TYPE

mine.df$ERW_TYPE %>% unique()
mine.df$MINE_TYPE %>% unique()

updated.mine.df <- mine.df %>%
  mutate(ERW_TYPE = if_else(DEVICES == "ERW" & is.na(ERW_TYPE), "Unknown", ERW_TYPE))
View(updated.mine.df) # all missing fields under ERW_TYPE which has NA value where DEVICES = ERW have been updated with the value "Unknown"

# Converting geometry variable into the lat-long coordinate system using st_transform
updated.mine.df$geometry <- st_transform(updated.mine.df$geometry, crs = 4326)
View(updated.mine.df)

print(class(updated.mine.df$geometry)) # To check if my geometry column is set up properly to represent spatial point data

coordinates.mine.df <- st_coordinates(updated.mine.df$geometry) # Extract coordinates
coordinates.mine.df <- as.data.frame(coordinates.mine.df) # Convert coordinates.mine.df into dataframe
colnames(coordinates.mine.df) <- c("Longitude", "Latitude") # Rename the columns of the extracted data
View(coordinates.mine.df)

new.mine.df <- cbind(updated.mine.df, coordinates.mine.df) # Merge coordinates back into original mine.df dataset
View(new.mine.df)

####################### FEAR DATASET #######################
# Data Analysis
str(fear.df) # Shows the number of observations(rows) and variables (columns) in fear dataframe
head(fear.df) # Shows the first 6 rows of the data
tail(fear.df) # Shows the bottom 6 rows of the data
summary(fear.df) # Indicates some descriptive statistics of fear dataframe

# Data Management
is.data.frame(fear.df) # To check if fear.df is a dataframe
colnames(fear.df)
options(scipen = 999) # Set options to display numbers without scientific notation
fear.df$VilCode <- as.numeric(fear.df$VilCode) # Converting fear.df dataset VilCode from "chr" to "num"
str(fear.df)

# Checking for any NA values for fear.df Dataset
is.na(fear.df) # to check if there is any missing values
colSums(is.na(fear.df)) # count the no of missing values F=0; T=1

fear.df$Fear_Level %>% unique()

updated.fear.df <- fear.df %>%
  mutate(Fear_Level = if_else(Fear_Level == "(blank)", "Unknown", Fear_Level))
View(updated.fear.df) # all the fields with a "(blank)" value under Fear_Level column have been updated with the value "Unknown"

# Converting geometry variable into the lat-long coordinate system using st_transform
updated.fear.df$geometry <- st_transform(updated.fear.df$geometry, crs = 4326)
view(updated.fear.df)

print(class(updated.fear.df$geometry)) # To check if my geometry column is set up properly to represent spatial point data

coordinates.fear.df <- st_coordinates(updated.fear.df$geometry) # Extract coordinates
coordinates.fear.df <- as.data.frame(coordinates.fear.df) # Convert coordinates.fear.df into dataframe
colnames(coordinates.fear.df) <- c("Longitude", "Latitude") # Rename the columns of the extracted data
View(coordinates.fear.df)

new.fear.df <- cbind(updated.fear.df, coordinates.fear.df) # Merge coordinates back into original fear.df dataset
View(new.fear.df)


################# DATA CLEANING #################
new.mine.df$Province %>% unique() # See what are the original province name in my mine and fear datasets
new.fear.df$Province %>% unique()

# Standardizing province names in both mine and fear datasets
new.mine.df$Province <- tolower(trimws(new.mine.df$Province)) # Convert everything into lowercase and remove any leading/trailing spaces between characters
new.fear.df$Province <- tolower(trimws(new.fear.df$Province))
View(new.mine.df)
View(new.fear.df)

# Creating named vector with correct spellings for Cambodia's province names
province_names_corrections <- c(
  "mondol kiri" = "mondulkiri",               # Correct to "mondulkiri"
  "mondul kiri" = "mondulkiri",               # Correct to "mondulkiri"
  "kampong cham" = "kampong cham",            # Already correct
  "banteay mean chey" = "banteay meanchey",   # Fix spacing
  "otdar mean chey" = "oddar meanchey",       # Correct to "oddar meanchey"
  "siem reap" = "siem reap",                  # Already correct
  "siemreap" = "siem reap",                  # Fix to "siem reap"
  "preah vihear" = "preah vihear",            # Already correct
  "pursat" = "pursat",                       # Already correct
  "battambang" = "battambang",               # Already correct
  "krong pailin" = "pailin",                 # Correct to "pailin"
  "kaoh kong" = "koh kong",                  # Correct to "koh kong"
  "kampot" = "kampot",                       # Already correct
  "kampong speu" = "kampong speu",           # Already correct
  "kandal" = "kandal",                       # Already correct
  "kracheh" = "kratie",                      # Correct to "kratie"
  "kampong thom" = "kampong thom",           # Already correct
  "kampong chhnang" = "kampong chhnang",     # Already correct
  "stueng traeng" = "stung treng",           # Correct to "stung treng"
  "svay rieng" = "svay rieng",               # Already correct
  "takaev" = "takeo",                        # Correct to "takeo"
  "prey veaeng" = "prey veng",               # Correct to "prey veng"
  "phnom penh" = "phnom penh",               # Already correct
  "rotanak kiri" = "ratanakiri",             # Correct to "ratanakiri"
  "ratanak kiri" = "ratanakiri",             # Correct to "ratanakiri"
  "krong preah sihanouk" = "preah sihanouk", # Correct to "preah sihanouk"
  "sihanoukville" = "preah sihanouk"         # Correct to "preah sihanouk"
)

# Using recode() to replace incorrect names with correct province names based on previous mapping
new.mine.df$Province <- recode(new.mine.df$Province, !!!province_names_corrections)
new.fear.df$Province <- recode(new.fear.df$Province, !!!province_names_corrections)

# Check the unique province names in each dataset after doing name correction
unique(new.mine.df$Province)
unique(new.fear.df$Province)

# Find common province names between both datasets
common_provinces <- intersect(unique(new.mine.df$Province), unique(new.fear.df$Province))
common_provinces


################# Data Visualization Analysis #################
# Spatial Plot Distribution of Incidents Overtime from cam.map
new.mine.df$Incident_D <- as.Date(new.mine.df$Incident_D) # Change data type for Incident_D into date type
new.mine.df$Year <- format(new.mine.df$Incident_D, "%Y") # Extract Year as character
view(new.mine.df) # Verify that a new column "Year" has been added

ggplot() + 
  geom_sf(data = cam.map) + # Plot the base map
  geom_sf(data = new.mine.df, 
          aes(color = Year),# Using "Year" variable for plotting
          alpha = 0.6) + # Overlaying incident data and sets the transparency level for my incident points
  scale_color_viridis_d(option = "plasma", direction = -1, name = "Incident Year") + # Use discrete color scale since my "Year" is categorical. Direction = -1 ensures that my colors are arranged in reverse order
  labs(title = "Incident Map", subtitle = "Spatial Distribution of Incidents Overtime") + # Adding main title and subtitle for my plot
  guides(fill = guide_legend(nrow = 1)) + # Adjust the fill legend
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"), # Sets the size and style of my title
        plot.subtitle = element_text(size = 12, face = "italic"), # Sets the size and style of my subtitle
        legend.position = "bottom", 
        legend.justification = "center", # Sets the position of my legend
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10), # Sets the size of my legend title and text
        panel.background = element_blank()) # customize legend position


# Provinces of Cambodia Map Overview
register_google(key = "AIzaSyAqaV9tLWSuhuWiIDhUJH1_1FdXkPe6LtI")
options(timeout = 600) # Increase timeout to 300 seconds

cambodia <- ggmap::geocode("Cambodia") # Obtaining Cambodia's location coordinates
cambodia
unique_provinces_df <- new.mine.df %>%
  distinct(Province, .keep_all = TRUE)

cambodia_map_terrain <- get_googlemap(center = c(lon = cambodia$lon, lat = cambodia$lat), zoom = 7, maptype = "terrain")

ggmap(cambodia_map_terrain) +
  geom_point(aes(x = Longitude, y = Latitude), 
             data = unique_provinces_df, 
             color = "blue", size = 3, alpha = 0.6) +  # Add transparency with alpha
  geom_text(aes(x = Longitude, y = Latitude, label = Province), data = unique_provinces_df, 
            hjust = 0.5, vjust = -1, size = 2, check_overlap = TRUE) +  # Reduce label size and avoid overlap
  ggtitle("Provinces of Cambodia with Longitude and Latitude") +
  theme(plot.title = element_text(size = 12, face = "bold"))


# Type of Devices that Caused Explosion in Cambodia
mine.df.province <- new.mine.df %>%
  group_by(Province) %>%
  summarise(Longitude = mean(Longitude), Latitude = mean(Latitude))
View(mine.df.province)

new.mine.df$devcolour <- ifelse(new.mine.df$DEVICES == "Mine", "Mine", "ERW")

ggmap(cambodia_map_terrain) +
  geom_point(data = new.mine.df, 
             mapping = aes(x = Longitude, y = Latitude, color = devcolour), 
             size = 3, alpha = 0.5) +
  geom_text_repel(data = mine.df.province, 
                  mapping = aes(x = Longitude, y = Latitude, label = Province),
                  colour = "darkblue", size = 3) +
  labs(title = "Type of Devices that Caused Explosion",
       x = "lon",
       y = "lat",
       color = "DEVICES") + 
  scale_color_manual(values = c("ERW" = "orange", "Mine" = "pink")) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5, margin = margin(r = 3)),
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5))


# Bar Plot to show Incident Count by Province
incident_count_by_province <- new.mine.df %>%
  group_by(Province) %>%
  summarise(incident_count = n())
View(incident_count_by_province)

incident_count_rearranged <- incident_count_by_province %>% # Rearrange incident count from highest to lowest count
  arrange(desc(incident_count))

ggplot(incident_count_rearranged, mapping = aes(x = reorder(Province, -incident_count), y = incident_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cambodia Incident Count by Province",
       x = "Province",
       y = "Incident Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

# Bar Plot to show the Distribution of Different Landmine Class
ggplot(new.fear.df, mapping = aes(x = Land_Class)) +
  geom_bar(fill = "brown", color = "black", alpha = 0.5) +
  labs(x = "Landmine Class", 
       y = "Frequency", 
       title = "Distribution of Different Landmine Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
  
  
# Heatmap with Facets:
heatmap_fear_landclass_proximity <- new.fear.df %>%
  group_by(Land_Class, Fear_Level, Proximity) %>%
  summarise(Count = n()) %>%  # Count occurrences for each combination
  ungroup()

unique(heatmap_fear_landclass_proximity$Proximity)

heatmap_fear_landclass_proximity$Proximity <- ifelse(heatmap_fear_landclass_proximity$Proximity == "(blank)", "Unknown", heatmap_fear_landclass_proximity$Proximity)
class(heatmap_fear_landclass_proximity$Proximity)

heatmap_fear_landclass_proximity$Proximity <- factor(heatmap_fear_landclass_proximity$Proximity, # Convert proximity from "chr" to "factor"
                                                     levels = c("Very Near", "Near", "Far", "Very Far", "Unknown"))

heatmap_fear_landclass_proximity <- heatmap_fear_landclass_proximity %>%
  filter(!is.na(Proximity)) %>% # Remove rows that contains a "NA" value in proximity column
  mutate_if(is.factor, droplevels)

unique(heatmap_fear_landclass_proximity$Proximity) # Check if NA value is removed

ggplot(heatmap_fear_landclass_proximity, mapping = aes(x = Land_Class, y = Fear_Level, fill = Count)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(x = "Land Class", 
       y = "Fear Level", 
       title = "Proximity vs Fear Level and Land Class") +
  facet_wrap(~ Proximity, scales = "free_x", ncol = 3) +  # Creates facets based on Proximity
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, hjust = 0.5),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        strip.title = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1, "lines"))


# Bubble Plot on Incident Severity - Scatter Plot with point size to represent Amputation
ggplot(new.mine.df, mapping = aes(x = INJURE, 
                                  y = KILLED, 
                                  size = AMPUTATION, 
                                  color = as.factor(AMPUTATION))) +
  geom_point(alpha = 0.5) +  # Slight transparency for overlapping points
  geom_smooth(aes(x = INJURE, y = KILLED), method = "lm", se = FALSE, color = "black", linetype = "dashed", inherit.aes = FALSE) + 
  scale_size_continuous(range = c(3, 8)) +  # Adjust bubble sizes
  scale_color_manual(values = c("#4575b4", "#91bfdb", "#fc8d59", "#fee08b")) +  # Color based on number of amputations
  labs(x = "Number of Injured Victims",
       y = "Number of Fatalities",
       size = "Amputation",
       title = "Relationship Between Injuries, Deaths, and Amputations in Explosive Incidents",
       color = "Amputation Level") +
  guides(size = "none",
         color = guide_legend(override.aes = list(size = 5))) +
  theme_minimal() +
  theme(legend.position = "right", # Move legend to the right for better visibility
        plot.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        panel.grid.major = element_line(size = 0.2, linetype = 'dashed'),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1, 1, 1.5, 1), "cm"))


# Using trend lines to view how Mine and ERW Incidents have evolved over time
incident_trends <- as.data.frame(new.mine.df) %>%
  group_by(Year, DEVICES) %>%
  summarise(TOTAL_INJURE = sum(INJURE),
            TOTAL_AMPUTATION = sum(AMPUTATION),
            TOTAL_KILLED = sum(KILLED)) %>%
  pivot_longer(cols = starts_with("TOTAL"),
               names_to = "CATEGORY",
               values_to = "Count")

View(incident_trends)

# Mine Incidents Trend
mine_incidents_trend <- ggplot(subset(incident_trends, DEVICES == "Mine")) + 
  geom_line(mapping = aes(x = Year, 
                          y = Count, 
                          group = CATEGORY,
                          color = CATEGORY),
            linewidth = 0.6) +
  labs(x = "Year",
       y = "Number of Victims",
       title = "Trend of Mine-Related Casualties") +
  scale_color_manual(values = c("TOTAL_INJURE" = "#56B4E9", "TOTAL_AMPUTATION" = "#009E73", "TOTAL_KILLED" = "#E69F00"),
                     labels = c("TOTAL_INJURE" = "Total Injure", "TOTAL_AMPUTATION" = "Total Amputation", "TOTAL_KILLED" = "Total Killed"),
                     name = "Casualty Suffered") +
  theme_clean() +
  theme(legend.position = "bottom", # Remove legend in this plot
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6, face = "bold"),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        panel.spacing = unit(0, "lines")) +
  coord_cartesian(ylim = c(0, 350)) # Sets consistent y -axis limits

mine_incidents_trend


# ERW Incidents Trend
ERW_incidents_trend <- ggplot(subset(incident_trends, DEVICES == "ERW")) + 
  geom_line(mapping = aes(x = Year, 
                          y = Count, 
                          group = CATEGORY,
                          color = CATEGORY),
            linewidth = 0.6) +
  labs(x = "Year",
       y = "Number of Victims",
       title = "Trend of ERW-Related Casualties") +
  scale_color_manual(values = c("TOTAL_INJURE" = "#56B4E9", "TOTAL_AMPUTATION" = "#009E73", "TOTAL_KILLED" = "#E69F00"),
                     labels = c("TOTAL_INJURE" = "Total Injure", "TOTAL_AMPUTATION" = "Total Amputation", "TOTAL_KILLED" = "Total Killed"),
                     name = "Casualties Suffered") +
  theme_clean() +
  theme(legend.position = "bottom", # Keeping the legend in this plot
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6, face = "bold"),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        panel.spacing = unit(0, "lines")) +
  coord_cartesian(ylim = c(0, 350)) # Sets consistent y -axis limits

ERW_incidents_trend

# Plotting Mine and ERW Casualties Together using grid.arrange()
grid.arrange(mine_incidents_trend, ERW_incidents_trend, nrow = 1)



# Incidents sort by Landuse
highest_landuse <- as.data.frame(new.mine.df) %>%
  group_by(Landuse) %>%
  summarise(TOTAL_VICTIM = sum(VICTIM),
            TOTAL_INJURE = sum(INJURE),
            TOTAL_AMPUTATION = sum(AMPUTATION),
            TOTAL_KILLED = sum(KILLED)) %>%
  arrange(TOTAL_VICTIM) %>%
  select(-TOTAL_VICTIM)

View(highest_landuse)
class(highest_landuse)

long_highest_landuse <- highest_landuse %>%
  pivot_longer(cols = starts_with("TOTAL"),
               names_to = "Casualty Type",
               values_to = "Value")

long_highest_landuse$Landuse <- fct_reorder(long_highest_landuse$Landuse, long_highest_landuse$Value, .fun = sum)

ggplot(long_highest_landuse, 
       mapping = aes(x = Landuse, y = Value, fill = `Casualty Type`)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(mapping = aes(label = ifelse(Value > 10, Value, "")),  # Only show labels for values greater than 10
            position = position_stack(vjust = 0.5), size = 2, color = "black") +
  coord_flip() +
  scale_fill_manual(values = c("TOTAL_INJURE" = "#56B4E9", "TOTAL_AMPUTATION" = "#009E73", "TOTAL_KILLED" = "#E69F00"),
                    labels = c("TOTAL_INJURE" = "Total Injure", "TOTAL_AMPUTATION" = "Total Amputation", "TOTAL_KILLED" = "Total Killed"),
                    name = "Casualty Type") + 
  labs(x = " ",
       y = "Number of Casualties", 
       title = "Casualty Type by Landuse in Cambodia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 8, face = "bold"),
        axis.text.y = element_text(size = 10))


# Plotting a box plot for distribution of victim counts by device type
victim_distribution <- as.data.frame(new.mine.df) %>%
  select(DEVICES, VICTIM)

View(victim_distribution)

ggplot(victim_distribution, mapping = aes(x = DEVICES, y = VICTIM)) +
  geom_boxplot() + # a tool that helps create box plot.
  stat_summary(fun = 'mean', geom = 'point', shape = 25, size = 3, fill = "orange") + # Highlighting the mean
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(x = "Devices",
       y = "Number of Victims",
       title = "Box Plot of Victims by Device Type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 10)))




#### Limitations

Possible limitations of these datasets is that the survey conducted on fear level is outdated. Based on the survey date, the survey was conducted way back to as long as 2009. Hence, it may not correctly reflect the current situation, which may limit the relevance of this analysis. As for the mine dataset, it only accounts for those who are killed, amputation and injured. It does not consider those that suffer from other health impacts or indirect consequences such as trauma or mental issues as a result of these explosives, which might be relevant to my research as well. 