
library(dplyr)
library(lubridate)
library(leaflet)
library(ggplot2)

set.seed(123)  
sample_crimes <- data.frame(
  longitude = runif(1000, -87.8, -87.5),
  latitude = runif(1000, 41.6, 42.0),
  crime_type = sample(c("theft", "assault", "burglary", "robbery", "vandalism"),
                      1000, replace = TRUE),
  date = sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"),
                1000, replace = TRUE)
)

sample_crimes <- sample_crimes %>%
  mutate(
    month = lubridate::month(date, label = TRUE),
    hour = sample(0:23, 1000, replace = TRUE)
  )


head(sample_crimes)

leaflet() %>%
  addTiles() %>%
  setView(lng = -87.6298, lat = 41.8781, zoom = 10)



leaflet(sample_crimes) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude,
    popup = ~paste("Crime:", crime_type, "<br>Date:", date),
    radius = 3,
    color = "red",
    fillOpacity = 0.7
  ) %>%
  setView(lng = -87.6298, lat = 41.8781, zoom = 11)



library(leaflet.extras)

crime_heatmap <- leaflet(sample_crimes) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~longitude, 
    lat = ~latitude,
    blur = 20,
    max = 0.05,
    radius = 15
  ) %>%
  setView(lng = -87.6298, lat = 41.8781, zoom = 11)

crime_heatmap

crime_summary <- sample_crimes %>%
  count(crime_type, sort = TRUE)

print(crime_summary)

ggplot(crime_summary, aes(x = reorder(crime_type, n), y = n, fill = crime_type)) +
  geom_col() +
  coord_flip() +
  labs(title = "Crime Count by Type",
       x = "Crime Type", 
       y = "Number of Incidents") +
  theme_minimal()

## Monthly Patterns
monthly_crimes <- sample_crimes %>%
  count(month, crime_type)

ggplot(monthly_crimes, aes(x = month, y = crime_type, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Count") +
  labs(title = "Crime Patterns by Month",
       x = "Month", 
       y = "Crime Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dashboard_map <- leaflet(sample_crimes) %>%
  addTiles(group = "Street Map") %>%
  addHeatmap(
    lng = ~longitude, lat = ~latitude,
    group = "Heat Map",
    blur = 20, max = 0.05, radius = 15
  ) %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    color = ~case_when(
      crime_type == "theft" ~ "red",
      crime_type == "assault" ~ "blue",
      crime_type == "burglary" ~ "green",
      crime_type == "robbery" ~ "orange",
      TRUE ~ "purple"
    ),
    popup = ~paste("Type:", crime_type, "<br>Date:", date, "<br>Hour:", hour),
    group = "Crime Points",
    radius = 4
  ) %>%
  addLayersControl(
    overlayGroups = c("Heat Map", "Crime Points"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  setView(lng = -87.6298, lat = 41.8781, zoom = 11)

dashboard_map


