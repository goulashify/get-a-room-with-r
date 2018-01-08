library(leaflet)
library(jsonlite)

###
# Visualization of free market long-term room rentals in Amsterdam.
# idea: opacity ~ time on market | to fade old "sold but not removed" ones?
###

# Constants.
ALLOWED_TYPES <- c("Kamer", "Studio")
SIZE_UPPER_LIMIT <- 30
COLOR_RANGE <- c("#1d0540", "#0f9b0f", "#15d915")
HIGHLIGHTED_COLOR_RANGE <- c("#5614B0", "#5614B0", "#9b5fed") # For "early bird" adverts.

# Reading in the JSON.
json_file <- "./data/example_rooms.json"
rooms <- fromJSON(json_file)

# Filtering the data.
rooms <- rooms[rooms$type %in% ALLOWED_TYPES, ]
rooms <- rooms[rooms$size <= SIZE_UPPER_LIMIT, ]

# Colors.
pricePal <- colorNumeric(rev(COLOR_RANGE), domain = rooms$price) # Rev, because we want cheaper ones to be green.
sizePal <- colorNumeric(COLOR_RANGE, domain = rooms$size) # Normal, because we want bigger ones to be green.

specialPricePal <- colorNumeric(rev(HIGHLIGHTED_COLOR_RANGE), domain = rooms$price)
specialSizePal <- colorNumeric(HIGHLIGHTED_COLOR_RANGE, domain = rooms$size)

valuesToColorBySpeciality <- function(is_special, values, pal, specialPal) {
  normalColors <- pal(values);
  specialColors <- specialPal(values);
  colors <- is_special;
  
  for(i in seq_along(is_special)) {
    if(is_special[i]) {
      colors[i] <- specialColors[i]; 
    } else {
      colors[i] <- normalColors[i];
    }
  }
  
  colors;
}

# Display the map.
# Fill: the greener the cheaper.
# Stroke: the greener the bigger.
# Size: grows with the room.
leaflet(data = rooms) %>% 
  addTiles() %>% 
  addCircleMarkers(
    ~lon,
    ~lat,
    color = ~valuesToColorBySpeciality(is_early_bird, size, sizePal, specialSizePal),
    fillColor = ~valuesToColorBySpeciality(is_early_bird, price, pricePal, specialPricePal),
    opacity = 1,
    fillOpacity = 1,
    radius = ~size*0.6,
    label = ~paste(as.character(price), " € - ",  as.character(size), "m²"),
    popup = ~url
  ) %>%
  addLegend(
    "bottomright",
    title = "Room Size (stroke color)",
    values = ~size,
    pal = sizePal,
    labFormat = labelFormat(suffix = "m²"),
    opacity = 1
  ) %>%
  addLegend(
    "bottomright",
    title = "Room Price (fill color)",
    values = ~price,
    pal = pricePal,
    labFormat = labelFormat(suffix = "€"),
    opacity = 1
  )

