## ------------------------------------------------------------------------
library(shiny)
library(mapgl)
library(mapboxapi)
library(sf)
library(dplyr)
library(plotly)
library(lubridate)
library(ggplot2)
library(viridis)
library(tidyverse)
library(scales)


## ------------------------------------------------------------------------
kanewaisp_condata <- read.csv("data_conductivity_Calibrated_temp_AprJuly2025_KalauhaihaiGaragegs - data_conductivity_Calibrated_temp_AprJuly2025_KalauhaihaiGarage_21415543.csv")

kalaug_condata <- read.csv("~/Downloads/DS421-Carto-Design/data/kalauhaihai_garage_conductivity_data.csv")


## ------------------------------------------------------------------------
clean_kscd <- na.omit(kanewaisp_condata)

clean_kgcd <- na.omit(kalaug_condata)

names(clean_kgcd)[names(clean_kgcd) == "Date.Time..GMT.10.00"] <- "Date.Time"



## ------------------------------------------------------------------------
# MAPBOX_API_TOKEN=pk.eyJ1Ijoic2FtYW50aGFnaWJzb24iLCJhIjoiY21jdmd2c243MDBlYzJycHdjenkyaW5jdyJ9.QnSPG2e5GSR0VDO5bUJwdw

Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1Ijoic2FtYW50aGFnaWJzb24iLCJhIjoiY21jdmd2c243MDBlYzJycHdjenkyaW5jdyJ9.QnSPG2e5GSR0VDO5bUJwdw")
mb_access_token(Sys.getenv("MAPBOX_API_TOKEN"))


## ------------------------------------------------------------------------
kanewai <- mb_geocode("5975 HI-72, Honolulu, HI 96821", output = "sf")
kanewai$name <- "Kānewai Spring"
kanewai$description <- "Culturally significant freshwater spring near UH Mānoa."
kanewai_simple <- kanewai[, c("name", "description", "geometry")]

# Recreate Kalauhaʻihaʻi to match
kalauhaihai <- st_sf(
  name = "Kalauhaʻihaʻi (Lucas Spring)",
  description = "Loko iʻa on Maunalua Bay, fed by a lava tube spring.",
  geometry = st_sfc(st_point(c(-157.7286, 21.2750))),
  crs = 4326
)

# Combine — now same structure
springs <- rbind(kanewai_simple, kalauhaihai)

# Kānewai Spring via geocode
kanewai <- mb_geocode("5975 HI-72, Honolulu, HI 96821", output = "sf")
kanewai$name <- "Kānewai Spring"
kanewai$description <- "One of the last open freshwater springs naturally feeding into the ocean in Hawaiʻi"

# Kalauhaʻihaʻi manually geolocated between 5839–5841 Kalanianaʻole Hwy
kalauhaihai <- st_sf(
  name = "Kalauhaʻihaʻi (Lucas Spring)",
  description = "Loko iʻa on Maunalua Bay, fed by a lava tube spring.",
  geometry = st_sfc(st_point(c(-157.7286, 21.2750))),  # approximate
  crs = 4326
)

# Simplify kanewai to just the needed columns
kanewai_simple <- kanewai[, c("name", "description", "geometry")]

# Recreate kalauhaihai to exactly match
kalauhaihai <- st_sf(
  name = "Kalauhaʻihaʻi (Lucas Spring)",
  description = "Loko iʻa on Maunalua Bay, fed by a lava tube spring.",
  geometry = st_sfc(st_point(c(-157.7286, 21.2750))),  # lon, lat
  crs = 4326
)


# Combine springs
springs <- rbind(kanewai_simple, kalauhaihai)



## ------------------------------------------------------------------------
# Add datetime
clean_kscd$datetime <- as.POSIXct(clean_kscd$Date.Time, format = "%m/%d/%y %I:%M:%S %p")
clean_kgcd$datetime <- as.POSIXct(clean_kgcd$Date.Time, format = "%m/%d/%y %I:%M:%S %p")

# Add location
clean_kscd$location <- "Kānewai Spring"
clean_kgcd$location <- "Kalauhaʻihaʻi"

# Add king_tide
king_tide_dates <- as.Date(c("2025-04-29", "2025-06-23", "2025-07-21"))
clean_kscd$king_tide <- as.Date(clean_kscd$datetime) %in% king_tide_dates
clean_kgcd$king_tide <- as.Date(clean_kgcd$datetime) %in% king_tide_dates



## ------------------------------------------------------------------------
# Combine both datasets
all_data <- rbind(clean_kscd, clean_kgcd)



## ------------------------------------------------------------------------
# kanewai_simple is already created using mb_geocode + simplified
# kalauhaihai is manually constructed sf point

springs <- rbind(kanewai_simple, kalauhaihai)



## ------------------------------------------------------------------------

# Define number of days in your desired range
n_days <- as.integer(as.Date("2025-07-15") - as.Date("2025-06-01")) + 1  # 45 days

total_rows <- n_days * 2

# ---- Dummy Data ----
set.seed(123)

n_days <- as.integer(as.Date("2025-07-15") - as.Date("2025-06-01")) + 1

all_data <- data.frame(
  Date.Time = rep(seq.Date(from = as.Date("2025-06-01"),
                             to = as.Date("2025-07-15"),
                             by = "day"), times = 2),
  Specific.Conductance = runif(n_days * 2, 300, 700),
  Temp...C. = runif(n_days * 2, 20, 28),
  location = rep(c("Kānewai Spring", "Kalauhaʻehaʻe Fishpond"), each = n_days)
) %>%
  mutate(datetime = as.Date(Date.Time))  

kanewai_coords <- c(-157.72711, 21.28418)
kalauhaehae_coords <- c(-157.73188, 21.28226)

springs_sf <- st_as_sf(
  data.frame(
    name = c("Kānewai Spring", "Kalauhaʻehaʻe Fishpond"),
    description = c(
      "The beginning of Kānewai Spring, 5975 HI‑72, Honolulu, HI 96821",
      "Kalauhaʻihaʻi Fishpond (Lucas Pond), spring‑fed loko iʻa in Niu Valley"
    ),
    lon = c(kanewai_coords[1], kalauhaehae_coords[1]),
    lat = c(kanewai_coords[2], kalauhaehae_coords[2])
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

kanewai_isochrone <- mb_isochrone(kanewai_coords, profile = "driving", time = 20)


# ---- UI ----
ui <- fluidPage(
  tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap", rel = "stylesheet"),
  
  story_map(
    map_id = "spring_map",
    font_family = "Poppins",
    sections = list(
      
      "intro" = story_section(
        title = "Story Map: Kānewai & Kalauhaʻihaʻi Springs",
        content = list(
          p("Explore Native Hawaiian loko iʻa in East Honolulu with conductivity data and temperature data on the dates of recent king tides."),
          
          tags$div(
            style = "background-color: #f9f9f9; border-left: 5px solid #00727B; padding: 12px; margin-top: 15px; border-radius: 6px;",
            tags$p(
              style = "margin-bottom: 8px;",
              strong("What is Conductivity?"),
              "Specific conductance measures the water’s ability to carry an electrical current, which relates to the amount of dissolved salts or minerals present in the water. Monitoring conductivity helps identify changes in water quality that could affect aquatic life."
            ),
            tags$p(
              strong("Why Study Temperature?"),
              "Studying temperature works hand in hand with looking at conductivity data because it gives more insight towards understanding aspects of water quality. Warm water changes how fresh and salt water mix, affects oxygen levels, and can stress aquatic life which makes it an important factor when studying the effects of king tides on smaller bodies of water."
            ),
            tags$p(
              strong("What is a King Tide?"),
              "King tides are unusually high tide events caused by the combined effects of gravitational pull from the moon and sun. In Hawaiʻi, these tides can flood coastal areas, impact infrastructure, and influence water quality in springs and fishponds."
            ),
            tags$p(
        strong("Why Study These Data Points Together? "),
        "King tides can push seawater into springs and fishponds, temporarily raising conductivity. Having the ability to track temperature and conductivity in relation to king tide dates differentiates short-term tidal effects from long-term environmental changes like sea level rise."
      )
          )
        ),
        position = "center"
      ),
      
      "kanewai_marker" = story_section(
        title = "Kānewai Spring Location",
        content = list(
          p("Located at 5975 HI-72, Kānewai Spring: An important traditional freshwater source for the local community. The highlighted portion in yellow contains the dates of the most recent king tides. On June 24 peak conductivity data was collected. As shown on the chart below, Specific Conductance was recorded at 697.7µS/cm which the highest recorded conductivity reading in this dataset."),
          plotlyOutput("kanewai_plot", height = "350px", width = "500px")
        )
      ),
      
      "kalauhaihai_marker" = story_section(
        title = "Kalauhaʻihaʻi Fishpond",
        content = list(
          p("Kalauhaʻihaʻi: a spring-fed loko iʻa in Maunalua Bay with ongoing restoration efforts. Monitoring activities at this site continue to provide insight of the overall health of the fishpond. The highlighted portion similar to the graph above represents the dates of the most recent king tides that occurred on Oahu this summer. During the king tides the Specific Conductance was recorded between 624.9µS/cm and 617.7µS/cm from June 22nd to June 24th. This is unusual for this site as data from dates outside of the king tides vary from high readings to low readings. As an example on June 5th Specific Conductance was recorded at 643.1µS/cm and on June 6th it was recorded at 318.3µS/cm."),
          plotlyOutput("kalauhaihai_plot", height = "350px", width = "500px")
        )
      ),
      
      "kanewai_isochrone" = story_section(
        title = "Kānewai Spring Isochrone",
        content = list(
          p("Within a 20-minute drive of Kānewai Spring are key East Honolulu landmarks and communities. Combining historic and culturally significant sites along with lively residential communities this stretch of Oahu is essential to the daily lives of many residents in Honolulu. This part of Oʻahu is also characterized by its urban environment that supports the island’s main business district.")
        )
      ),
      
      "kalanianaole_highway" = story_section(
        title = "Kalanianaʻole Highway Segment",
        content = list(
          p("This segment of Kalanianaʻole Hwy highlights a key commuting route for many residents and visitors of Oahu. Historically, during the construction of this highway, the lava tube that feeds fresh water was damaged and could no longer provide fresh water to Kalauhaʻihaʻi. Over time, residential construction reshaped the original Kānewai Spring site, and privatization of the area eventually led to its decline and loss of some traditional functions. Restoration efforts include restoring the flow of freshwater to both sites in order to improve water quality.")
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  output$spring_map <- renderMapboxgl({
    mapboxgl(
      scrollZoom = FALSE,
      center = c(mean(st_coordinates(springs_sf)[, 1]), mean(st_coordinates(springs_sf)[, 2])),
      zoom = 13
    )
  })
  
  on_section("spring_map", "intro", {
    mapboxgl_proxy("spring_map") |>
      clear_layer("springs") |>
      clear_layer("isochrone") |>
      fly_to(
        center = c(mean(st_coordinates(springs_sf)[, 1]), mean(st_coordinates(springs_sf)[, 2])),
        zoom = 13,
        pitch = 0,
        bearing = 0
      )
  })
  
  on_section("spring_map", "kanewai_marker", {
    mapboxgl_proxy("spring_map") |>
      clear_layer("isochrone") |>
      clear_layer("kalauhaihai_layer") |>
      clear_layer("kanewai_layer") |>
      add_source("kanewai_source", data = springs_sf[1, ]) |>
      add_circle_layer(
        id = "kanewai_layer",
        source = "kanewai_source",
        circle_color = "#007",
        circle_radius = 12,
        circle_opacity = 0.8,
        popup = "description"
      ) |>
      fly_to(
        center = kanewai_coords,
        zoom = 16,
        pitch = 30,
        bearing = -15
      )
  })
  
  on_section("spring_map", "kalauhaihai_marker", {
    mapboxgl_proxy("spring_map") |>
      clear_layer("isochrone") |>
      clear_layer("kanewai_layer") |>
      clear_layer("kalauhaihai_layer") |>
      add_source("kalauhaihai_source", data = springs_sf[2, ]) |>
      add_circle_layer(
        id = "kalauhaihai_layer",
        source = "kalauhaihai_source",
        circle_color = "#007",
        circle_radius = 12,
        circle_opacity = 0.8,
        popup = "description"
      ) |>
      fly_to(
        center = kalauhaehae_coords,
        zoom = 15,
        pitch = 45,
        bearing = 0
      )
  })
  
  on_section("spring_map", "kanewai_isochrone", {
    mapboxgl_proxy("spring_map") |>
      clear_layer("kanewai_layer") |>
      clear_layer("kalauhaihai_layer") |>
      add_fill_layer(
        id = "isochrone_layer",
        source = kanewai_isochrone,
        fill_color = "#00727B",
        fill_opacity = 0.3
      ) |>
      fit_bounds(
        kanewai_isochrone,
        animate = TRUE,
        duration = 4000,
        pitch = 45
      )
  })
  
  on_section("spring_map", "kalanianaole_highway", {
  mapboxgl_proxy("spring_map") |>
    clear_layer("isochrone_layer") |>
    clear_layer("kanewai_layer") |>
    clear_layer("kalauhaihai_layer") |>
    fly_to(
      center = c(-157.7275, 21.2835),  # approximate center along Kalanianaʻole Hwy
      zoom = 15,
      pitch = 30,
      bearing = 0

    )
})

  
  # ---- Kanewai Plot ----
  output$kanewai_plot <- renderPlotly({
    site_data <- all_data %>%
      filter(
        location == "Kānewai Spring",
        datetime >= as.Date("2025-06-01"),
        datetime <= as.Date("2025-07-15")
      ) %>%
      arrange(datetime)
    
    req(nrow(site_data) > 0)
    
    plot_ly() %>%
      add_lines(
        x = ~site_data$datetime,
        y = ~site_data$Specific.Conductance,
        name = "Specific Conductance (μS/cm)",
        yaxis = "y1",
        line = list(color = "blue4", width = 1, dash = "solid")
      ) %>%
      add_lines(
        x = ~site_data$datetime,
        y = ~site_data$Temp...C.,
        name = "Temperature (°C)",
        yaxis = "y2",
        line = list(color = "coral2", width = 1, dash = "dot")
      ) %>%
      layout(
        title = "Conductivity & Temperature",
        xaxis = list(title = "Date & Time"),
        yaxis = list(title = "Specific Conductance (μS/cm)", side = "left", color = "blue4"),
        yaxis2 = list(title = "Temperature (°C)", overlaying = "y", side = "right", color = "coral2"),
        shapes = list(
          list(
            type = "rect",
            xref = "x",
            yref = "paper",
            x0 = "2025-06-22",
            x1 = "2025-06-25",
            y0 = 0,
            y1 = 1,
            fillcolor = "rgba(255, 215, 0, 0.2)",
            line = list(width = 0)
          )
        )
      )
  })
  
  # ---- Kalauhaihai Plot ----
  output$kalauhaihai_plot <- renderPlotly({
    site_data <- all_data %>%
      filter(
        location == "Kalauhaʻehaʻe Fishpond",
        datetime >= as.Date("2025-06-01"),
        datetime <= as.Date("2025-07-15")
      ) %>%
      arrange(datetime)
    
    req(nrow(site_data) > 0)
    
    plot_ly() %>%
      add_lines(
        x = ~site_data$datetime,
        y = ~site_data$Specific.Conductance,
        name = "Specific Conductance (μS/cm)",
        yaxis = "y1",
        line = list(color = "blue4", width = 1, dash = "solid")
      ) %>%
      add_lines(
        x = ~site_data$datetime,
        y = ~site_data$Temp...C.,
        name = "Temperature (°C)",
        yaxis = "y2",
        line = list(color = "coral2", width = 1, dash = "dot")
      ) %>%
      layout(
        title = "Conductivity & Temperature",
        xaxis = list(title = "Date & Time"),
        yaxis = list(title = "Specific Conductance (μS/cm)", side = "left", color = "blue4"),
        yaxis2 = list(title = "Temperature (°C)", overlaying = "y", side = "right", color = "coral2"),
        shapes = list(
          list(
            type = "rect",
            xref = "x",
            yref = "paper",
            x0 = "2025-06-22",
            x1 = "2025-06-25",
            y0 = 0,
            y1 = 1,
            fillcolor = "rgba(255, 215, 0, 0.2)",
            line = list(width = 0)
          )
        )
      )
  })
}

# Run app
shinyApp(ui, server)


