library(shiny)
library(jsonlite)
library(tidyverse)

city_coords <- tibble(
  city = c("New York", "Los Angeles", "Chicago", "Houston", "Miami",
           "Seattle", "Denver", "Boston", "San Francisco", "Washington D.C.",
           "Blacksburg", "Atlanta", "Knoxville", "Wilmington"),
  latitude = c(40.7128, 34.0522, 41.8781, 29.7604, 25.7617,
               47.6062, 39.7392, 42.3601, 37.7749, 38.9072,
               37.2296, 33.7490, 35.9606, 34.2257),
  longitude = c(-74.0060, -118.2437, -87.6298, -95.3698, -80.1918,
                -122.3321, -104.9903, -71.0589, -122.4194, -77.0369,
                -80.4139, -84.3880, -83.9207, -77.9447)
)

prob_of_wearing_flip_flops <- function(latitude, longitude, variable = "temperature_2m", decision_threshold) {
  url <- paste0("https://ensemble-api.open-meteo.com/v1/ensemble?latitude=", latitude,
                "&longitude=", longitude, "&hourly=", variable,
                "&timezone=America%2FNew_York&forecast_days=7&models=gfs_seamless")
  df <- read_json(url, simplifyVector = TRUE)

  make_ensemble_df <- function(ens_index, df, variable) {
    curr_ens <- df$hourly[[ens_index + 1]]
    time <- as_datetime(paste0(df$hourly$time, ":00"), tz = df$timezone)
    tibble(time = time, ensemble = ens_index, variable = variable, value = curr_ens)
  }

  combined_df <- map_dfr(1:31, make_ensemble_df, df, variable)

  celsius_to_fahr <- function(temp) (temp * 9/5) + 32

  format_date_without_suffix <- function(date) format(date, "%B %d")

  prob_df <- combined_df |>
    mutate(value = ifelse(variable == "temperature_2m", celsius_to_fahr(value), value)) |>
    filter(format(time, "%H:%M:%S") == "15:00:00") |>
    mutate(wear_flip_flops = ifelse(value > decision_threshold, 1, 0)) |>
    group_by(time) |>
    summarize(prob_wear_flip_flops = round(sum(wear_flip_flops) / n() * 100, 0), .groups = 'drop') |>
    mutate(date_formatted = format_date_without_suffix(time))

  return(prob_df)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('https://synergysportswear.com/cdn/shop/products/301ALTS0-SRBR2_700x.png?v=1589311347');
        background-size: cover;
        background-position: center;
        color: black;
        margin: 0;
        height: 100vh;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .content-box {
        width: 100%;
        max-width: 400px;
        background-color: rgba(255, 255, 255, 0.7);
        padding: 20px;
        border-radius: 10px;
        text-align: center;
      }
      h2 { color: black; }
      .shiny-input-container input {
        background-color: rgba(255, 255, 255, 0.6);
        border-radius: 5px;
      }
      .shiny-action-button {
        background-color: rgba(0, 123, 255, 0.7);
        border: none;
        color: white;
        padding: 10px 15px;
        border-radius: 5px;
        font-size: 16px;
      }
      .center-table {
        display: flex;
        justify-content: center;
        margin-top: 10px;
      }
      .center-table table {
        width: auto;
        text-align: center;
      }
      table {
        margin-top: 10px;
        border-collapse: collapse;
      }
      th, td {
        padding: 8px;
        text-align: center;
        border-bottom: 1px solid #ddd;
      }
      th {
        background-color: rgba(0, 123, 255, 0.8);
        color: white;
      }
    "))
  ),
  div(class = "content-box",
      h2("Flopability"),
      selectInput("city", "Select a City:", choices = city_coords$city),
      numericInput("decision_threshold", "Preferred Flop Temp:", value = 70, min = -100, max = 150, step = 1),
      actionButton("calculate", "Calculate Probability"),
      br(), br(),
      div(class = "center-table", tableOutput("probTable"))
  )
)

server <- function(input, output) {
  prob_result <- eventReactive(input$calculate, {
    selected_city <- filter(city_coords, city == input$city)
    prob_of_wearing_flip_flops(latitude = selected_city$latitude, longitude = selected_city$longitude,
                               decision_threshold = input$decision_threshold)
  })

  output$probTable <- renderTable({
    prob_result() |>
      mutate(prob_wear_flip_flops = as.integer(prob_wear_flip_flops)) |>
      select(`Date` = date_formatted, `Probability (%)` = prob_wear_flip_flops)
  })
}

shinyApp(ui = ui, server = server)
