library(shiny)
library(jsonlite)
library(tidyverse)

prob_of_wearing_flip_flops <- function(variable = "temperature_2m", decision_threshold) {
  url <- paste0("https://ensemble-api.open-meteo.com/v1/ensemble?latitude=37.2296&longitude=-80.4139&hourly=",
                variable, "&timezone=America%2FNew_York&forecast_days=7&models=gfs_seamless")
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
      numericInput("decision_threshold", "Preferred Flop Temp:", value = 70, min = -100, max = 150, step = 1),
      actionButton("calculate", "Calculate Probability"),
      br(), br(),
      div(class = "center-table", tableOutput("probTable"))
  )
)

server <- function(input, output) {
  prob_result <- eventReactive(input$calculate, {
    prob_of_wearing_flip_flops(variable = "temperature_2m", decision_threshold = input$decision_threshold)
  })

  output$probTable <- renderTable({
    prob_result() |>
      mutate(prob_wear_flip_flops = as.integer(prob_wear_flip_flops)) |>
      select(`Date` = date_formatted, `Probability (%)` = prob_wear_flip_flops)
  })
}

shinyApp(ui = ui, server = server)
