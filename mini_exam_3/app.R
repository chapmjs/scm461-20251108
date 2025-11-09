library(shiny)
library(tidyverse)
library(lubridate)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  
  titlePanel("Forecast Monitoring System"),
  
  tabsetPanel(
    # Tab 1: Accuracy/Bias Dashboard (Requirement #4)
    tabPanel("Accuracy Analysis",
             h3("Forecast Accuracy Comparison"),
             plotOutput("accuracy_plot"),
             hr(),
             h3("Forecast Bias Comparison"),
             plotOutput("bias_plot"),
             hr(),
             tableOutput("accuracy_summary")
    ),
    
    # Tab 2: Historical Retrieval (Requirement #5)
    tabPanel("Historical Review",
             sidebarLayout(
               sidebarPanel(
                 selectInput("historical_month",
                             "Select Forecast Month:",
                             choices = NULL)
               ),
               mainPanel(
                 h3(textOutput("historical_title")),
                 plotOutput("historical_plot"),
                 tableOutput("historical_table")
               )
             )
    ),
    
    # Tab 3: Decision Dashboard (Requirements #7, #8)
    tabPanel("Current Decision",
             h3(textOutput("current_period_title")),
             
             fluidRow(
               column(6,
                      h4("Current Forecasts vs. Historical Demand"),
                      plotOutput("current_forecast_plot")
               ),
               column(6,
                      h4("Forecast Comparison"),
                      tableOutput("current_forecast_table")
               )
             ),
             
             hr(),
             
             h3("Archive New Data"),
             p("Enter this month's forecasts and last month's actual demand:"),
             
             fluidRow(
               column(4,
                      numericInput("new_marketing_fc", 
                                   "Marketing Forecast (12-month):", 
                                   value = NULL)),
               column(4,
                      numericInput("new_computer_fc",
                                   "Computer Forecast (12-month):",
                                   value = NULL)),
               column(4,
                      numericInput("last_month_demand",
                                   "Last Month Actual Demand:",
                                   value = NULL))
             ),
             
             actionButton("archive", "Archive Data",
                         class = "btn-primary btn-lg")
    )
  )
)

server <- function(input, output, session) {
  
  # Load historical forecast data
  forecast_data <- reactiveVal()
  
  observe({
    forecast_data(read_csv("forecast_history.csv"))
  })
  
  # Current month
  current_month <- reactive({
    floor_date(Sys.Date(), "month")
  })
  
  # Calculate accuracy metrics by horizon (Requirement #2)
  accuracy_by_horizon <- reactive({
    data <- forecast_data()
    
    # For each forecast horizon (1-12 months)
    horizons <- 1:12
    
    map_df(horizons, function(h) {
      # Filter to rows where actual demand exists h months later
      horizon_data <- data %>%
        mutate(
          actual_date = forecast_date %m+% months(h),
          forecast_marketing_h = get(paste0("marketing_fc_", h)),
          forecast_computer_h = get(paste0("computer_fc_", h))
        ) %>%
        left_join(
          data %>% select(actual_date = forecast_date, actual_demand = actual),
          by = "actual_date"
        ) %>%
        filter(!is.na(actual_demand))
      
      # Calculate metrics
      marketing_metrics <- horizon_data %>%
        summarize(
          MAD = mean(abs(actual_demand - forecast_marketing_h)),
          MAPE = mean(abs((actual_demand - forecast_marketing_h) / actual_demand)) * 100,
          Bias = mean(actual_demand - forecast_marketing_h)
        ) %>%
        mutate(type = "Marketing", horizon = h)
      
      computer_metrics <- horizon_data %>%
        summarize(
          MAD = mean(abs(actual_demand - forecast_computer_h)),
          MAPE = mean(abs((actual_demand - forecast_computer_h) / actual_demand)) * 100,
          Bias = mean(actual_demand - forecast_computer_h)
        ) %>%
        mutate(type = "Computer", horizon = h)
      
      bind_rows(marketing_metrics, computer_metrics)
    })
  })
  
  # Accuracy plot (Requirement #4)
  output$accuracy_plot <- renderPlot({
    accuracy_by_horizon() %>%
      ggplot(aes(x = horizon, y = MAPE, color = type, group = type)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Marketing" = "blue", "Computer" = "red")) +
      labs(
        title = "Forecast Accuracy by Horizon",
        x = "Forecast Horizon (Months Ahead)",
        y = "MAPE (%)",
        color = "Forecast Type"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # Bias plot (Requirement #3)
  output$bias_plot <- renderPlot({
    accuracy_by_horizon() %>%
      ggplot(aes(x = horizon, y = Bias, color = type, group = type)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_color_manual(values = c("Marketing" = "blue", "Computer" = "red")) +
      labs(
        title = "Forecast Bias by Horizon",
        x = "Forecast Horizon (Months Ahead)",
        y = "Bias (Positive = Under-forecast)",
        color = "Forecast Type"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # Historical retrieval (Requirement #5)
  observe({
    months_available <- unique(forecast_data()$forecast_date)
    updateSelectInput(session, "historical_month",
                      choices = sort(months_available, decreasing = TRUE))
  })
  
  output$historical_plot <- renderPlot({
    req(input$historical_month)
    
    selected_date <- as.Date(input$historical_month)
    
    # Get 12-month forecasts from that date
    forecast_row <- forecast_data() %>%
      filter(forecast_date == selected_date)
    
    # Get actual demand for those 12 months
    future_months <- seq.Date(selected_date, by = "month", length.out = 12)
    
    actuals <- forecast_data() %>%
      filter(forecast_date %in% future_months) %>%
      select(month = forecast_date, actual = actual_demand)
    
    # Combine
    forecast_long <- tibble(
      month = future_months,
      Marketing = as.numeric(forecast_row[, paste0("marketing_fc_", 1:12)]),
      Computer = as.numeric(forecast_row[, paste0("computer_fc_", 1:12)])
    ) %>%
      pivot_longer(cols = c(Marketing, Computer),
                   names_to = "type",
                   values_to = "forecast") %>%
      left_join(actuals, by = "month")
    
    # Plot
    ggplot(forecast_long, aes(x = month)) +
      geom_line(aes(y = forecast, color = type, linetype = "Forecast"), size = 1) +
      geom_line(aes(y = actual, linetype = "Actual"), size = 1.5) +
      geom_point(aes(y = actual), size = 2) +
      scale_color_manual(values = c("Marketing" = "blue", "Computer" = "red")) +
      labs(
        title = paste("Forecasts Created in", format(selected_date, "%B %Y")),
        x = "Month",
        y = "Demand",
        color = "Forecast Type",
        linetype = ""
      ) +
      theme_minimal(base_size = 14)
  })
  
  # Current decision dashboard (Requirement #7)
  output$current_forecast_plot <- renderPlot({
    # Show all historical actuals + current 12-month forecasts
    
    historical <- forecast_data() %>%
      filter(forecast_date < current_month()) %>%
      select(month = forecast_date, actual = actual_demand)
    
    current_forecasts <- forecast_data() %>%
      filter(forecast_date == current_month()) %>%
      select(starts_with("marketing_fc_"), starts_with("computer_fc_"))
    
    future_months <- seq.Date(current_month(), by = "month", length.out = 12)
    
    forecast_long <- tibble(
      month = future_months,
      Marketing = as.numeric(current_forecasts[, paste0("marketing_fc_", 1:12)]),
      Computer = as.numeric(current_forecasts[, paste0("computer_fc_", 1:12)])
    ) %>%
      pivot_longer(cols = c(Marketing, Computer),
                   names_to = "type",
                   values_to = "forecast")
    
    ggplot() +
      geom_line(data = historical,
                aes(x = month, y = actual),
                size = 1, color = "black") +
      geom_point(data = historical,
                 aes(x = month, y = actual),
                 size = 2, color = "black") +
      geom_line(data = forecast_long,
                aes(x = month, y = forecast, color = type),
                size = 1, linetype = "dashed") +
      scale_color_manual(values = c("Marketing" = "blue", "Computer" = "red")) +
      labs(
        title = "Historical Demand + Current Forecasts",
        x = "Month",
        y = "Demand",
        color = "Forecast Type"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # Archive new data (Requirement #8)
  observeEvent(input$archive, {
    req(input$new_marketing_fc, input$new_computer_fc, input$last_month_demand)
    
    # Create new row with current month's forecasts
    new_row <- tibble(
      forecast_date = current_month(),
      marketing_fc_1 = input$new_marketing_fc,
      # ... (would include all 12 months of forecast)
      computer_fc_1 = input$new_computer_fc,
      # ... (would include all 12 months of forecast)
    )
    
    # Update last month's actual demand
    last_month <- current_month() %m-% months(1)
    updated_data <- forecast_data() %>%
      mutate(
        actual_demand = if_else(
          forecast_date == last_month,
          input$last_month_demand,
          actual_demand
        )
      ) %>%
      bind_rows(new_row)
    
    # Write back to file
    write_csv(updated_data, "forecast_history.csv")
    
    # Reload data
    forecast_data(updated_data)
    
    showNotification("Data archived successfully!", type = "message")
  })
}

shinyApp(ui, server)
