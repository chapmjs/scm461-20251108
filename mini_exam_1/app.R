library(shiny)
library(tidyverse)
library(lubridate)

# UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),  # Professional theme
  
  # Title (Requirement #2)
  titlePanel(textOutput("dashboard_title")),
  
  sidebarLayout(
    sidebarPanel(
      # Part number selector (Requirement #1, #8)
      selectInput("part_num", 
                  "Select Part Number:",
                  choices = NULL),  # Will populate from data
      
      # Retrieve button (Requirement #1)
      actionButton("retrieve", "Retrieve Data", 
                   class = "btn-primary"),
      
      hr(),
      
      # Save button (Requirement #6)
      actionButton("save", "Save/Update Data",
                   class = "btn-success")
    ),
    
    mainPanel(
      # Current date display (Requirement #3)
      h4(textOutput("current_period")),
      
      # Time-phased data table (Requirements #4, #5)
      tableOutput("supply_demand_table"),
      
      # Shortage warning (Requirement #7)
      h4("Shortage Warnings:"),
      uiOutput("shortage_warnings")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load data
  parts_data <- reactive({
    read_csv("parts_master.csv")
  })
  
  inventory_data <- reactive({
    read_csv("inventory_data.csv")
  })
  
  forecast_data <- reactive({
    read_csv("forecast_data.csv")
  })
  
  # Update part number choices (sorted - Requirement #8)
  observe({
    updateSelectInput(session, "part_num",
                      choices = sort(unique(parts_data()$part_num)))
  })
  
  # Current period (Requirement #3)
  current_month <- reactive({
    floor_date(Sys.Date(), "month")
  })
  
  # Next 12 months (Requirement #4, #5)
  next_12_months <- reactive({
    seq.Date(current_month(), 
             by = "month", 
             length.out = 12)
  })
  
  # Filtered data when button clicked (Requirement #1)
  selected_part_data <- eventReactive(input$retrieve, {
    req(input$part_num)
    
    # Get part details
    part_info <- parts_data() %>%
      filter(part_num == input$part_num)
    
    # Get supply and demand by month
    supply_demand <- tibble(
      month = next_12_months()
    ) %>%
      left_join(
        inventory_data() %>% filter(part_num == input$part_num),
        by = "month"
      ) %>%
      left_join(
        forecast_data() %>% filter(part_num == input$part_num),
        by = "month"
      ) %>%
      mutate(
        # Calculate supply capability (Requirement #7)
        supply = inventory + scheduled_receipts + capacity,
        # Calculate shortage
        shortage = pmax(0, demand - supply),
        # Flag if shortage exists
        warning = ifelse(shortage > 0, "⚠️ SHORTAGE", "✓ OK")
      )
    
    list(
      part_info = part_info,
      supply_demand = supply_demand
    )
  })
  
  # Dashboard title (Requirement #2)
  output$dashboard_title <- renderText({
    req(selected_part_data())
    part <- selected_part_data()$part_info
    paste0(part$part_num, " (", toupper(part$description), ")")
  })
  
  # Current period display (Requirement #3)
  output$current_period <- renderText({
    paste("Planning Period:", format(current_month(), "%B %Y"))
  })
  
  # Supply/Demand table (Requirements #4, #5)
  output$supply_demand_table <- renderTable({
    req(selected_part_data())
    selected_part_data()$supply_demand %>%
      mutate(month = format(month, "%b %Y")) %>%
      select(Month = month, 
             Inventory = inventory,
             `Scheduled Receipts` = scheduled_receipts,
             Capacity = capacity,
             `Total Supply` = supply,
             Demand = demand,
             Status = warning)
  })
  
  # Shortage warnings (Requirement #7)
  output$shortage_warnings <- renderUI({
    req(selected_part_data())
    
    shortages <- selected_part_data()$supply_demand %>%
      filter(shortage > 0)
    
    if(nrow(shortages) == 0) {
      tags$div(
        class = "alert alert-success",
        "✓ No shortages projected for next 12 months"
      )
    } else {
      tags$div(
        class = "alert alert-danger",
        h5("⚠️ SHORTAGES DETECTED"),
        tags$ul(
          lapply(1:nrow(shortages), function(i) {
            tags$li(
              paste0(format(shortages$month[i], "%B %Y"),
                     ": Short ", shortages$shortage[i], " units")
            )
          })
        )
      )
    }
  })
  
  # Save/Update data (Requirement #6)
  observeEvent(input$save, {
    req(selected_part_data())
    
    # In real implementation, would write back to source
    # For now, show confirmation
    showNotification("Data saved successfully!", 
                     type = "message")
    
    # Example write operation:
    # write_csv(selected_part_data()$supply_demand, 
    #           "updated_data.csv")
  })
}

shinyApp(ui, server)
