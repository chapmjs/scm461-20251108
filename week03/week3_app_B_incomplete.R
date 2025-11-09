# ============================================
# Week 3: Shortage Detection Dashboard
# (Incomplete Version)
# ============================================

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)

# Load data
parts <- read_csv("parts_master.csv")
inventory <- read_csv("current_inventory.csv")
capacity <- read_csv("capacity_data.csv")
receipts <- read_csv("scheduled_receipts.csv")
forecasts <- read_csv("demand_forecasts.csv")

# UI
ui <- fluidPage(
  titlePanel("Shortage Detection Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("part", "Select Part:", 
                  choices = parts$part_number),
      actionButton("go", "Retrieve Data")
    ),
    
    mainPanel(
      h3(textOutput("title")),
      tableOutput("table"),
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  # PROBLEM: Calculation logic is incorrect
  data <- eventReactive(input$go, {
    
    selected <- input$part
    
    # Get capacity
    cap <- capacity %>% filter(part_number == selected)
    
    # Get demand
    dem <- forecasts %>% filter(part_number == selected)
    
    # INCORRECT: Just comparing capacity to demand
    # Missing: inventory, scheduled receipts, period-by-period logic
    
    result <- data.frame(
      month = 1:6,
      capacity = as.numeric(cap[1, 2:7]),
      demand = as.numeric(dem[1, 2:7])
    ) %>%
      mutate(shortage = demand - capacity)  # WRONG LOGIC
    
    return(result)
  })
  
  output$title <- renderText({
    paste("Analysis for", input$part)
  })
  
  output$table <- renderTable({
    data()
  })
  
  # PROBLEM: No shortage warnings!
  # PROBLEM: Plot exists but is basic, no formatting
  
  output$plot <- renderPlot({
    ggplot(data(), aes(x = month)) +
      geom_line(aes(y = capacity)) +
      geom_line(aes(y = demand))
    # Missing: colors, labels, legend, shading
  })
}

shinyApp(ui, server)
