# ============================================
# Week 3: Shortage Detection Dashboard
# Interactive Shiny App for Supply Chain Planning
# ============================================

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)

# ============================================
# LOAD DATA (before UI/Server)
# ============================================

# Load all data files
parts <- read_csv("parts_master.csv")
inventory <- read_csv("current_inventory.csv")
capacity <- read_csv("capacity_data.csv")
receipts <- read_csv("scheduled_receipts.csv")
forecasts <- read_csv("demand_forecasts.csv")

# Create list of parts for dropdown
part_choices <- setNames(parts$part_number, 
                        paste(parts$part_number, "-", parts$description))

# ============================================
# USER INTERFACE
# ============================================

ui <- fluidPage(
  
  # Application title
  titlePanel("Supply Chain Shortage Detection Dashboard"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      
      h4("Select Part to Analyze"),
      
      # Dropdown to select part
      selectInput("part_number",
                  "Part Number:",
                  choices = part_choices),
      
      # Button to trigger analysis
      actionButton("retrieve",
                   "Retrieve Data",
                   class = "btn-primary",
                   style = "width: 100%;"),
      
      hr(),
      
      # Instructions
      h5("Instructions:"),
      p("1. Select a part number from the dropdown"),
      p("2. Click 'Retrieve Data' to analyze"),
      p("3. Review supply/demand table and warnings"),
      p("4. Red warnings indicate shortage months"),
      
      hr(),
      
      p("Supply = Inventory + Scheduled Receipts + Capacity"),
      p("Shortage = Demand - Supply (when negative)")
    ),
    
    # Main panel with outputs
    mainPanel(
      
      # Part title
      h3(textOutput("part_title")),
      
      # Shortage warnings (if any)
      uiOutput("shortage_warnings"),
      
      hr(),
      
      # Supply/Demand table
      h4("6-Month Supply/Demand Analysis"),
      tableOutput("analysis_table"),
      
      hr(),
      
      # Supply vs Demand plot
      h4("Supply vs. Demand Visualization"),
      plotOutput("supply_demand_plot", height = "400px")
    )
  )
)

# ============================================
# SERVER LOGIC
# ============================================

server <- function(input, output, session) {
  
  # ==========================================
  # REACTIVE: Calculate analysis when button clicked
  # ==========================================
  
  analysis_data <- eventReactive(input$retrieve, {
    
    # Get selected part
    selected_part <- input$part_number
    
    # Get starting inventory
    starting_inv <- inventory %>%
      filter(part_number == selected_part) %>%
      pull(on_hand_qty)
    
    # Get capacity (convert from wide to long)
    capacity_long <- capacity %>%
      filter(part_number == selected_part) %>%
      tidyr::pivot_longer(cols = starts_with("month_"),
                         names_to = "month",
                         values_to = "capacity",
                         names_prefix = "month_") %>%
      mutate(month = as.integer(month))
    
    # Get demand (convert from wide to long)
    demand_long <- forecasts %>%
      filter(part_number == selected_part) %>%
      tidyr::pivot_longer(cols = starts_with("month_"),
                         names_to = "month",
                         values_to = "demand",
                         names_prefix = "month_") %>%
      mutate(month = as.integer(month))
    
    # Get scheduled receipts
    receipts_data <- receipts %>%
      filter(part_number == selected_part) %>%
      select(month = receipt_month, scheduled_receipts = quantity)
    
    # Build analysis data frame
    analysis <- data.frame(month = 1:6)
    
    # Add capacity
    analysis <- analysis %>%
      left_join(capacity_long, by = "month") %>%
      left_join(demand_long, by = "month") %>%
      left_join(receipts_data, by = "month")
    
    # Replace NA with 0 for scheduled receipts
    analysis$scheduled_receipts[is.na(analysis$scheduled_receipts)] <- 0
    
    # Calculate period by period
    analysis$beginning_inventory <- 0
    analysis$total_supply <- 0
    analysis$shortage <- 0
    analysis$ending_inventory <- 0
    
    current_inv <- starting_inv
    
    for(i in 1:nrow(analysis)) {
      analysis$beginning_inventory[i] <- current_inv
      
      analysis$total_supply[i] <- current_inv + 
                                  analysis$scheduled_receipts[i] +
                                  analysis$capacity[i]
      
      analysis$shortage[i] <- max(0, analysis$demand[i] - analysis$total_supply[i])
      
      analysis$ending_inventory[i] <- max(0, analysis$total_supply[i] - analysis$demand[i])
      
      current_inv <- analysis$ending_inventory[i]
    }
    
    return(analysis)
  })
  
  # ==========================================
  # OUTPUT: Part title
  # ==========================================
  
  output$part_title <- renderText({
    req(input$retrieve)
    
    part_info <- parts %>%
      filter(part_number == input$part_number)
    
    paste(input$part_number, "-", part_info$description)
  })
  
  # ==========================================
  # OUTPUT: Shortage warnings
  # ==========================================
  
  output$shortage_warnings <- renderUI({
    req(analysis_data())
    
    data <- analysis_data()
    
    shortages <- data %>%
      filter(shortage > 0)
    
    if(nrow(shortages) == 0) {
      # No shortages
      tags$div(
        class = "alert alert-success",
        style = "background-color: #d4edda; border-color: #c3e6cb; color: #155724; padding: 15px; border-radius: 5px;",
        h4("✓ No Shortages Detected"),
        p("Adequate supply for all periods in planning horizon.")
      )
    } else {
      # Shortages exist
      tags$div(
        class = "alert alert-danger",
        style = "background-color: #f8d7da; border-color: #f5c6cb; color: #721c24; padding: 15px; border-radius: 5px;",
        h4("⚠️ SHORTAGES DETECTED"),
        p(paste("Shortages projected in", nrow(shortages), "month(s):")),
        tags$ul(
          lapply(1:nrow(shortages), function(i) {
            tags$li(
              sprintf("Month %d: SHORT %d units (Demand: %d | Supply: %d)",
                     shortages$month[i],
                     shortages$shortage[i],
                     shortages$demand[i],
                     shortages$total_supply[i])
            )
          })
        ),
        p(strong("ACTION REQUIRED:"), "Review capacity, expedite orders, or adjust production plan.")
      )
    }
  })
  
  # ==========================================
  # OUTPUT: Analysis table
  # ==========================================
  
  output$analysis_table <- renderTable({
    req(analysis_data())
    
    analysis_data() %>%
      mutate(
        Month = month,
        `Beg. Inv.` = beginning_inventory,
        `Sched. Rcpts` = scheduled_receipts,
        Capacity = capacity,
        `Total Supply` = total_supply,
        Demand = demand,
        `Shortage` = shortage,
        Status = ifelse(shortage > 0, "⚠️ SHORT", "✓ OK")
      ) %>%
      select(Month, `Beg. Inv.`, `Sched. Rcpts`, Capacity, 
             `Total Supply`, Demand, Shortage, Status)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ==========================================
  # OUTPUT: Supply vs Demand plot
  # ==========================================
  
  output$supply_demand_plot <- renderPlot({
    req(analysis_data())
    
    data <- analysis_data()
    
    # Create plot
    ggplot(data, aes(x = month)) +
      # Add shaded region where shortage exists
      geom_ribbon(aes(ymin = pmin(total_supply, demand),
                      ymax = demand),
                  fill = "red", alpha = 0.2) +
      # Supply line
      geom_line(aes(y = total_supply, color = "Total Supply"), 
                size = 1.5) +
      geom_point(aes(y = total_supply, color = "Total Supply"),
                 size = 3) +
      # Demand line
      geom_line(aes(y = demand, color = "Demand"),
                size = 1.5) +
      geom_point(aes(y = demand, color = "Demand"),
                 size = 3) +
      # Formatting
      scale_color_manual(values = c("Total Supply" = "blue", 
                                     "Demand" = "red"),
                        name = "") +
      labs(
        title = "Supply vs. Demand Over 6-Month Planning Horizon",
        subtitle = "Red shaded area indicates projected shortages",
        x = "Month",
        y = "Quantity (units)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "top",
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank()
      ) +
      scale_x_continuous(breaks = 1:6)
  })
}

# ============================================
# RUN THE APP
# ============================================

shinyApp(ui = ui, server = server)
