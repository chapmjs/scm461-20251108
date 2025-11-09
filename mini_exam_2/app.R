library(shiny)
library(tidyverse)
library(DT)  # For professional tables

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  
  titlePanel("Inventory Management Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Part selection
      selectInput("part_num", "Select Part:", choices = NULL),
      
      hr(),
      
      # Confidence level scrollbar (Requirement #2)
      sliderInput("confidence", 
                  "Service Level (Confidence):",
                  min = 0.50, 
                  max = 0.999,
                  value = 0.95,
                  step = 0.001),
      
      # Supply uncertainty toggle (Requirement #4)
      checkboxInput("include_supply_unc",
                    "Include Supply Uncertainty",
                    value = FALSE),
      
      hr(),
      
      # System parameters
      numericInput("order_cost", "Order Cost ($):", value = 10.25),
      numericInput("holding_pct", "Holding Cost (% of unit cost):", value = 5),
      numericInput("invest_rate", "Investment Return (%):", value = 4)
    ),
    
    mainPanel(
      tabsetPanel(
        # Dashboard tab
        tabPanel("Dashboard",
                 h3(textOutput("part_title")),
                 hr(),
                 
                 # Key metrics
                 fluidRow(
                   column(4, 
                          h4("Current Status"),
                          valueBoxOutput("current_dos")),
                   column(4,
                          h4("Safety Stock"),
                          valueBoxOutput("safety_stock_display")),
                   column(4,
                          h4("Reorder Point"),
                          valueBoxOutput("rop_display"))
                 ),
                 
                 hr(),
                 
                 # Purchase warning (Requirement #7)
                 uiOutput("purchase_warning"),
                 
                 # Order details if needed
                 conditionalPanel(
                   condition = "output.purchase_needed == true",
                   h4("Order Details:"),
                   tableOutput("order_details")
                 ),
                 
                 hr(),
                 
                 # Aggregate info (Requirement #10)
                 h4("Aggregate Purchase Requirements:"),
                 h3(textOutput("total_purchase_amount"))
        ),
        
        # Purchases Required sheet (Requirement #13)
        tabPanel("Purchases Required",
                 h3("Parts Requiring Purchase Orders"),
                 DTOutput("purchases_table"),
                 downloadButton("download_purchases", "Download Purchase List"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Load data
  parts_data <- reactive({
    read_csv("parts_inventory.csv")
  })
  
  # Update part choices
  observe({
    updateSelectInput(session, "part_num",
                      choices = unique(parts_data()$part_num))
  })
  
  # Selected part data
  selected_part <- reactive({
    req(input$part_num)
    parts_data() %>% filter(part_num == input$part_num)
  })
  
  # Calculate inventory parameters
  inventory_params <- reactive({
    part <- selected_part()
    
    # Z-score for confidence level (Requirement #3)
    z <- qnorm(input$confidence)
    
    # Annual holding cost
    annual_holding_cost <- (input$holding_pct/100 + input$invest_rate/100) * 
                          part$unit_cost
    
    # EOQ (Requirement #8)
    EOQ <- sqrt((2 * part$annual_demand * input$order_cost) / 
                annual_holding_cost)
    
    # Safety stock (Requirements #3, #4, #5)
    if(input$include_supply_unc) {
      # Include both demand and supply uncertainty
      safety_stock <- sqrt(
        (z * part$demand_std)^2 * part$lead_time +
        (part$avg_demand)^2 * part$lead_time_std^2
      )
    } else {
      # Demand uncertainty only
      safety_stock <- z * part$demand_std * sqrt(part$lead_time)
    }
    
    # ROP (Requirement #6)
    demand_during_LT <- part$avg_demand * part$lead_time
    ROP <- demand_during_LT + safety_stock
    
    # Purchase needed? (Requirement #7)
    purchase_needed <- part$on_hand <= ROP
    
    # Order quantity (Requirement #8)
    order_qty <- if(purchase_needed) EOQ else 0
    
    # Order $ amount (Requirement #9)
    order_amount <- order_qty * part$unit_cost
    
    # Days of supply (Requirement #11)
    daily_demand <- part$annual_demand / 365
    current_DOS <- part$on_hand / daily_demand
    DOS_after_order <- (part$on_hand + order_qty) / daily_demand  # Req #12
    
    list(
      z = z,
      EOQ = EOQ,
      safety_stock = safety_stock,
      ROP = ROP,
      purchase_needed = purchase_needed,
      order_qty = order_qty,
      order_amount = order_amount,
      current_DOS = current_DOS,
      DOS_after = DOS_after_order
    )
  })
  
  # Calculate for ALL parts
  all_parts_analysis <- reactive({
    parts_data() %>%
      rowwise() %>%
      mutate(
        # Repeat calculations for each part
        z = qnorm(input$confidence),
        safety_stock = if(input$include_supply_unc) {
          sqrt((z * demand_std)^2 * lead_time + 
               (avg_demand)^2 * lead_time_std^2)
        } else {
          z * demand_std * sqrt(lead_time)
        },
        ROP = avg_demand * lead_time + safety_stock,
        purchase_needed = on_hand <= ROP,
        annual_holding_cost = ((input$holding_pct + input$invest_rate)/100) * unit_cost,
        EOQ = sqrt((2 * annual_demand * input$order_cost) / annual_holding_cost),
        order_qty = if(purchase_needed) EOQ else 0,
        order_amount = order_qty * unit_cost
      ) %>%
      ungroup()
  })
  
  # Total $ needed (Requirement #10)
  output$total_purchase_amount <- renderText({
    total <- all_parts_analysis() %>%
      filter(purchase_needed) %>%
      summarize(total = sum(order_amount)) %>%
      pull(total)
    
    paste0("$", format(total, big.mark = ",", digits = 2))
  })
  
  # Purchase warning (Requirement #7)
  output$purchase_warning <- renderUI({
    if(inventory_params()$purchase_needed) {
      tags$div(
        class = "alert alert-warning",
        h4("⚠️ PURCHASE ORDER REQUIRED"),
        p(paste("Current inventory:", selected_part()$on_hand, "units")),
        p(paste("Reorder point:", round(inventory_params()$ROP), "units"))
      )
    } else {
      tags$div(
        class = "alert alert-success",
        "✓ Inventory level adequate - no purchase needed"
      )
    }
  })
  
  # Purchases Required table (Requirement #13)
  output$purchases_table <- renderDT({
    all_parts_analysis() %>%
      filter(purchase_needed) %>%
      select(`Part Number` = part_num,
             Description = description,
             `Qty to Order` = order_qty,
             `Unit Cost` = unit_cost,
             `Order Amount` = order_amount) %>%
      datatable(options = list(pageLength = 25))
  })
}

shinyApp(ui, server)
