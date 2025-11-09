# ============================================
# Week 3: Proof of Concept
# Manual Shortage Calculation for Part P-102
# ============================================

library(dplyr)
library(readr)

# Load data
parts <- read_csv("parts_master.csv")
inventory <- read_csv("current_inventory.csv")
capacity <- read_csv("capacity_data.csv")
receipts <- read_csv("scheduled_receipts.csv")
forecasts <- read_csv("demand_forecasts.csv")

# Select part to analyze
selected_part <- "P-102"

# Get starting inventory
starting_inv <- inventory %>%
  filter(part_number == selected_part) %>%
  pull(on_hand_qty)

cat(sprintf("Starting inventory for %s: %d units\n\n", selected_part, starting_inv))

# Get capacity data
capacity_wide <- capacity %>%
  filter(part_number == selected_part)

# Get demand data  
demand_wide <- forecasts %>%
  filter(part_number == selected_part)

# Get scheduled receipts
receipts_data <- receipts %>%
  filter(part_number == selected_part)

# Manual calculation for each month
months <- 1:6
beginning_inv <- starting_inv

cat("Month-by-Month Analysis:\n")
cat("========================\n\n")

for (month in months) {
  
  # Get scheduled receipts for this month
  receipts_this_month <- receipts_data %>%
    filter(receipt_month == month) %>%
    summarize(total = sum(quantity)) %>%
    pull(total)
  
  if(length(receipts_this_month) == 0) receipts_this_month <- 0
  
  # Get capacity
  capacity_this_month <- capacity_wide %>%
    pull(paste0("month_", month))
  
  # Get demand
  demand_this_month <- demand_wide %>%
    pull(paste0("month_", month))
  
  # Calculate supply
  total_supply <- beginning_inv + receipts_this_month + capacity_this_month
  
  # Calculate shortage
  shortage <- max(0, demand_this_month - total_supply)
  surplus <- max(0, total_supply - demand_this_month)
  
  # Print results
  cat(sprintf("Month %d:\n", month))
  cat(sprintf("  Beginning Inventory: %d\n", beginning_inv))
  cat(sprintf("  + Scheduled Receipts: %d\n", receipts_this_month))
  cat(sprintf("  + Production Capacity: %d\n", capacity_this_month))
  cat(sprintf("  = Total Supply: %d\n", total_supply))
  cat(sprintf("  - Demand: %d\n", demand_this_month))
  
  if(shortage > 0) {
    cat(sprintf("  = SHORTAGE: %d units ⚠️\n", shortage))
  } else {
    cat(sprintf("  = Surplus: %d units ✓\n", surplus))
  }
  
  # Update inventory for next month
  beginning_inv <- max(0, total_supply - demand_this_month)
  
  cat(sprintf("  Ending Inventory: %d\n\n", beginning_inv))
}

cat("Manual calculation complete.\n")
cat("Use these results to validate your Shiny app.\n")
