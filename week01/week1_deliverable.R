# ============================================
# Week 1: Financial Analysis for Alex Rogo
# Supply Chain Intern
# Date: [Current Date]
# ============================================

# Load required libraries
library(readr)      # For reading CSV files

# ============================================
# Part 1: Data Import
# ============================================

# Load financial data
financial_data <- read_csv("financial_data.csv")

# Load inventory data
inventory_data <- read_csv("inventory_levels.csv")

# Explore the data
head(financial_data)
str(financial_data)
summary(financial_data)

# ============================================
# Part 2: Calculate TOC Metrics
# ============================================

# Calculate Throughput (T)
# T = Sales revenue - Truly variable costs (materials only)
financial_data$throughput <- financial_data$sales_revenue - financial_data$material_costs

# Inventory (I) is already separated in inventory_data
# Calculate total inventory
inventory_data$total_inventory <- inventory_data$raw_materials_value + 
  inventory_data$wip_value + 
  inventory_data$finished_goods_value

# Calculate Operating Expense (OE)
# OE = All costs except truly variable material costs
financial_data$operating_expense <- financial_data$labor_costs + 
  financial_data$overhead_costs + 
  financial_data$utilities + 
  financial_data$depreciation

# ============================================
# Part 3: Derived Metrics
# ============================================

# Merge financial and inventory data
analysis <- merge(financial_data, inventory_data, by = "month")

# Calculate Net Profit
analysis$net_profit <- analysis$throughput - analysis$operating_expense

# Calculate ROI
analysis$roi <- (analysis$throughput - analysis$operating_expense) / analysis$total_inventory

# Calculate Inventory Turns (annualized)
analysis$inventory_turns <- (analysis$throughput * 12) / analysis$total_inventory

# ============================================
# Part 4: Trend Analysis
# ============================================

# Calculate month-over-month changes
# (Note: First month will be NA since no previous month)
analysis$throughput_change <- c(NA, diff(analysis$throughput))
analysis$inventory_change <- c(NA, diff(analysis$total_inventory))
analysis$oe_change <- c(NA, diff(analysis$operating_expense))
analysis$np_change <- c(NA, diff(analysis$net_profit))

# ============================================
# Part 5: Create Summary Table
# ============================================

# Select key columns for summary
summary_table <- analysis[, c("month", "throughput", "total_inventory", 
                              "operating_expense", "net_profit", "roi")]

# Round for readability
summary_table$throughput <- round(summary_table$throughput, 0)
summary_table$total_inventory <- round(summary_table$total_inventory, 0)
summary_table$operating_expense <- round(summary_table$operating_expense, 0)
summary_table$net_profit <- round(summary_table$net_profit, 0)
summary_table$roi <- round(summary_table$roi, 3)

# Display results
print("=== UniCo Plant Financial Analysis ===")
print(summary_table)

# ============================================
# Part 6: Interpretation
# ============================================

cat("\n=== INTERPRETATION ===\n\n")

cat("Based on the analysis of the past 6 months:\n\n")

cat("1. PROFITABILITY: ")
if(mean(analysis$net_profit) > 0) {
  cat("The plant is making money on average, but...\n")
} else {
  cat("The plant is LOSING money on average.\n")
}

cat("\n2. TREND: ")
if(analysis$net_profit[6] > analysis$net_profit[1]) {
  cat("Net profit is improving (Month 6 better than Month 1).\n")
} else {
  cat("Net profit is DECLINING (Month 6 worse than Month 1).\n")
}

cat("\n3. CONCERNS:\n")
if(mean(analysis$inventory_change, na.rm = TRUE) > 0) {
  cat("- Inventory is growing, which ties up cash and suggests we're overproducing.\n")
}
if(mean(analysis$throughput_change, na.rm = TRUE) < 0) {
  cat("- Throughput is declining, meaning we're generating less money from sales.\n")
}

cat("\n4. KEY QUESTION: Why is [insert main concern] happening? This needs further investigation.\n")
