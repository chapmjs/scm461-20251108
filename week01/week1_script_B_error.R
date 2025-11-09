# ============================================
# Week 1: Financial Analysis for Alex Rogo
# Supply Chain Intern Analysis
# ============================================

# Load required libraries
library(readr)

# ============================================
# Part 1: Import Data
# ============================================

# Load financial data
financial_data <- read_csv("financial_data.csv")
cat("Financial data loaded successfully.\n")

# Load inventory data
inventory_data <- read_csv("inventory_levels.csv")
cat("Inventory data loaded successfully.\n")

# Quick check of data structure
cat("\nData structure check:\n")
str(financial_data)
str(inventory_data)

# ============================================
# Part 2: Calculate TOC Metrics
# ============================================

# Calculate Throughput (T)
# T = Sales revenue minus truly variable costs
# NOTE: This calculation includes labor, which is INCORRECT
# Labor is an operating expense, not a truly variable cost
financial_data$throughput <- financial_data$sales_revenue - 
                             (financial_data$material_costs + 
                              financial_data$labor_costs)  # <-- ERROR HERE

# Calculate total inventory
inventory_data$total_inventory <- inventory_data$raw_materials_value + 
                                   inventory_data$wip_value + 
                                   inventory_data$finished_goods_value

# Calculate Operating Expense (OE)
# OE includes all costs to run the operation
# NOTE: Because we incorrectly included labor in T, 
# we should not include it here, but we do anyway - double error
financial_data$operating_expense <- financial_data$overhead_costs + 
                                     financial_data$utilities + 
                                     financial_data$depreciation
# Missing labor_costs here because we put it in throughput (wrong!)

# ============================================
# Part 3: Merge and Calculate Derived Metrics
# ============================================

# Merge datasets
analysis <- merge(financial_data, inventory_data, by = "month")

# Calculate Net Profit
analysis$net_profit <- analysis$throughput - analysis$operating_expense

# Calculate ROI
analysis$roi <- (analysis$throughput - analysis$operating_expense) / 
                analysis$total_inventory

# Calculate Inventory Turns (annualized)
analysis$inventory_turns <- (analysis$throughput * 12) / analysis$total_inventory

# ============================================
# Part 4: Create Summary Report
# ============================================

# Select key metrics
summary_report <- analysis[, c("month", "throughput", "total_inventory", 
                                "operating_expense", "net_profit", "roi")]

# Format for readability
summary_report$throughput <- round(summary_report$throughput, 0)
summary_report$total_inventory <- round(summary_report$total_inventory, 0)
summary_report$operating_expense <- round(summary_report$operating_expense, 0)
summary_report$net_profit <- round(summary_report$net_profit, 0)
summary_report$roi <- round(summary_report$roi, 3)

# Display results
cat("\n========================================\n")
cat("  UniCo Plant Financial Analysis\n")
cat("========================================\n\n")
print(summary_report, row.names = FALSE)

# ============================================
# Part 5: Interpretation
# ============================================

cat("\n========================================\n")
cat("  ANALYSIS SUMMARY\n")
cat("========================================\n\n")

# Calculate key trends
throughput_change <- analysis$throughput[6] - analysis$throughput[1]
inventory_change <- analysis$total_inventory[6] - analysis$total_inventory[1]
np_change <- analysis$net_profit[6] - analysis$net_profit[1]

cat("FINANCIAL HEALTH:\n")
cat(sprintf("- Average monthly net profit: $%s\n", 
            format(round(mean(analysis$net_profit)), big.mark = ",")))

cat("\nTRENDS (Month 6 vs Month 1):\n")
cat(sprintf("- Throughput change: $%s (%+.1f%%)\n", 
            format(round(throughput_change), big.mark = ","),
            (throughput_change / analysis$throughput[1]) * 100))
cat(sprintf("- Inventory change: $%s (%+.1f%%)\n", 
            format(round(inventory_change), big.mark = ","),
            (inventory_change / analysis$total_inventory[1]) * 100))
cat(sprintf("- Net profit change: $%s\n", 
            format(round(np_change), big.mark = ",")))

cat("\nKEY OBSERVATIONS:\n")
cat("The plant appears profitable based on these calculations, but the trend\n")
cat("is concerning. While efficiency metrics (see production_output.csv) are\n")
cat("improving, our financial metrics show declining throughput and rising\n")
cat("inventory. This suggests we're overproducing - making parts that aren't\n")
cat("selling, which ties up cash without generating revenue.\n")

cat("\nRECOMMENDATION:\n")
cat("We need to investigate why throughput is declining despite high efficiency.\n")
cat("The root cause analysis should focus on the constraint that's limiting our\n")
cat("ability to convert production into sales.\n")
