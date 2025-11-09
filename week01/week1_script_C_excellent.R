# ============================================
# Financial Analysis: UniCo Plant Performance
# Week 1 Assignment: TOC Metrics Analysis
# Supply Chain Intern
# Date: January 2025
# ============================================
#
# PURPOSE:
# Analyze plant financial performance using Theory of Constraints metrics
# to determine if the plant is making money and identify concerning trends.
#
# METRICS CALCULATED:
# - Throughput (T): Rate of generating money through sales
# - Inventory (I): Money invested in materials
# - Operating Expense (OE): Money spent to convert inventory to throughput
# - Net Profit (NP): T - OE
# - Return on Investment (ROI): (T - OE) / I
#
# ============================================

# Load required libraries
library(readr)      # For reading CSV files

# ============================================
# PART 1: DATA IMPORT
# ============================================

cat("Loading data files...\n")

# Import financial data (revenue and costs)
financial_data <- read_csv("week01/financial_data.csv", 
                          show_col_types = FALSE)

# Import inventory data (RM, WIP, FG)
inventory_data <- read_csv("week01/inventory_levels.csv",
                          show_col_types = FALSE)

# Import production data (for context only)
production_data <- read_csv("week01/production_output.csv",
                           show_col_types = FALSE)

cat("Data loaded successfully.\n\n")

# Quick data validation
cat("Data validation:\n")
cat(sprintf("- Financial data: %d months\n", nrow(financial_data)))
cat(sprintf("- Inventory data: %d months\n", nrow(inventory_data)))
cat(sprintf("- Date range: Month %d to Month %d\n", 
            min(financial_data$month), max(financial_data$month)))
cat("\n")

# ============================================
# PART 2: CALCULATE TOC METRICS
# ============================================

cat("Calculating TOC metrics...\n\n")

# ---------------------------------------------
# THROUGHPUT (T)
# ---------------------------------------------
# Throughput = Sales revenue - Truly variable costs
# 
# IMPORTANT: Truly variable costs are ONLY costs that vary
# directly with each unit sold (typically just materials).
# Labor is NOT truly variable because we pay workers
# whether they produce or not.

financial_data$throughput <- financial_data$sales_revenue - 
                             financial_data$material_costs

# ---------------------------------------------
# INVENTORY (I)
# ---------------------------------------------
# Total value of all materials in the system
# Valued at purchase cost (no labor added)

inventory_data$total_inventory <- inventory_data$raw_materials_value + 
                                   inventory_data$wip_value + 
                                   inventory_data$finished_goods_value

# ---------------------------------------------
# OPERATING EXPENSE (OE)
# ---------------------------------------------
# All costs to run the operation except truly variable costs
# Includes: labor, overhead, utilities, depreciation

financial_data$operating_expense <- financial_data$labor_costs + 
                                     financial_data$overhead_costs + 
                                     financial_data$utilities + 
                                     financial_data$depreciation

# ============================================
# PART 3: MERGE DATASETS & CALCULATE DERIVED METRICS
# ============================================

# Merge financial and inventory data by month
analysis <- merge(financial_data, inventory_data, by = "month")

# Also merge production data for context
analysis <- merge(analysis, production_data, by = "month")

# ---------------------------------------------
# NET PROFIT (NP)
# ---------------------------------------------
# NP = Throughput - Operating Expense
# This is the money the plant makes after covering all expenses

analysis$net_profit <- analysis$throughput - analysis$operating_expense

# ---------------------------------------------
# RETURN ON INVESTMENT (ROI)
# ---------------------------------------------
# ROI = Net Profit / Inventory
# Measures how well we're using invested capital

analysis$roi <- (analysis$throughput - analysis$operating_expense) / 
                analysis$total_inventory

# ---------------------------------------------
# INVENTORY TURNS (annualized)
# ---------------------------------------------
# How many times per year we turn over our inventory
# Higher is generally better (cash not tied up)

analysis$inventory_turns <- (analysis$throughput * 12) / 
                            analysis$total_inventory

# ============================================
# PART 4: TREND ANALYSIS
# ============================================

# Calculate month-over-month changes
# Note: First month will have NA since no previous month exists
analysis$throughput_change <- c(NA, diff(analysis$throughput))
analysis$inventory_change <- c(NA, diff(analysis$total_inventory))
analysis$oe_change <- c(NA, diff(analysis$operating_expense))
analysis$np_change <- c(NA, diff(analysis$net_profit))

# Calculate percentage changes
analysis$throughput_pct_change <- c(NA, diff(analysis$throughput) / 
                                    head(analysis$throughput, -1) * 100)
analysis$inventory_pct_change <- c(NA, diff(analysis$total_inventory) / 
                                   head(analysis$total_inventory, -1) * 100)

# ============================================
# PART 5: GENERATE SUMMARY REPORT
# ============================================

cat("\n============================================\n")
cat("        UNICO PLANT FINANCIAL ANALYSIS       \n")
cat("============================================\n\n")

# Create summary table with key metrics
summary_table <- data.frame(
  Month = analysis$month,
  Throughput = sprintf("$%s", format(round(analysis$throughput), big.mark = ",")),
  Inventory = sprintf("$%s", format(round(analysis$total_inventory), big.mark = ",")),
  Operating_Exp = sprintf("$%s", format(round(analysis$operating_expense), big.mark = ",")),
  Net_Profit = sprintf("$%s", format(round(analysis$net_profit), big.mark = ",")),
  ROI = sprintf("%.1f%%", analysis$roi * 100)
)

print(summary_table, row.names = FALSE)

# ============================================
# PART 6: DETAILED ANALYSIS & INTERPRETATION
# ============================================

cat("\n\n============================================\n")
cat("        DETAILED ANALYSIS                    \n")
cat("============================================\n\n")

# Overall profitability
avg_np <- mean(analysis$net_profit)
cat("PROFITABILITY:\n")
if (avg_np > 0) {
  cat(sprintf("✓ The plant is making money (average monthly profit: $%s)\n",
              format(round(avg_np), big.mark = ",")))
} else {
  cat(sprintf("✗ The plant is LOSING money (average monthly loss: $%s)\n",
              format(round(abs(avg_np)), big.mark = ",")))
}

# Trend analysis: First month vs. Last month
first_month <- 1
last_month <- nrow(analysis)

cat("\nTRENDS (Month 1 vs Month 6):\n")

# Throughput trend
t_change <- analysis$throughput[last_month] - analysis$throughput[first_month]
t_pct <- (t_change / analysis$throughput[first_month]) * 100
if (t_change < 0) {
  cat(sprintf("✗ Throughput DECLINING: $%s (%+.1f%%)\n",
              format(round(t_change), big.mark = ","), t_pct))
} else {
  cat(sprintf("✓ Throughput improving: $%s (%+.1f%%)\n",
              format(round(t_change), big.mark = ","), t_pct))
}

# Inventory trend
i_change <- analysis$total_inventory[last_month] - analysis$total_inventory[first_month]
i_pct <- (i_change / analysis$total_inventory[first_month]) * 100
if (i_change > 0) {
  cat(sprintf("✗ Inventory GROWING: $%s (%+.1f%%)\n",
              format(round(i_change), big.mark = ","), i_pct))
} else {
  cat(sprintf("✓ Inventory declining: $%s (%+.1f%%)\n",
              format(round(i_change), big.mark = ","), i_pct))
}

# Operating Expense trend
oe_change <- analysis$operating_expense[last_month] - analysis$operating_expense[first_month]
oe_pct <- (oe_change / analysis$operating_expense[first_month]) * 100
if (oe_change > 0) {
  cat(sprintf("✗ Operating expenses INCREASING: $%s (%+.1f%%)\n",
              format(round(oe_change), big.mark = ","), oe_pct))
} else {
  cat(sprintf("✓ Operating expenses declining: $%s (%+.1f%%)\n",
              format(round(oe_change), big.mark = ","), oe_pct))
}

# Net Profit trend
np_change <- analysis$net_profit[last_month] - analysis$net_profit[first_month]
if (np_change < 0) {
  cat(sprintf("✗ Net profit DECLINING: $%s\n",
              format(round(np_change), big.mark = ",")))
} else {
  cat(sprintf("✓ Net profit improving: $%s\n",
              format(round(np_change), big.mark = ",")))
}

# ============================================
# PART 7: THE EFFICIENCY PARADOX
# ============================================

cat("\n============================================\n")
cat("        THE EFFICIENCY PARADOX              \n")
cat("============================================\n\n")

cat("Traditional Metrics (from production_output.csv):\n")
cat(sprintf("- Efficiency: %.0f%% (Month 1) → %.0f%% (Month 6) [IMPROVING]\n",
            production_data$efficiency_pct[1],
            production_data$efficiency_pct[6]))
cat(sprintf("- Utilization: %.0f%% (Month 1) → %.0f%% (Month 6) [IMPROVING]\n",
            production_data$utilization_pct[1],
            production_data$utilization_pct[6]))

cat("\nTOC Metrics:\n")
cat(sprintf("- Throughput: $%s (Month 1) → $%s (Month 6) [DECLINING]\n",
            format(round(analysis$throughput[1]), big.mark = ","),
            format(round(analysis$throughput[6]), big.mark = ",")))
cat(sprintf("- Net Profit: $%s (Month 1) → $%s (Month 6) [DECLINING]\n",
            format(round(analysis$net_profit[1]), big.mark = ","),
            format(round(analysis$net_profit[6]), big.mark = ",")))

cat("\nPARADOX: The plant looks more efficient by traditional metrics,\n")
cat("but is actually performing WORSE financially. This demonstrates why\n")
cat("local optimization (improving efficiency) doesn't guarantee global\n")
cat("optimization (making money).\n")

# ============================================
# PART 8: KEY CONCERNS & RECOMMENDATIONS
# ============================================

cat("\n============================================\n")
cat("        KEY CONCERNS                         \n")
cat("============================================\n\n")

concerns <- c()
if (t_change < 0) {
  concerns <- c(concerns, "Throughput declining despite improved efficiency")
}
if (i_change > 0) {
  concerns <- c(concerns, "Inventory growing - cash tied up in unsold products")
}
if (oe_change > 0) {
  concerns <- c(concerns, "Operating expenses creeping upward")
}
if (np_change < 0) {
  concerns <- c(concerns, "Overall profitability declining")
}

for (i in seq_along(concerns)) {
  cat(sprintf("%d. %s\n", i, concerns[i]))
}

cat("\n============================================\n")
cat("        RECOMMENDATIONS                      \n")
cat("============================================\n\n")

cat("1. URGENT: Investigate why throughput is declining\n")
cat("   - Are we losing customers?\n")
cat("   - Are we missing due dates?\n")
cat("   - Is there a bottleneck limiting our output?\n\n")

cat("2. Address inventory buildup\n")
cat("   - Growing WIP suggests production flow problems\n")
cat("   - Need to understand where materials are accumulating\n")
cat("   - Identify the constraint that's causing backup\n\n")

cat("3. Stop optimizing for efficiency\n")
cat("   - Efficiency is improving but profits are declining\n")
cat("   - Focus on throughput, not local efficiency\n")
cat("   - Need constraint-focused approach\n\n")

cat("4. Next steps\n")
cat("   - Map production flow to identify bottlenecks\n")
cat("   - Analyze customer order data (late deliveries?)\n")
cat("   - Review capacity constraints vs. demand\n")

cat("\n============================================\n")
cat("        END OF ANALYSIS                      \n")
cat("============================================\n\n")

# ============================================
# PART 9: SAVE RESULTS FOR FUTURE REFERENCE
# ============================================

# Save analysis results to CSV for further use
write_csv(analysis, "week1_analysis_results.csv")
cat("Analysis results saved to: week1_analysis_results.csv\n")

