# ============================================
# Scout Hike Simulation: Demonstrating TOC Principles
# Week 2 Assignment
# Supply Chain Intern Analysis
# ============================================
#
# PURPOSE:
# Simulate a scout hike to demonstrate how dependent events and
# statistical fluctuations lead to system accumulation and constraints.
# This mirrors what happens in production systems.
#
# KEY CONCEPTS DEMONSTRATED:
# 1. Dependent Events: Each scout depends on scout ahead (like production steps)
# 2. Statistical Fluctuations: Speeds vary randomly (like processing times)
# 3. Accumulation: Line spreads out over time (like WIP buildup)
# 4. Constraint: Slowest scout limits system (like bottleneck limits throughput)
#
# ============================================

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Set seed for reproducibility
set.seed(123)

# ============================================
# PART 1: SIMULATION PARAMETERS
# ============================================

cat("Setting up scout hike simulation...\n\n")

# Number of scouts in line
n_scouts <- 10

# Duration of hike (minutes)
simulation_minutes <- 60

# Base speed for each scout (miles per hour)
# Scouts 1-5: Fast (2.5 mph)
# Scouts 6-9: Medium (2.0 mph)  
# Scout 10 (Herbie): Slow (1.0 mph)
base_speeds <- c(rep(2.5, 5), rep(2.0, 4), 1.0)

# Standard deviation for speed variability
# Represents natural fluctuations in pace
speed_sd <- 0.3

cat("Simulation parameters:\n")
cat(sprintf("- Scouts: %d\n", n_scouts))
cat(sprintf("- Duration: %d minutes\n", simulation_minutes))
cat(sprintf("- Scout speeds: %s\n", paste(base_speeds, collapse = ", ")))
cat(sprintf("- Speed variability (SD): %.1f mph\n\n", speed_sd))

# ============================================
# PART 2: RUN SIMULATION
# ============================================

cat("Running simulation...\n")

# Initialize position tracking matrix
# Rows = time points (0 to 60 minutes)
# Columns = scouts (1 to 10)
positions <- matrix(0, nrow = simulation_minutes + 1, ncol = n_scouts)

# Track desired vs. actual speeds for analysis
speed_log <- list()

# Simulate each minute
for (t in 1:simulation_minutes) {
  
  # Generate desired speeds with random variability
  # Using normal distribution around base speed
  desired_speeds <- rnorm(n_scouts, mean = base_speeds, sd = speed_sd)
  
  # Ensure all speeds are positive (can't walk backwards)
  desired_speeds <- pmax(desired_speeds, 0.1)
  
  # Apply dependent events constraint
  # Each scout can only move as fast as scout in front
  actual_speeds <- numeric(n_scouts)
  
  # First scout (leader) is unconstrained
  actual_speeds[1] <- desired_speeds[1]
  
  # All other scouts are constrained by scout ahead
  for (scout in 2:n_scouts) {
    
    # Where would I end up if I moved at desired speed?
    my_new_position <- positions[t, scout] + desired_speeds[scout] / 60
    
    # Where is the scout in front of me?
    scout_ahead_position <- positions[t, scout - 1]
    
    # Can't pass the scout in front
    if (my_new_position > scout_ahead_position) {
      # Slow down to match their position
      actual_speeds[scout] <- max(0, (scout_ahead_position - positions[t, scout]) * 60)
    } else {
      # Move at desired speed
      actual_speeds[scout] <- desired_speeds[scout]
    }
  }
  
  # Update positions based on actual speeds
  # Convert mph to miles per minute (/60)
  positions[t + 1, ] <- positions[t, ] + actual_speeds / 60
  
  # Log speeds for analysis
  speed_log[[t]] <- data.frame(
    time = t,
    scout = 1:n_scouts,
    desired = desired_speeds,
    actual = actual_speeds
  )
}

cat("Simulation complete.\n\n")

# ============================================
# PART 3: PREPARE DATA FOR VISUALIZATION
# ============================================

cat("Preparing data for visualization...\n")

# Convert position matrix to long format data frame
simulation_df <- data.frame()

for (scout in 1:n_scouts) {
  scout_data <- data.frame(
    time = 0:simulation_minutes,
    scout = scout,
    scout_label = paste("Scout", scout),
    position = positions[, scout],
    is_herbie = (scout == n_scouts),
    base_speed = base_speeds[scout]
  )
  simulation_df <- rbind(simulation_df, scout_data)
}

# Calculate line spread over time
line_spread <- simulation_df %>%
  group_by(time) %>%
  summarize(
    leader_position = max(position),
    last_position = min(position),
    spread = leader_position - last_position,
    .groups = 'drop'
  )

# Final positions
final_positions <- simulation_df %>%
  filter(time == simulation_minutes)

cat("Data preparation complete.\n\n")

# ============================================
# PART 4: VISUALIZATION
# ============================================

cat("Creating visualizations...\n\n")

# Define color scheme
color_herbie <- "#d62728"  # Red
color_others <- "#7f7f7f"  # Gray
color_leader <- "#1f77b4"  # Blue

# -----------------------------------------------
# PLOT 1: Scout Positions Over Time
# -----------------------------------------------

plot1 <- ggplot(simulation_df, aes(x = time, y = position, 
                                     group = scout, 
                                     color = is_herbie,
                                     alpha = is_herbie)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("FALSE" = color_others, "TRUE" = color_herbie),
    labels = c("Other Scouts", "Herbie (Constraint)"),
    name = ""
  ) +
  scale_alpha_manual(
    values = c("FALSE" = 0.5, "TRUE" = 1),
    guide = "none"
  ) +
  labs(
    title = "Scout Positions Over Time: The Line Spreads Out",
    subtitle = "Despite similar average speeds, dependent events cause accumulation",
    x = "Time (minutes)",
    y = "Distance from Start (miles)",
    caption = "Each line represents one scout. Herbie (slowest) is highlighted in red."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top",
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

print(plot1)
ggsave("plot1_positions_over_time.png", plot1, width = 10, height = 6, dpi = 300)

# -----------------------------------------------
# PLOT 2: Final Positions Bar Chart
# -----------------------------------------------

plot2 <- ggplot(final_positions, aes(x = reorder(scout_label, -position), 
                                       y = position,
                                       fill = is_herbie)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.2f mi", position)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(
    values = c("FALSE" = "steelblue", "TRUE" = color_herbie),
    guide = "none"
  ) +
  labs(
    title = "Final Scout Positions After 60 Minutes",
    subtitle = "Herbie determines system throughput; fast scouts can't compensate",
    x = "Scout",
    y = "Distance Traveled (miles)",
    caption = "Notice: Fast scouts traveled much farther, but the troop is only as fast as Herbie"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(final_positions$position) * 1.15)

print(plot2)
ggsave("plot2_final_positions.png", plot2, width = 10, height = 6, dpi = 300)

# -----------------------------------------------
# PLOT 3: Line Spread Over Time
# -----------------------------------------------

max_spread <- max(line_spread$spread)
max_spread_time <- line_spread$time[which.max(line_spread$spread)]

plot3 <- ggplot(line_spread, aes(x = time, y = spread)) +
  geom_line(color = color_herbie, size = 1.5) +
  geom_area(fill = color_herbie, alpha = 0.2) +
  geom_point(data = line_spread %>% filter(time == simulation_minutes),
             size = 5, color = color_herbie) +
  annotate("text", 
           x = max_spread_time, 
           y = max_spread + 0.1,
           label = sprintf("Max spread: %.2f miles", max_spread),
           size = 5, fontface = "bold") +
  labs(
    title = "How the Line Spreads Out: Accumulation Over Time",
    subtitle = "Distance between first and last scout grows due to statistical fluctuations",
    x = "Time (minutes)",
    y = "Distance Between Leader and Herbie (miles)",
    caption = "This represents WIP accumulation in production: parts pile up before the constraint"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

print(plot3)
ggsave("plot3_line_spread.png", plot3, width = 10, height = 6, dpi = 300)

# -----------------------------------------------
# PLOT 4: System Throughput Comparison
# -----------------------------------------------

# Calculate expected vs. actual throughput
avg_speed <- mean(base_speeds)
theoretical_distance <- avg_speed * (simulation_minutes / 60)
actual_distance <- min(final_positions$position)  # Limited by Herbie
lost_throughput <- theoretical_distance - actual_distance

throughput_comparison <- data.frame(
  scenario = factor(c("Expected\n(Average Speed)", 
                      "Actual\n(Limited by Constraint)"),
                    levels = c("Expected\n(Average Speed)", 
                               "Actual\n(Limited by Constraint)")),
  distance = c(theoretical_distance, actual_distance),
  label = sprintf("%.2f miles", c(theoretical_distance, actual_distance))
)

plot4 <- ggplot(throughput_comparison, aes(x = scenario, y = distance, fill = scenario)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = label), 
            vjust = -0.7, size = 6, fontface = "bold") +
  annotate("segment",
           x = 1, xend = 2,
           y = actual_distance + 0.15, yend = actual_distance + 0.15,
           arrow = arrow(ends = "both", length = unit(0.3, "cm")),
           color = color_herbie, size = 1) +
  annotate("text",
           x = 1.5, y = actual_distance + 0.25,
           label = sprintf("Lost throughput:\n%.2f miles (%.0f%%)", 
                          lost_throughput, 
                          (lost_throughput/theoretical_distance)*100),
           size = 5, fontface = "bold", color = color_herbie) +
  scale_fill_manual(
    values = c("lightblue", "coral"),
    guide = "none"
  ) +
  labs(
    title = "System Throughput: Expected vs. Actual",
    subtitle = "Dependent events + variability = lower throughput than average suggests",
    x = "",
    y = "Distance Traveled (miles)",
    caption = "In production: Plant output is limited by bottleneck, not average capacity"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(throughput_comparison$distance) * 1.25)

print(plot4)
ggsave("plot4_throughput_comparison.png", plot4, width = 10, height = 6, dpi = 300)

cat("All visualizations created and saved.\n\n")

# ============================================
# PART 5: QUANTITATIVE ANALYSIS
# ============================================

cat("\n")
cat("============================================\n")
cat("     SIMULATION RESULTS & ANALYSIS         \n")
cat("============================================\n\n")

# Key metrics
leader_distance <- max(final_positions$position)
herbie_distance <- min(final_positions$position)
spread_final <- leader_distance - herbie_distance

cat("BASIC RESULTS:\n")
cat("──────────────────────────────────────────\n")
cat(sprintf("1. Distance traveled by troop (Herbie): %.2f miles\n", herbie_distance))
cat(sprintf("2. Distance traveled by leader: %.2f miles\n", leader_distance))
cat(sprintf("3. Line spread at finish: %.2f miles\n", spread_final))
cat(sprintf("4. Expected distance (avg speed): %.2f miles\n", theoretical_distance))
cat(sprintf("5. Lost throughput: %.2f miles (%.1f%% loss)\n\n",
            lost_throughput, (lost_throughput/theoretical_distance)*100))

# System throughput rate
system_throughput_mph <- herbie_distance / (simulation_minutes / 60)
expected_throughput_mph <- theoretical_distance / (simulation_minutes / 60)

cat("THROUGHPUT RATES:\n")
cat("──────────────────────────────────────────\n")
cat(sprintf("- Expected system throughput: %.2f mph\n", expected_throughput_mph))
cat(sprintf("- Actual system throughput: %.2f mph\n", system_throughput_mph))
cat(sprintf("- Herbie's average speed: %.2f mph\n", base_speeds[n_scouts]))
cat(sprintf("- System throughput ≈ Herbie's speed: %s\n\n",
            ifelse(abs(system_throughput_mph - base_speeds[n_scouts]) < 0.1, "YES ✓", "Close")))

# ============================================
# PART 6: ANSWERING KEY QUESTIONS
# ============================================

cat("ANSWERS TO KEY QUESTIONS:\n")
cat("==========================================\n\n")

cat("Q1: How far did the troop travel in 60 minutes?\n")
cat(sprintf("A1: %.2f miles (determined by Herbie, the constraint)\n\n", herbie_distance))

cat("Q2: How far apart were first and last scout at the end?\n")
cat(sprintf("A2: %.2f miles spread (%.1f%% of distance traveled)\n", 
            spread_final, (spread_final/leader_distance)*100))
cat("    This represents WIP accumulation in production systems.\n\n")

cat("Q3: What was system throughput vs. average scout speed?\n")
cat(sprintf("A3: System: %.2f mph | Average scout: %.2f mph | Difference: %.2f mph\n",
            system_throughput_mph, avg_speed, avg_speed - system_throughput_mph))
cat("    System throughput << average capability due to constraint.\n\n")

cat("Q4: If we improve Scout 1's (fastest) speed, would troop go faster?\n")
cat("A4: NO. Scout 1 is not the constraint. Herbie limits system throughput.\n")
cat("    Improving non-constraints doesn't increase system output.\n")
cat("    This is like improving a non-bottleneck work center in production.\n\n")

cat("Q5: If we improve Herbie's speed, would troop go faster?\n")
cat("A5: YES. Herbie is the constraint. Any improvement to Herbie directly\n")
cat("    improves system throughput. This is why TOC focuses on the bottleneck.\n\n")

# ============================================
# PART 7: BUSINESS IMPLICATIONS
# ============================================

cat("============================================\n")
cat("     IMPLICATIONS FOR PRODUCTION SYSTEMS   \n")
cat("============================================\n\n")

cat("KEY INSIGHT #1: Dependent Events Create Accumulation\n")
cat("─────────────────────────────────────────────────────\n")
cat("• Each production step depends on the previous step\n")
cat("• Parts can't 'skip ahead' when a step is slow\n")
cat("• This causes queues to form before constraints\n")
cat("• Result: WIP accumulation, long lead times\n\n")

cat("KEY INSIGHT #2: Statistical Fluctuations Compound\n")
cat("─────────────────────────────────────────────────────\n")
cat("• Processing times vary randomly (like scout speeds)\n")
cat("• 'Average' lead times are misleading\n")
cat("• Variability compounds through dependent steps\n")
cat("• Result: Actual lead times >> sum of average times\n\n")

cat("KEY INSIGHT #3: System Output = Constraint Output\n")
cat("─────────────────────────────────────────────────────\n")
cat("• The slowest point determines system throughput\n")
cat(sprintf("• Herbie at %.1f mph limited troop to %.2f mph\n", 
            base_speeds[n_scouts], system_throughput_mph))
cat("• In production: Bottleneck determines plant output\n")
cat("• Formula: System throughput = MIN(all resource capacities)\n\n")

cat("KEY INSIGHT #4: Focus Improvement on the Constraint\n")
cat("─────────────────────────────────────────────────────\n")
cat("• Improving non-constraints doesn't help (Scout 1 example)\n")
cat("• Only improving the constraint increases throughput\n")
cat("• This is the foundation of Theory of Constraints\n")
cat("• Practical rule: Identify bottleneck, maximize its output\n\n")

cat("KEY INSIGHT #5: Need Time Buffers for Protection\n")
cat("─────────────────────────────────────────────────────\n")
cat("• Can't eliminate variability (Murphy's Law applies)\n")
cat("• Need buffers to protect against fluctuations\n")
cat("• Buffer = time cushion before constraint\n")
cat("• Coming in Week 6-7: Drum-Buffer-Rope scheduling\n\n")

# ============================================
# PART 8: CONNECTION TO UNICO PLANT
# ============================================

cat("============================================\n")
cat("     CONNECTION TO UNICO PLANT             \n")
cat("============================================\n\n")

cat("What This Means for Alex's Plant:\n")
cat("─────────────────────────────────────────────────────\n")
cat("1. IDENTIFY THE CONSTRAINT\n")
cat("   - Find our 'Herbie' (bottleneck work center)\n")
cat("   - All improvement efforts should focus there\n")
cat("   - Don't waste time improving non-bottlenecks\n\n")

cat("2. EXPECT ACCUMULATION\n")
cat("   - WIP will naturally accumulate before bottleneck\n")
cat("   - This is normal in systems with variability\n")
cat("   - Can't eliminate it, but can manage it with buffers\n\n")

cat("3. PLAN FOR VARIABILITY\n")
cat("   - Can't use average processing times in MRP\n")
cat("   - Need time buffers to protect against fluctuations\n")
cat("   - Safety stock serves similar purpose for materials\n\n")

cat("4. RETHINK SUCCESS METRICS\n")
cat("   - High efficiency on non-bottlenecks is meaningless\n")
cat("   - Better: non-bottlenecks have slack capacity\n")
cat("   - Focus: throughput through the bottleneck\n\n")

# ============================================
# PART 9: RECOMMENDATIONS
# ============================================

cat("============================================\n")
cat("     RECOMMENDATIONS FOR NEXT STEPS        \n")
cat("============================================\n\n")

cat("For Bob (Production Manager):\n")
cat("─────────────────────────────────────────────────────\n")
cat("1. Map production flow to identify the constraint\n")
cat("2. Stop optimizing non-constraint resources\n")
cat("3. Implement time buffers before the bottleneck\n")
cat("4. Focus all improvement efforts on the bottleneck\n\n")

cat("For Stacey (Materials Manager):\n")
cat("─────────────────────────────────────────────────────\n")
cat("1. Expect WIP to accumulate before the bottleneck\n")
cat("2. Don't release materials too early (creates excess WIP)\n")
cat("3. Coordinate material release with bottleneck schedule\n")
cat("4. Implement 'rope' to control material flow\n\n")

cat("For Alex (Plant Manager):\n")
cat("─────────────────────────────────────────────────────\n")
cat("1. This simulation validates Jonah's insights\n")
cat("2. Need systematic constraint identification (Week 3-4)\n")
cat("3. Need to implement DBR scheduling (Week 6-7)\n")
cat("4. Need to educate team on TOC principles\n\n")

cat("============================================\n")
cat("     END OF ANALYSIS                       \n")
cat("============================================\n\n")

# ============================================
# PART 10: SAVE RESULTS
# ============================================

# Save simulation results
write.csv(simulation_df, "week2_simulation_results.csv", row.names = FALSE)
write.csv(line_spread, "week2_line_spread.csv", row.names = FALSE)

# Save summary statistics
summary_stats <- data.frame(
  metric = c("System_Throughput_mph", "Expected_Throughput_mph", 
             "Lost_Throughput_pct", "Final_Spread_miles",
             "Herbie_Distance", "Leader_Distance"),
  value = c(system_throughput_mph, expected_throughput_mph,
            (lost_throughput/theoretical_distance)*100, spread_final,
            herbie_distance, leader_distance)
)
write.csv(summary_stats, "week2_summary_statistics.csv", row.names = FALSE)

cat("Results saved to CSV files for future reference.\n")
cat("\nSimulation complete. All outputs generated successfully.\n")

