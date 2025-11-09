# ============================================
# Week 2: Scout Hike Simulation
# Demonstrating Dependent Events & Statistical Fluctuations
# Supply Chain Intern
# ============================================

# Load required libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# ============================================
# PART 1: SIMULATION SETUP
# ============================================

# Parameters
n_scouts <- 10
simulation_minutes <- 60
base_speeds <- c(rep(2.5, 5), rep(2.0, 4), 1.0)  # mph for each scout
speed_sd <- 0.3  # Standard deviation for speed variability

# Initialize position tracking
positions <- matrix(0, nrow = simulation_minutes + 1, ncol = n_scouts)
# Row 1 = time 0 (start), Rows 2-61 = minutes 1-60

# ============================================
# PART 2: RUN SIMULATION
# ============================================

for (t in 1:simulation_minutes) {
  
  # For each scout, calculate desired speed (with variability)
  desired_speeds <- rnorm(n_scouts, mean = base_speeds, sd = speed_sd)
  
  # Make sure speeds are positive
  desired_speeds <- pmax(desired_speeds, 0.1)
  
  # Apply dependent events constraint
  # Scout 1 moves at his desired speed
  # Each subsequent scout moves at MIN(desired speed, speed of scout ahead)
  
  actual_speeds <- numeric(n_scouts)
  actual_speeds[1] <- desired_speeds[1]
  
  for (scout in 2:n_scouts) {
    # Can't go faster than scout in front
    # Speed of scout in front = (current position - previous position)
    # But we need to check current positions...
    
    # Actually, easier to think of it as: can't pass scout in front
    # So actual speed is limited by maintaining distance
    
    # If I would pass the scout in front, slow down to match them
    speed_if_no_constraint <- desired_speeds[scout]
    
    # Calculate where I'd end up
    my_new_position <- positions[t, scout] + speed_if_no_constraint / 60
    
    # Where is scout in front?
    scout_ahead_position <- positions[t, scout - 1]
    
    # Can't pass them
    if (my_new_position > scout_ahead_position) {
      # Slow down to match their position
      actual_speeds[scout] <- (scout_ahead_position - positions[t, scout]) * 60
    } else {
      actual_speeds[scout] <- speed_if_no_constraint
    }
  }
  
  # Update positions based on actual speeds
  # Speed in mph, convert to miles per minute
  positions[t + 1, ] <- positions[t, ] + actual_speeds / 60
}

# ============================================
# PART 3: PREPARE DATA FOR VISUALIZATION
# ============================================

# Convert matrix to long format data frame
time_seq <- 0:simulation_minutes
simulation_df <- data.frame()

for (scout in 1:n_scouts) {
  scout_data <- data.frame(
    time = time_seq,
    scout = scout,
    position = positions[, scout],
    is_herbie = (scout == n_scouts)
  )
  simulation_df <- rbind(simulation_df, scout_data)
}

# ============================================
# PART 4: VISUALIZATIONS
# ============================================

# Plot 1: Scout Positions Over Time
plot1 <- ggplot(simulation_df, aes(x = time, y = position, 
                                     group = scout, 
                                     color = is_herbie)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("gray60", "red"),
                     labels = c("Other Scouts", "Herbie"),
                     name = "") +
  labs(
    title = "Scout Positions Over Time",
    subtitle = "Notice how the line spreads out despite similar average speeds",
    x = "Time (minutes)",
    y = "Distance from Start (miles)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(plot1)

# Plot 2: Final Positions Bar Chart
final_positions <- simulation_df %>%
  filter(time == simulation_minutes) %>%
  mutate(scout_label = paste("Scout", scout))

plot2 <- ggplot(final_positions, aes(x = scout_label, y = position, 
                                       fill = is_herbie)) +
  geom_col() +
  scale_fill_manual(values = c("steelblue", "red"), guide = "none") +
  labs(
    title = "Final Scout Positions After 60 Minutes",
    subtitle = "Herbie (Scout 10) determines the system throughput",
    x = "Scout",
    y = "Distance Traveled (miles)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot2)

# Plot 3: Line Spread Over Time
line_spread <- simulation_df %>%
  group_by(time) %>%
  summarize(
    first_scout = max(position),
    last_scout = min(position),
    spread = first_scout - last_scout
  )

plot3 <- ggplot(line_spread, aes(x = time, y = spread)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(data = line_spread %>% filter(time == simulation_minutes),
             size = 4, color = "darkred") +
  labs(
    title = "How Far Apart Are First and Last Scout?",
    subtitle = "Gap grows over time due to statistical fluctuations",
    x = "Time (minutes)",
    y = "Distance Between First and Last Scout (miles)"
  ) +
  theme_minimal(base_size = 12)

print(plot3)

# Plot 4: System Throughput Comparison
avg_speed <- mean(base_speeds)
theoretical_distance <- avg_speed * (simulation_minutes / 60)
actual_distance <- min(final_positions$position)  # Limited by Herbie

throughput_comparison <- data.frame(
  scenario = c("Expected\n(Average Speed)", 
               "Actual\n(Limited by Herbie)"),
  distance = c(theoretical_distance, actual_distance)
)

plot4 <- ggplot(throughput_comparison, aes(x = scenario, y = distance, fill = scenario)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.2f miles", distance)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("lightblue", "coral"), guide = "none") +
  labs(
    title = "System Throughput: Expected vs. Actual",
    subtitle = "Dependent events + variability = lower throughput than average suggests",
    x = "",
    y = "Distance Traveled (miles)"
  ) +
  theme_minimal(base_size = 12) +
  ylim(0, max(throughput_comparison$distance) * 1.15)

print(plot4)

# ============================================
# PART 5: ANALYSIS
# ============================================

cat("\n========================================\n")
cat("        SIMULATION ANALYSIS              \n")
cat("========================================\n\n")

cat("SIMULATION RESULTS:\n\n")

cat(sprintf("1. Distance traveled by troop (Herbie): %.2f miles\n", 
            actual_distance))

cat(sprintf("2. Distance traveled by fastest scout: %.2f miles\n",
            max(final_positions$position)))

cat(sprintf("3. Line spread at end: %.2f miles\n",
            max(final_positions$position) - min(final_positions$position)))

cat(sprintf("4. Expected distance (based on average speed): %.2f miles\n",
            theoretical_distance))

cat(sprintf("5. Lost throughput: %.2f miles (%.1f%%)\n",
            theoretical_distance - actual_distance,
            ((theoretical_distance - actual_distance) / theoretical_distance) * 100))

cat("\nKEY INSIGHTS:\n\n")

cat("INSIGHT 1: System Throughput = Constraint Throughput\n")
cat("- The troop can only move as fast as Herbie (the constraint)\n")
cat("- Fast scouts can't compensate for slow scouts due to dependency\n")
cat("- In production: Plant output = bottleneck output\n\n")

cat("INSIGHT 2: Dependent Events + Statistical Fluctuations = Accumulation\n")
cat("- Even with average speeds, the line spreads out\n")
cat("- Parts accumulate (queue) before the constraint\n")
cat("- This is why WIP grows in production systems\n\n")

cat("INSIGHT 3: Improving Non-Constraints Doesn't Help\n")
cat("- If we speed up Scout 1 (fastest), does the troop go faster? NO.\n")
cat("- Herbie still limits the system\n")
cat("- In production: Improving non-bottlenecks doesn't increase throughput\n\n")

cat("INSIGHT 4: Only Improving the Constraint Helps\n")
cat("- If we speed up Herbie, the whole troop goes faster\n")
cat("- This is why we need to identify and focus on the constraint\n")
cat("- All improvement efforts should focus on the bottleneck\n\n")

cat("========================================\n")
