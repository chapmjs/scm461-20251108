# ============================================
# Week 2: Scout Hike Simulation
# Dependent Events and Statistical Fluctuations
# ============================================

library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# ============================================
# Simulation Parameters
# ============================================

n_scouts <- 10
simulation_minutes <- 60
base_speeds <- c(rep(2.5, 5), rep(2.0, 4), 1.0)  # mph
speed_sd <- 0.3

# ============================================
# Run Simulation
# ============================================

# Initialize position matrix
positions <- matrix(0, nrow = simulation_minutes + 1, ncol = n_scouts)

# Simulate each minute
for (t in 1:simulation_minutes) {
  
  # Calculate desired speeds with variability
  desired_speeds <- rnorm(n_scouts, mean = base_speeds, sd = speed_sd)
  desired_speeds <- pmax(desired_speeds, 0.1)
  
  # Apply dependent events constraint
  actual_speeds <- numeric(n_scouts)
  actual_speeds[1] <- desired_speeds[1]
  
  for (scout in 2:n_scouts) {
    # Can't pass scout in front
    my_new_position <- positions[t, scout] + desired_speeds[scout] / 60
    scout_ahead_position <- positions[t, scout - 1]
    
    if (my_new_position > scout_ahead_position) {
      actual_speeds[scout] <- (scout_ahead_position - positions[t, scout]) * 60
    } else {
      actual_speeds[scout] <- desired_speeds[scout]
    }
  }
  
  # Update positions
  positions[t + 1, ] <- positions[t, ] + actual_speeds / 60
}

# ============================================
# Prepare Data
# ============================================

simulation_df <- data.frame()
for (scout in 1:n_scouts) {
  scout_data <- data.frame(
    time = 0:simulation_minutes,
    scout = scout,
    position = positions[, scout],
    is_herbie = (scout == n_scouts)
  )
  simulation_df <- rbind(simulation_df, scout_data)
}

# ============================================
# Visualization (INCOMPLETE)
# ============================================

# Plot 1: Positions over time
# PROBLEM: No title, poor labels, no legend positioning
ggplot(simulation_df, aes(x = time, y = position, group = scout, color = is_herbie)) +
  geom_line() +
  scale_color_manual(values = c("gray", "red"))
# Missing: title, axis labels, legend configuration, theme

# Plot 2: Final positions
# PROBLEM: Missing completely! Assignment requires 4 plots, only 1 provided
final_positions <- simulation_df %>% filter(time == simulation_minutes)
# TODO: Add bar chart here

# Plot 3 & 4: Also missing

# ============================================
# Analysis (INCOMPLETE)
# ============================================

# Some calculations but no interpretation
actual_distance <- min(final_positions$position)
avg_speed <- mean(base_speeds)
expected_distance <- avg_speed * 1

print(paste("Actual:", actual_distance))
print(paste("Expected:", expected_distance))

# MISSING: Detailed analysis answering the 5 required questions
# MISSING: Business interpretation
# MISSING: Connection to production systems
