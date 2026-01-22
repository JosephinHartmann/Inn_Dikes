# Load required packages
library(here)
library(ggplot2)

# Read data using here() for portable paths
available_specs <- read.csv2(
  here("data", "raw", "seed_mix_2.csv"),
  header = TRUE
)

# Create the plot, excluding type = "hemi"
ggplot(subset(available_specs, type != "hemi"), aes(x = log(seedmass), y = log(sla), color = as.factor(type))) +
  geom_point() +  # Add points
  stat_smooth(method = "lm", aes(group = as.factor(type)), se = FALSE, linetype = "dashed") +  # Add regression lines
  labs(
    x = "Log(Seed Mass)",
    y = "Log(SLA)",
    color = "type"
  ) +
  theme_minimal() +  # Minimal theme for better appearance
  theme(legend.position = "right")  # Place legend on the right

###
ggplot(available_specs, aes(x = log(seedmass), y = log(sla), color = as.factor(R1A))) +
  geom_point() +  # Add points
  stat_smooth(method = "lm", aes(group = as.factor(R1A)), se = FALSE, linetype = "dashed") +  # Add regression lines
  labs(
    x = "Log(Seed Mass)",
    y = "Log(SLA)",
    color = "R1A"
  ) +
  theme_minimal() +  # Minimal theme for better appearance
  theme(legend.position = "right")  # Place legend on the right

###
ggplot(available_specs, aes(x = log(seedmass), y = log(sla), color = as.factor(R22))) +
  geom_point() +  # Add points
  stat_smooth(method = "lm", aes(group = as.factor(R22)), se = FALSE, linetype = "dashed") +  # Add regression lines
  labs(
    x = "Log(Seed Mass)",
    y = "Log(SLA)",
    color = "R22"
  ) +
  theme_minimal() +  # Minimal theme for better appearance
  theme(legend.position = "right")  # Place legend on the right
