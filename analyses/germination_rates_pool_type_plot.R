# Calculate Average Germination Rate per Sown Plot
# This script combines species germination rates with plot-level species ratios
# to calculate the expected average germination rate per plot

library(here)
library(dplyr)
library(readr)

# ============================================================================
# PART 1: LOAD DATA
# ============================================================================

# Load germination rates per species (from germination_rates.R output)
germination_rates <- read_csv(
  here("output", "germination_rates_per_species.csv"),
  col_types = cols(
    species_name = col_character(),
    species_ID = col_character(),
    germination_rate_AB = col_double(),
    germination_rate_CD = col_double(),
    long_strat_germination_rate = col_double(),
    germination_rate_100 = col_double()
  )
)

cat("Loaded germination rates for", nrow(germination_rates), "species\n")
cat("Species with germination_rate_100:", sum(!is.na(germination_rates$germination_rate_100)), "\n\n")

# Load plot species composition data (ratios per plot)
# This file has a complex structure:
# - Rows 1-6: header/metadata (id_plot, pool, trait_constraint, seedmix_type, timepoint, year)
# - Rows 7+: species names with ratios (proportions) per plot (columns = plots)
Inn_Dike_species_raw <- read.csv2(
  here("data", "raw", "Inn_Dike_species.csv"),
  row.names = 1,              # use first column as rownames
  stringsAsFactors = FALSE,   # prevent automatic factor conversion
  check.names = FALSE         # preserve original column names
)

cat("Loaded plot species data:", nrow(Inn_Dike_species_raw), "rows x", ncol(Inn_Dike_species_raw), "columns\n")

# ============================================================================
# PART 2: FILTER BY YEAR 
# ============================================================================

# Extract header/metadata (first 6 rows) to check year values
plot_metadata_temp <- Inn_Dike_species_raw[1:6, , drop = FALSE]
rownames(plot_metadata_temp) <- c("id_plot", "pool", "trait_constraint", "seedmix_type", "timepoint", "year")

# Filter plots: keep only those where year == "0" (seeded plots)
year_row <- plot_metadata_temp["year", , drop = TRUE]
plots_to_keep <- which(year_row == "0" | year_row == 0)

cat("Total plots before filtering:", ncol(plot_metadata_temp), "\n")
cat("Plots with year == '0':", length(plots_to_keep), "\n")

# Check pool distribution before filtering
pool_before <- table(plot_metadata_temp["pool", plots_to_keep, drop = TRUE], useNA = "ifany")
cat("\nPool distribution (year == 0):\n")
print(pool_before)

# Filter data to only include selected plots (year == 0)
Inn_Dike_species_raw <- Inn_Dike_species_raw[, plots_to_keep, drop = FALSE]

cat("\nFiltered data:", nrow(Inn_Dike_species_raw), "rows x", ncol(Inn_Dike_species_raw), "columns\n\n")

# ============================================================================
# PART 3: EXTRACT PLOT METADATA AND SPECIES RATIOS
# ============================================================================

# Extract header/metadata (first 6 rows) - now filtered
plot_metadata <- Inn_Dike_species_raw[1:6, , drop = FALSE]
rownames(plot_metadata) <- c("id_plot", "pool", "trait_constraint", "seedmix_type", "timepoint", "year")

# Check pool distribution after filtering
pool_after <- table(plot_metadata["pool", , drop = TRUE], useNA = "ifany")
cat("Pool distribution after filtering:\n")
print(pool_after)
cat("\n")

# Store plot pools for later use in debugging
plot_pools <- plot_metadata["pool", , drop = TRUE]
names(plot_pools) <- colnames(plot_metadata)

# Store plot pools for later use in debugging
plot_pools <- plot_metadata["pool", , drop = TRUE]
names(plot_pools) <- colnames(plot_metadata)

# Extract species data (rows 7 onwards) - now filtered
species_ratios_raw <- Inn_Dike_species_raw[7:nrow(Inn_Dike_species_raw), , drop = FALSE]

# IMPORTANT: Keep original column names (these will match plot_metadata_df$plot_id after transpose)
# The original column names are preserved and will become rownames when we transpose plot_metadata
original_colnames <- colnames(species_ratios_raw)

# Convert species ratios to numeric
# Store original species names (row names)
species_names_from_ratios <- rownames(species_ratios_raw)

# Convert all values to numeric
species_ratios <- as.data.frame(
  lapply(species_ratios_raw, function(x) as.numeric(as.character(x))),
  stringsAsFactors = FALSE
)
rownames(species_ratios) <- species_names_from_ratios
# Keep original column names for matching (these match rownames after transpose)
colnames(species_ratios) <- original_colnames

cat("Extracted species ratios for", nrow(species_ratios), "species across", ncol(species_ratios), "plots\n\n")

# ============================================================================
# PART 4: MATCH SPECIES NAMES AND PREPARE FOR CALCULATION
# ============================================================================

# Check species name matching between germination rates and plot data
cat("=== Species Name Matching ===\n")
cat("Species in germination rates:", nrow(germination_rates), "\n")
cat("Species in plot data:", nrow(species_ratios), "\n")

# Find matching species
# Note: Species names might need cleaning/normalization
# For now, we'll try direct matching and see what matches

# Create a lookup: species name -> germination_rate_100
germination_lookup <- germination_rates %>%
  filter(!is.na(germination_rate_100)) %>%
  select(species_name, germination_rate_100) %>%
  # Remove any whitespace and convert to lowercase for matching
  mutate(species_name_clean = trimws(tolower(species_name)))

cat("\nSpecies with germination_rate_100 available:", nrow(germination_lookup), "\n")

# Debug: Check species names in germination_lookup
cat("\n=== Sample species names in germination_lookup (first 10) ===\n")
print(head(germination_lookup[, c("species_name", "species_name_clean")], 10))
cat("\n")

# ============================================================================
# PART 5: CALCULATE AVERAGE GERMINATION RATE PER PLOT
# ============================================================================

# For each plot, calculate weighted average germination rate
# Formula: sum(species_ratio * germination_rate_100) for all species in that plot
# Note: Ratios should sum to 1 (or close to it) per plot

plot_germination_rates <- data.frame(
  plot_id = character(),
  avg_germination_rate = numeric(),
  n_species_matched = integer(),
  total_ratio_matched = numeric(),
  stringsAsFactors = FALSE
)

# Process each plot
for (plot_col in colnames(species_ratios)) {
  # Get species ratios for this plot
  plot_ratios <- species_ratios[, plot_col, drop = FALSE]
  plot_ratios_df <- data.frame(
    species_name = rownames(plot_ratios),
    ratio = plot_ratios[, 1],
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(ratio), ratio > 0)  # Only species with non-zero ratios
  
  if (nrow(plot_ratios_df) == 0) next
  
  # Clean species names for matching
  plot_ratios_df <- plot_ratios_df %>%
    mutate(species_name_clean = trimws(tolower(species_name)))
  
  # Match with germination rates
  plot_ratios_matched <- plot_ratios_df %>%
    left_join(germination_lookup, by = "species_name_clean") %>%
    filter(!is.na(germination_rate_100))
  
  if (nrow(plot_ratios_matched) == 0) {
    # No matches for this plot
    plot_germination_rates <- rbind(
      plot_germination_rates,
      data.frame(
        plot_id = plot_col,
        avg_germination_rate = NA_real_,
        n_species_matched = 0L,
        total_ratio_matched = 0,
        stringsAsFactors = FALSE
      )
    )
    next
  }
  
  # Calculate weighted average germination rate
  # Weighted by species ratio in the plot
  total_ratio <- sum(plot_ratios_matched$ratio)
  weighted_sum <- sum(plot_ratios_matched$ratio * plot_ratios_matched$germination_rate_100)
  avg_germination_rate <- weighted_sum / total_ratio
  
  # Use original column name as plot_id (this will match plot_metadata_df$plot_id)
  plot_germination_rates <- rbind(
    plot_germination_rates,
    data.frame(
      plot_id = plot_col,  # This is the original column name
      avg_germination_rate = avg_germination_rate,
      n_species_matched = nrow(plot_ratios_matched),
      total_ratio_matched = total_ratio,
      stringsAsFactors = FALSE
    )
  )
}

cat("\n=== Calculated Average Germination Rates per Plot ===\n")
cat("Plots processed:", nrow(plot_germination_rates), "\n")
cat("Plots with calculated rates:", sum(!is.na(plot_germination_rates$avg_germination_rate)), "\n")
cat("Plots without matches:", sum(is.na(plot_germination_rates$avg_germination_rate)), "\n")

# Debug: Check which plots have no matches and their pool
if (sum(is.na(plot_germination_rates$avg_germination_rate)) > 0) {
  plots_no_match <- plot_germination_rates[is.na(plot_germination_rates$avg_germination_rate), ]
  cat("\nPlots with no matches (first 10):\n")
  print(head(plots_no_match, 10))
  
  # Check a specific plot that has no match to see what species it has
  if (nrow(plots_no_match) > 0) {
    example_plot <- plots_no_match$plot_id[1]
    cat("\n=== Debugging example plot with no match:", example_plot, "===\n")
    
    # Get species in this plot
    plot_species <- species_ratios[, example_plot, drop = FALSE]
    plot_species_df <- data.frame(
      species_name = rownames(plot_species),
      ratio = plot_species[, 1],
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(ratio), ratio > 0) %>%
      mutate(species_name_clean = trimws(tolower(species_name)))
    
    cat("Species in this plot:", nrow(plot_species_df), "\n")
    cat("First 10 species:\n")
    print(head(plot_species_df[, c("species_name", "ratio")], 10))
    
    # Check if any match germination lookup
    matched <- plot_species_df %>%
      left_join(germination_lookup, by = "species_name_clean")
    
    cat("\nSpecies with germination rate matches:", sum(!is.na(matched$germination_rate_100)), "\n")
    cat("Species without matches:", sum(is.na(matched$germination_rate_100)), "\n")
    
    if (sum(!is.na(matched$germination_rate_100)) > 0) {
      cat("\nMatched species:\n")
      print(matched[!is.na(matched$germination_rate_100), c("species_name", "ratio", "germination_rate_100")])
    }
    
    if (sum(is.na(matched$germination_rate_100)) > 0) {
      cat("\nUnmatched species (first 10):\n")
      print(head(matched[is.na(matched$germination_rate_100), c("species_name", "species_name_clean")], 10))
      
      # Check if any of these species exist in germination_lookup with different names
      cat("\nChecking if unmatched species exist in germination_lookup with different names:\n")
      unmatched_names <- matched[is.na(matched$germination_rate_100), "species_name_clean"]
      germination_names <- germination_lookup$species_name_clean
      
      # Look for partial matches
      for (unmatched in head(unmatched_names, 5)) {
        partial_matches <- germination_names[grepl(unmatched, germination_names, fixed = TRUE) | 
                                               grepl(germination_names, unmatched, fixed = TRUE)]
        if (length(partial_matches) > 0) {
          cat("  '", unmatched, "' might match:", paste(head(partial_matches, 3), collapse = ", "), "\n")
        }
      }
    }
  }
}
cat("\n")

# ============================================================================
# PART 6: ADD PLOT METADATA
# ============================================================================

# Check pool values in plot_metadata before transposing
cat("\n=== Debugging Pool Values ===\n")
cat("Pool values in plot_metadata (before transpose):\n")
pool_in_metadata <- table(plot_metadata["pool", , drop = TRUE], useNA = "ifany")
print(pool_in_metadata)

# Transpose metadata to have plots as rows
plot_metadata_df <- as.data.frame(t(plot_metadata), stringsAsFactors = FALSE)
plot_metadata_df$plot_id <- as.character(rownames(plot_metadata_df))  # Ensure character type

# Check pool values after transposing
cat("\nPool values in plot_metadata_df (after transpose):\n")
pool_after_transpose <- table(plot_metadata_df$pool, useNA = "ifany")
print(pool_after_transpose)

# Check plot_ids before merging
cat("\n=== Plot ID Comparison ===\n")
cat("Plot IDs in plot_germination_rates:", length(unique(plot_germination_rates$plot_id)), "\n")
cat("Plot IDs in plot_metadata_df:", length(unique(plot_metadata_df$plot_id)), "\n")

# Ensure both are character for proper matching
plot_germination_rates$plot_id <- as.character(plot_germination_rates$plot_id)
plot_metadata_df$plot_id <- as.character(plot_metadata_df$plot_id)

cat("Matching plot_ids:", sum(plot_germination_rates$plot_id %in% plot_metadata_df$plot_id), "\n")

# Debug: Show sample plot_ids from each
cat("\nSample plot_ids from plot_germination_rates (first 10):\n")
print(head(plot_germination_rates$plot_id, 10))
cat("\nSample plot_ids from plot_metadata_df (first 10):\n")
print(head(plot_metadata_df$plot_id, 10))
cat("\nSample plot_ids from plot_metadata_df (r22 plots, first 10):\n")
r22_plots <- plot_metadata_df[plot_metadata_df$pool == "r22", "plot_id"]
print(head(r22_plots, 10))

# Check which plots are missing from plot_germination_rates
missing_plots <- setdiff(plot_metadata_df$plot_id, plot_germination_rates$plot_id)
cat("\nPlots missing from plot_germination_rates:", length(missing_plots), "\n")
if (length(missing_plots) > 0) {
  cat("Missing plot IDs:", paste(head(missing_plots, 10), collapse = ", "), "\n")
  # Check pool distribution of missing plots
  missing_plots_metadata <- plot_metadata_df[plot_metadata_df$plot_id %in% missing_plots, ]
  cat("\nPool distribution of missing plots:\n")
  print(table(missing_plots_metadata$pool, useNA = "ifany"))
}

# IMPORTANT: Use right_join or full_join to keep ALL plots from metadata
# This ensures all plots (including those with no matching species) are included
cat("\n=== Merging with full_join to keep all plots ===\n")
plot_germination_final <- plot_metadata_df %>%
  left_join(plot_germination_rates, by = "plot_id") %>%
  select(plot_id, id_plot, pool, trait_constraint, seedmix_type, timepoint, year,
         avg_germination_rate, n_species_matched, total_ratio_matched)

# Check pool values after merging
cat("\nPool values after merging:\n")
pool_after_merge <- table(plot_germination_final$pool, useNA = "ifany")
print(pool_after_merge)

# Check which pools have calculated rates vs NA
cat("\n=== Pool-specific Analysis ===\n")
for (pool_name in names(pool_after_merge)) {
  pool_data <- plot_germination_final[plot_germination_final$pool == pool_name, ]
  cat("\nPool:", pool_name, "\n")
  cat("  Total plots:", nrow(pool_data), "\n")
  cat("  Plots with calculated rates:", sum(!is.na(pool_data$avg_germination_rate)), "\n")
  cat("  Plots with NA rates:", sum(is.na(pool_data$avg_germination_rate)), "\n")
  
  if (sum(is.na(pool_data$avg_germination_rate)) > 0) {
    na_plots <- pool_data[is.na(pool_data$avg_germination_rate), ]
    cat("  Example plot with NA:", na_plots$plot_id[1], "\n")
    
    # Check species in this plot
    example_plot_id <- na_plots$plot_id[1]
    if (example_plot_id %in% colnames(species_ratios)) {
      plot_species <- species_ratios[, example_plot_id, drop = FALSE]
      plot_species_df <- data.frame(
        species_name = rownames(plot_species),
        ratio = plot_species[, 1],
        stringsAsFactors = FALSE
      ) %>%
        filter(!is.na(ratio), ratio > 0) %>%
        mutate(species_name_clean = trimws(tolower(species_name)))
      
      # Try to match
      matched <- plot_species_df %>%
        left_join(germination_lookup, by = "species_name_clean")
      
      cat("  Species in plot:", nrow(plot_species_df), "\n")
      cat("  Species matched:", sum(!is.na(matched$germination_rate_100)), "\n")
      cat("  Species NOT matched:", sum(is.na(matched$germination_rate_100)), "\n")
      
      if (sum(is.na(matched$germination_rate_100)) > 0) {
        unmatched <- matched[is.na(matched$germination_rate_100), ]
        cat("  First 5 unmatched species:\n")
        for (i in 1:min(5, nrow(unmatched))) {
          cat("    - '", unmatched$species_name[i], "' (cleaned: '", unmatched$species_name_clean[i], "')\n", sep = "")
        }
      }
    }
  }
}

cat("=== Summary Statistics ===\n")
cat("Mean average germination rate:", 
    mean(plot_germination_final$avg_germination_rate, na.rm = TRUE), "\n")
cat("Median average germination rate:", 
    median(plot_germination_final$avg_germination_rate, na.rm = TRUE), "\n")
cat("Range:", 
    range(plot_germination_final$avg_germination_rate, na.rm = TRUE), "\n\n")

# ============================================================================
# PART 7: SAVE OUTPUTS
# ============================================================================

# All plots in plot_germination_final should already be filtered (year == 0)
# But let's verify and add a check
cat("\n=== Final Data Check ===\n")
cat("Total plots in final data:", nrow(plot_germination_final), "\n")

# Check pool distribution in final data
pool_final <- table(plot_germination_final$pool, useNA = "ifany")
cat("\nPool distribution in final data:\n")
print(pool_final)

# Check timepoint distribution
timepoint_final <- table(plot_germination_final$timepoint, useNA = "ifany")
cat("\nTimepoint distribution in final data:\n")
print(timepoint_final)

# Save to output folder (all plots with year == 0)
write_csv(plot_germination_final, 
          here("output", "plot_average_germination_rates.csv"))

cat("\nSaved plot average germination rates to: output/plot_average_germination_rates.csv\n")
cat("(All plots with year == 0)\n")

# Display first few rows
cat("\n=== First 10 Plots ===\n")
print(head(plot_germination_final, 10))

# ============================================================================
# PART 8: PRE ANALYSIS OF GERMINATION RATES PER POOL,TYPE
# ============================================================================

# Load helper functions for saving figures
source(here("R", "helpers.R"))

# Boxplot: Germination rate by pool
png(here("figures", "germination_rate_by_pool.png"), 
    width = 8, height = 6, units = "in", res = 300)
boxplot(avg_germination_rate ~ pool, data = plot_germination_final,
        main = "Average Germination Rate by Species Pool",
        xlab = "Species Pool",
        ylab = "Average Germination Rate",
        col = c("lightblue", "lightcoral"))
dev.off()

# Create caption file for pool boxplot
caption_pool <- paste0(
  "Figure: germination_rate_by_pool\n\n",
  "Description:\n",
  "Boxplot showing the distribution of average germination rates per plot across different species pools (r1a and r22). ",
  "Each plot represents a sown plot with its calculated average germination rate based on the weighted species composition ",
  "and individual species germination rates from germination tests.\n\n",
  "Details:\n",
  "Data source: Calculated from species germination rates (germination_rate_100) weighted by species ratios in each plot.\n",
  "Number of plots: r1a = ", sum(plot_germination_final$pool == "r1a", na.rm = TRUE), 
  ", r22 = ", sum(plot_germination_final$pool == "r22", na.rm = TRUE), "\n",
  "Filtered for: year == 0 (seeded plots only)\n",
  "Method: Weighted average germination rate = sum(species_ratio × germination_rate_100) for all species in plot\n\n",
  "Generated from: ", getwd(), "\n",
  "Date: ", Sys.Date(), "\n"
)
writeLines(caption_pool, here("figures", "captions", "germination_rate_by_pool.txt"))

# Boxplot: Germination rate by seedmix_type
png(here("figures", "germination_rate_by_seedmix_type.png"), 
    width = 12, height = 6, units = "in", res = 300)
par(mar = c(8, 4, 4, 2))  # Increase bottom margin for rotated labels
boxplot(avg_germination_rate ~ seedmix_type, data = plot_germination_final,
        main = "Average Germination Rate by Seed Mix Type",
        xlab = "",
        ylab = "Average Germination Rate",
        las = 2,  # Rotate x-axis labels
        col = rainbow(length(unique(plot_germination_final$seedmix_type))))
dev.off()
par(mar = c(5, 4, 4, 2))  # Reset margins

# Create caption file for seedmix_type boxplot
caption_seedmix <- paste0(
  "Figure: germination_rate_by_seedmix_type\n\n",
  "Description:\n",
  "Boxplot showing the distribution of average germination rates per plot across different seed mix types. ",
  "Seed mix types combine species pool (r1a or r22) with trait constraint (high or low). ",
  "Each plot represents a sown plot with its calculated average germination rate.\n\n",
  "Details:\n",
  "Data source: Calculated from species germination rates (germination_rate_100) weighted by species ratios in each plot.\n",
  "Seed mix types: ", paste(unique(plot_germination_final$seedmix_type), collapse = ", "), "\n",
  "Filtered for: year == 0 (seeded plots only)\n",
  "Method: Weighted average germination rate = sum(species_ratio × germination_rate_100) for all species in plot\n\n",
  "Generated from: ", getwd(), "\n",
  "Date: ", Sys.Date(), "\n"
)
writeLines(caption_seedmix, here("figures", "captions", "germination_rate_by_seedmix_type.txt"))

cat("\n=== Boxplots saved ===\n")
cat("Saved to: figures/germination_rate_by_pool.png\n")
cat("Saved to: figures/germination_rate_by_seedmix_type.png\n")
cat("Captions saved to: figures/captions/\n")
boxplot(avg_germination_rate ~ seedmix_type, data = plot_germination_final)
