# Read Excel file - Germination Tests (Groups AB and CD)
# This script processes germination data from two campaigns:
# - Group AB: 2 petri dishes per species (25 seeds each)
# - Group CD: 2 petri dishes per species (25 seeds each)
#   Some species in CD have 4 dishes: 2 short stratification + 2 long stratification

library(readxl)
library(dplyr)
library(here)

# ============================================================================
# PART 1: READ AND PROCESS GROUP AB DATA
# ============================================================================

# Read the Excel file for Group AB
file_path_AB <- "Z:/VERBUND_MAC/germination_test/Keimversuch_GruppeAB.xlsx"

# List all available sheets
cat("Available sheets in Group AB Excel file:\n")
sheet_names_AB <- excel_sheets(file_path_AB)
print(sheet_names_AB)

# Read Tabelle3 from Group AB
table3_AB <- read_excel(file_path_AB, sheet = "Tabelle3")

# Process Group AB data
colnames(table3_AB) <- as.character(table3_AB[1, ])
table3_AB <- table3_AB[-1, ]  # Remove first row (now used as column names)

# Delete all columns after column 45
if (ncol(table3_AB) > 45) {
  table3_AB <- table3_AB[, 1:45]
}

# Remove any additional header rows if needed
table3_AB <- table3_AB[-1, ]

# Convert column 44 to numeric (germinated seeds from Group AB)
table3_AB$germination_final_AB <- as.numeric(table3_AB[[44]])

# Create working dataframe with species name, ID, and AB germination data
table3_2 <- table3_AB[, c(1, 2, 46)]  # Columns: species_name, ID, germination_final_AB
colnames(table3_2) <- c("species_name", "species_ID", "germination_final_AB")

# ============================================================================
# PART 2: READ AND PROCESS GROUP CD DATA (PLACEHOLDER)
# ============================================================================

# TODO: When CD data is available, uncomment and update the path below
# file_path_CD <- "Z:/VERBUND_MAC/germination_test/Keimversuch_GruppeCD.xlsx"
# 
# cat("\nAvailable sheets in Group CD Excel file:\n")
# sheet_names_CD <- excel_sheets(file_path_CD)
# print(sheet_names_CD)
# 
# # Read Tabelle3 from Group CD (or appropriate sheet name)
# table3_CD <- read_excel(file_path_CD, sheet = "Tabelle3")  # Update sheet name if different
# 
# # Process Group CD data (similar structure to AB)
# colnames(table3_CD) <- as.character(table3_CD[1, ])
# table3_CD <- table3_CD[-1, ]
# 
# if (ncol(table3_CD) > 45) {
#   table3_CD <- table3_CD[, 1:45]
# }
# 
# table3_CD <- table3_CD[-1, ]
# 
# # Convert column 44 to numeric (germinated seeds from Group CD)
# table3_CD$germination_final_CD <- as.numeric(table3_CD[[44]])
# 
# # Extract CD data: species name, ID, germination_final_CD
# table3_CD_subset <- table3_CD[, c(1, 2, 46)]
# colnames(table3_CD_subset) <- c("species_name", "species_ID", "germination_final_CD")
# 
# # Merge CD data into table3_2
# table3_2 <- table3_2 %>%
#   full_join(table3_CD_subset, by = c("species_name", "species_ID"))
# 
# # TODO: Extract long stratification data (if column exists)
# # For species with 4 dishes in CD: 2 short strat + 2 long strat
# # Update column index if different from 44
# # table3_CD$long_strat_germination <- as.numeric(table3_CD[[XX]])  # Update XX with correct column
# # table3_CD_long <- table3_CD[, c(1, 2, XX)]  # Update XX
# # colnames(table3_CD_long) <- c("species_name", "species_ID", "long_strat_germination")
# # table3_2 <- table3_2 %>%
# #   full_join(table3_CD_long, by = c("species_name", "species_ID"))

# For now, add placeholder columns for CD data
table3_2$germination_final_CD <- NA
table3_2$long_strat_germination <- NA

cat("\n\nTable3_2 structure (with placeholder columns for CD data):\n")
print(head(table3_2))
cat("\nColumns:", paste(names(table3_2), collapse = ", "), "\n")

# ============================================================================
# PART 3: CALCULATE GERMINATION RATES
# ============================================================================

seeds_per_dish <- 25

# ----------------------------------------------------------------------------
# 3a) Calculate germination_rate_AB
# Group AB: 2 petri dishes per species (25 seeds each = 50 seeds total)
# ----------------------------------------------------------------------------

germination_rate_AB <- table3_2 %>%
  # Filter to only AB data (where germination_final_AB is not NA)
  filter(
    !is.na(species_name), 
    species_name != "", 
    !is.na(species_ID), 
    species_ID != "",
    !is.na(germination_final_AB)
  ) %>%
  # Group by species (both name and ID together)
  group_by(species_name, species_ID) %>%
  summarize(
    n_dishes_AB = dplyr::n(),  # Should be 2 per species
    germinated_total_AB = sum(germination_final_AB, na.rm = TRUE),  # Keep raw count for later calculation
    germination_rate_AB = germinated_total_AB / (n_dishes_AB * seeds_per_dish),
    .groups = "drop"
  ) %>%
  # Keep both raw counts and rates for later use
  select(species_name, species_ID, n_dishes_AB, germinated_total_AB, germination_rate_AB)

cat("\n\n=== Germination Rate AB (calculated) ===\n")
print(germination_rate_AB)
cat("\nRange:", range(germination_rate_AB$germination_rate_AB), "\n")
cat("Median:", median(germination_rate_AB$germination_rate_AB), "\n")
cat("Mean:", mean(germination_rate_AB$germination_rate_AB), "\n")

# ----------------------------------------------------------------------------
# 3b) Calculate germination_rate_CD (PLACEHOLDER - will be active when CD data is added)
# Group CD: 2 petri dishes per species (25 seeds each = 50 seeds total)
# ----------------------------------------------------------------------------

# TODO: Uncomment when CD data is available
# germination_rate_CD <- table3_2 %>%
#   filter(
#     !is.na(species_name), 
#     species_name != "", 
#     !is.na(species_ID), 
#     species_ID != "",
#     !is.na(germination_final_CD)
#   ) %>%
#   group_by(species_name, species_ID) %>%
#   summarize(
#     n_dishes_CD = dplyr::n(),  # Should be 2 per species
#     germinated_total_CD = sum(germination_final_CD, na.rm = TRUE),  # Keep raw count for later calculation
#     germination_rate_CD = germinated_total_CD / (n_dishes_CD * seeds_per_dish),
#     .groups = "drop"
#   ) %>%
#   # Keep both raw counts and rates for later use
#   select(species_name, species_ID, n_dishes_CD, germinated_total_CD, germination_rate_CD)

# Placeholder: empty dataframe with correct structure (includes raw counts for later calculation)
germination_rate_CD <- data.frame(
  species_name = character(0),
  species_ID = character(0),
  n_dishes_CD = integer(0),
  germinated_total_CD = numeric(0),
  germination_rate_CD = numeric(0)
)

# ----------------------------------------------------------------------------
# 3c) Calculate long_strat_germination_rate (PLACEHOLDER)
# For species with 4 dishes in CD: 2 short strat + 2 long strat
# Long stratification rate: from 2 long strat dishes (25 seeds each = 50 seeds total)
# ----------------------------------------------------------------------------

# TODO: Uncomment when long stratification data is available
# long_strat_germination_rate <- table3_2 %>%
#   filter(
#     !is.na(species_name), 
#     species_name != "", 
#     !is.na(species_ID), 
#     species_ID != "",
#     !is.na(long_strat_germination)
#   ) %>%
#   group_by(species_name, species_ID) %>%
#   summarize(
#     n_dishes_long = dplyr::n(),  # Should be 2 per species (long strat only)
#     germinated_total_long = sum(long_strat_germination, na.rm = TRUE),
#     long_strat_germination_rate = germinated_total_long / (n_dishes_long * seeds_per_dish),
#     .groups = "drop"
#   ) %>%
#   select(species_name, species_ID, long_strat_germination_rate)

# Placeholder: empty dataframe with correct structure
long_strat_germination_rate <- data.frame(
  species_name = character(0),
  species_ID = character(0),
  long_strat_germination_rate = numeric(0)
)

# ============================================================================
# PART 4: COMBINE ALL RESULTS INTO FINAL DATAFRAME
# ============================================================================

# Combine AB, CD, and long stratification rates
germination_rates_per_species <- germination_rate_AB %>%
  full_join(germination_rate_CD, by = c("species_name", "species_ID")) %>%
  full_join(long_strat_germination_rate, by = c("species_name", "species_ID"))

# ----------------------------------------------------------------------------
# 4a) Calculate germination_rate_100 FROM RAW DATA
# Combined rate from 4 petri dishes total (2 from AB + 2 from CD)
# Maximum: 4 dishes × 25 seeds = 100 seeds per species
# 
# IMPORTANT: We calculate directly from raw germinated seed counts, not from
# the already-calculated rates. This avoids rounding errors and is more accurate.
# Mathematically: (germinated_AB + germinated_CD) / (total_seeds_AB + total_seeds_CD)
# ----------------------------------------------------------------------------

germination_rates_per_species <- germination_rates_per_species %>%
  mutate(
    # Calculate total seeds across both campaigns (from raw dish counts)
    total_seeds_AB = ifelse(!is.na(n_dishes_AB), n_dishes_AB * seeds_per_dish, NA_real_),
    total_seeds_CD = ifelse(!is.na(n_dishes_CD), n_dishes_CD * seeds_per_dish, NA_real_),
    total_seeds_all = case_when(
      !is.na(total_seeds_AB) & !is.na(total_seeds_CD) ~ total_seeds_AB + total_seeds_CD,
      !is.na(total_seeds_AB) ~ total_seeds_AB,
      !is.na(total_seeds_CD) ~ total_seeds_CD,
      TRUE ~ NA_real_
    ),
    # Calculate total germinated seeds across both campaigns (from raw counts)
    germinated_total_all = case_when(
      !is.na(germinated_total_AB) & !is.na(germinated_total_CD) ~ 
        germinated_total_AB + germinated_total_CD,
      !is.na(germinated_total_AB) ~ germinated_total_AB,
      !is.na(germinated_total_CD) ~ germinated_total_CD,
      TRUE ~ NA_real_
    ),
    # Calculate germination_rate_100 directly from raw data
    # This is more accurate than calculating from rates (avoids rounding errors)
    germination_rate_100 = case_when(
      !is.na(germinated_total_all) & !is.na(total_seeds_all) ~ 
        germinated_total_all / total_seeds_all,
      TRUE ~ NA_real_
    )
  ) %>%
  # Remove intermediate columns (keep only final rates and essential info)
  # Note: Some columns may not exist yet if CD data hasn't been added
  select(species_name, species_ID, 
         any_of(c("germination_rate_AB", "germination_rate_CD", 
                  "long_strat_germination_rate", "germination_rate_100")))

cat("\n\n=== Final Germination Rates per Species ===\n")
print(germination_rates_per_species)

# ============================================================================
# PART 5: SUMMARY STATISTICS AND TOP/BOTTOM PERFORMERS
# ============================================================================

# Summary statistics for AB rates
cat("\n\n=== SUMMARY STATISTICS: Germination Rate AB ===\n")
cat("Range:", range(germination_rate_AB$germination_rate_AB, na.rm = TRUE), "\n")
cat("Median:", median(germination_rate_AB$germination_rate_AB, na.rm = TRUE), "\n")
cat("Mean:", mean(germination_rate_AB$germination_rate_AB, na.rm = TRUE), "\n")

# Top 10 and bottom 10 germinators (AB)
cat("\n\n=== TOP 10 GERMINATORS (AB) ===\n")
top_10_AB <- germination_rate_AB %>%
  arrange(desc(germination_rate_AB)) %>%
  head(10)
print(top_10_AB)

cat("\n\n=== BOTTOM 10 GERMINATORS (AB) ===\n")
bottom_10_AB <- germination_rate_AB %>%
  arrange(germination_rate_AB) %>%
  head(10)
print(bottom_10_AB)

# Summary for combined rate (when available)
if (sum(!is.na(germination_rates_per_species$germination_rate_100)) > 0) {
  cat("\n\n=== SUMMARY STATISTICS: Germination Rate 100 (AB + CD) ===\n")
  cat("Range:", range(germination_rates_per_species$germination_rate_100, na.rm = TRUE), "\n")
  cat("Median:", median(germination_rates_per_species$germination_rate_100, na.rm = TRUE), "\n")
  cat("Mean:", mean(germination_rates_per_species$germination_rate_100, na.rm = TRUE), "\n")
  
  cat("\n\n=== TOP 10 GERMINATORS (Combined AB + CD) ===\n")
  top_10_combined <- germination_rates_per_species %>%
    filter(!is.na(germination_rate_100)) %>%
    arrange(desc(germination_rate_100)) %>%
    head(10)
  print(top_10_combined)
}

# ============================================================================
# PART 6: SAVE OUTPUTS
# ============================================================================

# Ensure output directory exists
if (!dir.exists(here("output"))) {
  dir.create(here("output"), recursive = TRUE)
}

# Save final combined dataframe to project output folder
write.csv(germination_rates_per_species, 
          here("output", "germination_rates_per_species.csv"), 
          row.names = FALSE)
cat("\n\nSaved germination rates to:", here("output", "germination_rates_per_species.csv"), "\n")

# Save AB rates separately (for reference)
write.csv(germination_rate_AB, 
          here("output", "germination_rate_AB.csv"), 
          row.names = FALSE)
cat("Saved AB rates to:", here("output", "germination_rate_AB.csv"), "\n")

