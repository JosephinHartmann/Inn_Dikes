# Analysis Outputs

This folder contains analysis results and model outputs.

## Model Results

### Establishment Models

- **[establishment_model_depth_pool_functional_summary.txt](establishment_model_depth_pool_functional_summary.txt)** - Summary of three-way interaction model (substrate depth × species pool × functional type)
  - [Description](descriptions/establishment_model_depth_pool_functional_description.txt)
  - [Anova table](establishment_model_depth_pool_functional_anova.txt)

- **[establishment_model_depth_pool_functional_biomass_summary.txt](establishment_model_depth_pool_functional_biomass_summary.txt)** - Summary of three-way interaction model with biomass covariate
  - [Description](descriptions/establishment_model_depth_pool_functional_biomass_description.txt)
  - [Anova table](establishment_model_depth_pool_functional_biomass_anova.txt)

---

## Adding New Results

When saving model results:

1. Use `save_model_results()` function in your R script
2. The function automatically creates:
   - Model summary file (`*_summary.txt`)
   - Anova table file (`*_anova.txt`) if requested
   - Description file in `descriptions/` folder
3. Update this README to include the new results

## Structure

- **`*.txt`** - Model summaries and Anova tables
- **`descriptions/`** - Detailed descriptions and interpretations of results
- **`*.csv`** - Data outputs (if any)

