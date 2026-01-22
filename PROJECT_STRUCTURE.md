# Project Structure Overview

## Current Structure

```
Inn_Dikes/
├── .gitignore              # Files to ignore in Git
├── README.md               # Project documentation
├── GITHUB_SETUP.md         # Step-by-step GitHub setup guide
├── PROJECT_STRUCTURE.md    # This file
├── Inn_Dikes.Rproj         # RStudio project file
│
├── R/                      # R scripts (newly created)
│   └── (can move analyses/ scripts here if desired)
│
├── analyses/               # Analysis scripts
│   ├── _prepare_data_commentsJH.R
│   ├── _prepare_seed_mix_commentsJH.R
│   ├── _prepare_seed_mixtures_sla_210_230.R
│   ├── explore_seed_mix.R
│   └── Saatgut_knapp.R
│
├── data/
│   ├── raw/                # Original, unprocessed data (DO NOT EDIT)
│   │   ├── Inn_Dike_species.csv
│   │   ├── biomasse.csv
│   │   ├── df_sown_species.txt
│   │   ├── data_raw_compositions_sla_210_230.csv
│   │   └── ... (other raw data files)
│   │
│   └── processed/          # Cleaned and processed data
│       ├── data_processed_traits.csv
│       ├── data_processed_sites.csv
│       ├── data_processed_species.csv
│       └── ... (other processed data files)
│
├── output/                 # Analysis outputs (newly created)
│   └── (tables, intermediate results)
│
├── figures/                # Figures and plots
│   ├── RDA_seed_mix.png
│   └── ... (other figures)
│
├── pictures/               # Additional images
│   ├── Sankey_excl_Avena.jpeg
│   └── Sankey_incl_Avena.jpeg
│
├── text_Rmd/               # R Markdown documents
│   ├── Vegetation_analysis.Rmd
│   ├── establishment_rates.Rmd
│   ├── Methods_Verbund_aktuell.Rmd
│   ├── Veg_analysis_diversity_soerensen.Rmd
│   └── ... (other Rmd files)
│
├── ESy-master/             # External tool (vegetation classification)
│   └── (expert system files)
│
└── Select/                 # R package for species selection
    └── (package files)
```

## Path Standardization

All R scripts now use the `here()` package for file paths:

### Reading Data
```r
library(here)
data <- read_csv(here("data", "raw", "filename.csv"))
```

### Saving Processed Data
```r
write_csv(data, here("data", "processed", "processed_data.csv"))
```

### Saving Outputs
```r
write_csv(results, here("output", "analysis_results.csv"))
```

### Saving Figures
```r
ggsave(here("figures", "plot_name.png"))
```

## Changes Made

1. ✅ Created standard project structure (`R/`, `output/` folders)
2. ✅ Created `.gitignore` to exclude temporary files
3. ✅ Created `README.md` with project documentation
4. ✅ Updated all R scripts to use `here()` package
5. ✅ Standardized all data paths to use `data/raw/` and `data/processed/`
6. ✅ Standardized all output paths to use `output/` and `figures/`
7. ✅ Created GitHub setup guide (`GITHUB_SETUP.md`)

## Next Steps

1. **Install Git** (if not already installed):
   - Download from: https://git-scm.com/downloads
   - Follow installation wizard

2. **Set up GitHub**:
   - Follow instructions in `GITHUB_SETUP.md`
   - Create repository on GitHub
   - Push your code

3. **Optional: Move scripts to R/ folder**:
   - You can move scripts from `analyses/` to `R/` if you prefer
   - Update any references if needed

4. **Test your scripts**:
   - Run a few scripts to ensure paths work correctly
   - Verify outputs are saved to correct folders

## Notes

- The `ESy-master/` and `Select/` folders are external tools/packages
- Consider adding these to `.gitignore` if they're large or can be downloaded separately
- The `pictures/` folder might be merged with `figures/` in the future
- HTML outputs from R Markdown are ignored by `.gitignore` (they're generated files)

