# Inn Dikes Project

## Project Description

This project analyzes vegetation establishment and development on Inn Dikes, focusing on seed mixture compositions and trait-based community assembly.

## Project Structure

```
Inn_Dikes/
├── R/                    # R scripts for data processing and analysis
│   ├── 01_data_preparation.R
│   ├── 02_seed_mixtures.R
│   └── ...
├── data/
│   ├── raw/              # Raw, unprocessed data (DO NOT EDIT)
│   └── processed/        # Processed data files
├── output/               # Analysis outputs (tables, intermediate results)
├── figures/              # Figures and plots
├── text_Rmd/             # R Markdown documents for reports
└── README.md
```

## Setup

### Prerequisites

- R (>= 4.0.0)
- RStudio (recommended)

### Installation

1. Clone this repository:
```bash
git clone <repository-url>
cd Inn_Dikes
```

2. Open the project in RStudio by double-clicking `Inn_Dikes.Rproj`

3. Install required packages:
```r
install.packages(c("here", "tidyverse", "renv"))
```

4. If using `renv` for package management:
```r
renv::restore()
```

## Usage

All scripts use the `here()` package for path management, ensuring paths work across different machines and operating systems.

### Data Reading
```r
library(here)
data <- read_csv(here("data", "raw", "filename.csv"))
```

### Saving Outputs
```r
# Save figures
ggsave(here("figures", "plot_name.png"))

# Save processed data
write_csv(data, here("data", "processed", "processed_data.csv"))

# Save analysis outputs
write_csv(results, here("output", "analysis_results.csv"))
```

## Data

- **Raw data**: Located in `data/raw/` - original, unmodified data files
- **Processed data**: Located in `data/processed/` - cleaned and processed data ready for analysis

## Contributing

Please follow these guidelines:
- Use `here()` package for all file paths
- Save outputs to appropriate folders (`output/` or `figures/`)
- Document your code with comments
- Follow the existing naming conventions

## License

[Add your license information here]

## Contact

[Add contact information here]

