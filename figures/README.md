# Figures

This folder contains figures generated from the analyses.

## Figure Index

### Establishment Rates

- **[est_subs.png](est_subs.png)** - Establishment rate by substrate depth and species pool. Shows mean establishment (with standard error bars) across different substrate depths, separated by species pool (R1A vs R22).
  - [Caption details](captions/est_subs.txt)

### Seed Mixtures

- **[RDA_seed_mix.png](RDA_seed_mix.png)** - RDA analysis of seed mixtures.
  - [Caption details](captions/RDA_seed_mix.txt)

### Germination Rates

- **[germination_rate_by_pool.png](germination_rate_by_pool.png)** - Boxplot showing average germination rates per plot by species pool (r1a vs r22).
  - [Caption details](captions/germination_rate_by_pool.txt)

- **[germination_rate_by_seedmix_type.png](germination_rate_by_seedmix_type.png)** - Boxplot showing average germination rates per plot by seed mix type.
  - [Caption details](captions/germination_rate_by_seedmix_type.txt)

---

## Adding New Figures

When adding a new figure:

1. Save the figure in this folder using `ggsave(here("figures", "figure_name.png"))`
2. Create a caption file: `figures/captions/figure_name.txt`
3. Add an entry to this README with:
   - **filename.png** - Brief description with link to caption file

## Figure Metadata

For detailed metadata about each figure, see the corresponding analysis scripts in `text_Rmd/` or `analyses/`.
