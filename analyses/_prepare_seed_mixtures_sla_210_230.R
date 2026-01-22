#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Inn Dikes: Experiment
# Prepare data ####
# Seed mixtures: constrained for SLA
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2024-02-11



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
install.packages("tidyverse")
library(tidyverse)
# Note: Select package installation - adjust path if needed
# install.packages("Select")
# install.packages(here("Select_1.4.tar.gz"), repos = NULL, type = "source")
library(Select)

### Start ###
rm(list = ls())

### Load traits matrix with species list ###
traits <- read_csv(
  here("data", "processed", "data_processed_traits.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
      .default = "?"
    )      
) %>%
  filter(
    !(accepted_name == "Papaver rhoeas") &
      !(accepted_name == "Centaurea cyanus") &
      !(ellenberg >= 5410 & ellenberg < 5420) |
      is.na(ellenberg)
  ) %>%
  select(
    accepted_name, ellenberg, R1A, R22, both, sla, height, seedmass, family,
    available, company, taxonomic_status
  ) %>%
  filter(available == 1 & !(taxonomic_status == "Illegitimate")) # Update available seeds from Saaten-Zeller!!!

trait_ids <- c("1.2.2", "1.6.3", "3.2.3", "4.1.3")

GIFT::GIFT_traits_meta() %>%
  filter(Lvl3 %in% trait_ids) %>%
  tibble()

### Logarithmize trait data and create dummies ###
traits <- traits %>%
  mutate(
    sla = log(sla),
    height = log(height),
    seedmass = log(seedmass)
  ) %>%
  mutate(
    grass = if_else(family == "Poaceae", 1, 0),
    legume = if_else(family == "Fabaceae", 1, 0),
    hemiparasite = if_else(family == "Orobanchaceae", 1, 0)
  )



## 1 Create subsamples ########################################################


# Ellenberg et al. (2001) Scripta Geobotanica https://search.worldcat.org/de/title/57545938
# EUNIS: Chytrý et al. (2020) Appl Veg Sci https://doi.org/10.1111/avsc.12519


### a Herbs --------------------------------------------------------------------

herbs <- traits %>%
  filter(
    !(family == "Poaceae" | family == "Fabaceae" | family == "Orobanchaceae") &
      !(ellenberg %in% c(3000, 3521)) &
      !(accepted_name %in% c(
        "Echium vulgare",
        "Lycopus europaeus",
        "Lysimachia vulgaris",
        "Silene dioica"
      ))
  )
herbs_r1a <- herbs %>%
  filter(
    ((ellenberg >= 5300 & ellenberg < 5400) |
       (ellenberg >= 6100 & ellenberg < 7000)) |
      accepted_name %in% c(
        "Buphthalmum salicifolium", # free after Ellenberg and EUNIS
        "Primula veris" # free after Ellenberg and R1A after EUNIS
      )
  ) %>%
  mutate(type = "herbs_r1a")
herbs_r22 <- herbs %>%
  filter(
    (ellenberg >= 5420 & ellenberg < 5430) |
      ellenberg == 5400 |
      accepted_name %in% c(
        "Cardamine pratensis", # free after Ellenberg BUT R22 after EUNIS
        "Hypochaeris radicata", # free after Ellenberg BUT R22 after EUNIS
        "Leontodon hispidus", # free after Ellenberg and EUNIS
        "Stellaria graminea" # free after Ellenberg BUT R22 after EUNIS
      )
  ) %>%
  mutate(type = "herbs_r22")
herbs_rest <- herbs %>%
  anti_join(herbs_r22, by = "accepted_name") %>%
  anti_join(herbs_r1a, by = "accepted_name") %>%
  mutate(type = "herbs_rest")


### b Grass --------------------------------------------------------------------

grass <- traits %>%
  filter(
    family == "Poaceae" &
      !(accepted_name %in% c(
        "Agrostis stolonifera",
        "Festuca nigrescens",
        "Alopecurus pratensis",
        "Bromus hordeaceus",
        "Cynosurus cristatus",
        "Poa trivialis"
      ))
  )
grass_r1a <- grass %>%
  filter(
    (ellenberg >= 5300 & ellenberg < 5400) |
      accepted_name %in% c(
        "Anthoxanthum odoratum", # free after Ellenberg and EUNIS
        "Briza media", # free after Ellenberg and EUNIS
        "Molinia caerulea", # free after Ellenberg and EUNIS
        "Festuca rubra", # against Ellenberg BUT free after EUNIS
        "Helictotrichon pubescens" # against Ellenberg BUT free after EUNIS
      )
  ) %>%
  mutate(type = "grass_r1a")
grass_r22 <- grass %>%
  filter(
    (ellenberg >= 5420 & ellenberg < 5430) |
      ellenberg == 5400 |
      accepted_name %in% c(
        "Dactylis glomerata" # free after Ellenberg and EUNIS
      )
  ) %>%
  filter(
    !(accepted_name %in% c(
      "Festuca rubra", # --> moved to R1A
      "Helictotrichon pubescens" # --> moved to R1A
    ))
  ) %>%
  mutate(type = "grass_r22")
grass_rest <- grass %>%
  anti_join(grass_r22, by = "accepted_name") %>%
  anti_join(grass_r1a, by = "accepted_name") %>%
  mutate(type = "grass_rest")


### c Legumes ------------------------------------------------------------------

legumes <- traits %>%
  filter(
    family == "Fabaceae" &
      !(accepted_name %in% c("Vicia cracca")) # reduce to 3 legumes per pool
  )
legumes_r1a <- legumes %>%
  filter(
    ellenberg >= 5300 & ellenberg < 5400 |
      accepted_name %in% c("Lotus corniculatus") # free after Ellenberg and EUNIS
  ) %>%
  mutate(type = "legumes_r1a")
legumes_r22 <- legumes %>%
  filter(
    (ellenberg >= 5420 & ellenberg < 5430) |
      ellenberg == 5400 |
      accepted_name %in% c("Vicia sepium") # free after Ellenberg but R22 after EUNIS
  ) %>%
  mutate(type = "legumes_r22")
legumes_rest <- legumes %>%
  anti_join(legumes_r22, by = "accepted_name") %>%
  anti_join(legumes_r1a, by = "accepted_name") %>%
  mutate(type = "legumes_rest")

### d Hemiparasites ------------------------------------------------------------

hemiparasites <- traits %>%
  filter(family == "Orobanchaceae")  %>%
  mutate(type = "hemiparasites")

### e Save species list --------------------------------------------------------

data <- grass_r1a %>%
  relocate(type, .after = R22) %>%
  bind_rows(grass_r22) %>%
  bind_rows(grass_rest) %>%
  bind_rows(herbs_r1a) %>%
  bind_rows(herbs_r22) %>%
  bind_rows(herbs_rest) %>%
  bind_rows(legumes_r1a) %>%
  bind_rows(legumes_r22) %>%
  bind_rows(legumes_rest) %>%
  bind_rows(hemiparasites)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Seed mixture creation #####################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Taxonomic composition ####################################################


set.seed(18)

compositions_r1a <- as.data.frame(
  replicate(48, {
    
    dummy <- c(
      sample(herbs_r1a$accepted_name, 9),
      sample(herbs_rest$accepted_name, 0),
      sample(grass_r1a$accepted_name, 7),
      sample(grass_rest$accepted_name, 0),
      sample(legumes_r1a$accepted_name, 3),
      sample(legumes_rest$accepted_name, 0),
      sample(hemiparasites$accepted_name, 1)
    )
    
  }
  
  )) %>%
  rowid_to_column("dummy") %>%
  pivot_longer(-dummy, names_to = "id", values_to = "accepted_name") %>%
  select(-dummy) %>%
  mutate(
    id_plot = str_replace(id, "V", "") %>% as.numeric(),
    id = str_c(id, "_seeded")
  ) %>%
  arrange(id_plot) %>%
  mutate(
    pool = "r1a",
    trait_constraint = c(replicate(n()/2, "high"), replicate(n()/2, "low"))
  )

set.seed(185)

compositions_r22 <- as.data.frame(
  replicate(48, {
    
    dummy <- c(
      sample(herbs_r22$accepted_name, 9),
      sample(herbs_rest$accepted_name, 0),
      sample(grass_r22$accepted_name, 7),
      sample(grass_rest$accepted_name, 0),
      sample(legumes_r22$accepted_name, 3),
      sample(legumes_rest$accepted_name, 0), 
      sample(hemiparasites$accepted_name, 1)
    )
    
  }
  )
) %>%
  rowid_to_column("dummy") %>%
  pivot_longer(-dummy, names_to = "id", values_to = "accepted_name") %>%
  select(-dummy) %>%
  mutate(
    id = str_replace(id, "V", ""),
    id = as.numeric(id) + 48,
    id_plot = id,
    id = str_c("V", id, "_seeded")
  ) %>%
  arrange(id_plot) %>%
  mutate(
    pool = "r22",
    trait_constraint = c(replicate(n()/2, "high"), replicate(n()/2, "low"))
  )

compositions <- compositions_r1a %>%
  bind_rows(compositions_r22) %>%
  inner_join(traits, by = "accepted_name") %>%
  select(
    id, id_plot, pool, trait_constraint, accepted_name, sla, seedmass, family,
    grass, legume, hemiparasite
  )

table(compositions$accepted_name)
min(table(compositions$accepted_name))
max(table(compositions$accepted_name))
length(table(compositions$accepted_name)) # Total species pool

rm(list = setdiff(ls(), c("compositions", "traits")))



## 2 Calculate abundance values ##############################################


# Laughlin et al. 2018 Methods Ecol Evol
# https://doi.org/10.1111/2041-210X.13023


### a Seed mixtures with high SLA ----------------------------------------------

seedabundance <- c(100)

for (i in c(1:24, 49:72)) { #high SLA
  
  constraints <- compositions %>%
    filter(id_plot == i) %>%
    select(sla, grass, legume, hemiparasite) %>%
    as.matrix()
  rownames(constraints) <- compositions %>%
    filter(id_plot == i) %>%
    select(accepted_name) %>%
    as.matrix()
  diversify <- compositions %>%
    filter(id_plot == i) %>%
    select(seedmass) %>%
    as.matrix()
  rownames(diversify) <- compositions %>%
    filter(id_plot == i) %>%
    select(accepted_name) %>%
    as.matrix()
  mix <- selectSpecies(
    t2c = constraints,
    constraints = c(
      sla = log(230),
      grass = 0.6,
      legume = 0.05,
      hemiparasite = 0.05
    ),
    t2d = diversify,
    obj = "QH",
    capd = TRUE
  )
  seedabundance <- append(seedabundance, mix$prob)
}

seedabundance <- seedabundance[-1] # removes first '100' value


### b Seed mixtures with low SLA -----------------------------------------------

for (i in c(25:48, 73:96)) {
  
  constraints <- compositions %>%
    filter(id_plot == i) %>%
    select(sla, grass, legume, hemiparasite) %>%
    as.matrix()
  rownames(constraints) <- compositions %>%
    filter(id_plot == i) %>%
    select(accepted_name) %>%
    as.matrix()
  diversify <- compositions %>%
    filter(id_plot == i) %>%
    select(seedmass) %>%
    as.matrix()
  rownames(diversify) <- compositions %>%
    filter(id_plot == i) %>%
    select(accepted_name) %>%
    as.matrix()
  mix <- selectSpecies(
    t2c = constraints,
    constraints = c(
      sla = log(210),
      grass = 0.6,
      legume = 0.05,
      hemiparasite = 0.05
    ),
    t2d = diversify,
    obj = "QH",
    capd = TRUE
  )
  seedabundance <- append(seedabundance, mix$prob)
}

compositions <- compositions %>%
  arrange(trait_constraint) %>%
  mutate(
    ratio = seedabundance,
    seedmix_type = str_c(pool, trait_constraint, sep = "_")
  ) %>%
  relocate(ratio, .after = accepted_name)


#rm(list = setdiff(ls(), c("compositions", "traits")))



## 3 Control of seedmixtures ##################################################


### a Check summed abundances -------------------------------------------------

compositions %>%
  group_by(id_plot) %>%
  summarise(plot_sum = sum(ratio)) %>%
  mutate(plot_sum = round(plot_sum, digit = 2)) %>%
  filter(plot_sum < 0.99 | plot_sum > 1.01)
compositions %>%
  filter(ratio > 0.25)
compositions %>%
  filter(ratio < 0.01)
compositions %>%
  filter(ratio < 0.001)
compositions %>%
  arrange(ratio) %>%
  filter(id_plot == 1)


### b Calculate needed seeds --------------------------------------------------

data <- compositions %>%
  group_by(accepted_name) %>%
  summarize(sum = sum(ratio)) %>%
  mutate(
    ratio = sum / 96,
    weight_kg = ratio * 77,
    weight_kg = round(weight_kg, digits = 2)
  ) %>%
  arrange(accepted_name) %>%
  left_join(
    traits %>% select(accepted_name, company, family), by = "accepted_name"
    )

data %>%
  filter(company == "Rieger-Hofmann") %>%
  select(accepted_name, family, weight_kg) %>%
  write_csv(here("output", "weight_rieger-hofmann_sla_210_230.csv"))

data %>%
  filter(company == "Saaten-Zeller") %>%
  select(accepted_name, family, weight_kg) %>%
  write_csv(here("output", "weight_zeller_sla_210_230.csv"))

data %>%
  filter(company == "Krimmer") %>%
  select(accepted_name, family, weight_kg) %>%
  write_csv(here("output", "weight_krimmer_sla_210_230.csv"))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## C Export ##################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# write_csv(
#   compositions,
#   here("data", "raw", "data_raw_compositions_sla_210_230.csv")
# )
