#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Inn Dikes: Experiment
# Prepare data ####
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus bauer
# 2024-12-18



### Packages ###
library(renv)
library(installr)
library(here)
library(tidyverse)
library(TNRS)
library(GIFT)
library(FD)

### Start ###
# Create hashtag infront of a line: shift + strg + c
rm(list = ls())
# installr::updateR(
#   browse_news = FALSE,
#   install_R = TRUE,
#   copy_packages = TRUE,
#   copy_site_files = TRUE,
#   keep_old_packages = FALSE,
#   update_packages = FALSE,
#   start_new_R = FALSE,
#   quit_R = TRUE
#   )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Load data #################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Sites ####################################################################


sites <- read_csv(
  here("data", "raw", "data_raw_sites.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
      .default = "?",
      survey_date.2025 = col_date(format = "%d.%m.%Y")
    )
)



## 2 Species ##################################################################


species <- read_csv(
  here("data", "raw", "data_raw_species_20241217.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
      .default = "?"
    )
)



## 3 Traits ###################################################################


traits <- read_csv(
  here("data", "raw", "data_raw_traits.csv"),
  col_names = TRUE, na = c("", "NA", "na")
)



## 4 FloraVeg.EU species #######################################################

# Chytrý et al. (2020) Appl Veg Sci https://doi.org/10.1111/avsc.12519
# Version 2021-06-01: https://doi.org/10.5281/zenodo.4812736


traits_eunis <- readxl::read_excel(
  here(
    "data", "raw",
    "Characteristic-species-combinations-EUNIS-habitats-2021-06-01.xlsx"
  ),
  col_names = TRUE, na = c("", "NA", "na")
)


rm(list = setdiff(ls(), c("species", "sites", "traits", "traits_eunis")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Select target species from FloraVeg.EU ###################################

data <- traits_eunis %>%
  rename_with(~ tolower(gsub(" ", "_", .x))) %>%
  filter(
    habitat_code %in% c("R1A", "R22") &
      species_type %in% c("Diagnostic", "Constant")
  ) %>%
  select(species, habitat_code, species_type) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = "habitat_code", values_from = "value") %>%
  group_by(species) %>%
  summarize(across(c("R1A", "R22"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(
    across(c("R1A", "R22"), ~ if_else(. > 0, 1, NA)),
    both = if_else(R1A > 0 & R22 > 0, 1, NA)
  ) %>%
  mutate(
    species = str_replace(species, "Cirsium acaulon", "Cirsium acaule"),
    species = str_replace(
      species, "Avenula pubescens", "Helictotrichon pubescens"
    )
  )
traits <- traits %>%
  full_join(data %>% rename(name = species), by = "name")


rm(list = setdiff(ls(), c("species", "sites", "traits")))



## 2 Names from TNRS database #################################################


metadata <- TNRS_metadata()
metadata$version
metadata$sources %>% tibble()


### a Harmonize names of traits_eunis matrix ----------------------------------

traits <- traits %>%
  mutate(
    name = str_replace(name, "Tragopogon pratensis ssp orientalis", "Tragopogon pratensis"),
    name = str_replace(name, "Betonica officinalis", "Stachys officinalis")
  )

## Run only once and thän load saved file
# harmonized_names <- traits %>%
#     rowid_to_column("id") %>%
#     select(id, name) %>%
#     TNRS::TNRS(
#       sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#       classification = "wfo", # family classification
#       mode = "resolve"
#     )
# 
# write_csv(
#     harmonized_names, here("data", "processed", "data_processed_tnrs_traits.csv")
#     )

names_traits <- read.csv(
  here("data", "processed", "data_processed_tnrs_traits.csv")
) %>%
  rename_with(tolower)


### b Harmonize names of species matrix ----------------------------------------

# --> No vegetation surveys yet

## Run only once and thän load saved file
# harmonized_names <- species %>%
#     rowid_to_column("id") %>%
#     select(id, name) %>%
#     TNRS::TNRS(
#       sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#       classification = "wfo", # family classification
#       mode = "resolve"
#     )
# 
# write_csv(
#     harmonized_names, here("data", "processed", "data_processed_tnrs_species.csv")
#     )

# names_species <- read.csv(
#   here("data", "processed", "data_processed_tnrs_species.csv")
# ) %>%
# rename_with(tolower)



### c Summarize duplicates of traits matrix ------------------------------------

data <- traits %>%
  rename("name_submitted" = "name") %>%
  left_join(
    names_traits %>% select(name_submitted, name_matched, accepted_name),
    by = "name_submitted"
  ) %>%
  select(name_submitted, name_matched, everything())

# --> No vegetation surveys yet

# data <- traits %>%
#   right_join(
#     species %>% select(accepted_name), by = c("Name_matched" = "accepted_name")
#     ) %>%
#   rename(accepted_name = Name_matched) %>%
#   left_join(data_names, by = "accepted_name") %>%
#   select(name_submitted, accepted_name, everything())

data %>% filter(duplicated(accepted_name))

data_summarized <- data %>%
  group_by(accepted_name) %>%
  summarize(across(everything(), ~ first(.x))) %>%
  select(name_submitted, accepted_name, everything())

data_summarized %>% filter(duplicated(accepted_name))

traits <- data_summarized %>%
  left_join(
    names_traits %>%
      select(
        name_submitted, taxonomic_status, accepted_family,
        accepted_name_url
      ),
    by = "name_submitted"
  ) %>%
  select(
    accepted_name, taxonomic_status, accepted_family, everything(),
    accepted_name_url
  )

rm(list = setdiff(ls(), c("species", "sites", "traits")))


### d Summarize duplicates of species matrix -----------------------------------

# --> no vegetation surveys yet

# data <- species %>% 
#   rename(name_submitted = name) %>%
#   left_join(
#     data_names %>% select(name_submitted, accepted_name),
#     by = "name_submitted"
#   ) %>%
#   select(name_submitted, accepted_name, everything())
# 
# data %>% filter(duplicated(accepted_name))
# 
# data_summarized <- data %>%
#   group_by(accepted_name) %>%
#   summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
# 
# data_summarized %>% filter(duplicated(accepted_name))
# 
# species <- data_summarized



## 4 Get red list status ######################################################


### a Load red list ------------------------------------------------------------

data_redlist <- readxl::read_excel(
  here("data", "raw", "data_raw_species_redlist_2018.xlsx"),
  col_names = TRUE, na = c("", "NA", "na")
) %>%
  rename(redlist_germany = "RL Kat.", responsibility = Verantwortlichkeit) %>%
  rename_with(tolower) %>%
  select(name, status, redlist_germany) %>%
  mutate(
    name = str_replace(
      name, "Cirsium acaulon \\(L\\.\\) Scop\\.", "Cirsium acaule"
    )
  )

# Calculate just once to save time (afterwards load file)
# harmonized_names <- data_redlist %>%
#   rowid_to_column("id") %>%
#   select(id, name) %>%
#   TNRS::TNRS(
#     sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#     classification = "wfo", # family classification
#     mode = "resolve"
#   )
# write_csv(
#   harmonized_names,
#   here("data", "processed", "data_processed_tnrs_redlist.csv")
#   )

redlist <- read_csv(
  here("data", "processed", "data_processed_tnrs_redlist.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(.default = "?")
) %>%
  select(
    Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_url,
    Accepted_family
  ) %>%
  rename_with(tolower) %>%
  full_join(
    data_redlist %>% rename(name_submitted = name), by = "name_submitted"
  ) %>%
  mutate(
    redlist_germany = str_replace(redlist_germany, "\\*", NA_character_),
    redlist_germany = str_replace(redlist_germany, "nb", NA_character_),
    status = str_replace(status, "I", NA_character_)
  )


### b Combine red list status and traits --------------------------------------

data <- traits %>%
  left_join(
    redlist %>%
      select(accepted_name, status, redlist_germany) %>%
      mutate(
        accepted_name = str_replace(
          accepted_name, "Betonica officinalis", "Stachys officinalis"
        ),
        accepted_name = str_replace(
          accepted_name,
          "Cerastium fontanum", "Cerastium fontanum subsp. vulgare"
        ),
        accepted_name = str_replace(
          accepted_name, "Potentilla verna", "Potentilla tabernaemontani"
        )
      ),
    by = "accepted_name"
  ) %>%
  select(accepted_name, name_submitted, status, redlist_germany, everything())

data %>% filter(duplicated(accepted_name))

data_summarized <- data %>%
  group_by(accepted_name) %>%
  summarize(across(everything(), ~ first(.x))) %>%
  select(
    accepted_name, taxonomic_status, accepted_family,
    everything(), -name_submitted
  ) %>%
  mutate(
    redlist_germany = if_else(
      accepted_name %in% c(
        "Festuca ovina", "Bromus erectus", "Taraxacum", "Thymus praecox"
      ) & !is.na(redlist_germany), NA_character_, redlist_germany
    )
  )

data_summarized  %>% filter(duplicated(accepted_name))

traits <- data_summarized


rm(list = setdiff(ls(), c("species", "sites", "traits")))



## 5 Traits from GIFT database ################################################


### a Load traits from GIFT ---------------------------------------------------

trait_ids <- c("1.2.2", "1.6.3", "3.2.3", "4.1.3")

GIFT::GIFT_traits_meta() %>%
  filter(Lvl3 %in% trait_ids) %>%
  tibble()

data_gift <- GIFT::GIFT_traits(
  trait_IDs = trait_ids,
  agreement = 0.66, bias_ref = FALSE, bias_deriv = FALSE
)

# Harmonization ran once and were than saved --> load below

# harmonized_names <- data_gift %>%
#   rowid_to_column("id") %>%
#   select(id, work_species) %>%
#   TNRS::TNRS(
#     sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#     classification = "wfo", # family classification
#     mode = "resolve"
#   )
# 
# write_csv(
#   harmonized_names, here("data", "processed", "data_processed_tnrs_gift.csv")
#   )

gift <- data.table::fread(
  here("data", "processed", "data_processed_tnrs_gift.csv")
) %>%
  rename_with(tolower) %>%
  full_join(data_gift, by = c("name_submitted" = "work_species")) %>%
  select(name_submitted, accepted_name, starts_with("trait_value_"))


### b Combine gift and traits -------------------------------------------------


data <- traits %>%
  left_join(
    gift %>%
      select(
        accepted_name, trait_value_1.6.3, trait_value_3.2.3, trait_value_4.1.3,
        trait_value_1.2.2
      ) %>%
      mutate(
        accepted_name = str_replace(
          accepted_name, "Betonica officinalis", "Stachys officinalis"
        ),
        accepted_name = str_replace(
          accepted_name, "Potentilla verna", "Potentilla tabernaemontani"
        ),
        accepted_name = str_replace(
          accepted_name, "Tragopogon orientalis", "Tragopogon pratensis"
        ),
        accepted_name = str_replace(
          accepted_name,
          "Cerastium fontanum", "Cerastium fontanum subsp. vulgare"
        ),
        accepted_name = str_replace(
          accepted_name, "Avenula pubescens", "Helictotrichon pubescens"
        )
      ),
    by = "accepted_name"
  ) %>%
  rename(
    family = accepted_family,
    sla = trait_value_4.1.3,
    height = trait_value_1.6.3,
    seedmass = trait_value_3.2.3,
    lifeform = trait_value_1.2.2
  )

data %>% filter(duplicated(accepted_name))

data_summarized <- data %>%
  group_by(accepted_name) %>%
  summarize(across(everything(), ~ first(.x, na_rm = TRUE))) %>%
  select(accepted_name, sla, height, seedmass, lifeform, everything())

data_summarized  %>%
  filter(duplicated(accepted_name))

gift %>%
  filter(str_detect(accepted_name, "Festuca_rubra")) %>%
  select(1:2, starts_with("trait"))

traits <- data_summarized %>%
  mutate(
    across(c("R1A", "R22", "both"), replace_na, 0),
    sla = if_else(accepted_name == "Festuca nigrescens", 193.57, sla), # Value of Festuca rubra
    sla = if_else(accepted_name == "Festuca rupicola", 193.57, sla), # Value of Festuca rubra
    sla = if_else(accepted_name == "Medicago falcata", 219.5, sla), # Value of Medicago sativa
    height = if_else(accepted_name == "Medicago falcata", 0.52, height), # Value of Medicago sativa
    seedmass = if_else(
      accepted_name == "Leucanthemum ircutianum", 0.0008119666, seedmass
    ) # Value of Leucanthemum vulgare
  )


rm(list = setdiff(ls(), c("species", "sites", "traits")))



## 6 Alpha diversity ##########################################################

# Dieser Code ist noch nciht auf unseren Datensatz abgestimmt. (Wenn das gemacht
# ist kann man die Sätze hier löschen)

### a Species richness -------------------------------------------------------

# warum nutzen wir nicht einfach die traits tabelle?
richness <- species %>%
  left_join(traits, by = "name") %>%
  select(
    name, status, redlist_germany, target, starts_with("X")
  ) %>%
  pivot_longer(names_to = "id", values_to = "n", cols = starts_with("X")) %>%
  mutate(n = if_else(n > 0, 1, 0)) %>%
  group_by(id)

#### Total species richness ###
richness_total <- richness %>%
  summarise(species_richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

#### Red list Germany (species richness) ###
richness_rlg <- richness %>%
  filter(rlg == "1" | rlg == "2" | rlg == "3" | rlg == "V") %>%
  summarise(rlg_richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

#### Target species (species richness) ###
richness_target <- richness %>%
  filter(target != "no") %>%
  summarise(target_richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

sites_dikes <- sites_dikes %>%
  right_join(richness_total, by = "id") %>%
  right_join(richness_rlg, by = "id") %>%
  right_join(richness_target, by = "id")
mutate(
  target_richness_ratio = target_richness / species_richness
)


### b Species eveness ---------------------------------------------

data <- species_dikes %>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>%
  pivot_longer(-name, names_to = "id", values_to = "value") %>%
  pivot_wider(names_from = "name", values_from = "value") %>%
  column_to_rownames("id") %>%
  diversity(index = "shannon") %>%
  as_tibble(rownames = NA) %>%
  rownames_to_column(var = "id") %>%
  mutate(id = factor(id)) %>%
  rename(shannon = value)
sites_dikes <- sites_dikes %>%
  left_join(data, by = "id") %>%
  mutate(eveness = shannon / log(species_richness))

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 7 Calculation of CWMs ######################################################


### a CWM Plant height 1.6.3 --------------------------------------------------

### b CWM Seed mass 3.2.3 -----------------------------------------------------

### c CWM SLA 4.1.3 -----------------------------------------------------------



### d Add to sites table ------------------------------------------------------


rm(list = setdiff(ls(), c("species", "sites", "traits")))



## 8 Markus: ESy: EUNIS expert vegetation classification system ################


### a Preparation --------------------------------------------------------------

# Markus: Not prepared yet. Version copied from Garchinger Heide

expertfile <- "EUNIS-ESy-2020-06-08.txt" ### file of 2021 is not working

obs <- species %>%
  pivot_longer(
    cols = -name,
    names_to = "RELEVE_NR",
    values_to = "Cover_Perc"
  ) %>%
  rename(TaxonName = "name") %>%
  mutate(
    TaxonName = str_replace_all(TaxonName, "_", " "),
    TaxonName = str_replace_all(TaxonName, "ssp", "subsp."),
    TaxonName = as.factor(TaxonName),
    RELEVE_NR = as.factor(RELEVE_NR),
    TaxonName = fct_recode(
      TaxonName,
      "Centaurea pannonica" = "Centaurea angustifolia",
      "Euphrasia picta" = "Euphrasia officinalis subsp. picta",
      "Euphrasia officinalis subsp. pratensis" =
        "Euphrasia officinalis subsp. rostkoviana"#,
      # "Lotus corniculatus" = "Lotus corniculatus var corniculatus",
      # "Lotus corniculatus" = "Lotus corniculatus var hirsutus"
    )
  ) %>%
  filter(!is.na(Cover_Perc)) %>%
  data.table::as.data.table()


# Coordinates are in WGS84
header <- sites %>%
  left_join(coordinates %>% select(-plot), by = "id") %>%
  rename(
    RELEVE_NR = id,
    Latitude = latitude,
    Longitude = longitude,
    "Altitude (m)" = elevation
  ) %>%
  mutate(
    RELEVE_NR = as.factor(RELEVE_NR),
    Country = "Germany",
    Coast_EEA = "N_COAST",
    Dunes_Bohn = "N_DUNES",
    Ecoreg = 686,
    dataset = "garchinger_heide"
  ) %>%
  select(
    RELEVE_NR, "Altitude (m)", Latitude, Longitude, Country,
    Coast_EEA, Dunes_Bohn, Ecoreg, dataset
  )


### b Calculation --------------------------------------------------------------

### Bruelheide et al. 2021 Appl Veg Sci
### https://doi.org/10.1111/avsc.12562

setwd(here("R", "esy"))
source(here("R", "esy", "code", "prep.R"))

#### Step 1 and 2: Load and parse the expert file ###
source(here("R", "esy", "code", "step1and2_load-and-parse-the-expert-file.R"))

#### Step 3: Create a numerical plot x membership condition matrix  ###
plot.cond <- array(
  0,
  c(length(unique(obs$RELEVE_NR)), length(conditions)),
  dimnames = list(
    as.character(unique(obs$RELEVE_NR)),
    conditions
  )
)

### Step 4: Aggregate taxon levels ###
source(here("R", "esy", "code", "step4_aggregate-taxon-levels.R"))

(data <- obs %>%
    group_by(TaxonName) %>%
    slice(1) %>%
    anti_join(AGG, by = c("TaxonName" = "ind")))

#### Step 5: Solve the membership conditions ###
mc <- 1
source(
  here(
    "R", "esy", "code", "step3and5_extract-and-solve-membership-conditions.R"
  )
)


### c Summary and integration --------------------------------------------------

table(result.classification)
eval.EUNIS(which(result.classification == "//?")[2], "//?")

data <- sites %>%
  mutate(
    esy = result.classification#,
    # esy = if_else(id == "X05_xxx", "R1A", esy) # nothing to change
  )
table(data$esy)
sites <- data

rm(list = setdiff(ls(), c("species", "sites", "traits")))



## 9 Finalization ############################################################


### a Rounding ----------------------------------------------------------------


### b Final selection of variables --------------------------------------------

traits <- traits %>%
  select(
    accepted_name, taxonomic_status, family, status, redlist_germany,
    everything(), accepted_name_url
  )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data #######################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



write_csv(
  sites,
  here("data", "processed", "data_processed_sites.csv")
)
write_csv(
  species,
  here("data", "processed", "data_processed_species.csv")
)
write_csv(
  traits,
  here("data", "processed", "data_processed_traits.csv")
)