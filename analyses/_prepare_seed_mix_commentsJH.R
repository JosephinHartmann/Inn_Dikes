#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Inn Dikes: Experiment
# Prepare data ####
# Seed mixtures
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2024-12-18



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# !!! Festuca ovina as available but is not available CHECK

#* JH: compare line 42 -> you exclude those that have Illegitimate taxonomic status; 
#* Festuca ovina has Illegitimate tax status. 

### Packages ###
install.packages("here")
library(here)
install.packages("tidyverse")
library(tidyverse)
install.packages("Select")
library(Select)


### Start ###
rm(list = ls())

### Load species list ###
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
      !(ellenberg >= 5410 & ellenberg < 5420)
  ) %>%
  select(
    accepted_name, ellenberg, R1A, R22, both, sla, height, seedmass, family,
    available, taxonomic_status
  ) %>%
  filter(available == 1 & !(taxonomic_status == "Illegitimate"))

trait_ids <- c("1.2.2", "1.6.3", "3.2.3", "4.1.3")

GIFT::GIFT_traits_meta() %>%
  filter(Lvl3 %in% trait_ids) %>%
  tibble()

### Logarithmize trait data ###
traits <- traits %>%
  mutate(
    sla_log = log(sla),
    height_log = log(height),
    seedmass_log = log(seedmass)
  )



## 1 Create subsamples ########################################################

#* JH:why are species excluded that have NA in ellenberg? 

### a Herbs --------------------------------------------------------------------

herbs <- traits %>%
  filter(
    !(family == "Poaceae" | family == "Fabaceae" | family == "Orobanchaceae")
  ) #%>%
#select(accepted_name, family)
herbs_r22 <- traits %>%
  filter(
    !(family == "Poaceae" | family == "Fabaceae" | family == "Orobanchaceae") &
      (ellenberg >= 5420 & ellenberg < 5430 | ellenberg == 5400)
  ) #%>%
#select(accepted_name, family)
herbs_r1a <- traits %>%
  filter(
    !(family == "Poaceae" | family == "Fabaceae" | family == "Orobanchaceae") &
      (ellenberg >= 5300 & ellenberg < 5400)
  ) #%>%
#select(accepted_name, family)
herbs_rest <- traits %>%
  filter(
    !(family == "Poaceae" | family == "Fabaceae" | family == "Orobanchaceae") &
      !(ellenberg >= 5420 & ellenberg < 5430 | ellenberg == 5400) &
      !(ellenberg >= 5300 & ellenberg < 5400)
  ) #%>%
#select(accepted_name, family)


### b Grass --------------------------------------------------------------------

grass <- traits %>%
  filter(
    family == "Poaceae" &
      !(accepted_name == "Agrostis stolonifera") &
      !(accepted_name == "Festuca nigrescens")
  ) #%>%
# select(accepted_name, family)
grass_r22 <- traits %>%
  filter(
    family == "Poaceae" &
      !(accepted_name == "Agrostis stolonifera") &
      !(accepted_name == "Festuca nigrescens") &
      (ellenberg >= 5420 & ellenberg < 5430 | ellenberg == 5400)
  ) #%>%
# select(accepted_name, family)
grass_r1a <- traits %>%
  filter(
    family == "Poaceae" &
      !(accepted_name == "Agrostis stolonifera") &
      !(accepted_name == "Festuca nigrescens") &
      (ellenberg >= 5300 & ellenberg < 5400)
  ) #%>%
# select(accepted_name, family)
grass_rest <- traits %>%
  filter(
    family == "Poaceae" &
      !(accepted_name == "Agrostis stolonifera") &
      !(accepted_name == "Festuca nigrescens") &
      !(ellenberg >= 5420 & ellenberg < 5430 | ellenberg == 5400) &
      !(ellenberg >= 5300 & ellenberg < 5400)
  ) #%>%
# select(accepted_name, family)


### c Legumes ------------------------------------------------------------------

legumes <- traits %>%
  filter(family == "Fabaceae") #%>%
#select(name, family)
legumes_r22 <- traits %>%
  filter(
    family == "Fabaceae" &
      (ellenberg >= 5420 & ellenberg < 5430 | ellenberg == 5400)
  ) #%>%
# select(accepted_name, family)
legumes_r1a <- traits %>%
  filter(
    family == "Fabaceae" &
      (ellenberg >= 5300 & ellenberg < 5400)
  ) #%>%
# select(accepted_name, family)
legumes_rest <- traits %>%
  filter(
    family == "Fabaceae" &
      !(ellenberg >= 5420 & ellenberg < 5430 | ellenberg == 5400) &
      !(ellenberg >= 5300 & ellenberg < 5400)
  ) #%>%
# select(accepted_name, family)


### d Hemiparasites ------------------------------------------------------------

hemiparasites <- traits %>%
  filter(family == "Orobanchaceae") #%>%
# select(name, family)


grass_r1a %>%
  mutate(type = "grass_r1a") %>%
  relocate(type, .after = R22) %>%
  bind_rows(grass_r22 %>% mutate(type = "grass_r22")) %>%
  bind_rows(grass_rest %>% mutate(type = "grass_rest")) %>%
  bind_rows(herbs_r1a %>% mutate(type = "herbs_r1a")) %>%
  bind_rows(herbs_r22 %>% mutate(type = "herbs_r22")) %>%
  bind_rows(herbs_rest %>% mutate(type = "herbs_rest")) %>%
  bind_rows(legumes_r1a %>% mutate(type = "legumes_r1a")) %>%
  bind_rows(legumes_r22 %>% mutate(type = "legumes_r22")) %>%
  bind_rows(legumes_rest %>% mutate(type = "legumes_rest")) %>%
  bind_rows(hemiparasites %>% mutate(type = "hemiparasites")) %>%
  write_csv(here("data", "raw", "data_raw_seed_mixtures.csv"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Seed mixture creation #####################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Taxonomic composition ####################################################

#* JH: unfortunately I do not fully understand this step: 
#* a) we create compositions randomly from both species pools incl. "rest-pool";
#* b) 48 compositions/replicates each - does this number come from the available site-combinations per grassland type 
#* c) you choose how many species should be drawn per group, aiming for compositions with 18 species
#* d) WHY 18 species (why aiming for 20 - literature?)

compData <- as.data.frame(replicate(48, {
  comp <- c(
    sample(herbs_r1a$accepted_name, 8), # of totally 8
    sample(herbs_rest$accepted_name, 4), # of totally 4
    sample(grass_r1a$accepted_name, 1), # of totally 1
    sample(grass_rest$accepted_name, 1), # of totally 1
    sample(legumes_r1a$accepted_name, 2), # of totally 2
    sample(legumes_rest$accepted_name, 1), # of totally 1
    sample(hemiparasites$accepted_name, 1) # of totally 1
  )
}
)) %>%
  mutate(id = 1:18) %>%
  pivot_longer(-id, names_to = "name", values_to = "value")

compData <- as.data.frame(replicate(48, {
  comp <- c(
    sample(herbs_r22$accepted_name, 5), # of totally 14
    sample(herbs_rest$accepted_name, 4), # of totally 4
    sample(grass_r22$accepted_name, 4), # of totally 9
    sample(grass_rest$accepted_name, 1), # of totally 1
    sample(legumes_r22$accepted_name, 2), # of totally 2
    sample(legumes_rest$accepted_name, 1), # of totally 1
    sample(hemiparasites$accepted_name, 1) # of totally 1
  )
}
)) %>%
  gather(compData, "comp", "name", 1:48)

table(compData$name)
length(table(compData$name))

compData <- traits %>%
  inner_join(compData, by = "name")



## 2 Calculate abundance values ##############################################


# Laughlin et al. 2018 Methods Ecol Evol
# https://doi.org/10.1111/2041-210X.13023

#* JH: here we fill data gaps - but what is "r"; the chosen values are means? why seedmass "0"

compData[c(which(is.na(compData$sla))), "sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))), "seedmass"] <- 0
compData[c(which(is.na(compData$r))), "r"] <- 6.5
compData <- compData %>%
  select(name, comp, sla, seedmass, grass, legume) %>%
  mutate(comp = as.numeric(gsub("V", "", comp)))


#* JH: take 24 combinations (from R22) but contrain them to avg. SLA of 24
#* on basis of which considerations are the constraints of grass, legumes and hemiparasites set to those numbers? half proportion grasses (50 % abudance)? 9 grasses? 5% legumes and hemiparasites?

ratioResults <- c(0)
for (i in 1:24) { #high SLA, R22
  plotcompData <- compData[which(compData$comp == i), ]
  row.names(plotcompData) <- plotcompData[, "accepted_name"]
  plotcompData <- plotcompData[, -(1:2)]
  mix <- selectSpecies(
    t2c = as.matrix(plotcompData),
    constraints = c(
      sla = log(24),
      grass = 0.5,
      legumes = 0.05,
      hemiparasites = 0.05
    ),
    t2d = as.matrix(plotcompData$seedmass),
    obj = "QH",
    capd = TRUE
  )
  ratioResults <- append(ratioResults, mix$prob)
}
ratioResults <- ratioResults[-1]

#* JH: take 24 combinations (from R22) but contrain them to avg. SLA of 20
#* 
for (i in 25:48) { #low SLA, R22
  plotcompData <- compData[which(compData$comp == i), ]
  row.names(plotcompData) <- plotcompData[, "accepted_name"]
  plotcompData <- plotcompData[, -(1:2)]
  mix <- selectSpecies(
    t2c = as.matrix(plotcompData),
    constraints = c(
      sla = log(20),
      grass = 0.5,
      legumes = 0.1,
      hemiparasites = 0.05
    ),
    t2d = as.matrix(plotcompData$seedmass),
    obj = "QH",
    capd = TRUE
  )
  ratioResults <- append(ratioResults, mix$prob)
}

#* the following step only occurs here, only for this set - I dont understand

# Define constraints once
constraints_list <- list(
  sla = 20
)

# Use map to iterate over unique `comp` values in the range 25:48
ratioResults <- compData %>%
  filter(comp %in% 25:48) %>% # Filter for the desired range
  group_split(comp) %>%       # Split data by `comp` into a list of data frames
  map_dbl(~ {
    plotcompData <- .x %>%
      column_to_rownames(var = "accepted_name") %>%
      select(-c(comp, accepted_name)) %>%
      as.matrix()
    
    mix <- selectSpecies(
      plotcompData,
      constraints = constraints_list,
      plotcompData,
      obj = "QH",
      capd = TRUE
    )
    
    mix$prob # Extract the probability result for each group
  })


#* for R1A, you also constrain r=7 - WHAT is R and WHY constraining?
#* also, I do not really understand which considerations follow grass and legume constraint; 

for (i in 49:72) { #high SLA, R1A
  plotcompData <- compData[which(compData$comp == i), ]
  row.names(plotcompData) <- plotcompData[, "accepted_name"]
  plotcompData <- plotcompData[, -(1:2)]
  mix <- selectSpecies(as.matrix(plotcompData),
                       constraints = c(sla = 3.135494,
                                       seedmass = -0.2876821,
                                       r = 7,
                                       grass = 0.6,
                                       legume = 0.05),
                       as.matrix(plotcompData),
                       obj = "QH",
                       capd = TRUE)
  ratioResults <- append(ratioResults, mix$prob)
}

for (i in 73:96) { #lowh SLA, R1A
  plotcompData <- compData[which(compData$comp == i), ]
  row.names(plotcompData) <- plotcompData[, "accepted_name"]
  plotcompData <- plotcompData[, -(1:2)]
  mix <- selectSpecies(as.matrix(plotcompData),
                       constraints = c(sla = 3.135494,
                                       seedmass = -0.2876821,
                                       r = 7,
                                       grass = 0.6,
                                       legume = 0.05),
                       as.matrix(plotcompData),
                       obj = "QH",
                       capd = TRUE)
  ratioResults <- append(ratioResults, mix$prob)
}

rm(plotcompData, mix)
compData <- arrange(compData, comp)
compData$ratio <- ratioResults


## 3 Control of seedmixtures #################################################

#* sorry, but the whole control seedmixtures procedure I do not yet understand what is going on. 

### a Control of seedmixtures ------------------------------------------------
#### Control
compData[which(compData$ratio > 0.75), ]
plotRatio <- compData %>%
  group_by(comp) %>%
  summarise(sum = sum(ratio))
table(round(plotRatio$sum, 2))
plotRatio[which(plotRatio$sum < 0.99 | plotRatio$sum > 1.01), ]

#### Correct unsolvable taxonomic composition #####
compData[compData$comp == 7, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 25, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 71, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 82, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 94, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 103, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 104, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 106, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 112, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 113, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 117, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 126, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 127, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 128, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
names <- select(compData, name, comp)
compData <- inner_join(vdata, names, by = "name")
compData[c(which(is.na(compData$sla))), "sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))), "seedmass"] <- 0
compData[c(which(is.na(compData$r))), "r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)

#### Correct wrong mixture ratios ####
plotcompData <- compData[which(compData$comp == 128), ]
row.names(plotcompData) <- plotcompData[, "name"]
plotcompData <- plotcompData[, -(1:2)]
mix <- selectSpecies(as.matrix(plotcompData),
                     constraints = c(sla = 3.135494,
                                     seedmass = -0.2876821,
                                     r = 7,
                                     grass = 0.6,
                                     legume = 0.05),
                     as.matrix(plotcompData),
                     obj = "QH",
                     capd = TRUE)
X128 <- mix$prob

#### Implement new ratios to compData ####
compData <- arrange(compData, comp)
compData$ratio <- ratioResults
compData[compData$comp == 7, "ratio"] <- X7
compData[compData$comp == 25, "ratio"] <- X25
compData[compData$comp == 71, "ratio"] <- X71
compData[compData$comp == 94, "ratio"] <- X94
compData[compData$comp == 103, "ratio"] <- X103
compData[compData$comp == 104, "ratio"] <- X104
compData[compData$comp == 106, "ratio"] <- X106
compData[compData$comp == 112, "ratio"] <- X112
compData[compData$comp == 117, "ratio"] <- X117
compData[compData$comp == 126, "ratio"] <- X126
compData[compData$comp == 127, "ratio"] <- X127
compData[compData$comp == 128, "ratio"] <- X128
ratioResults <- compData$ratio
rm(X7, X25, X71, X94, X103, X104, X106, X112, X117, X126, X127, X128)

### b Control of seedmixtures ------------------------------------------------
####Control
compData[which(compData$ratio > 0.6), ]
plotRatio <- compData %>%
  group_by(comp) %>%
  summarise(sum = sum(ratio))
table(round(plotRatio$sum, 2))
plotRatio[which(plotRatio$sum < 0.995 | plotRatio$sum > 1.005), ]

#### Correct unsolvable taxonomic composition ####
compData[compData$comp == 48, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 82, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 83, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 113, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
names <- select(compData, name, comp)
compData <- inner_join(vdata, names, by = "name")
compData[c(which(is.na(compData$sla))), "sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))), "seedmass"] <- 0
compData[c(which(is.na(compData$r))), "r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)

#### Correct wrong mixture ratios ####
plotcompData <- compData[which(compData$comp == 48), ]
row.names(plotcompData) <- plotcompData[, "name"]
plotcompData <- plotcompData[, -(1:2)]
mix <- selectSpecies(as.matrix(plotcompData),
                     constraints = c(sla = 2.995732,
                                     seedmass = 0.2231436,
                                     r = 7,
                                     grass = 0.3,
                                     legume = 0.15),
                     as.matrix(plotcompData),
                     obj = "QH",
                     capd = TRUE)
X48 <- mix$prob

#### Implement new ratios to compData ####
compData <- arrange(compData, comp)
compData$ratio <- ratioResults
compData[compData$comp == 82, "ratio"] <- X82
compData[compData$comp == 83, "ratio"] <- X83
compData[compData$comp == 113, "ratio"] <- X113
ratioResults <- compData$ratio
rm(X82, X83, X113)

### c Control of seedmixtures -------------------------------------------------
#### Control
compData[which(compData$ratio > 0.4), ]
plotRatio <- compData %>%
  group_by(comp) %>%
  summarise(sum = sum(ratio))
table(round(plotRatio$sum, 2))
plotRatio[which(plotRatio$sum < 0.995 | plotRatio$sum > 1.005), ]

#### Correct unsolvable taxonomic composition ####
compData[compData$comp == 48, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
names <- select(compData, name, comp)
compData <- inner_join(vdata, names, by = "name")
compData[c(which(is.na(compData$sla))), "sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))), "seedmass"] <- 0
compData[c(which(is.na(compData$r))), "r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)

#### Correct wrong mixture ratios ####
plotcompData <- compData[which(compData$comp == 48), ]
row.names(plotcompData) <- plotcompData[, "name"]
plotcompData <- plotcompData[, -(1:2)]
mix <- selectSpecies(as.matrix(plotcompData),
                     constraints = c(sla = 2.995732,
                                     seedmass = 0.2231436,
                                     r = 7,
                                     grass = 0.3,
                                     legume = 0.15),
                     as.matrix(plotcompData),
                     obj = "QH",
                     capd = TRUE)
X48 <- mix$prob

#### Implement new ratios to compData ####
compData <- arrange(compData, comp)
compData$ratio <- ratioResults
compData[compData$comp == 48, "ratio"] <- X48
ratioResults <- compData$ratio
rm(X48)

### d Control of seedmixtures -------------------------------------------------
#### Control
compData[which(compData$ratio > 0.5), ]
plotRatio <- compData %>%
  group_by(comp) %>%
  summarise(sum = sum(ratio))
table(round(plotRatio$sum, 2))
plotRatio[which(plotRatio$sum < 0.995 | plotRatio$sum > 1.005), ]



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## C Export ##################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

####Assign right composition numbers
compData <- read.table(
  here("data", "raw", "composition.txt"),
  header = TRUE,
  na.strings = "na",
  dec = "."
)
compData[compData$comp == 1, "comp"] <- "x5"
compData[compData$comp == 2, "comp"] <- "x6"
compData[compData$comp == 3, "comp"] <- "x7"
compData[compData$comp == 4, "comp"] <- "x8"
compData[compData$comp == 5, "comp"] <- "x17"
compData[compData$comp == 6, "comp"] <- "x18"
compData[compData$comp == 7, "comp"] <- "x25"
compData[compData$comp == 8, "comp"] <- "x26"
compData[compData$comp == 9, "comp"] <- "x27"
compData[compData$comp == 10, "comp"] <- "x28"
compData[compData$comp == 11, "comp"] <- "x37"
compData[compData$comp == 12, "comp"] <- "x38"
compData[compData$comp == 13, "comp"] <- "x45"
compData[compData$comp == 14, "comp"] <- "x46"
compData[compData$comp == 15, "comp"] <- "x47"
compData[compData$comp == 16, "comp"] <- "x48"
compData[compData$comp == 17, "comp"] <- "x57"
compData[compData$comp == 18, "comp"] <- "x58"
compData[compData$comp == 19, "comp"] <- "x65"
compData[compData$comp == 20, "comp"] <- "x66"
compData[compData$comp == 21, "comp"] <- "x67"
compData[compData$comp == 22, "comp"] <- "x68"
compData[compData$comp == 23, "comp"] <- "x77"
compData[compData$comp == 24, "comp"] <- "x78"
compData[compData$comp == 25, "comp"] <- "x85"
compData[compData$comp == 26, "comp"] <- "x86"
compData[compData$comp == 27, "comp"] <- "x87"
compData[compData$comp == 28, "comp"] <- "x88"
compData[compData$comp == 29, "comp"] <- "x97"
compData[compData$comp == 30, "comp"] <- "x98"
compData[compData$comp == 31, "comp"] <- "x105"
compData[compData$comp == 32, "comp"] <- "x106"
compData[compData$comp == 33, "comp"] <- "x107"
compData[compData$comp == 34, "comp"] <- "x108"
compData[compData$comp == 35, "comp"] <- "x117"
compData[compData$comp == 36, "comp"] <- "x118"
compData[compData$comp == 37, "comp"] <- "x125"
compData[compData$comp == 38, "comp"] <- "x126"
compData[compData$comp == 39, "comp"] <- "x127"
compData[compData$comp == 40, "comp"] <- "x128"
compData[compData$comp == 41, "comp"] <- "x137"
compData[compData$comp == 42, "comp"] <- "x138"
compData[compData$comp == 43, "comp"] <- "x145"
compData[compData$comp == 44, "comp"] <- "x146"
compData[compData$comp == 45, "comp"] <- "x147"
compData[compData$comp == 46, "comp"] <- "x148"
compData[compData$comp == 47, "comp"] <- "x157"
compData[compData$comp == 48, "comp"] <- "x158"
compData[compData$comp == 49, "comp"] <- "x9"
compData[compData$comp == 50, "comp"] <- "x10"
compData[compData$comp == 51, "comp"] <- "x11"
compData[compData$comp == 52, "comp"] <- "x12"
compData[compData$comp == 53, "comp"] <- "x29"
compData[compData$comp == 54, "comp"] <- "x30"
compData[compData$comp == 55, "comp"] <- "x31"
compData[compData$comp == 56, "comp"] <- "x32"
compData[compData$comp == 57, "comp"] <- "x49"
compData[compData$comp == 58, "comp"] <- "x50"
compData[compData$comp == 59, "comp"] <- "x51"
compData[compData$comp == 60, "comp"] <- "x52"
compData[compData$comp == 61, "comp"] <- "x69"
compData[compData$comp == 62, "comp"] <- "x70"
compData[compData$comp == 63, "comp"] <- "x71"
compData[compData$comp == 64, "comp"] <- "x72"
compData[compData$comp == 65, "comp"] <- "x89"
compData[compData$comp == 66, "comp"] <- "x90"
compData[compData$comp == 67, "comp"] <- "x91"
compData[compData$comp == 68, "comp"] <- "x92"
compData[compData$comp == 69, "comp"] <- "x109"
compData[compData$comp == 70, "comp"] <- "x110"
compData[compData$comp == 71, "comp"] <- "x111"
compData[compData$comp == 72, "comp"] <- "x112"
compData[compData$comp == 73, "comp"] <- "x129"
compData[compData$comp == 74, "comp"] <- "x130"
compData[compData$comp == 75, "comp"] <- "x131"
compData[compData$comp == 76, "comp"] <- "x132"
compData[compData$comp == 77, "comp"] <- "x149"
compData[compData$comp == 78, "comp"] <- "x150"
compData[compData$comp == 79, "comp"] <- "x151"
compData[compData$comp == 80, "comp"] <- "x152"
compData[compData$comp == 81, "comp"] <- "x13"
compData[compData$comp == 82, "comp"] <- "x14"
compData[compData$comp == 83, "comp"] <- "x15"
compData[compData$comp == 84, "comp"] <- "x16"
compData[compData$comp == 85, "comp"] <- "x19"
compData[compData$comp == 86, "comp"] <- "x20"
compData[compData$comp == 87, "comp"] <- "x33"
compData[compData$comp == 88, "comp"] <- "x34"
compData[compData$comp == 89, "comp"] <- "x35"
compData[compData$comp == 90, "comp"] <- "x36"
compData[compData$comp == 91, "comp"] <- "x39"
compData[compData$comp == 92, "comp"] <- "x40"
compData[compData$comp == 93, "comp"] <- "x53"
compData[compData$comp == 94, "comp"] <- "x54"
compData[compData$comp == 95, "comp"] <- "x55"
compData[compData$comp == 96, "comp"] <- "x56"
compData[compData$comp == 97, "comp"] <- "x59"
compData[compData$comp == 98, "comp"] <- "x60"
compData[compData$comp == 99, "comp"] <- "x73"
compData[compData$comp == 100, "comp"] <- "x74"
compData[compData$comp == 101, "comp"] <- "x75"
compData[compData$comp == 102, "comp"] <- "x76"
compData[compData$comp == 103, "comp"] <- "x79"
compData[compData$comp == 104, "comp"] <- "x80"
compData[compData$comp == 105, "comp"] <- "x93"
compData[compData$comp == 106, "comp"] <- "x94"
compData[compData$comp == 107, "comp"] <- "x95"
compData[compData$comp == 108, "comp"] <- "x96"
compData[compData$comp == 109, "comp"] <- "x99"
compData[compData$comp == 110, "comp"] <- "x100"
compData[compData$comp == 111, "comp"] <- "x113"
compData[compData$comp == 112, "comp"] <- "x114"
compData[compData$comp == 113, "comp"] <- "x115"
compData[compData$comp == 114, "comp"] <- "x116"
compData[compData$comp == 115, "comp"] <- "x119"
compData[compData$comp == 116, "comp"] <- "x120"
compData[compData$comp == 117, "comp"] <- "x133"
compData[compData$comp == 118, "comp"] <- "x134"
compData[compData$comp == 119, "comp"] <- "x135"
compData[compData$comp == 120, "comp"] <- "x136"
compData[compData$comp == 121, "comp"] <- "x139"
compData[compData$comp == 122, "comp"] <- "x140"
compData[compData$comp == 123, "comp"] <- "x153"
compData[compData$comp == 124, "comp"] <- "x154"
compData[compData$comp == 125, "comp"] <- "x155"
compData[compData$comp == 126, "comp"] <- "x156"
compData[compData$comp == 127, "comp"] <- "x159"
compData[compData$comp == 128, "comp"] <- "x160"
#### Export
compData$weight <- round(compData$ratio * 0.48, 2)
(weights <- compData %>%
    group_by(name) %>%
    summarise(sum = sum(weight)))
plotWeights <- compData %>%
  group_by(comp) %>%
  summarise(sum = sum(weight))
table(plotWeights$sum)

write_csv(compData,
          here("data", "raw",
               "data_raw_experiment_3_compositions.csv"))