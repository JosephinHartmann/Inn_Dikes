# Load required packages
library(here)

# Read data using here() for portable paths
knapp <- read.csv(
  here("data", "raw", "data_raw_compositions_sla_210_230.csv"),
  header = TRUE
)

View(knapp)
unique(knapp$accepted_name)
length(unique(knapp$accepted_name))


knapp1a_high <- subset(knapp, pool == "r1a" & trait_constraint == "high")
knapp1a_low <- subset(knapp, pool == "r1a" & trait_constraint == "low")
knapp22 <- subset(knapp, pool =="r22")
unique(knapp1a_low$accepted_name)
unique(knapp1a_high$accepted_name)
common_species_R1A <- intersect(knapp1a_high$accepted_name, knapp1a_low$accepted_name)
print(common_species_R1A)

View(knapp1a)
R1Aspecs <- as.data.frame(unique(knapp1a$accepted_name))
View(R1Aspecs)

R22specs <- as.data.frame(unique(knapp22$accepted_name))
View(R22specs)
colnames(R22specs) <- "species"
colnames(R1Aspecs) <- "species"

specs <- anti_join(R1Aspecs, R22specs, by="species")
View(specs)
library(dplyr)



View(species)

knapp1a_40er <- subset(knapp1a, id_plot >= 40)
View(knapp1a_40er)

R1Aspecs_40er <- as.data.frame(unique(knapp1a_40er$accepted_name))

rbind(R1Aspecs, R1Aspecs_40er)
