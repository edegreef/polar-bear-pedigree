# Preparing input files COLONY

library(tidyverse)

setwd("C:/Users/eveli/Dropbox/Polar_bears_ECCC/COLONY")

# This script to make OffspringGenotype.txt, MaternalGenotype.txt, and KnownMaternalAssignments.txt

# KnownAlleleFrequency.txt and MarkerTypeErrorRate.txt were created manually. The allele frequency values created with GenAlEx on main genotype dataset and then manually formatted to COLONY format.

# Need microsat data file and pedigree

# Load microsatellite data
dat <- read.csv("C:/Users/eveli/Dropbox/Polar_bears_ECCC/data/microsat/Micros_and_field_dataset.csv", header=T)

# Remove "Born" "Death" and "OriginalSex"
dat2 <- subset(dat, select=-c(Born, Death, OriginalSex))

# Load pedigree
ped <- read.table("C:/Users/eveli/Dropbox/Polar_bears_ECCC/FRANz/results/without_adopt/reformat/pedigree_formated.txt", header=T)

# Ultimately want to include offspring that have genetically assigned dams but unassigned sires.
with_dams <- ped %>% drop_na(dam)
with_dams_nosire <- with_dams %>% filter(if_any(everything(), is.na))
colnames(with_dams_nosire) <- c("ID", "DAM", "SIRE")

# Make list of offspring, list of moms
offspring_list <- as.data.frame(with_dams_nosire$ID)
colnames(offspring_list)<-"ID"

mom_list <- as.data.frame(with_dams_nosire$DAM)
colnames(mom_list)<-"ID"

# Add microsatellite data, starting with moms
mom_genotype <- left_join(mom_list, dat2, by="ID")
mom_genotype2 <- na.omit(mom_genotype)

# Keep unique entries
mom_genotype3 <- mom_genotype2 %>% distinct()

# Exclude offspring that does NOT have an assigned genotyped mom (did not check this on initial run and some offspring without genotyped moms got in)
ped_mom_geno <- semi_join(ped, mom_genotype3, by=c("dam"="ID"))
ped_mom_geno_nosire <- ped_mom_geno %>% filter(if_any(everything(), is.na))
colnames(ped_mom_geno_nosire) <- c("ID", "DAM", "SIRE")

# List of offspring we want to extract genotypes, first pick ones that have a genotyped-mother
offspring_list_mom_geno <- semi_join(offspring_list, ped_mom_geno_nosire, by="ID")
# Add genotype data
offspring_genotype <- left_join(offspring_list_mom_geno, dat2, by="ID")
# Remove entires with NAs (no genotypes)
offspring_genotype2 <- na.omit(offspring_genotype)

# Save as text files
write.table(offspring_genotype2, "C:/Users/eveli/Dropbox/Polar_bears_ECCC/COLONY/input/OffspringGenotype.txt", row.names = F, col.names = F, quote=F)
write.table(mom_genotype3, "C:/Users/eveli/Dropbox/Polar_bears_ECCC/COLONY/input/MaternalGenotype.txt", row.names = F, col.names = F, quote=F)

# Next, prepare KnownMaternalAssignments list.
# COLONY wants specific format, based on manual it should mom ID in column 1 (no duplicates), and then offspring in each column (also no duplicates)
# So for example, heree are two moms, 3 offspring total, 1 sibling pair.
#F1  O1  O2
#F2  O3

# Use lists with genotypes only, should be total of 1357 offspring, and 630 moms
# Make list of offspring included in OffspringGenotype.txt
offspring_list_geno <- as.data.frame(offspring_genotype2$ID)
colnames(offspring_list_geno) <- "OffspringID"

# Add in mom assignments from pedigree
mat_assign_geno <- left_join(offspring_list_geno, ped, by=c("OffspringID"="id"))

# Change order of dataframe to put mother first
mat_assign_geno <- mat_assign_geno[,c(2,1)]

# Rename columns
colnames(mat_assign_geno) <- c("MotherID", "OffspringID")

# Now need to do opposite of melt?
# Try pivot_wider
mat_assign_geno2 <- mat_assign_geno %>% pivot_wider(names_from = OffspringID, values_from=OffspringID)

# Not exactly what I had in mind, but it works and I can just rename columns and take out NAs

# Function for removing NAs and moving data to the left
library(SwimmeR)
# i dont know why R is not reading fill_left, so just using the source code for the function
fill_left <- function(df) {
  df <- as.data.frame(t(apply(df, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))
  
  df <- Filter(function(x)
    ! all(is.na(x)), df)
  return(df)
}

mat_assign_geno3 <- fill_left(mat_assign_geno2)

# Rename all columns, here there are 9
colnames(mat_assign_geno3) <- c("MotherID", "OffspringID1", "OffspringID2", "OffspringID3", "OffspringID4", "OffspringID5", "OffspringID6", "OffspringID7", "OffspringID8")

# Remember to remove mothers without genotypes - should be 615 rows total.
# List in MaternalGenotypes.txt
mom_list_geno <- as.data.frame(mom_genotype3$ID)
colnames(mom_list_geno) <- "ID"

# Select assignments that include mother genotypes
mat_assign_geno_final <- semi_join(mat_assign_geno3, mom_list_geno, by=c("MotherID"="ID"))

# Change NAs to blank
mat_assign_geno_final[is.na(mat_assign_geno_final)] <- ""

# Save file
write.table(mat_assign_geno_final, "C:/Users/eveli/Dropbox/Polar_bears_ECCC/COLONY/input/KnownMaternalAssignment.txt", row.names = F, col.names = F, quote=F)
