# Look at polar bear SNP data
# 3 different data sets: PB_Chip1_AllBears, PB_Chip2_All_Individuals, SNP_Probe

library(adegenet)
library(vcfR)
library(dplyr)
library(tidyverse)

setwd("C:/Users/eveli/Dropbox/Polar_bears_ECCC/data/snps")


#### Starting with SNP Chip1
# Read in vcf
chip1 <- read.vcfR("PB_Chip1_AllBears.vcf") 

# Reformat  vcfR object into genlight object
chip1 <- vcfR2genlight(chip1)

# Look at sample IDs
chip1@ind.names

# looks like there is a number in family_ID slot so that's why each sample name is like "25_X10676"... need to remove the first few digits since this looks more like a row number than a proper family ID. (numbers 25 through 1488, though there are 1430 individuals. Maybe some numbers skipped?)

chip1_inds <- as.data.frame(chip1@ind.names)
colnames(chip1_inds) <- "ID"
chip1_IDs <- as.data.frame(gsub(".*_","", chip1_inds$ID))
colnames(chip1_IDs) <- "ID"

# SNP Chip1 has 1430 individuals, 5370 SNPs

#### SNP Chip2
# Read in vcf
chip2 <- read.vcfR("PB_Chip2_All_Individuals.vcf") 

# Reformat  vcfR object into genlight object
chip2 <- vcfR2genlight(chip2)

# Look at sample IDs
chip2@ind.names

# same thing here about the family_ID column. Also noting here that some individual IDs are weird, some are just numbers like "5" or "312" or something. Also multiple "2"s. These will just have to be dropped later... keeping in for now but will remove at end.

chip2_inds <- as.data.frame(chip2@ind.names)
colnames(chip2_inds) <- "ID"
chip2_IDs <- as.data.frame(gsub(".*_","", chip2_inds$ID))
colnames(chip2_IDs) <- "ID"

# SNP Chip2 has 1085 individuals, 7239 SNPs

#### SNP Probe
# Read in vcf
probe <- read.vcfR("SNP_Probe.vcf") 

# Reformat  vcfR object into genlight object
probe <- vcfR2genlight(probe)

# Look at sample IDs
probe@ind.names

# here, the family ID column just says "WH", which we will remove too

probe_inds <- as.data.frame(probe@ind.names)
colnames(probe_inds) <- "ID"
probe_IDs <- as.data.frame(gsub(".*_","", probe_inds$ID))
colnames(probe_IDs) <- "ID"

# SNP Probe has 391 individuals, 4180 SNPs

##### Look at # unique individuals, also see which ones overlap or don't overlap with the pedigree ? 

chip1_IDs$dat <- "chip1"
chip2_IDs$dat <- "chip2"
probe_IDs$dat <- "probe"

#snps_IDs <- rbind(chip1_IDs, chip2_IDs, probe_IDs)
#snps_IDs_unique <- distinct(snps_IDs, ID, .keep_all = FALSE)

# Can use "inds_list_all.txt" for list of individuals in initial pedigree
inds_list_ped <- read.table("C:/Users/eveli/Dropbox/Polar_bears_ECCC/data/microsat/inds_list_all.txt")

colnames(inds_list_ped) <- "ID"
inds_list_ped$dat <- "ped"

ids <- full_join(inds_list_ped, chip1_IDs, by="ID") %>%
  full_join(chip2_IDs, by="ID") %>% full_join(probe_IDs, by="ID")

ids_just_ped_match <- left_join(inds_list_ped, chip1_IDs, by="ID") %>%
  left_join(chip2_IDs, by="ID") %>% left_join(probe_IDs, by="ID")

# mark which ones have snp dat to go with it
# if data in col 3-5, then mark X in col 6?
# or another way, sum the number of NAs and anything that is 3 NAs can remove/ignore?

ids_just_ped_match$NAs <- apply(ids_just_ped_match, MARGIN=1, function(x) sum(is.na(x)))

ids_ped_and_snps <- subset(ids_just_ped_match, NAs < 3)                     

### So looks like 1268 individuals in SNP data overlapping with microsat
