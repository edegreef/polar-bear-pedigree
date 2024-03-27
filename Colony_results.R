# Look at COLONY outputs

library(tidyverse)
library(pedantics)
setwd("C:/Users/eveli/Dropbox/Polar_Bears_ECCC/COLONY/results")

# Load in the .BestConfig file
config <- read.table("polarbear_highprecision/polarbear.BestConfig", header=T)

# Change order to Offspring, Dam, and Sire and rename columns to match initial pedigree
new_ped <- config[,c(1,3,2)]
colnames(new_ped) <- c("id", "dam", "sire")

# Change colony-reconstructed Sire names to COLONYID# (replace asterisk(*) with "COLONYID")
new_ped$sire <- sub("\\*", "COLONYID", new_ped$sire)

# If there any new maternities (which means some mothers without genotypes snuck in), they are labeled with "#" but R will not read it. Used notepad++ to manually replace "#" to "_" before loading, and then remove the "_" here. But for final COLONY run, should not have any new maternities.
#new_ped$dam <- sub("_", NA, new_ped$dam)
#new_ped <- na.omit(new_ped)

# So this would be reconstructing 1322 paternities
# Count how many dads here
n_distinct(new_ped$sire)
# 524 new dads

# Next, need to add to initial pedigree

# Load initial pedigree
ped <- read.table("C:/Users/eveli/Dropbox/Polar_bears_ECCC/FRANz/results/without_adopt/reformat/pedigree_formated.txt", header=T)

## Add Sire data for those that match Offspring #
# pull out just sires
new_ped_sires <- new_ped[,c(1,3)]

# Start with left_join?
full_ped <- left_join(ped, new_ped_sires, by="id")

# merge the sire columns
full_ped <- full_ped %>% mutate(final_sire = coalesce(sire.x, sire.y))

# clean up
full_ped <- full_ped[,c("id", "dam", "final_sire")]

colnames(full_ped) <- c("id", "dam", "sire")

final_pedigree <- fixPedigree(full_ped)

write.table(final_pedigree, "polarbear_highprecision/postCOLONY/final_pedigree.txt", row.names = F, col.names = T, quote=F)

# red = mums, blue=dads
drawPedigree(final_pedigree, dotSize=0.002, dots="y", sexColours=c("#d53838", "#0382ef"))

# get final stats
stats <- pedigreeStats(final_pedigree)
stats_summary <- pedStatSummary(stats)
stats_sum_tab <- as.data.frame(stats_summary)

write.table(stats_sum_tab, "polarbear_highprecision/postCOLONY/final_pedigree_stats.txt", quote=F)
