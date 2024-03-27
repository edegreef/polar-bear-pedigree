# Examining resulting outputs from FRANz

library(tidyverse)
library(devtools)

# look at mismatches in the "with_adopt" results
# And look at it again in the "without_adopt" results too just to double check.
setwd("C:/Users/eveli/Dropbox/Polar_bears_ECCC/FRANz/results/without_adopt/")

# mismatches_table.txt was manually formed using the "mismatches.txt" output from FRANz. Just adjusted header to make it readable in R and removed extra info outside of mismatches list
mismatches <- read.table("mismatches_table.txt", header=T)
mismatches$row <- 1:nrow(mismatches)

unique(mismatches[c("Offspring_ID", "Known_parent_ID")])

# maybe instead combine the offspring and parent ID and count unique values that way
mismatches$ID_combine <- paste(mismatches$Offspring_ID, mismatches$Known_parent_ID, sep="_")

unique_combos <- as.data.frame(table(unlist(mismatches$ID_combine)))
sum(unique_combos$Freq)

unique_combos$Prop <- (unique_combos$Freq / 169)

# sum values in Prop when Freq = 1, 2, 3...
sum(unique_combos[which(unique_combos$Freq==1), 3]) # 0.49
sum(unique_combos[which(unique_combos$Freq==2), 3]) # 0.14
sum(unique_combos[which(unique_combos$Freq==3), 3]) # 0.01
sum(unique_combos[which(unique_combos$Freq==4), 3]) # 0.01
sum(unique_combos[which(unique_combos$Freq==5), 3]) # 0.05
sum(unique_combos[which(unique_combos$Freq==6), 3]) # 0.06
sum(unique_combos[which(unique_combos$Freq==7), 3]) # 0.04
sum(unique_combos[which(unique_combos$Freq==8), 3]) # 0.05
sum(unique_combos[which(unique_combos$Freq==9), 3]) # 0.06
sum(unique_combos[which(unique_combos$Freq==10), 3]) # 0.03
sum(unique_combos[which(unique_combos$Freq==11), 3]) # 0
sum(unique_combos[which(unique_combos$Freq==12), 3]) # 0.04

# "with_adopt"
df <- data.frame(num_mismatch = (1:12), 
                 prop = c(0.49, 0.14, 0.01, 0.01, 0.05, 0.06, 0.04, 0.05, 0.06, 0.03,0,0.04))

# "without_adopt"
df <- data.frame(num_mismatch = (1:12), 
                 prop = c(0.87, 0.25, 0.02, 0, 0, 0, 0, 0, 0, 0,0,0))

ggplot(data=df,aes(x=num_mismatch, y=prop))+
  geom_bar(stat="identity")



#install pedantics a pain b/c archived. downloaded the tar.gz from https://cran.r-project.org/src/contrib/Archive/pedantics/
#use Tools > install packages... 
# had to also install dependencies 'MasterBayes','MCMCglmm', 'kinship2', 'genetics', 'mvtnorm' are not available for package 'pedantics'
# MasterBayes also needs archived version
# Trying pedantics installation again.

library(pedantics) # yay it worked - this one for fixPedigree and drawPedigree

# Load pedigree data
setwd("C:/Users/eveli/Dropbox/Polar_bears_ECCC/FRANz/results/without_adopt/")
df <- read.table("pedigree.txt", header=T)

# swap order of dam & sire columns
df <- df[c("ID", "DAM", "SIRE")]

# convert the "*"s to "NA"s
df[df=="*"] <- NA

# use fixpedigree to create file where each dam/sire appears before their offspring
bear_pedigree <- fixPedigree(df)

# remove X33740  for now
#bear_pedigree <- filter(bear_pedigree, id != "X33740")

# Count how many dams and sires assigned
sum(!is.na(bear_pedigree$dam)) # 3173 maternal assignment (maternities) - genetic/field
sum(!is.na(bear_pedigree$sire)) # 1363 paternal assignment (paternities) - genetic only


# Save this file for using in other programs
write.table(bear_pedigree, "reformat/pedigree_formated.txt", quote=FALSE, row.names=FALSE)

drawPedigree(bear_pedigree)

# red =  mom, blue = dad
drawPedigree(bear_pedigree, dotSize=0.001, dots="y", sexColours=c("#d53838", "#0382ef"))

# just mom
drawPedigree(bear_pedigree, dotSize=0.001, dots="y", sexColours=c("#d53838", "#0382ef"), links="mums")

# just dad
drawPedigree(bear_pedigree, dotSize=0.001, dots="y", sexColours=c("#d53838", "#0382ef"), links="dads")

# get stats
stats <- pedigreeStats(final_pedigree)
stats_summary <- pedStatSummary(stats)
stats_sum_tab <- as.data.frame(stats_summary)

