# Preparing polar bear data input files for FRANz
# 
# To do:
# 1) Prep microsat data file: 
#    - Combine "Micros_WH_ESR_2023.xlsx" and "Micros_2023_Captures.xlsx"
#    - Check for duplicates & remove
#    - Load meta data files and combine: "Extract Evelien de Greef January 9 2024.xlsx" and "Metadata_part2_Feb6_2024.xlsx"
#    - See which microsatellite entries do not have meta data and remove

# Use the resulting microsat for GenAlEx

# 2) Prepare microsat for FRANz format. And prepare subpedigree data - start with the combined meta data file


setwd("C:/Users/eveli/Dropbox/Polar_bears_ECCC/data/microsat/")

library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)

####### Step 1) Prep microsat data file

### 1.1) Load and combine microsatellite data and remove duplicates
# Load the two microsat files
df1 <- read_excel("raw/Micros_WH_ESR_2023.xlsx")
df2 <- read_excel("raw/Micros_2023_Captures.xlsx")

# Remove unnecessary columns in df2
df2 <- subset(df2, select = -c(1,3:7,56))

# Combine df1 and df2
df_combined <- rbind(df1,df2)

# Saw a lower case x somewhere, make sure all are uppercase
df_combined$ID <- toupper(df_combined$ID)

# Save this file to run in GenAlEx to look for genotype duplicates
#write.csv(df_combined, "Micros_all_temoporary.csv")

# Add row number here to mark unique ID so can delete specific duplicates later
df_combined$row <- paste(df_combined$ID,1:nrow(df_combined),sep="_")

# Pull out duplicate IDs
n_occur <- data.frame(table(df_combined$ID)) # Get freq of each ID
dups <- n_occur[n_occur$Freq > 1,] # Pull out ones with more than 1 occurrence
dups

# Create dataframe with just dups
dups_ids <- as.data.frame(dups$Var1)
colnames(dups_ids) <- "ID"
df_dups <- subset(df_combined, (ID %in% dups_ids$ID))

# Put in order so duplicates IDs pair together
df_dups <- df_dups[order(df_dups$ID),]

# Move the "row"column to right
df_dups <- df_dups %>% relocate(row, .after=ID)

# Save
#write.csv(df_dups, "Micros_combined_dups_only.csv")

# Remove duplicates and problem entries (X33851 has dup IDs but different genotypes):
# Looked at the df_dups manually
# Note that while X17608 and X17605 have different metadata, there is no way these two are proper twins b/c different birth years. More likely it was a mis-ID in the last digit from 5 to 8. Removing the specific X17608 entry that has genotype match to X1760
remove <- c("X12416_2260","X17426_915","X17605_3437","X17605_3439","X17608_3440","X19975_1462","X19997_2093","X33362_859","X33362_3451","X33737_2748","X33851_898","X33851_3394","X33920_3175","X34000_2305","X34193_3463")


df_combined_nodups <- df_combined[ ! df_combined$row %in% remove, ]


### 1.2) Load in metadata file and check IDs
## note: the "Extract .... ED") ending in "ED" - the only change I made was manually adding in birth year for 16 COY/YRLG
meta_data1 <- read_excel("C:/Users/eveli/Dropbox/Polar_bears_ECCC/data/Extract Evelien de Greef January 9 2024 - ED.xlsx")
meta_data2 <- read_excel("C:/Users/eveli/Dropbox/Polar_bears_ECCC/data/Metadata_part2_Feb6_2024.xlsx")

# Combine the files
meta_combined <- rbind(meta_data1,meta_data2)

meta_ids <- as.data.frame(meta_combined$BearCode)
colnames(meta_ids) <- "ID"

# Check if any lowercase x's in the IDs
meta_ids[apply(meta_ids == sapply(meta_ids, tolower), 1, any),]
# looks good

# Pull IDs from combined microsat df 
micro_ids <- as.data.frame(df_combined$ID)
colnames(micro_ids) <- "ID"

# See which IDs from microsat do not appear in metadata (potential typos)
mismatch_ids <- anti_join(micro_ids, meta_ids, by = "ID") 
# 44 cases

# The "mismatch_ids" will have to be removed 
df_combined_nodups_nomismatch <- anti_join(df_combined_nodups, mismatch_ids, by = "ID")

n_distinct(mismatch_ids$ID)

# total in df_combined_nodups_nomismatch hould be 3460-44+1 = 3417 (the +1 is because there is a dup in the mismatch_ids)

# Remove the "row" column which is the 50th column 
df_combined_nodups_nomismatch <- subset(df_combined_nodups_nomismatch, select=-50)

# The microsatellite data file here is ready for GenAlEx!
#write_xlsx(df_combined_nodups_nomismatch,"Micros_combined_set.xlsx")


####### Step 2) Prepare input files for FRANz

### 2.1) The main input file for FRANz will include the microsatellite genotypes, AND the individuals with just field data. So first make the sub-pedigree to know final list of individuals to include in genotype file.

# Prepare sub-pedigree file (list of mother-offspring pairs)
# start from the meta_combined df

# subset data to have BearCode, OriginalSex, ANBearCodeListX, Class
sub <- meta_combined[,c("BearCode", "Date","OriginalSex", "ANBearCodeListX", "Class")] 

# remove rows with no ANBearCodeListX
sub_sub <- na.omit(sub, cols="ANBearCodeListX")

# Splitting the ANBearCodeListX
sub_sub[c("AN1", "AN2", "AN3", "AN4", "AN5", "AN6", "AN7", "AN8")] <- str_split_fixed(sub_sub$ANBearCodeListX, ",", 8)

# also note that X33257 has sex discrepancy - probably exclude this out of BearCode column
sub_sub <- subset(sub_sub, BearCode != "X33257")

# Only want the adult females for the mother section
sub_sub_moms <- filter(sub_sub, OriginalSex == "F" & Class == "ADULT")

# Next want to make a row for each mom and AN individual (AN1 through AN8)
# clean up a lil
sub_sub_moms2 <- subset(sub_sub_moms, select = c(1,6:13))

# Change format so there is a row for each mother-offspring pair, using melt() here
library(reshape2)
sub_sub_moms2_melt <- reshape2::melt(sub_sub_moms2,id.vars = "BearCode")

# Remove the 2nd column that just says "AN1, AN2..."
sub_sub_moms2_melt <- subset(sub_sub_moms2_melt, select = -2)

# Replace any AN cells that have "ADULT" or "SUBAD" with NA. Also remove any with "N" for nearby- just want to keep the ones that were accompanying "A" with mom
sub_sub_moms2_melt[] <- lapply(sub_sub_moms2_melt, function(x) replace(x, grepl("ADULT|SUBAD|N", x), NA))

colnames(sub_sub_moms2_melt) <- c("Mom", "Offspring")

# Remove NAs
sub_sub_moms2_melt <- sub_sub_moms2_melt %>% mutate_all(na_if,"")
sub_sub_moms2_melt <- na.omit(sub_sub_moms2_melt, cols="value")

# For the AN column, just keep the ID. looks like some start with "A:" but some with " A:" and that space shift everything one character for some samples. Instead let's first remove everything up to the :
sub_sub_moms2_melt$Offspring_ID <- gsub(".*:","",sub_sub_moms2_melt$Offspring)

# Ok, now want to keep 2:7
sub_sub_moms2_melt$Offspring_ID <- substr(sub_sub_moms2_melt$Offspring_ID, 2, 7)
# Remove middle column
sub_sub_moms2_melt2 <- subset(sub_sub_moms2_melt, select = -2)

subpedigree <- subset(sub_sub_moms2_melt2, select = c(1,2))
colnames(subpedigree) <- c("ID", "Offspring")

# remove duplicate ID pairs-- otherwise FRANz don't like and will say impossible pedigree
subpedigree <- subpedigree[!duplicated(subpedigree[c(1,2)]),]

# This will be the second part of the pedigree file 
#write.table(subpedigree, "mom_cub_pairs_nodups.txt", quote=FALSE, row.names=FALSE, col.names=FALSE, eol="\n")



# First part of pedigree file is list of all individuals (microsat and field-only bears).

# Combine columns 1 and 2 in "subpedigree" into one long column, then remove duplicate entries
inds <- data.frame(ID=unlist(subpedigree, use.names = FALSE))
inds <- unique(inds)

# Create a list of IDs that are in field-only (which we'll add to the df_combined_nodups_mismatch)
final_micro_ids <- as.data.frame(df_combined_nodups_nomismatch$ID)
colnames(final_micro_ids) <- "ID"

# See which IDs from subpedigree list do not appear in micros
field_only <- anti_join(inds, final_micro_ids, by = "ID") 
# 735 individuals field-only data


### 2.2) Add in the birth and death years and sex if known for microsatellite data
# code is not elegant here, but it works.

# ADDING BIRTH YEAR
# Maybe use left_join but simplify the meta_data df
meta_birth <- meta_combined[,c("BearCode", "Born")] 

# Remove dups (b/c recaps/resights...)
meta_birth_nodup <- distinct(meta_birth) 
n_distinct(meta_birth_nodup$BearCode) # double check. Also here looks like total 4421 distinct bear entries for the metadata file

# left_join
df_combined_edit <- left_join(df_combined_nodups_nomismatch, meta_birth_nodup, by = c("ID"="BearCode"))
# count how many with "NA" birth year
sum(is.na(df_combined_edit$Born)) #37 NA entries, maybe also manually double check these.
# Some adults, but some are COY and YRLG and have capture date (COY use capture year, YRLG use capture year - 1). Since not too many just going to add in manually
# X34197 - 2023
# X34202 - 2022
# X34196 - 2023
# X34198 - 2023
# X34201 - 2022
# X34199 - 2023
# X34187 - 2023
# X34188 - 2023
# X34184 - 2022
# X34182 - 2023
# X34159 - 2022
# X34156 - 2022
# X34161 - 2022
# X34158 - 2022
# X34162 - 2022
# X34155 - 2023

# decided to add in manually in the excel sheet, then reloaded metadata (starting from line 70)

# move column to 2nd one
df_combined_edit <- df_combined_edit %>% relocate(Born, .after=ID)

# remove NA birth years (21 individuals)
df_combined_edit_nonas <- na.omit(df_combined_edit, cols="Born")

### learned that "1950" means unknown birth year. Have to remove these too.
# remove Born 1950
df_combined_edit_nonas <- df_combined_edit_nonas %>% filter(Born != 1950)

# removed 4 with "1950"
## this mean 3392 individuals for the total microsatellite data to go into the pedigree

## ADDING DEATH YEAR
# Death year might be a bit more steps. 
# Pull out metadata files with "M-DEATH" and "M-DEATHR" in the "Reason" column
meta_death <- meta_combined[,c("BearCode", "Date", "Reason")] 
meta_death <- subset(meta_death, Reason == "M-DEATH" | Reason == "M-DEATHR")
# extract year from the Date
meta_death$Death <- stringr::str_extract(meta_death$Date, "^.{4}")
meta_death <- meta_death[,c("BearCode", "Death")]

n_distinct(meta_death$BearCode) # double check for dups, no dups here

# left_join
df_combined_edit2 <- left_join(df_combined_edit_nonas, meta_death, by = c("ID"="BearCode"))

# move column to 3nd one
df_combined_edit2 <- df_combined_edit2 %>% relocate(Death, .after=Born)

## ADDING SEX - here want to add sex for field-only data too I think
# Maybe first add the field-only IDs to the bottom of this df
# full_join might work?

##### previous problem when a few that did have microsat were dropped b/c no birth years are also not in "field_only" but still need a blank entry.-- is fine now i think with the added years for the 21 youngins, but can just run this through anyway
# See what doesn't match from micros with the inds_total
final_micro_list <- as.data.frame(df_combined_edit2$ID)
colnames(final_micro_list) <- "ID"
field_only2 <- anti_join(inds, final_micro_list, by = "ID") 

# ok try again
all <- full_join(df_combined_edit2, field_only2, by = "ID")

# 3392 + 735 = 4127 total

# Maybe use left_join but simplify the meta_data df
meta_sex <- meta_combined[,c("BearCode", "OriginalSex")] 
# Remove dups (b/c recaps/resights...)
meta_sex_nodup <- distinct(meta_sex) 
n_distinct(meta_sex_nodup$BearCode) # double check. hmm doesn't add up. some dup IDs here.

# remove NAs
meta_sex_nodup_nonas <- na.omit(meta_sex_nodup, cols="OriginalSex")
n_distinct(meta_sex_nodup_nonas$BearCode)

# see which IDs are duplicated here
meta_sex_nodup_nonas[duplicated(meta_sex_nodup_nonas$BearCode),]
# X33639, X17466, X33795, X33257 --- first three have a U so can remove Us, but the last one has an M and F so this one just needs to be removed

# remove Us
meta_sex_nodup_nonas <- meta_sex_nodup_nonas[!(meta_sex_nodup_nonas$OriginalSex=="U") ,]
meta_sex_nodup_nonas <- meta_sex_nodup_nonas[!(meta_sex_nodup_nonas$BearCode=="X33257") ,]

n_distinct(meta_sex_nodup_nonas$BearCode) # ok now is good

# left_join
df_combined_edit3 <- left_join(all, meta_sex_nodup_nonas, by = c("ID"="BearCode"))

# move column to 4th one
df_combined_edit3 <- df_combined_edit3 %>% relocate(OriginalSex, .after=Death)


## ok I think the main input file for FRANz is now ready to be formatted to FRANZ format! ust the csv.pl script they provide
#write.csv(df_combined_edit3, "C:/Users/eveli/Dropbox/Polar_bears_ECCC/data/microsat/Micros_and_field_dataset.csv", quote=FALSE, row.names = FALSE, na="",eol="\n")

# also redo the inds_total
inds_total <- as.data.frame(df_combined_edit3$ID)
# Total 4127 bears

# save list for adding to first part of pedigree file
#write.table(inds_total, "inds_list_all.txt", quote=FALSE, row.names=FALSE, col.names=FALSE, eol="\n")

# decided to deal with the ID spacing to 10 characters using sed and awk in linux instead of here

###############

### make a histogram of the polar bear ages
# Use bears that went into pedigree
# inds_total
colnames(inds_total) <- "BearCode"
inds_total_ages <- left_join(inds_total, meta_combined, by="BearCode")
inds_total_ages$Year_cap <- substr(inds_total_ages$Date,1,4)
inds_total_ages$Age <- as.numeric(inds_total_ages$Year_cap) - as.numeric(inds_total_ages$Born)

inds_total_ages <- inds_total_ages %>% filter(Born != 1950)

age_only <- dplyr::select(inds_total_ages, BearCode, Age, Class, OriginalSex)

# remove dup IDs while keeping oldest age in
age_only_nodup <- merge(aggregate(Age ~ BearCode, age_only, max), age_only)

#histogram
all_age <- ggplot()+
  geom_histogram(data=age_only_nodup, aes(x=Age), fill="#BDC9E1", bins=33, alpha=1, colour="black", linewidth=0.2)+
  theme_bw()+
  #ggtitle("Distribution of polar bear ages")+
  ylab("Frequency")+
  xlab("Age (years)")
#ggsave("age_distribution_all.png", width=5, height=3.5, dpi=400)

meta_combined_nobaby <- subset(age_only_nodup, Age > 1)


sub_and_adult <- ggplot()+
  geom_histogram(data=meta_combined_nobaby, aes(x=Age, fill=OriginalSex), bins=31, alpha=1, colour="black", linewidth=0.2)+
  theme_bw() +
  scale_fill_manual(values=c("F"="#EF8A62", "M"="#67A9CF", "U"="#ffffbf"))+
  #ggtitle("Distribution of polar bear ages separated by sex \n(excluding COY and YRLG)")+
  labs(fill="Sex")+
  ylab("Frequency")+
  xlab("Age (years)")+
  geom_vline(xintercept = 4, colour="black", linetype="dashed")


library(patchwork)
(all_age + sub_and_adult)
#ggsave("age_distributions_updated.png", width=8, height=3, dpi=500)




