# Using MCMCglmm to estimate heritabilities of body length and skull size
# And estimate breeding values
# Help from tutorial: https://juliengamartin.github.io/wam_tuto/mcmcglmm-1.html #Looks like site no longer available..
# Now using help from this one: https://devillemereuil.legtux.org/wp-content/uploads/2012/12/tuto_en.pdf

### Some parts of the script is not super efficient, but it works- will clean up soon.

library(MCMCglmm)
library(tidyverse) 
library(readxl)
library(patchwork)
library(RColorBrewer)

setwd("C:/Users/eveli/Dropbox/Polar_bears_ECCC/")

## Load input files
# Field and meta data and pedigree
field <- read_excel("data/Extract Evelien de Greef January 9 2024.xlsx")
ped <- read.table("COLONY/results/polarbear_highprecision/postCOLONY/final_pedigree.txt", header=T)

# Want to extract field data for those listed in pedigree and not the others
ped_IDs <- as.data.frame(ped$id)
colnames(ped_IDs) <- "ID"

# Semi_join to include IDs only listed in pedigree. Also note here that need to make it as a data.frame otherwise will automatically be a tibble and seems like MCMCglmm don't do so good with tibbles
df <- as.data.frame(semi_join(field, ped_IDs, by=c("BearCode"="ID")))

# Remember to change BornYear of 1950 to NA
df$Born <- replace(df$Born, df$Born==1950, NA)

# also add how old (age in years)
df$Year_cap <- substr(df$Date,1,4)
df$Age <- as.numeric(df$Year_cap) - as.numeric(df$Born)

# Remove NA ages
df <- df %>% drop_na(Age)

# Change NAs in Sex to U
df$OriginalSex[is.na(df$OriginalSex)] <- "U"

# And then remove rows with U as OriginalSex
df <- subset(df, OriginalSex != "U")

# Rename columns
colnames(df) <- c("animal", "Date", "Sex", "Born", "ANBearCodeListX", "Class", "Reason", "Axillary_girth", "Body_length", "Zygomatic_breadth", "Head_length", "Year_cap", "Age")

# Make sure variables in proper format
df$animal <- as.factor(df$animal)
df$Sex <- as.factor(df$Sex)
df$Born <- as.factor(df$Born)
df$Axillary_girth <- as.numeric(df$Axillary_girth)
df$Body_length <- as.numeric(df$Body_length)
df$Zygomatic_breadth <- as.numeric(df$Zygomatic_breadth)
df$Head_length <- as.numeric(df$Head_length)
df$Age <- as.factor(df$Age)

str(df)

df_backup <- df

### Quick look at data distribution
## Axillary girth
ax_girth <- ggplot(data=df, aes(x=Axillary_girth))+
  theme_bw()+
  geom_histogram(fill="#BDC9E1", color="black",bins=50) 
ax_girth
# Something seriously wrong here. Two COYs with < 10, and two COYs with > 800. I think best to drop these 4 because seems impossible. Looks like COY with 280 also seems unreasonable.. drop this one too 
# First reset row numbers
df <- df %>% as.data.frame(row.names = 1:nrow(.))

# Turn Axillary_girth values into NAs for rows 6995, 7067, 6805, 6806
df$Axillary_girth[6994] = NA # COY 2
df$Axillary_girth[7066] = NA # COY 7
df$Axillary_girth[6804] = NA # COY > 800
df$Axillary_girth[6805] = NA # COY > 800
df$Axillary_girth[6430] = NA # COY 280

# Try again
ax_girth <- ggplot(data=df, aes(x=Axillary_girth))+
  theme_bw()+
  geom_histogram(fill="#BDC9E1", color="black",bins=50)+
  xlab("Axillary girth (cm)")+
  ylab("Frequency")
ax_girth
# Looks better now.

## Body Length
body_length <- ggplot(data=df, aes(x=Body_length))+
  theme_bw()+
  geom_histogram(fill="#BDC9E1", color="black",bins=50) +
  xlab("Body length (cm)")+
  ylab("Frequency")

# One F ADULT 340 seems kind of insane..
df$Body_length[6705] = NA

## try again
body_length <- ggplot(data=df, aes(x=Body_length))+
  theme_bw()+
  geom_histogram(fill="#BDC9E1", color="black",bins=50) +
  xlab("Body length (cm)")+
  ylab("Frequency")
body_length

## Zygomatic breadth
zyg_breadth <- ggplot(data=df, aes(x=Zygomatic_breadth))+
  theme_bw()+
  geom_histogram(fill="#BDC9E1", color="black",bins=50) +
  xlab("Zygomatic breadth (mm)")

# Fishy ones
df$Zygomatic_breadth[6416] = NA # COY 10
df$Zygomatic_breadth[6942] = NA # COY 10
df$Zygomatic_breadth[6938] = NA # M ADULT 26
df$Zygomatic_breadth[6769] = NA # F ADULT bigger than head length
df$Zygomatic_breadth[6931] = NA # M ADULT bigger than head length


# try again
zyg_breadth <- ggplot(data=df, aes(x=Zygomatic_breadth))+
  theme_bw()+
  geom_histogram(fill="#BDC9E1", color="black",bins=50) +
  xlab("Zygomatic breadth (mm)")+
  ylab("Frequency")
zyg_breadth


## Head length
head_length <- ggplot(data=df, aes(Head_length))+
  theme_bw()+
  geom_histogram(fill="#BDC9E1", color="black",bins=50) +
  xlab("Head length (mm)")+
  ylab("Frequency")

# I seriously question head lengths below 50, especially adults, and COY at 14???
#6429,6636,6943,6894,6285,7031,6867,7117
df$Head_length[6428] = NA 
df$Head_length[6635] = NA 
df$Head_length[6942] = NA 
df$Head_length[6893] = NA 
df$Head_length[6284] = NA 
df$Head_length[7030] = NA 
df$Head_length[6866] = NA 
df$Head_length[7116] = NA 

head_length <- ggplot(data=df, aes(Head_length))+
  theme_bw()+
  geom_histogram(fill="#BDC9E1", color="black",bins=50)+
  xlab("Head length (mm)")+
  ylab("Frequency")


# Plot all
(ax_girth + body_length) / (zyg_breadth + head_length)


# turn COY-A into COY, 2YR-A into 2YR, and YRLG-A into YRLG. basically remove the "-A"
df$Class <- sub("-A", "", df$Class)

### Plot morphometric data by age class
ax_girth_age <- ggplot(data=df, aes(x=Axillary_girth, fill=Class))+
  theme_bw()+
  geom_histogram(color="black",linewidth=0.2,bins=50)+
  scale_fill_brewer(palette="PuBu",
                    limits = c("COY", "YRLG", "2YR", "SUBAD", "ADULT"))+
  xlab("Axillary girth (cm)")+
  ylab("Frequency")
ax_girth_age

body_length_age <- ggplot(data=df, aes(x=Body_length, fill=Class))+
  theme_bw()+
  geom_histogram(color="black",linewidth=0.2,bins=50)+
  scale_fill_brewer(palette="PuBu",
                    limits = c("COY", "YRLG", "2YR", "SUBAD", "ADULT"))+
  xlab("Body length (cm)")+
  ylab("Frequency")

zyg_breadth_age <- ggplot(data=df, aes(x=Zygomatic_breadth, fill=Class))+
  theme_bw()+
  geom_histogram(color="black",linewidth=0.2,bins=50)+
  scale_fill_brewer(palette="PuBu",
                    limits = c("COY", "YRLG", "2YR", "SUBAD", "ADULT"))+
  xlab("Zygomatic breadth (mm)")+
  ylab("Frequency")

head_length_age <- ggplot(data=df, aes(x=Head_length, fill=Class))+
  theme_bw()+
  geom_histogram(color="black",linewidth=0.2,bins=50)+
  scale_fill_brewer(palette="PuBu",
                    limits = c("COY", "YRLG", "2YR", "SUBAD", "ADULT"))+
  xlab("Head length (mm)")+
  ylab("Frequency") 

(ax_girth_age + body_length_age) / (zyg_breadth_age + head_length_age) + plot_layout(guides = "collect")

#ggsave("MCMCglmm/trait_age_nocrazyoutliers2.png", width=9, height=5, dpi=1000)


############
# Plot morphometric data by ages by number (years)
ax_girth_box <- ggplot(data=df, aes(x=Age, y=Axillary_girth, colour=Sex))+
  theme_bw()+
  #geom_jitter(alpha=0.6)+
  geom_boxplot(width=.9, lwd=0.2, outlier.size = 0.1)+
  scale_colour_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  xlab("Age (years)")+
  ylab("Axilliary girth (cm)")

body_length_box <- ggplot(data=df, aes(x=Age, y=Body_length, fill=Sex))+
  theme_bw()+
  geom_boxplot(width=.9, lwd=0.2, outlier.size = 0.1)+
  scale_fill_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  xlab("Age (years)")+
  ylab("Body length (cm)")

zyg_breadth_box <- ggplot(data=df, aes(x=Age, y=Zygomatic_breadth, fill=Sex))+
  theme_bw()+
  geom_boxplot(width=.9, lwd=0.2, outlier.size = 0.1)+
  scale_fill_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  xlab("Age (years)")+
  ylab("Zygomatic breadth (mm)")

head_length_box <- ggplot(data=df, aes(x=Age, y=Head_length, fill=Sex))+
  theme_bw()+
  geom_boxplot(width=.9, lwd=0.2, outlier.size = 0.1)+
  scale_fill_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  xlab("Age (years)")+
  ylab("Head length (mm)")

(ax_girth_box / body_length_box / zyg_breadth_box / head_length_box)

#ggsave("MCMCglmm/traits_by_age_boxplots.png", width=8, height=10, dpi=1000)
############

## Create dataframe for just adults
#df_adults <- subset(df, Class == "ADULT") #this includes a few four year olds. just make cut-off 5 years to be consistent..
df_adults <- subset(df, Age != "0" & Age != "1" & Age != "2" & Age !="3" & Age !="4")

# just for counting entires
#write.csv(df_adults, "MCMCglmm/df_adults.csv")

# averages
mean(df_adults$Body_length, na.rm=TRUE)
mean(df_adults$Zygomatic_breadth, na.rm=TRUE)

## Updated morphometric plots of adults, coloured by sex
ax_girth_sex <- ggplot(data=df_adults, aes(x=Axillary_girth, fill=Sex))+
  theme_bw()+
  geom_histogram(color="black",linewidth=0.2,bins=50) +
  scale_fill_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  xlab("Axillary girth (cm)")+
  ylab("Frequency") 

body_length_sex <- ggplot(data=df_adults, aes(x=Body_length, fill=Sex))+
  theme_bw()+
  geom_histogram(color="black",linewidth=0.2,bins=50)+
  scale_fill_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  xlab("Body length (cm)")+
  ylab("Frequency")  

zyg_breadth_sex <- ggplot(data=df_adults, aes(x=Zygomatic_breadth, fill=Sex))+
  theme_bw()+
  geom_histogram(color="black",linewidth=0.2,bins=50) +
  scale_fill_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  xlab("Zygomatic breadth (mm)")+
  ylab("Frequency") 

head_length_sex <- ggplot(data=df_adults, aes(x=Head_length, fill=Sex))+
  theme_bw()+
  geom_histogram(color="black",linewidth=0.2,bins=50) +
  scale_fill_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  xlab("Head length (mm)")+
  ylab("Frequency") 

(ax_girth_sex + body_length_sex) / (zyg_breadth_sex + head_length_sex)+plot_layout(guides = "collect")
#ggsave("MCMCglmm/trait_sex_ADULTS_nocrazyoutliers.png", width=8.5, height=5, dpi=1000)


# Look at correlation of traits
library(psych)
pairs.panels(df_adults[8:11],ellipses=F,hist.col ="#BDC9E1")


######################
# Format pedigree
ped$id <- as.factor(ped$id)
ped$dam <- as.factor(ped$dam)
ped$sire <- as.factor(ped$sire)

str(ped)

# Also based on gryphon example data and another tutorial, it looks like the order should be "id" (offspring), "sire", "dam". also name "id" as "animal"
ped <- ped[,c(1,3,2)]
colnames(ped) <- c("animal", "sire", "dam")

# Set up prior. Looks like this one is a standard one to define prior on variances. If we add more random effects though will need to add more G's
prior <- list(R = list(V=1, nu = 0.002), 
              G = list(G1 = list(V = 1, nu = 0.002)))

# need new prior for using 2 random effects 
prior2 <- list(R = list(V=1, nu = 0.002), 
               G = list(G1 = list(V = 1, nu = 0.002),
                        G2 = list(V = 1, nu = 0.002)))
# Creating an inverse relatedness matrix with the pedigree - try without this first.
#Ainv <- inverseA(ped)$Ainv

# Pedigree will be random effect and sex as fixed effect. Looks like need to also add Class as fixed effect.

############ First start with body length
model_bodylength <- MCMCglmm(Body_length ~ Sex + Age,
                             random = ~animal + Born, family = "gaussian",
                             #ginv = list(animal = Ainv),
                             pedigree = ped, 
                             data = df_adults, prior = prior2, singular.ok=TRUE,
                             nitt = 65000, burnin = 15000, thin = 50, pr=TRUE)

model_bodylength_noage <- MCMCglmm(Body_length ~ Sex,
                             random = ~animal + Born, family = "gaussian",
                             #ginv = list(animal = Ainv),
                             pedigree = ped, 
                             data = df_adults, prior = prior2,
                             nitt = 65000, burnin = 15000, thin = 50, pr=TRUE)

############ Next do zygomatic breadth
# First start with body length
model_zygbreadth <- MCMCglmm(Zygomatic_breadth ~ Sex + Age,
                             random = ~animal + Born, family = "gaussian",
                             #ginv = list(animal = Ainv),
                             pedigree = ped, 
                             data = df_adults, prior = prior2, singular.ok = TRUE,
                             nitt = 65000, burnin = 15000, thin = 50, pr=TRUE)

model_zygbreadth_noage <- MCMCglmm(Zygomatic_breadth ~ Sex,
                             random = ~animal + Born, family = "gaussian",
                             #ginv = list(animal = Ainv),
                             pedigree = ped, 
                             data = df_adults, prior = prior2,
                             nitt = 65000, burnin = 15000, thin = 50, pr=TRUE)

#
# plot
plot(model_bodylength$Sol)
plot(model_bodylength$VCV)

# autocorr
autocorr.diag(model_bodylength$VCV)

# CI
HPDinterval(model_bodylength$VCV)
posterior.mode(model_bodylength$VCV)

# assess significance of sex
posterior.mode(model_bodylength$Sol[, "Sex"])
HPDinterval(model_bodylength$Sol[, "SexM"], 0.95)

# Estimate heritability body length
posterior.model <- model_bodylength_noage$VCV[, "animal"] /
  (model_bodylength_noage$VCV[, "animal"] + model_bodylength_noage$VCV[, "units"])

posterior.mode(posterior.model)
mean(posterior.model)
HPDinterval(posterior.model, 0.95)
plot(posterior.model)

# Estimate heritability zygomatic breadth
posterior.model <- model_zygbreadth_noage$VCV[, "animal"] /
  (model_zygbreadth_noage$VCV[, "animal"] + model_zygbreadth_noage$VCV[, "units"])

posterior.mode(posterior.model)
mean(posterior.model)
HPDinterval(posterior.model, 0.95)
plot(posterior.model)


plot(model_bodylength$Sol)


### Looking at breeding values
# tip from https://stackoverflow.com/questions/47598123/how-do-i-extract-random-effects-from-mcmcglmm

#devtools::install_github("JWiley/postMCMCglmm")
library("postMCMCglmm")
qfun <- function(x,lev) unname(quantile(x,lev))

# Format for body length breeding values
rsum_bod <- as.data.frame(t(apply(ranef(model_bodylength_noage),1,
                              function(x) c(est=mean(x),
                                            min=qfun(x,0.025),max=qfun(x,0.975)))))
# order for plot
rsum_bod$term <- reorder(factor(rownames(rsum_bod)),
                         rsum_bod$est)
# plot
ggplot(rsum_bod,aes(term,est))+
  geom_pointrange(aes(ymin=min,ymax=max))+
  coord_flip()

# add morphometric data
rsum_bod$term <- sub("animal.", "", rsum_bod$term)
rsum_bod <- left_join(rsum_bod, df_adults, by=c("term"="animal"))


# Plot
body_length_BV <- ggplot(rsum_bod)+
  geom_point(aes(x=Body_length, y=est, color=Sex), alpha=0.7)+
  scale_color_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  theme_bw()+
  xlab("Body length (cm)")+
  ylab("Breeding values (est)")
body_length_BV


# Format for zygomatic breadth breeding values
rsum_zyg <- as.data.frame(t(apply(ranef(model_zygbreadth_noage),1,
                              function(x) c(est=mean(x),
                                            min=qfun(x,0.025),max=qfun(x,0.975)))))
# Order for plot
rsum_zyg$term <- reorder(factor(rownames(rsum_zyg)),
                         rsum_zyg$est)

rsum_zyg$term <- sub("animal.", "", rsum_zyg$term)
rsum_zyg <- left_join(rsum_zyg, df_adults, by=c("term"="animal"))

# plot
zyg_breadth_BV <- ggplot(rsum_zyg)+
  geom_point(aes(x=Zygomatic_breadth, y=est, color=Sex), alpha=0.7)+
  scale_color_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  theme_bw()+
  xlab("Zygomatic breadth (mm)")+
  ylab("Breeding values (est)")
zyg_breadth_BV

# Combine plots
body_length_BV + zyg_breadth_BV+plot_layout(guides = "collect")
#ggsave("MCMCglmm/breeding_values_sex.png", width=8, height=3.5, dpi=600)

### Looking at breeding values over time
# Group by cohort
# 1960 earlier birth year and 2018 latest birth year for adults
# One option is 1960-1969, 1970-1979, 1980-1989, 1990-1999, 2000-2009, 2010-2018.
# Ended up choosing in 5's.

### Code below is inefficient but it works, will make it cleaner/ more elegant later.

########### BODY LENGTH
rsum_bod2 <- rsum_bod[,c("est", "Born", "Sex")]

# Year is already saved as factor and harder to turn back to numeric. should probably left_join with the old field data but anyway here is another way
rsum_bod2_co1 <- subset(rsum_bod2, Born == "1960" | Born == "1961" | Born == "1962" | Born == "1963" | Born == "1964")
rsum_bod2_co1$Cohort <- 1
rsum_bod2_co2 <- subset(rsum_bod2, Born == "1965" | Born == "1966" | Born == "1967" | Born == "1968" | Born == "1969")
rsum_bod2_co2$Cohort <- 2
rsum_bod2_co3 <- subset(rsum_bod2, Born == "1970" | Born == "1971" | Born == "1972" | Born == "1973" | Born == "1974")
rsum_bod2_co3$Cohort <- 3
rsum_bod2_co4 <- subset(rsum_bod2, Born == "1975" | Born == "1976" | Born == "1977" | Born == "1978" | Born == "1979")
rsum_bod2_co4$Cohort <- 4
rsum_bod2_co5 <- subset(rsum_bod2, Born == "1980" | Born == "1981" | Born == "1982" | Born == "1983" | Born == "1984")
rsum_bod2_co5$Cohort <- 5
rsum_bod2_co6 <- subset(rsum_bod2, Born == "1985" | Born == "1986" | Born == "1987" | Born == "1988" | Born == "1989")
rsum_bod2_co6$Cohort <- 6
rsum_bod2_co7 <- subset(rsum_bod2, Born == "1990" | Born == "1991" | Born == "1992" | Born == "1993" | Born == "1994")
rsum_bod2_co7$Cohort <- 7
rsum_bod2_co8 <- subset(rsum_bod2, Born == "1995" | Born == "1996" | Born == "1997" | Born == "1998" | Born == "1999")
rsum_bod2_co8$Cohort <- 8
rsum_bod2_co9 <- subset(rsum_bod2, Born == "2000" | Born == "2001" | Born == "2002" | Born == "2003" | Born == "2004")
rsum_bod2_co9$Cohort <- 9
rsum_bod2_co10 <- subset(rsum_bod2, Born == "2005" | Born == "2006" | Born == "2007" | Born == "2008" | Born == "2009")
rsum_bod2_co10$Cohort <- 10
rsum_bod2_co11 <- subset(rsum_bod2, Born == "2010" | Born == "2011" | Born == "2012" | Born == "2013" | Born == "2014")
rsum_bod2_co11$Cohort <- 11
rsum_bod2_co12 <- subset(rsum_bod2, Born == "2015" | Born == "2016" | Born == "2017" | Born == "2018" | Born == "2019")
rsum_bod2_co12$Cohort <- 12

rsum_bod2_allco <- rbind(rsum_bod2_co1, rsum_bod2_co2, rsum_bod2_co3, rsum_bod2_co4, rsum_bod2_co5, rsum_bod2_co6, rsum_bod2_co7, rsum_bod2_co8, rsum_bod2_co9, rsum_bod2_co10, rsum_bod2_co11, rsum_bod2_co12)

str(rsum_bod2_allco)

# If doing cohort of 10 years, could just remove the last digit in Born and it would be grouped
#rsum_bod2$Born <- as.character(rsum_bod2$Born)
#rsum_bod2$cohort <- substr(rsum_bod2$Born, 1, nchar(rsum_bod2$Born)-1)

#rsum_bod2 <- na.omit(rsum_bod2)
coefs_ball <- coef(lm(est ~ Cohort, data = rsum_bod2_allco))

rsum_bod_M <- subset(rsum_bod2_allco,Sex=="M")
coefs_bM <- coef(lm(est ~ Cohort, data = rsum_bod_M))
rsum_bod_F <- subset(rsum_bod2_allco,Sex=="F")
coefs_bF <- coef(lm(est ~ Cohort, data = rsum_bod_F))

# Initial plot
rsum_bod_plot <- ggplot(data=rsum_bod2_allco, aes(x=as.factor(Cohort), y=est, fill=Sex))+
  theme_bw()+
  #geom_jitter()+
  geom_boxplot(width=.8, lwd=0.3, outlier.size = 0.3)+
  scale_fill_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  xlab("Cohort")+
  ylab("Breeding values (est)")+
  ggtitle("Body length")+
  geom_abline(intercept = coefs_bF[1], slope = coefs_F[2], colour="red")+
  geom_abline(intercept = coefs_bM[1], slope = coefs_M[2], colour="blue")
  
#  scale_x_discrete(labels=c("196" = "1960's", "197" = "1970's", "198" = "1980's", "199" = "1990's", "200" = "2000's", "201" = "2010's"))
rsum_bod_plot

### Plot all BV of body length together
rsum_bod_plot_overall <- ggplot(data=rsum_bod2_allco, aes(x=as.factor(Cohort), y=est))+
  theme_bw()+
  geom_boxplot(width=.6, lwd=0.3, outlier.size = 0.5, fill="#BDC9E1", colour="gray30")+
 # xlab("Cohort by birth year")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Breeding values (est)")+
  ggtitle("Body length")+
  geom_abline(intercept = coefs_ball[1], slope = coefs_ball[2], colour="red")
  #theme(axis.text.x = element_text(angle = 45,hjust = 1))+
 # scale_x_discrete(labels=c("1" = "1960-1964", "2" = "1965-1969", "3" = "1970-1974", "4" = "1975-1979", "5" = "1980-1984", "6" = "1985-1989", "7" = "1990-1994", "8"="1995-1999", "9"="2000-2004", "10"="2005-2009", "11"="2010-2014", "12"="2015-2018"))
rsum_bod_plot_overall

summary(lm(est ~ Cohort, data = rsum_bod2_allco)) #### slope -0.08 (cohort), p value 0.026

## female only
rsum_bod_plot_F <- ggplot(data=rsum_bod_F, aes(x=as.factor(Cohort), y=est, fill=Sex))+
  theme_bw()+
  geom_boxplot(width=.6, lwd=0.3, outlier.size = 0.5, fill="#EF8A62", colour="gray30")+
 # xlab("Cohort by birth year")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Breeding values (est)")+
 # ggtitle("Body length in females")+
  geom_abline(intercept = coefs_bF[1], slope = coefs_bF[2], colour="red")
 # theme(axis.text.x = element_text(angle = 45,hjust = 1))+
 # scale_x_discrete(labels=c("1" = "1960-1964", "2" = "1965-1969", "3" = "1970-1974", "4" = "1975-1979", "5" = "1980-1984", "6" = "1985-1989", "7" = "1990-1994", "8"="1995-1999", "9"="2000-2004", "10"="2005-2009", "11"="2010-2014", "12"="2015-2018"))
rsum_bod_plot_F

summary(lm(est ~ Cohort, data = rsum_bod_F))# slope -0.02 p value 0.577

## male only
rsum_bod_plot_M <- ggplot(data=rsum_bod_M, aes(x=as.factor(Cohort), y=est, fill=Sex))+
  theme_bw()+
  #geom_jitter()+
  geom_boxplot(width=.6, lwd=0.3, outlier.size = 0.5, fill="#67A9CF", colour="gray30")+
  xlab("Cohort by birth year")+
  ylab("Breeding values (est)")+
  #ggtitle("Body length in males")+
 geom_abline(intercept = coefs_bM[1], slope = coefs_bM[2], colour="red")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_discrete(labels=c("1" = "1960-1964", "2" = "1965-1969", "3" = "1970-1974", "4" = "1975-1979", "5" = "1980-1984", "6" = "1985-1989", "7" = "1990-1994", "8"="1995-1999", "9"="2000-2004", "10"="2005-2009", "11"="2010-2014", "12"="2015-2018"))
rsum_bod_plot_M


summary(lm(est ~ Cohort, data = rsum_bod_M))# slope -0.28 p value 0.000054

(rsum_bod_plot_overall + plot_spacer() )/ (rsum_bod_plot_F + rsum_bod_plot_M)

#########

# Next do zygobreadth
rsum_zyg2 <- rsum_zyg[,c("est", "Born", "Sex")]

rsum_zyg2_co1 <- subset(rsum_zyg2, Born == "1960" | Born == "1961" | Born == "1962" | Born == "1963" | Born == "1964")
rsum_zyg2_co1$Cohort <- 1
rsum_zyg2_co2 <- subset(rsum_zyg2, Born == "1965" | Born == "1966" | Born == "1967" | Born == "1968" | Born == "1969")
rsum_zyg2_co2$Cohort <- 2
rsum_zyg2_co3 <- subset(rsum_zyg2, Born == "1970" | Born == "1971" | Born == "1972" | Born == "1973" | Born == "1974")
rsum_zyg2_co3$Cohort <- 3
rsum_zyg2_co4 <- subset(rsum_zyg2, Born == "1975" | Born == "1976" | Born == "1977" | Born == "1978" | Born == "1979")
rsum_zyg2_co4$Cohort <- 4
rsum_zyg2_co5 <- subset(rsum_zyg2, Born == "1980" | Born == "1981" | Born == "1982" | Born == "1983" | Born == "1984")
rsum_zyg2_co5$Cohort <- 5
rsum_zyg2_co6 <- subset(rsum_zyg2, Born == "1985" | Born == "1986" | Born == "1987" | Born == "1988" | Born == "1989")
rsum_zyg2_co6$Cohort <- 6
rsum_zyg2_co7 <- subset(rsum_zyg2, Born == "1990" | Born == "1991" | Born == "1992" | Born == "1993" | Born == "1994")
rsum_zyg2_co7$Cohort <- 7
rsum_zyg2_co8 <- subset(rsum_zyg2, Born == "1995" | Born == "1996" | Born == "1997" | Born == "1998" | Born == "1999")
rsum_zyg2_co8$Cohort <- 8
rsum_zyg2_co9 <- subset(rsum_zyg2, Born == "2000" | Born == "2001" | Born == "2002" | Born == "2003" | Born == "2004")
rsum_zyg2_co9$Cohort <- 9
rsum_zyg2_co10 <- subset(rsum_zyg2, Born == "2005" | Born == "2006" | Born == "2007" | Born == "2008" | Born == "2009")
rsum_zyg2_co10$Cohort <- 10
rsum_zyg2_co11 <- subset(rsum_zyg2, Born == "2010" | Born == "2011" | Born == "2012" | Born == "2013" | Born == "2014")
rsum_zyg2_co11$Cohort <- 11
rsum_zyg2_co12 <- subset(rsum_zyg2, Born == "2015" | Born == "2016" | Born == "2017" | Born == "2018" | Born == "2019")
rsum_zyg2_co12$Cohort <- 12

rsum_zyg2_allco <- rbind(rsum_zyg2_co1, rsum_zyg2_co2, rsum_zyg2_co3, rsum_zyg2_co4, rsum_zyg2_co5, rsum_zyg2_co6, rsum_zyg2_co7, rsum_zyg2_co8, rsum_zyg2_co9, rsum_zyg2_co10, rsum_zyg2_co11, rsum_zyg2_co12)

str(rsum_zyg2_allco)

coefs_zall <- coef(lm(est ~ Cohort, data = rsum_zyg2_allco))

rsum_zyg_M <- subset(rsum_zyg2_allco,Sex=="M")
coefs_zM <- coef(lm(est ~ Cohort, data = rsum_zyg_M))
rsum_zyg_F <- subset(rsum_zyg2_allco,Sex=="F")
coefs_zF <- coef(lm(est ~ Cohort, data = rsum_zyg_F))

rsum_zyg_plot <- ggplot(data=rsum_zyg2_allco, aes(x=as.factor(Cohort), y=est, fill=Sex))+
  theme_bw()+
  #geom_jitter()+
  geom_boxplot(width=.8, lwd=0.3, outlier.size = 0.3)+
  scale_fill_manual(values=c("F"="#EF8A62", "M"="#67A9CF"))+
  xlab("Cohort")+
   theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  ylab("Breeding values (est)")+
  ggtitle("Zygomatic breadth")+
  geom_abline(intercept = coefs_zF[1], slope = coefs_zF[2], colour="red")+
  geom_abline(intercept = coefs_zM[1], slope = coefs_zM[2], colour="blue")

#  scale_x_discrete(labels=c("196" = "1960's", "197" = "1970's", "198" = "1980's", "199" = "1990's", "200" = "2000's", "201" = "2010's"))
rsum_zyg_plot

### all 
rsum_zyg_plot_overall <- ggplot(data=rsum_zyg2_allco, aes(x=as.factor(Cohort), y=est))+
  theme_bw()+
  geom_boxplot(width=.6, lwd=0.3, outlier.size = 0.5, fill="#BDC9E1", colour="gray30")+
 # xlab("Cohort by birth year")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #ylab("Breeding values (est)")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("Zygomatic breadth")+
  geom_abline(intercept = coefs_zall[1], slope = coefs_zall[2], colour="red")
  #theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  #scale_x_discrete(labels=c("1" = "1960-1964", "2" = "1965-1969", "3" = "1970-1974", "4" = "1975-1979", "5" = "1980-1984", "6" = "1985-1989", "7" = "1990-1994", "8"="1995-1999", "9"="2000-2004", "10"="2005-2009", "11"="2010-2014", "12"="2015-2018"))
rsum_zyg_plot_overall

summary(lm(est ~ Cohort, data = rsum_zyg2_allco)) #### slope +0.027 (cohort), p value 0.53

## female only
rsum_zyg_plot_F <- ggplot(data=rsum_zyg_F, aes(x=as.factor(Cohort), y=est, fill=Sex))+
  theme_bw()+
  geom_boxplot(width=.6, lwd=0.3, outlier.size = 0.5, fill="#EF8A62", colour="gray30")+
 # xlab("Cohort by birth year")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #ylab("Breeding values (est)")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
 # ggtitle("Zygomatic breadth in females")+
  geom_abline(intercept = coefs_zF[1], slope = coefs_zF[2], colour="red")
  #theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  #scale_x_discrete(labels=c("1" = "1960-1964", "2" = "1965-1969", "3" = "1970-1974", "4" = "1975-1979", "5" = "1980-1984", "6" = "1985-1989", "7" = "1990-1994", "8"="1995-1999", "9"="2000-2004", "10"="2005-2009", "11"="2010-2014", "12"="2015-2018"))
rsum_zyg_plot_F

summary(lm(est ~ Cohort, data = rsum_zyg_F))# slope +0.17 p value 0.0001

## male only
rsum_zyg_plot_M <- ggplot(data=rsum_zyg_M, aes(x=as.factor(Cohort), y=est, fill=Sex))+
  theme_bw()+
  #geom_jitter()+
  geom_boxplot(width=.6, lwd=0.3, outlier.size = 0.5, fill="#67A9CF", colour="gray30")+
  xlab("Cohort by birth year")+
  #ylab("Breeding values (est)")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  #ggtitle("Zygomatic breadth in males")+
  geom_abline(intercept = coefs_zM[1], slope = coefs_zM[2], colour="red")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_discrete(labels=c("1" = "1960-1964", "2" = "1965-1969", "3" = "1970-1974", "4" = "1975-1979", "5" = "1980-1984", "6" = "1985-1989", "7" = "1990-1994", "8"="1995-1999", "9"="2000-2004", "10"="2005-2009", "11"="2010-2014", "12"="2015-2018"))
rsum_zyg_plot_M

summary(lm(est ~ Cohort, data = rsum_zyg_M))# slope -0.42 p value 0.000028

#(rsum_zyg_plot_overall + plot_spacer() )/ (rsum_zyg_plot_F + rsum_zyg_plot_M)

# Make mega plot with all of them together
(rsum_bod_plot_overall + rsum_zyg_plot_overall )/ (rsum_bod_plot_F + rsum_zyg_plot_F) / (rsum_bod_plot_M + rsum_zyg_plot_M)

#ggsave("MCMCglmm/breeding_values_cohort5year_megaplot.png", width=8, height=8,dpi=1000)
