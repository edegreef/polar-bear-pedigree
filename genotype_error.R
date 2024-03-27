# genotyping error with the dups

# trying MasterBayes
setwd("C:/Users/eveli/Dropbox/Polar_bears_ECCC/data/microsat/")
library(MasterBayes)

# test data
#data(WarblerG)
#GdP<-GdataPed(G=WarblerG, categories=NULL)
#tP<-tunePed(E1=15)
#model.dupE<-MCMCped(GdP=GdP,tP=tP,verbose=FALSE)

# real data
df <- read.csv("C:/Users/eveli/Dropbox/Polar_bears_ECCC/data/microsat/Micros_combined_dups_only_MasterBayes.csv", header=TRUE)
colnames(df)[2] <- "id"

df <- subset(df, select = -c(1,3))
GdP<-GdataPed(G=df, categories=NULL)
model.dupE<-MCMCped(GdP=GdP,verbose=FALSE)

E1 <- as.data.frame(model.dupE$E1)
summary(model.dupE$E1)
summary(model.dupE$E2)


plot(model.dupE$E1)
plot(model.dupE$E2)



plot(model.dupE$E2)
library(ggplotify)

p <- as.ggplot(function() plot(model.dupE$E2, type="n", main="title")  )
p

