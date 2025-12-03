install.packages("arules")
library(arules)

# Ladda ner data

library(readr)
groceries <- read_csv("groceries.csv")
groceries2 <- read.transactions("groceries.csv", header = "False", sep = ",")

library(readr)
lastfm <- read_csv("Last FM Data.csv")

### Uppgift 1 - Market Basket Analysis

# Studera datasetet

summary(groceries)
class(groceries)
dim(groceries)

summary(groceries2)
class(groceries2)
dim(groceries2)

# Datainspektion

inspect(groceries2[1:10])

itemFrequency(groceries2[,1:12])

### Plot:

# All support, relative frequencis greater than than 0.05
itemFrequencyPlot(groceries2, support = 0.05, cex = 0.6, col = "red", main = "All with support greater than 0.05 ")

# The top 15 items with regard to the relatice frequency
itemFrequencyPlot(groceries2, topN = 15, cex = 0.8, col="blue", main = "Top 15 Support")

### Print a random sample of 200

image(sample(groceries2, 200))

### Association rule 1

rule1 <- apriori(groceries2, parameter= list(support = 0.05, confidence = 0.3))
inspect(rule1)

rule2 <-apriori(groceries2, parameter= list(support=0.006, confidence=0.25, minlen = 3))
inspect(rule2)

### Association rule 2

# Print the ten rules with the highest value on lift
inspect(sort(rule2, by = "lift")[1:10])

### Subset of items

# Choose all rules that includes berries
hamburgerrules<- subset(rule2, items %in% "hamburger meat")
inspect(hamburgerrules)

### Analyze the Last FM Data.

dim(lastfm)
names(lastfm)
lastfm[1:20,]
length(lastfm$user)


#Fixing the data 

lastfm$user <- as.character(lastfm$user)
lastfm$artist <- as.character(lastfm$artist)
lastfm$sex <- as.character(lastfm$sex)
lastfm$country <- as.character(lastfm$country)

levels(lastfm$user)
levels(lastfm$artist)
levels(lastfm$country)

###Turn the data into transactions: 

#Splitintoalistofusers
playlist <- split(x=lastfm[,"artist"], f=lastfm$user)

#Removeartistduplicates
playlist <- lapply(playlist, unique)

#Turnthedataintoasetoftransactions
playlist_transactions <- as(playlist, "transactions")

summary(playlist_transactions)
inspect(playlist_transactions[1:10])
  
