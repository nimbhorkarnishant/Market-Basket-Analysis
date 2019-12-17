
#--------------------Market Basket Analysis-------------------------------

#Arules - to provide frequent itemset and association rules
#install.packages("arules")
library(arules)
library('RColorBrewer') #for create colourful graphs, 3 categories of palettes: qualitative, diverging, and sequential
#Plyr - to split and mash data together
#install.packages("plyr", dependencies= TRUE)
library('plyr')
library(dplyr)
#ARulesViz - visualization of assosiation rules and item set
library(arulesViz)

#read and load data set
Bakery = read.csv("./BreadBasket_DMS.csv")
Bakery
str(Bakery)
glimpse(Bakery)

#95 unique items
unique(Bakery$Item)

#min transacion is 1 and max 9684
summary(Bakery$Transaction)

#frequeny of each data item
summary(Bakery$Item)
sort(table(Bakery$Item))

#plot(Bakery$Item, measure=c("support","confidence"))

#sort transactions in ascending order 
df_sorted <- Bakery[order(Bakery$Transaction),]
df_sorted$Transaction <- as.numeric(df_sorted$Transaction)
str(df_sorted)

#pivots the item descriptions with same date and same member number in one line,
df_itemList <- ddply(Bakery, c("Transaction", "Date"),function(df1)paste(df1$Item,
                                                                         collapse =","))

df_itemList$Transaction <- NULL
df_itemList$Date <- NULL

#Rename column headers for ease of use
colnames(df_itemList) <- c("itemList")

#write.csv used to export data set
write.csv(df_itemList, "ItemList.csv", quote = FALSE, row.names = TRUE)

#Generating association rules - Converting to transaction format
txn=read.transactions(file="ItemList.csv", rm.duplicates = TRUE,
                      format = "basket", sep = ",", cols=1);

txn@itemInfo$labels <- gsub("\"","", txn@itemInfo$labels)
txn
inspect(txn[2:10])

#itemfrequency plot
itemFrequencyPlot(txn, topN = 20)

#support - basic probaility of an event to occur  transaction(a)/total transactions
#confidence - conditional probaility of occurance/ Confidence indicates the number of times the if-then statements are found true.
#lift - ratio of confidence to expected confidence

basket_rules <-apriori(txn,parameter = list(sup=0.001, conf=0.75));


if(sessionInfo()['basePkgs']== "tm" | sessionInfo()['otherPkgs'] == "tm"){
  detach(package:tm, unload=TRUE)
}
inspect(basket_rules)

#toprules = basket_rules[1:10]
#plot(basket_rules, method= "graph")
#plot(toprules, method= "graph")
#The larger the lift ratio, the more significant the association.

df_basket <- as(basket_rules, "data.frame")
View(df_basket)

plot(basket_rules)  #As the scatter plot gets darker the lift is more
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method = "grouped", control = list(brewer.pal(11,"Spectral")),main="")
plot(basket_rules, method= "graph", control=list(type="items"))
#plot(basket_rules, method= "paracoord", control=list(alpha=.5, reorder=TRUE)) 
#rules with high lift have low support
#larger the lift ratio stronger is the assocition
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=TRUE)
#plot(basket_rules, method = "matrix", measure=c("support","confidence"))

#" toprules = basket_rules[1:15]
#plot(basket_rules, method = "graph")

#plot(toprules, method = "graph", engine = "htmlwidget")

#toprules_lift = head(basket_rules, n=20, by ="lift")
#plot(toprules_lift, method="paracoord") "
#recommendation
#central tendancy
#refine rules
