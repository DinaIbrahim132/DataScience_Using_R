library(arules)
library(arulesViz)
#data
Groceries <-read.transactions("transactions.csv", sep = ",")
#plot top 10 
itemFrequencyPlot(Groceries, topN = 10)

# default settings result in zero rules learned
apriori(Groceries)
# set =support and confidence levels to learn more rules
groceryrules <- apriori(Groceries, parameter = list(support =
                                                      0.002, confidence = 0.20, maxlen = 3))
groceryrules

groceryrules2 <- apriori(Groceries, parameter = list(support =
                                                      0.002, confidence = 0.20, maxlen = 2))
groceryrules2

# sorting grocery rules by lift to determine actionable rules
inspect(sort(groceryrules, by = "lift")[1:10])

inspect(sort(groceryrules2, by = "lift")[1:10])

# writing the rules to a CSV file
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# converting the rule set to a data frame
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)
