library(recommenderlab)
#data 
data <- read.csv(file = 'data.csv',row.names = 'X')
#cosin
ratingmat = as.matrix(data)
ratingmat = as(ratingmat, "realRatingMatrix")
similarity(ratingmat,method = 'cosine')


#Create Recommender Model. The parameters are UBCF and Cosine similarity. We take 14 nearest neighbours rgarding cosin matrix
rec_mod = Recommender(ratingmat, method = "UBCF", param=list(method="Cosine",nn=14)) 

#Obtain top 5 recommendations for 4st user entry in dataset
Top_1_pred = predict(rec_mod, ratingmat[4], n=1)


#Convert the recommendations to a list
Top_1_List = as(Top_1_pred, "list")
Top_1_List

#Create Recommender Model. The parameters are UBCF and Cosine similarity. We take 14 nearest neighbours rgarding cosin matrix
rec_mod = Recommender(ratingmat, method = "IBCF", param=list(method="Cosine", nn=14)) 

#Obtain top 5 recommendations for 4st user entry in dataset
Top_1_pred = predict(rec_mod, ratingmat[4], n=1)


#Convert the recommendations to a list
Top_1_List = as(Top_1_pred, "list")
Top_1_List


#Create Recommender Model. The parameters are UBCF and Cosine similarity. We take 14 nearest neighbours rgarding cosin matrix
rec_mod = Recommender(ratingmat, method = "IBCF", param=list(method="pearson", nn=14)) 

#Obtain top 5 recommendations for 4st user entry in dataset
Top_1_pred = predict(rec_mod, ratingmat[4], n=1)


#Convert the recommendations to a list
Top_1_List = as(Top_1_pred, "list")
Top_1_List
  
