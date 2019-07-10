
library(wordVectors)

prep_word2vec(origin="~/Documents/n3gram/",destination="~/Documents/n3gram/cookbooks.txt",lowercase=T,bundle_ngrams=3)


model = train_word2vec("~/Documents/n3gram/cookbooks.txt","~/Documents/n3gram/cookbook_vectors.bin",vectors=200,threads=4,window=12,iter=5,negative_samples=0)
model =read.vectors("~/Documents/n3gram/cookbook_vectors.bin")


set.seed(10)
centers = 150
clustering = kmeans(model,centers=centers,iter.max = 40)

sapply(sample(1:centers,50),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:50])
})


ingredients = c("guinness")
term_set = lapply(ingredients, 
       function(ingredient) {
          nearest_words = model %>% closest_to(model[[ingredient]],20)
          nearest_words$word
        }) %>% unlist


subset = model[[term_set,average=F]]
subset %>%
  cosineDist(subset) %>% 
  as.dist %>%
  hclust %>%
  plot



  tastes = model[[c("bitter"),average=F]]
# model[1:3000,] here restricts to the 3000 most common words in the set.
common_similarities_tastes = model[1:3000,] %>% cosineSimilarity(tastes)
high_similarities_to_tastes = common_similarities_tastes[rank(-apply(common_similarities_tastes,1,max)) < 75,]










high_similarities_to_tastes = common_similarities_tastes[rank(-apply(common_similarities_tastes,1,max)) < 75,]
tastes = model[[c("sweet","salty","savory","bitter","sour"),average=F]]
# model[1:3000,] here restricts to the 3000 most common words in the set.
common_similarities_tastes = model[1:3000,] %>% cosineSimilarity(tastes)
high_similarities_to_tastes = common_similarities_tastes[rank(-apply(common_similarities_tastes,1,max)) < 75,]

