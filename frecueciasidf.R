library('slam')
library('NLP')
library('tm')
library('ggplot2')
library('proxy')
library('cluster') 
library('fpc')
library('infotheo')
library('clValid')
library('plotly')
library('dplyr')



reviews <- read.csv("/home/jennifer/Escritorio/hc.csv", stringsAsFactors = F, encoding="UTF-8")
review_text <- reviews$HC
review_source <- VectorSource(review_text)
corpus <- Corpus(review_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
matriz_dtm <- DocumentTermMatrix(corpus)

tdm <- removeSparseTerms(matriz_dtm, 0.7)
terms <-DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x,normalize = FALSE)))
terms$dimnames$Docs

inspect(terms)
freqTFIDF=colSums(as.matrix(terms))
head(freqTFIDF,10)
plot(sort(freqTFIDF, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")
tail(sort(freqTFIDF),n=20)
high.freqTFIDF=tail(sort(freqTFIDF),n=30)
hfp.dfTFIDF=as.data.frame(sort(high.freqTFIDF))
hfp.dfTFIDF$names <- rownames(hfp.dfTFIDF) 
ggplot(hfp.dfTFIDF, aes(reorder(names,high.freqTFIDF), high.freqTFIDF)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terminos") + ylab("Frecuencia") +
  ggtitle("Grafica TF/IDF")


z <- as.matrix(terms)
head(z)
d <-as.data.frame(z)
write.csv(hfp.dfTFIDF,'/home/jennifer/Escritorio/idffrecuencias.csv')

frequencia <- colSums(z)
frequencia <- sort(frequencia, decreasing = T)
head(frequencia)

x<-weightBin(terms)
r<-as.matrix(x)
head(r) #matriz de terminos conpeso
head(z) # matriz de termnnos

coseno <-dist(as.matrix(z, method = 'cosine'))
head(coseno)
diag(coseno) <- NA
cosine_dist <- apply(coseno, 2, mean, na.rm=TRUE)
head(cosine_dist, n = 60L)

write.csv(cosine_dist,'/home/jennifer/Escritorio/coseno.csv')


coseno1 <-dist(r, method = 'cosine')
diag(coseno) <- NA
cosine_dist1 <- apply(coseno1, 2, mean, na.rm=TRUE)
head(cosine_dist1, n = 60L)

cluster1<-kmeans(terms,4)
cluster2<-kmeans(cosine_dist,4)

cluster2$cluster

validacion <-(silhouette(cluster1$cluster,z))
head(validacion)
plot(validacion)
head(cosine_dist)

plot_ly(x = coseno, y = coseno, type = "histogram2d")
plot_ly(x = coseno1, y = coseno1, type = "histogram2d")

which(dimnames(terms)$Terms == "cancer")
which(dimnames(terms)$Terms == "fibrosis")
zika_y_virus<-terms[,c(214,210)]
No_de_Clusters <-cluster2$cluster
No_de_Clusters
plot_ly(as.list(cosine_dist),x = No_de_Clusters, y= cosine_dist, color = No_de_Clusters, size = No_de_Clusters, mode = "markers")


  plot_ly(x=cluster2$cluster,y=cluster2$size, type = "bar", color = cluster2$size)
cluster2$size

write.csv(cluster1$size,"tamaño.csv")
mode(cosine_)
cluster4 <- as.matrix(cosine_dist)
cluster3 <- kmeans(cluster4,40)
No_de_clusters <- cluster3$cluster

Distancia_coseno<-as.list(cosine_dist)

plot_ly(Distancia_coseno,x = No_de_clusters, y= Distancia_coseno, color = No_de_clusters, mode = "markers")

plot_ly(x=No_de_clusters,y=cluster3$size, type = "bar")

mydata <- cosine_dist
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Numero de clusters", ylab="Numero de clustes", main="Numero de clusters optimos", pch=20, cex=2)

w<-as.list(wss)
plot_ly(w,x = No_de_clusters, y= Distancia_coseno, color = No_de_clusters, mode = "markers")
dim(cosine_dist)
View(wss)

distancia <- dist(as.matrix(z))
f<-hclust(distancia)
plot(f)

rect.hclust(distancia, n)
  

clusters <- hclust(dist(z[2]))
plot(clusters)
clusterCut <- cutree(clusters, 10)
rect.hclust(clusters, k=10, border="red")



table(clusterCut, z[,2])

row(terms)
d.scala<-scale(terms)
distancia<-dist(d.scala, method = 'cosine')
fit<-hclust(distancia, method = "ward.D2")
mode(distancia)

distancia$merge
plot(fit)

n<-50
muestra<- sample(1:nrow(terms),size=n,replace=FALSE)
muestra

muestra1 <- terms[muestra,]
head(muestra1$dimnames)
d.scala<-scale(muestra1)
distancia<-dist(d.scala, method = 'cosine')
fit<-hclust(distancia, method = "ward.D2")
plot(fit, main="Dendograma")
rect.hclust(fit,5)
fit

fit$merge
mydata <- na.omit(as.numeric(cosine_dist)) # listwise deletion of missing
mydata <- scale(as.numeric(cosine_dist)) # standardize variables
# Determine number of clusters


wss <- (nrow(cosine_dist)-1)*sum(apply(mydata,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares"
     ,main="Assessing the Optimal Number of Clusters ",pch=20, cex=2, col="red")


o<-fit$merge
rownames(o)<-fit$merge

mydata1 <- na.omit(terms) # listwise deletion of missing
mydata1 <- scale(terms) 

intern <- clValid(mydata,5,clMethods = "hierarchical", validation = "internal" )
summary(intern)

optimalScores(intern)

intern1 <- clValid(as.matrix(muestra1), nClust =12:5,clMethods = c("kmeans","hierarchical"), validation = "internal" )
summary(intern1)
optimalScores(intern1)
par(mfrow=c(2,1)) 
plot(intern1)

mode(intern1)
plot_ly(inter,x =intern, y= intern, mode = "markers")
plot(intern1)



write.csv(summary(intern),"mediadasdevlaidadiciol.csv")

