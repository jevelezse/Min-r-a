library(NLP)
library(tm)
library(ggplot2)
library(languageR)

tdm2<-function(doc){
  docCor<-Corpus(VectorSource(doc))
  docs <- tm_map(docCor, stripWhitespace)
  #docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removePunctuation)
  docs <-tm_map(docs,removeNumbers)
  docs <- tm_map(docs,content_transformer(tolower))
  DocsTDM <- TermDocumentMatrix(docs) 
  return(DocsTDM)
}

TablaFreq<-function(TDM){
  docmatrix <- as.matrix(TDM)
  doc.counts <- rowSums(docmatrix)
  doc.df <- data.frame(names(doc.counts),as.numeric(doc.counts),stringsAsFactors = FALSE)
  names(doc.df) <- c("Términos", "Frecuencia")
  doc.df$Frecuencia <- as.numeric(doc.df$Frecuencia)
  doc.occurrence <- sapply(1:nrow(docmatrix),
                           function(i)
                           {
                             length(which(docmatrix[i, ] > 0)) / ncol(docmatrix)
                           })
  doc.density <- doc.df$Frecuencia / sum(doc.df$Frecuencia)
  
  # Add the term density and occurrence rate
  doc.df <- transform(doc.df,density = doc.density,ocurrencia =doc.occurrence)
  S=head(doc.df[with(doc.df, order(-Frecuencia)),], n=50)
  return(S)
}

alice <- read.csv('/home/jennifer/Escritorio/hc.csv')
L=tdm2(alice)
View(L)
L1=TablaFreq(L)
heas(L)

  write.csv(L1,'/home/jennifer/Escritorio/frecuencias.csv')
#Gráfica de frecuencia de palabras.

ggplot(L1,aes(L1$Frecuencia,factor(L1$Términos,levels=L1$Términos)))+geom_point(stat="identity", colour="red")+ggtitle("Tabla de Frecuencias sin stopswords")+xlab("Frecuencia de la palabra")+ylab("Las 50 palabras más frecuentes")

coseno <-dist(L1, method = 'cosine')
diag(coseno) <- NA
cosine_dist <- apply(coseno, 2, mean, na.rm=TRUE)
head(cosine_dist, n = 60L)

coseno1 <-dist(r, method = 'cosine')
diag(coseno) <- NA
cosine_dist1 <- apply(coseno1, 2, mean, na.rm=TRUE)
head(cosine_dist1, n = 60L)

library(cluster) 
cluster1<-kmeans(tdm,10)
plot(tdm,cluster1$cluster)
