#Importando librarys
library(cluster)
library(xlsx)
library(imputeTS)

#Base de dados
dados_brutos = read.csv("water-treatment.data", header=T)
dados_transpostos = t(dados_brutos)
dados_na <- dados_transpostos[-1,]

#Verificando tipo dos dados
sapply(dados_na, class)

#Convertendo dados e verificando o tipo novamente
dados_convertidos_na <- data.frame(apply(dados_na, 2, function(x) as.numeric(as.character(x))))
sapply(dados_convertidos_na, class)

#Verificando se há dados faltantes
any(is.na(dados_convertidos_na))

#Percentual de dados faltantes
dados_faltantes_percent <- round(colSums(is.na(dados_convertidos_na))*100/nrow(dados_convertidos_na),2)

#Percentual de dados faltantes maiores que 0
dados_faltantes_percent[dados_faltantes_percent>0]

#Encontrando variáveis com "?"
idx <- dados_na == "?"

#Substituindo variáveis com "?"
is.na(dados_na) <- idx

#Preenchendo dados faltantes com a média
data <- na_mean(dados_convertidos_na)

#Definir a quantidade ótima de clusters
fviz_nbclust(data, kmeans, method = "silhouette" )

#Gerar o kmeans
dados_kmeans <- kmeans(data, 2)

#Visualizar o kmeans
fviz_cluster(dados_kmeans, data=data)

#Padronizando os dados
data_scaled <- scale(dados_convertidos_na)

#Preenchendo dados faltantes com a média no dataset padronizado
data_mean_scaled <- na_mean(data_scaled)

#Gerar o kmeans com o dataset padronizado
dados_kmeans_scaled <- kmeans(data_mean_scaled, 2)

#Visualizar o kmeans do dataset padronizado
fviz_cluster(dados_kmeans_scaled, data=data_mean_scaled)


#Gerando os dendogramas (dist gera uma matriz de distâncias que é necessária
#no input de hclust)
dendograma_complete <- hclust(dist(data), method="complete")
dendograma_average <- hclust(dist(data), method="average")
dendograma_single <- hclust(dist(data), method="single")

plot(dendograma_complete, main="Complete")
plot(dendograma_average, main="Average")
plot(dendograma_single, main="Single")

#Gerando os dendogramas do dataset padronizado

dendograma_complete_scaled <- hclust(dist(data_mean_scaled), method="complete")
dendograma_average_scaled <- hclust(dist(data_mean_scaled), method="average")
dendograma_single_scaled <- hclust(dist(data_mean_scaled), method="single")

plot(dendograma_complete_scaled, main="Complete Scaled")
plot(dendograma_average_scaled, main="Average Scaled")
plot(dendograma_single_scaled, main="Single Scaled")
