#Importando librarys
library(FactoMineR)
library(factoextra)
library(cluster)
library(xlsx)

#Base de dados
dados_brutos = read.csv("water-treatment.data", header=TRUE)

#Definir a quantidade otima de cluster
#(dados_brutos, kmeans, method = "silhouette" )

#Gerar o kmeans
dados_kmeans <- kmeans(dados_brutos, 3)

#Visualizar o kmeans
fviz_cluster(dados_kmeans, data=dados_brutos)
