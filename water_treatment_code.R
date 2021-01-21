#Importando librarys
library(FactoMineR)
library(factoextra)
library(cluster)
library(xlsx)
library(imputeTS)

#Base de dados
dados_brutos = read.csv("water-treatment.data", header=T)
dados_transpostos = t(dados_brutos)
dados_na <- dados_transpostos
dados_na <- dados_na[-1,]

#Encontrando variáveis com "?"
idx <- dados_na == "?"

#Substituindo variáveis com "?"
is.na(dados_na) <- idx

#Verificando tipo dos dados
sapply(dados_na, class)

#Convertendo dados
dados_convertidos_na <- data.frame(apply(dados_na, 2, function(x) as.numeric(as.character(x))))
sapply(dados_convertidos_na, class)

#Verificando se há dados faltantes
any(is.na(dados_convertidos_na))

#Percentual de dados faltantes
dados_faltantes_percent <- round(colSums(is.na(dados_convertidos_na))*100/nrow(dados_convertidos_na),2)
dados_faltantes_percent[dados_faltantes_percent>0]

#Preechendo dados faltantes com a média
data <- na_mean(dados_convertidos_na)

#Definir a quantidade ótima de clusters
fviz_nbclust(data, kmeans, method = "silhouette" )

#Gerar o kmeans
dados_kmeans <- kmeans(data, 2)

#Visualizar o kmeans
fviz_cluster(dados_kmeans, data=data)
