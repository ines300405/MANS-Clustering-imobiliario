##########################################################
#                                                        #
#                  Grupo 2 - Turma 2                     #
#________________________________________________________#
#                                                        #
#             Carolina Morgado - 123794                  #   
#                Diogo Nobre - 123441                    #
#                 Inês Silva - 123407                    #
#               Marcos Mestre - 123436                   #
#               Maria Pereira - 123393                   #
#                                                        #
##########################################################

# PASS0 0 - ESTABELECER UM OBJETIVO
# Agrupar as casas pelas suas características intrínsecas
# Realizar Principal Components com todas as variáveis exceto id e preço

# Bibliotecas necessárias
library(psych)
library(corrplot)
library(cluster)
library(mclust)
library(ggplot2)

set.seed(1234)

# Upload da base de dados
file_path <- file.choose()
bd <- read.csv(file_path, sep=";")


# PASSO 1 - VERIFICAR SE REALIZAR PCs É ADEQUADO
#   - Mais de 300 observações - ✔
#   - Coeficientes de correlação > 0.3 - X (há valores menores)
#   - Teste de Bartlett's - p < 0.05 siginifica prosseguir com PCA - ✔
#   - Medida de KMO - ✔/X - deu medíocre (0.58)

# Matriz de correlações (arredondada com 3 casas decimais) e respetivo gráfico 
# com as variáveis input escolhidas
correlation <- round(cor(bd[,3:13]),3)
correlation
corrplot.mixed(correlation,
               tl.pos = "lt") # posição do texto

# Teste de Bartlett's -  mede se os dados se adequam à realização de PCs
# Resultado: p value = 0, logo é adequado
cortest.bartlett(correlation, n = nrow(bd))

# KMO - mede se os dados se adequam à realização de PCAs
# Resultado: 0.58 - é medíocre
KMO(correlation)


# PASSO 2 - EXTRAÇÃO DOS PCs
#   - Critério de Kaiser - só manter PCs com eigenvalue > 1 (os menores que 1 
#                          não são melhores do que uma variável)
#                        - este critério tende a sobrevalorizar o número de PCs
#   - Variância total extraída - idealmente, pelo menos da 60% da variância deve 
#     ser mantida com os PCAs escolhidos
#   - Scree plot - retirar o número de PCAs no "cotovelo" do gráfico


# Antes de realizar a PCA deve-se standardizar os dados, porque as diferentes 
# unidades das diferentes variáveis influenciam os resultados
data_scaled <- scale(bd[,3:13])
data_scaled

# Realizamos no máximo 11 PCs porque são 11 variáveis de input
# No maximo há um PC por variável, depois extraímos os que forem adequados
pc11 <- principal(data_scaled, nfactors=11, rotate="none")
pc11$loadings

# Método do critério de Kaiser - escolheríamos 4 porque são os que têm 
#                                eigenvalue > 1
# Método variância total extraida - escolheriámos 3 para ter no mínimo 60%
# Método gráfico - cotovelo
plot(pc11$values, type = "b", main = "Scree plot - Método do Cotovelo",
     xlab = "Número de componentes", ylab = "Eigenvalue")
# Por este método escolheríamos 3 PCs, porque é o local do "cotovelo" do gráfico


# DECISÃO FINAL: retirar os 4 primeiros principal components
pc4 <- principal(data_scaled, nfactors=4, rotate="none", scores=TRUE)
pc4$loadings

# Verificar quanta variância de cada variável ficou explicada pelos 
# PCs selecionados
round(pc4$communality,3)
# A variável vista é a menos representada


# PASSO 3 - REALIZAR ROTAÇÕES E INTERPRETAR
#   - Cada PC deve ser interpretado em termos das variáveis que têm o respetivo 
#     loading maior
#   - Visto que o número de PCs já está definido, devemos rodá-los para lhes dar 
#     mais significado e os tornar mais fáceis de interpretar
#   - Vai ser utilizado o método de rotação Varimax
#        - Torna os loadings grandes ainda maiores e os loadings pequenos ainda 
#          menores dentro de cada PC
#   - Cada PC deve receber um nome que espelhe as variáveis mais nele 
#     representadas 
pc4rotated <- principal(data_scaled, nfactors=4, rotate="varimax")
pc4rotated$loadings

# PC1 - Relaciona nrWC, nrAndares, piso_m2 e ano_construcao 
# Nome: Índice de Estrutura e Idade 

# PC2 - Relaciona nrQuartos, Sala_estar_m2 e arrecadacao_m2 
# Nome: Índice de Espaço Interior e Arrumos

# PC3 - Relaciona Condicao e Ano_renovacao (inverso) 
# Nome: Índice de Condição e Renovação

# PC4 - Relaciona lote_m2 apenas
# Nome: Índice do Terreno



# PASSO 4 - CRIAÇAO DAS NOVAS VARIÁVEIS 
bd$estr_idd <- pc4$scores[,1]
bd$espint_arr <- pc4$scores[,2]
bd$cond_ren <- pc4$scores[,3]
bd$terreno <- pc4$scores[,4]

# Visualizaçao da relação entre os PCs
pairs(bd[, 14:17], pch = 19, cex = 0.5)




# PASSO 5 - CRIAÇÃO DOS CLUSTERS
# - Hierarchical clustering
# - Partition clustering
# - Probabilistic clustering

# Hierarchical clustering - cada observação só pertence a um cluster
# HC - Método Ward D2
distance_matrix <- dist(bd[,14:17])
hierarchical_clusters_W2 <- hclust(distance_matrix, method='ward.D2')
plot(hierarchical_clusters_W2, hang=-1, labels=FALSE)
rect.hclust(hierarchical_clusters_W2, k=5, border="red")

# Pela interpretação do dendograma, foram escolhidos realizar 5 clusters
clusters_hcw2 <- cutree(hierarchical_clusters_W2, k=5)

# Silhouette score - entre -1 e 1
# Valores perto de 1 - os pontos estão bem classificados no seu cluster
# Valores perto de 0 - os pontos estão localizados entre clusters
# Valores negativos - sugerem que os pontos foram classificados no cluster errado
plot(silhouette(clusters_hcw2, distance_matrix))
# Valor da silhueta - 0.33, indica uma estrutura fraca

# Para não ficar tudo a branco:
#sil <- silhouette(clusters_hcw2, distance_matrix)
#pdf('grafico.pdf')
#plot(sil)
#dev.off()

# Adicionar ao dataset o número de cluster a que pertence cada casa
bd$cluster_W2 <- as.factor(clusters_hcw2)


# HC - Método Complete
hierarchical_clusters_comp <- hclust(distance_matrix,method='complete')
plot(hierarchical_clusters_comp, hang=-1, labels=FALSE, main="Hierarchical Clustering - Complete")
rect.hclust(hierarchical_clusters_comp, k=10, border="red")
# Tem clusters deproporcionais, agrega tudo no mesmo


# HC - Método Single - não dá para visualizar
hierarchical_clusters_single <- hclust(distance_matrix,method='single')
plot(hierarchical_clusters_single, hang=-1, labels=FALSE, main="Hierarchical Clustering - Single")


# Abordagens de partição 
# K-MEANS
# O número de clusters é definido logo à partida
# Função utilizada para criar um gráfico que permite extrair o valor ideal de k 
# através do método do cotovelo
wssplot <- function(data, k_max, seed=1234) {
  wss <- (nrow(data)-1)*sum(apply(data, 2, var))
  for (i in 2:k_max) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:k_max, wss, type="b", 
       xlab="Number of Clusters", 
       ylab="Within groups sum of squares")}

wssplot(bd[,14:17], k_max=10)

# Pelo método do cotovelo foram extraidos 5 clusters
kmeans_k5 <- kmeans(bd[,14:17], 5, nstart=1)
plot(silhouette(kmeans_k5$cluster, distance_matrix))
# Para não ficar tudo a branco:
#sil <- silhouette(kmeans_k5$cluster, distance_matrix)
#pdf('grafico.pdf')
#plot(sil)
#dev.off()

# Adicionar ao dataset o número de cluster a que pertence cada casa
bd$cluster_km <- as.factor(kmeans_k5$cluster)




# PARTIÇÃO - PAM - o número de clusters é definido à partida
pam_k5 <- pam(bd[,c(14:17)], 5)
plot(silhouette(pam_k5$clustering, distance_matrix))
# Para nao ficar tudo branco
#sil <- silhouette(pam_k5$clustering, distance_matrix)
#pdf('grafico.pdf')
#plot(sil)
#dev.off()

# Adicionar ao dataset o número de cluster a que pertence cada casa
bd$cluster_PAM <- pam_k5$clustering 




# Comparar os métodos 
table(clusters_hcw2, kmeans_k5$cluster)
table(kmeans_k5$cluster,pam_k5$clustering)
table(clusters_hcw2,pam_k5$clustering)




# Visualizar como os PCs se relacionam com os clusters
# HIERÁRQUICO W2
barplot(colMeans(subset(bd,cluster_W2==1)[,14:17]),
        main= "Cluster 1 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_W2==2)[,14:17]),
        main= "Cluster 2 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_W2==3)[,14:17]),
        main= "Cluster 3 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_W2==4)[,14:17]),
        main= "Cluster 4 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_W2==5)[,14:17]),
        main= "Cluster 5 - Score médio para cada PC")

# PARTIÇAO - K-MEANS
barplot(colMeans(subset(bd,cluster_km==1)[,14:17]),
        main= "Cluster 1 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_km==2)[,14:17]),
        main= "Cluster 2 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_km==3)[,14:17]),
        main= "Cluster 3 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_km==4)[,14:17]),
        main= "Cluster 4 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_km==5)[,14:17]),
        main= "Cluster 5 - Score médio para cada PC")


# Partição PAM
barplot(colMeans(subset(bd,cluster_PAM==1)[,14:17]),
        main= "Cluster 1 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_PAM==2)[,14:17]),
        main= "Cluster 2 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_PAM==3)[,14:17]),
        main= "Cluster 3 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_PAM==4)[,14:17]),
        main= "Cluster 4 - Score médio para cada PC")
barplot(colMeans(subset(bd,cluster_PAM==5)[,14:17]),
        main= "Cluster 5 - Score médio para cada PC")




# Probablistic Cluster - cada observação pode pertencer a mais do que um cluster
# Bayesian Information Criterion (BIC) - Para escolher o número de clusters e o
#                                        modelo GMM a utilizar
# O modelo com maior número deve ser o escolhido
BIC <- mclustBIC(bd[,c(14:17)])
print(BIC)
plot(BIC, legendArgs = list(x = "topleft", inset = c(0, 0), 
                       cex = 0.6)) # Posiciona e ajusta a legenda

# Escolhido o modelo VVV com 7 clusters
modelo <- Mclust(bd[,c(14:17)], G=7, modelNames = "VVV")

plot(modelo, what = "classification")
plot(modelo, what = "uncertainty")

# Conclusao: Todos os métodos formam estruturas fracas

