#---------------------------------------------------------#
# CARREGAMENTO DA BASE DE DADOS DE MANTIMENTOS (GROCERIES)#
#---------------------------------------------------------#

# Carregamento e leitura dos dados
library(readr)
groceries <- read.table("/cloud/project/Data/Data_Groceries.csv", header=TRUE, dec=".", sep="\t", stringsAsFactors=T)
ncol(groceries)
colnames(groceries)

# Representação de dados em formato transacional
install.packages("arules")
library(arules)
groceries_tr <- as(groceries, "transactions")
summary(groceries_tr)

# Histograma dos 10 itens mais frequentes
itemFrequencyPlot(groceries_tr, topN=10)
itemFrequencyPlot(groceries_tr, topN=10,type="absolute")
itemFrequencyPlot(groceries_tr, support=0.10)

# Visualização de números reais com 3 casas decimais
options(digits=3)

#----------------------------------------------#
# EXTRAINDO CONJUNTOS DE ITENS MAIS FREQUENTES #
#----------------------------------------------#

# Número de conjuntos de itens potencialmente frequentes
2^ncol(groceries_tr)

# Extração de conjuntos de itens frequentes para minsupport = 2%
fi <- apriori(groceries, parameter = list(supp = 0.02, target = "frequent itemsets"))

# Exibindo conjuntos de itens frequentes em 'fi'
inspect(fi)

# Exibindo um histograma dos tamanhos de conjuntos de itens freqüentes em 'fi'
barplot(table(size(fi)), xlab="Taille itemset", ylab="Nombre")

# Classificação de conjuntos de itens frequentes em ordem decrescente de seu meio
fi <- sort(fi, by="support")

# Visualizando os 30 conjuntos de itens mais comuns
inspect(head(fi, n=30))
# Ou também
inspect(fi[1:30,])

# Exibindo uma lista de conjuntos de itens comuns com tamanho ≥ 3, ou seja, consistindo em mais de 2 itens.
inspect(fi[size(fi)>2])

#--------------------------------#
# EXTRAINDO REGRAS DE ASSOCIAÇÃO #
#--------------------------------#

# Extraindo regras de associação para minsupport = 1% e mintrust = 40%
rules1 <- apriori(groceries, parameter = list(supp = 0.01, conf = 0.4, target = "rules"))
inspect(rules1)

# Calendarização das regras através de medidas geradoras de confiança e de apoio, respetivamente
rules1 <- sort(rules1, by = c("confidence", "support"))
inspect(rules1)

# Regras de associação de tamanho máximo de 2 itens para minsupport = 1% e minconfidence = 40% 
rules2 <- apriori(groceries, parameter = list(supp = 0.01, conf = 0.4, target = "rules", maxlen=2))
inspect(rules2)

# Regras de associação de tamanho máximo de 2 itens para minsupport = 0,1% e minconfidence = 50%
rules3 <- apriori(groceries, parameter = list(supp = 0.001, conf = 0.5, target = "rules", maxlen=2))
rules3 <- sort(rules3, by=c("confidence", "support"))
inspect(rules3)

# Segmentação do item 'Whole.milk=Y' na história do período
wm1 <- apriori(data=groceries, parameter=list(supp=0.01, conf=0.2), appearance=list(lhs="Whole.milk=Y"), control=list(verbose=F))
wm1 <- sort(wm1, by = c("confidence", "support"))
inspect(wm1)

# Direcionamento do item 'Whole.milk=Y' como resultado das regras
wm2 <- apriori(data=groceries, parameter=list(supp=0.01, conf=0.55), appearance=list(rhs="Whole.milk=Y"), control=list(verbose=F))
wm2 <- sort(wm2, by = c("confidence", "support"))
inspect(wm2)
