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
