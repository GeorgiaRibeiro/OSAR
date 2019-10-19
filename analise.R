#--------------------------------------------#
# Panorama de Residuos Solidos - Olinda 2017 #
#   Georgia Ribeiro Git: @georgiaribeiro     #
#--------------------------------------------#

#Carregar pacotes [1. Analises 2. DataViz]
library(tidyverse)
library(tidyselect)

library(ggplot2)

#Carregar banco
df = read.csv("dados_olinda.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)
glimpse(df)

#--------- ajustes no banco ---------#
#Renomear variaveis p/ facilitar analise
var.original = names(df[c(6:49)])
var_novo = (str_sub(var.original, 1, 5))

names(df)[6:49] =  var_novo

#Alterar tipos das variaveis
df$Municipio = as.character(df$Municipio)
df$Estado = as.character(df$Estado)
df$Prestador = as.character(df$Prestador)
df$Sigla.do.Prestador = as.character(df$Sigla.do.Prestador)

#df[c(8,9,13:15,23,24)] - NÃO CONSEGUI AUTOMATIZAR 
df$FN208 = gsub("\\.", "", df$FN208)
df$FN208 = gsub("\\,", ".", df$FN208)

df$FN220 = gsub("\\.", "", df$FN220)
df$FN220 = gsub("\\,", ".", df$FN220)

df$CO119 = gsub("\\.", "", df$CO119)
df$CO119 = gsub("\\,", ".", df$CO119)

df$POP_T = gsub("\\.", "", df$POP_T)
df$POP_U = gsub("\\.", "", df$POP_U)
df$CO164 = gsub("\\.", "", df$CO164)
df$CO165 = gsub("\\.", "", df$CO165)

df$FN208 = as.numeric(df$FN208)
df$FN220 = as.numeric(df$FN220)
df$CO119 = as.numeric(df$CO119)
df$POP_T = as.numeric(df$POP_T)
df$POP_U = as.numeric(df$POP_U)
df$CO164 = as.numeric(df$CO164)
df$CO165 = as.numeric(df$CO165)

str(df)

#--------- graficos ---------#
#g1 = qde de RS/ano

g1 = ggplot(df, aes(Ano.de.Referência, CO119)) +
  geom_line()
print(g1)

# ~ resolvendo erro de "unique name"
df = distinct(df)
df2 = unique.data.frame((df))

unique_name1 = c("cs009")
unique_name2 = c("in031")
names(df$CS009) =  unique_name1
names(df$IN031) = unique_name2

#mesmo grafico depois unificar o banco (mesmo erro)
g1 = ggplot(df2, aes(Ano.de.Referência, CO119)) +
  geom_line()
print(g1)
