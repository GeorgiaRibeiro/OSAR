#--------------------------------------------#
# Panorama de Residuos Solidos - Olinda 2017 #
#   Georgia Ribeiro Git: @georgiaribeiro     #
#--------------------------------------------#

#Carregar pacotes [1. Analises 2. DataViz]
library(tidyverse)
library(tidyselect)

library(ggplot2)
library(plotly)

#Carregar banco
df = read.csv("dados_olinda.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)
glimpse(df)

qdelixo=read.csv("qde_lixo.csv")
str(qdelixo)

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

df$Ano = as.character(df$Ano.de.Referência)
df$Ano.de.Referência = NULL

qdelixo$Ano = as.character(qdelixo$Ano)
qdelixo$CO111 = as.numeric(qdelixo$CO111)

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

#--------- Corrigir Erro ---------#
#Erro: `data` must be uniquely named but has duplicate columns
distinct(df)

#Alterar nomes das variaveis CS009 e IN031 para serem unicos
df$CS9 = df$CS009
df$IN31 = df$IN031
df$CS009 = NULL
df$IN031 = NULL

#--------- graficos ---------#
#Quantidade de Resíduos produzidos em Olinda

glixoanos = ggplot(qdelixo, aes(x=Ano, y=CO111, group = 1)) +
  geom_line(color = "dodgerblue3", size=1.1) +
  geom_point(color = "dodgerblue4") +
  labs(y="Quantidade de RDO* coletados (ton)",
       caption = "*Resíduos Sólidos Domésticos")+
  theme_light() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 11))
  
glixoanos = ggplotly(glixoanos)
chart_link = plotly_POST(glixoanos, filename="geom_line/basic")

#Taxa de geração per capta de RSU (Kg/dia/hab)
 #aguardando serie histórica

#IN31 - Recuperação de materiais recicláveis: serie historica

ggplot(df, aes(x=Ano, y=IN31)) +
  geom_bar(stat = "identity", fill= "deepskyblue4") +
  labs(y="Taxa de recuperação de material recicláveis") +
  geom_text(aes(label = IN31, y = IN31 + 0.01),
            position = position_dodge(0.9),
            vjust = 0,
            color = "gray30",
            size = 3) +
  theme_light() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
