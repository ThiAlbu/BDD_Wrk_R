# -*- coding: utf-8 -*-
"""Trab_Tedas_R

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1CDAqeTccSHJYCpfKL_pHJO_WSsUi8cFt

# Extração:
"""

library(tidyverse)

#Extração de dados:
dat <- read.csv("/perfil_eleitorado_2024.csv", encoding = 'latin1', sep = ';')

#Resumo da base de dados:
nrow(dat)
summary(dat)

"""#Tratamento:"""

# Atributos interessantes para a Análise:
Atributos <- c('SG_UF','NM_MUNICIPIO','CD_GENERO','DS_GENERO','CD_ESTADO_CIVIL','DS_ESTADO_CIVIL','CD_FAIXA_ETARIA','DS_FAIXA_ETARIA','CD_GRAU_ESCOLARIDADE','DS_GRAU_ESCOLARIDADE','QT_ELEITORES_PERFIL')

# Pipeline de transformações > Selecionar colunas > Definir as Cidades > Limpar Nulos:

dado_rj <- dat %>%
  select(all_of(Atributos))%>%
  filter(SG_UF == 'RJ')%>%
  distinct()%>%
  filter(DS_FAIXA_ETARIA != "Inválida")%>%
  filter(if_all(all_of(Atributos), ~ . != "NÃO INFORMADO"))

dado_ba <- dat %>%
  select(all_of(Atributos))%>%
  filter(SG_UF == 'BA')%>%
  distinct()%>%
  filter(DS_FAIXA_ETARIA != "Inválida")%>%
  filter(if_all(all_of(Atributos), ~ . != "NÃO INFORMADO"))

ndadostotaisrj <- dat %>% select(all_of(Atributos)) %>% filter(SG_UF == "RJ") %>% nrow
ndadostotaisba <- dat %>% select(all_of(Atributos)) %>% filter(SG_UF == "BA") %>% nrow


#Estatísticas Básicas das tabelas:
print("Número de linhas válidas em dado_rj e porcentagem nas linhas totais: ")
nrow(dado_rj)
dado_rj %>% select(QT_ELEITORES_PERFIL) %>% sum()
n_distinct(dado_rj)
nrow(dado_rj)/ndadostotaisrj
summary(dado_rj)

print("Número de linhas válidas em dado_ba e porcentagem nas linhas totais: ")
nrow(dado_ba)
dado_ba %>% select(QT_ELEITORES_PERFIL) %>% sum()
n_distinct(dado_ba)
nrow(dado_ba)/ndadostotaisba
summary(dado_ba)

"""#Análise:

Seleções por perfil:
"""

#Homem, Casado, 35 a 39 anos, Médio Completo
dado_rj %>% dplyr::filter(DS_GENERO == 'MASCULINO', DS_ESTADO_CIVIL == 'CASADO',DS_FAIXA_ETARIA == '35 a 39 anos', DS_GRAU_ESCOLARIDADE == 'ENSINO MÉDIO COMPLETO')
dado_ba %>% dplyr::filter(DS_GENERO == 'MASCULINO', DS_ESTADO_CIVIL == 'CASADO',DS_FAIXA_ETARIA == '35 a 39 anos', DS_GRAU_ESCOLARIDADE == 'ENSINO MÉDIO COMPLETO')

#Mulher, solteira, 25 a 29 anos, Superior Completo
dado_rj %>% dplyr::filter(DS_GENERO == 'FEMININO', DS_ESTADO_CIVIL == 'SOLTEIRO',DS_FAIXA_ETARIA == '25 a 29 anos', DS_GRAU_ESCOLARIDADE == 'ENSINO MÉDIO COMPLETO')
dado_ba %>% dplyr::filter(DS_GENERO == 'FEMININO', DS_ESTADO_CIVIL == 'SOLTEIRO',DS_FAIXA_ETARIA == '25 a 29 anos', DS_GRAU_ESCOLARIDADE == 'ENSINO MÉDIO COMPLETO')

"""Contagem por atributos qualitativos:"""

#Por Gênero
dado_rj %>% count(DS_GENERO, wt = QT_ELEITORES_PERFIL) %>% arrange(desc(n))
dado_ba %>% count(DS_GENERO, wt = QT_ELEITORES_PERFIL) %>% arrange(desc(n))

#Estado Civil
dado_rj %>% count(DS_ESTADO_CIVIL, wt = QT_ELEITORES_PERFIL) %>% arrange(desc(n))
dado_ba %>% count(DS_ESTADO_CIVIL, wt = QT_ELEITORES_PERFIL) %>% arrange(desc(n))

#Faixa Etária
dado_rj %>% count(DS_FAIXA_ETARIA, wt = QT_ELEITORES_PERFIL) %>% arrange(desc(n))
dado_ba %>% count(DS_FAIXA_ETARIA, wt = QT_ELEITORES_PERFIL) %>% arrange(desc(n))

#Escolaridade
dado_rj %>% count(DS_GRAU_ESCOLARIDADE, wt = QT_ELEITORES_PERFIL) %>% arrange(desc(n))
dado_ba %>% count(DS_GRAU_ESCOLARIDADE, wt = QT_ELEITORES_PERFIL) %>% arrange(desc(n))

#Município
dado_rj %>% count(NM_MUNICIPIO, wt = QT_ELEITORES_PERFIL) %>% arrange(desc(n))
dado_ba %>% count(NM_MUNICIPIO, wt = QT_ELEITORES_PERFIL) %>% arrange(desc(n))

#Combinações Relevantes:

#Genero x Idade
dado_rj %>%
group_by(DS_GENERO,DS_FAIXA_ETARIA) %>%
summarise(quantidade = sum(QT_ELEITORES_PERFIL))%>%
pivot_wider(names_from = DS_GENERO,values_from = (quantidade),values_fill = 0)

dado_ba %>%
group_by(DS_GENERO,DS_FAIXA_ETARIA) %>%
summarise(quantidade = sum(QT_ELEITORES_PERFIL))%>%
pivot_wider(names_from = DS_GENERO,values_from = (quantidade),values_fill = 0)

#Genero x Escolaridade

dado_rj %>%
group_by(DS_GENERO,DS_GRAU_ESCOLARIDADE) %>%
summarise(quantidade = sum(QT_ELEITORES_PERFIL))%>%
pivot_wider(names_from = DS_GENERO,values_from = (quantidade),values_fill = 0)

dado_ba %>%
group_by(DS_GENERO,DS_GRAU_ESCOLARIDADE) %>%
summarise(quantidade = sum(QT_ELEITORES_PERFIL))%>%
pivot_wider(names_from = DS_GENERO,values_from = (quantidade),values_fill = 0)

#Idade x Escolaridade
dado_rj %>%
group_by(DS_FAIXA_ETARIA,DS_GRAU_ESCOLARIDADE) %>%
summarise(quantidade = sum(QT_ELEITORES_PERFIL))%>%
pivot_wider(names_from = DS_GRAU_ESCOLARIDADE,values_from = (quantidade),values_fill = 0)

dado_ba %>%
group_by(DS_FAIXA_ETARIA,DS_GRAU_ESCOLARIDADE) %>%
summarise(quantidade = sum(QT_ELEITORES_PERFIL))%>%
pivot_wider(names_from = DS_GRAU_ESCOLARIDADE,values_from = (quantidade),values_fill = 0)