setwd("C:/Users/Thiago A/Desktop/BancoDados-CSVs/Dataset - R-Tidyverse/trab")

library(tidyverse)

x=c("DATA", "HORA", "PRECIPITACAOTOTAL", "PRESSAOATMOSFERICA", "PRESSAOATMOSFERICAMAX", "PRESSAOATMOSFERICAMIN", "RADIACAOGLOBAL", "TEMPERATURADOAR", "TEMPERATURADOPONTODEORVALHO", "TEMPERATURAMAXIMA", "TEMPERATURAMINIMA", "TEMPERATURAORVALHOMAX", "TEMPERATURAORVALHOMIN", "UMIDADERELMAX", "UMIDADERELMIN", "UMIDADERELATIVADOAR", "VENTODIRECAO", "VENTORAJADA", "VENTOVELOCIDADE", "APAGAR")

vinte_dois = read.csv2("MA_IMPERATRIZ_2022.csv", sep=";", skip=8, fileEncoding = "latin1", header = TRUE, col.names=x)

vinte_tres = read.csv2("MA_IMPERATRIZ_2023.csv", sep=";", skip=8, fileEncoding = "latin1", header = TRUE, col.names=x)

view(vinte_dois)

vinte_dois %>%
rowwise()


Analise_2022 = vinte_dois %>%

select(DATA:PRESSAOATMOSFERICAMIN, TEMPERATURADOAR, TEMPERATURAMAXIMA, TEMPERATURAMINIMA, VENTODIRECAO:VENTOVELOCIDADE)

view(Analise_2022)

Analise_2022 %>%
rowwise()

#Filtragem de linhas 

filtrado = Analise_2022 %>%
filter(!is.na(TEMPERATURADOAR))

view(filtrado)

filtrado %>%
rowwise()

#A quantidade de linhas diminuiu em 1

#Limpando dataset 2023
filtrado_2023 = vinte_tres %>%
select(DATA:PRESSAOATMOSFERICAMIN, TEMPERATURADOAR, TEMPERATURAMAXIMA, TEMPERATURAMINIMA, VENTODIRECAO:VENTOVELOCIDADE) %>%
filter(!is.na(TEMPERATURADOAR))

#Analises de Dados

#1 trimestre
trimestre = filtrado %>%
filter(month(DATA) < 4)

view(trimestre)

estatistica_tri = trimestre %>%
summarise(media_temp = mean(TEMPERATURADOAR), max_temp = max(TEMPERATURADOAR), min_temp = min(TEMPERATURADOAR))

view(estatistica_tri)


#16 HORAS
h16 = filtrado %>%
filter(HORA == "1600 UTC")

view(h16)

estatistica_h16 = h16 %>%
summarise(media = mean(TEMPERATURADOAR), max = max(TEMPERATURADOAR), min = min(TEMPERATURADOAR))

view(estatistica_h16)


##Estatistica basica de cada variavel

#TEMPERATURA DO AR
temperatura = filtrado %>%
summarise(media_temp = mean(TEMPERATURADOAR), mediana_temp = median(TEMPERATURADOAR), max_temp = max(TEMPERATURADOAR), min_temp = min(TEMPERATURADOAR), sd_temp = sd(TEMPERATURADOAR))
view(temperatura)

#PRECIPITAÇÃO TOTAL
precipitacao = filtrado %>%
summarise(media_precipitacao = mean(PRECIPITACAOTOTAL, na.rm = TRUE), mediana_precipitacao = median(PRECIPITACAOTOTAL, na.rm = TRUE), min_precipitacao = min(PRECIPITACAOTOTAL, na.rm = TRUE), max_precipitacao = max(PRECIPITACAOTOTAL, na.rm = TRUE), sd_precip = sd(PRECIPITACAOTOTAL, na.rm = TRUE))
view(precipitacao)

#PRESSAO ATMOSFERICA
pressao = filtrado %>%
summarise(media_pressao = mean(PRESSAOATMOSFERICA), mediana_pressao = median(PRESSAOATMOSFERICA), min_pressao = min(PRESSAOATMOSFERICA), max_pressao = max(PRESSAOATMOSFERICA), sd_pressao = sd(PRESSAOATMOSFERICA))
view(pressao)

#VENTO VELOCIDADE
vento = filtrado %>%
summarise(media_vento_vel = mean(VENTOVELOCIDADE), mediana_vento_vel = median(VENTOVELOCIDADE), min_vento_vel = min(VENTOVELOCIDADE), max_vento_vel = max(VENTOVELOCIDADE), sd_vento_vel = sd(VENTOVELOCIDADE))
view(vento)



##Temperatura max, min e media

#PRECIPITACAO MENSAL 2022
mensal_2022 = filtrado %>%
group_by(month(DATA)) %>%
summarise(media_precipitacao = mean(PRECIPITACAOTOTAL, na.rm = TRUE), max_precipitacao = max(PRECIPITACAOTOTAL, na.rm = TRUE), min_precipitacao = min(PRECIPITACAOTOTAL, na.rm = TRUE))
view(mensal_2022)

#PRECIPITACAO MENSAL 2023
mensal_2023 = filtrado_2023 %>%
group_by(month(DATA)) %>%
summarise(media_precipitacao = mean(PRECIPITACAOTOTAL), max_precipitacao = max(PRECIPITACAOTOTAL), min_precipitacao = min(PRECIPITACAOTOTAL))
view(mensal_2023)

#TEMPERATURA DIA-SEMANA 2022
dia_semana_2022 = filtrado %>%
group_by(wday(DATA)) %>%
summarise(media_temp = mean(TEMPERATURADOAR), max_temp = max(TEMPERATURADOAR), min_temp = min(TEMPERATURADOAR))
view(dia_semana_2022)

#TEMPERATURA DIA-SEMANA 2023
dia_semana_2023 = filtrado_2023 %>%
group_by(wday(DATA)) %>%
summarise(media_temp = mean(TEMPERATURADOAR), max_temp = max(TEMPERATURADOAR), min_temp = min(TEMPERATURADOAR))
view(dia_semana_2023)


#QUAL O ANO MAIS QUENTE?

#2022
filtrado %>%
summarise(media_temp = mean(TEMPERATURADOAR))

#2023
filtrado_2023 %>%
summarise(media_temp = mean(TEMPERATURADOAR))

#O ano mais quente é o de 2023


##GRAFICOS

# DIAS DA SEMANA 2022
filtrado %>%
group_by(dia = wday(DATA)) %>%
summarise(media_temp = mean(TEMPERATURADOAR, na.rm = TRUE)) %>%
ggplot() +
geom_line(aes(x = dia, y = media_temp)) +
geom_point(aes(x = dia, y = media_temp)) +
labs(title = "DIAS DA SEMANA 2022") +
scale_x_continuous(breaks = seq(1, 7, 1))

# DIAS DA SEMANA 2023
filtrado_2023 %>%
group_by(dia = wday(DATA)) %>%
summarise(media_temp = mean(TEMPERATURADOAR, na.rm = TRUE)) %>%
ggplot() +
geom_line(aes(x = dia, y = media_temp)) +
geom_point(aes(x = dia, y = media_temp)) +
labs(title = "DIAS DA SEMANA 2023") +
scale_x_continuous(breaks = seq(1, 7, 1))


# PRECIPITACAO MENSAL 2022
filtrado %>%
group_by(mes = month(DATA)) %>%
summarise(media_precipitacao = mean(PRECIPITACAOTOTAL, na.rm = TRUE)) %>%
ggplot() +
geom_col(aes(x = mes, y = media_precipitacao)) +
labs(title = "PRECIPITAÇÃO MENSAL 2022") +
scale_x_continuous(breaks = seq(1, 12, 1))

# PRECIPITACAO MENSAL 2023
filtrado_2023 %>%
group_by(mes = month(DATA)) %>%
summarise(media_precipitacao = mean(PRECIPITACAOTOTAL, na.rm = TRUE)) %>%
ggplot() +
geom_col(aes(x = mes, y = media_precipitacao)) +
labs(title = "PRECIPITAÇÃO MENSAL 2023") +
scale_x_continuous(breaks = seq(1, 12, 1))