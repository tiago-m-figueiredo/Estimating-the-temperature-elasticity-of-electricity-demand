#set working directory

setwd("C:/Users/tiago/Desktop/Investigação/Datasets")

#read the data

library(readr)

consumos <- read_delim("consumos_horario_codigo_postal.csv", delim = ";", locale = locale(decimal_mark = "."))

names(consumos)[names(consumos) == "Data/Hora"] <- "Data_Hora"
names(consumos)[names(consumos) == "Código Postal"] <- "Codigo_postal"
names(consumos)[names(consumos) == "Energia ativa (kWh)"] <- "Energia_ativa_kwh"
names(consumos)[names(consumos) == "Dia da Semana"] <- "Dia_da_semana"

consumos <- consumos[-(which(consumos$Codigo_postal == "OUTROS")),]

consumos$Codigo_postal <- as.factor(consumos$Codigo_postal)
consumos$Dia_da_semana <- as.factor(consumos$Dia_da_semana)

library(readxl)

caracteristicas <- read_excel("Características dos códigos postais.xlsx")

caracteristicas$Codigo_postal <- as.factor(caracteristicas$Codigo_postal)

lista_dados_meteo <- lapply(1:58, function(i) read_excel("Dados meteorológicos.xlsx", sheet = i))

library(dplyr)
library(purrr)

dados_meteo <- lista_dados_meteo %>% 
  map(~ select(.x, 'Estação', 'Data', 'Temperatura', 'Humidade relativa', 'Precipitação')) %>%
  bind_rows()

estacoes_meteo <- read_excel("Lista estações meteorológicas.xlsx")

#Creation of new columns - postcode characteristics

library(tidyr)

tipoareas <- caracteristicas %>% 
  group_by(Codigo_postal, Tipologia_areas_urbanas) %>%
  summarise(pop = sum(Populacao_residente_Total), .groups = "drop") %>%
  group_by(Codigo_postal) %>%
  mutate(percentagem = (pop / sum(pop))*100) %>%
  select(-pop) %>%
  pivot_wider(names_from = Tipologia_areas_urbanas, values_from = percentagem, values_fill = 0)
tipoareas$Tipo_area <- ifelse(tipoareas$`Área urbana` >= 50, "Urban area", "Rural area")
consumos <- consumos %>% left_join(tipoareas %>% select(Codigo_postal, Tipo_area), by = "Codigo_postal")
consumos$Tipo_area <- as.factor(consumos$Tipo_area)

popempregada <- aggregate(caracteristicas$Populacao_empregada_Total, by=list(Codigo_postal=caracteristicas$Codigo_postal), FUN=sum)
popempregada <- popempregada %>% rename(populacao_empregada_total = x)
consumos <- consumos %>% left_join(popempregada, by = "Codigo_postal")

popprimario <- aggregate(caracteristicas$Populacao_empregada_Setor_primário, by=list(Codigo_postal=caracteristicas$Codigo_postal), FUN=sum)
popprimario <- popprimario %>% rename(populacao_setor_primario = x)
consumos <- consumos %>% left_join(popprimario, by = "Codigo_postal")

consumos$populacao_setor_primario_percentagem <- (consumos$populacao_setor_primario / consumos$populacao_empregada_total) * 100

popsecundario <- aggregate(caracteristicas$Populacao_empregada_Setor_secundário, by=list(Codigo_postal=caracteristicas$Codigo_postal), FUN=sum)
popsecundario <- popsecundario %>% rename(populacao_setor_secundario = x)
consumos <- consumos %>% left_join(popsecundario, by = "Codigo_postal")

consumos$populacao_setor_secundario_percentagem <- (consumos$populacao_setor_secundario / consumos$populacao_empregada_total) * 100

popterciario <- aggregate(caracteristicas$Populacao_empregada_Setor_terciário, by=list(Codigo_postal=caracteristicas$Codigo_postal), FUN=sum)
popterciario <- popterciario %>% rename(populacao_setor_terciario = x)
consumos<-consumos %>% left_join(popterciario, by = "Codigo_postal")

consumos$populacao_setor_terciario_percentagem <- (consumos$populacao_setor_terciario / consumos$populacao_empregada_total) * 100

idademedia <- aggregate(caracteristicas$Idade_media, by=list(Codigo_postal=caracteristicas$Codigo_postal), FUN=mean)
idademedia <- idademedia %>% rename(idade_media = x)
consumos <- consumos %>% left_join(idademedia, by = "Codigo_postal")

ganhomedio <- aggregate(caracteristicas$Ganho_medio_mensal, by=list(Codigo_postal=caracteristicas$Codigo_postal), FUN=mean)
ganhomedio <- ganhomedio %>% rename(ganho_medio = x)
consumos <- consumos %>% left_join(ganhomedio, by = "Codigo_postal")

ensino <- aggregate(caracteristicas$Ensino_superior_completo_perc, by=list(Codigo_postal=caracteristicas$Codigo_postal), FUN=mean)
ensino <- ensino %>% rename(ensino_superior_percentagem = x)
consumos <- consumos %>% left_join(ensino, by = "Codigo_postal")

popresidente <- aggregate(caracteristicas$Populacao_residente_Total, by=list(Codigo_postal=caracteristicas$Codigo_postal), FUN=sum)
popresidente <- popresidente %>% rename(populacao_residente = x)
consumos <- consumos %>% left_join(popresidente, by = "Codigo_postal")

arcondicionado <- aggregate(caracteristicas$Populacao_residente_Com_ar_condicionado, by=list(Codigo_postal=caracteristicas$Codigo_postal), FUN=sum)
arcondicionado <- arcondicionado %>% rename(populacao_com_ar_condiconado = x)
consumos <- consumos %>% left_join(arcondicionado, by = "Codigo_postal")

consumos$populacao_com_ar_condicionado_percentagem <- (consumos$populacao_com_ar_condiconado / consumos$populacao_residente) * 100

#Cleaning observations

consumos <- na.omit(consumos)
consumos$Codigo_postal <- droplevels(as.factor(consumos$Codigo_postal))

#Arranging data by postcode and time

consumos <- consumos %>% arrange(Codigo_postal, Data_Hora)

#Weather stations - Postcodes

codigo_estacoes <- estacoes_meteo %>% distinct(`Nome da estação`, `Código postal`)
dados_meteo <- dados_meteo %>% left_join(codigo_estacoes, by= c("Estação" = "Nome da estação"))
dados_meteo <- dados_meteo %>% filter(!is.na(`Código postal`))

dados_meteo <- dados_meteo %>% rename(Estacao = Estação, Data_Hora = Data, Codigo_postal = `Código postal`, Humidade_relativa = `Humidade relativa`)

dados_meteo$Codigo_postal <- as.factor(dados_meteo$Codigo_postal)

#Weather data

library(lubridate)
dados_meteo <- dados_meteo %>% mutate(Data_Hora = floor_date(Data_Hora, "hour"))

dados_meteo <- dados_meteo %>%
  group_by(Codigo_postal, Data_Hora) %>%
  summarise(
    Temperatura = mean(Temperatura, na.rm = TRUE),
    Humidade_relativa = mean(Humidade_relativa, na.rm = TRUE),
    Precipitacao = sum(Precipitação, na.rm = TRUE),
    .groups = "drop")

#Joining weather data to consumption

consumos <- consumos %>% left_join(dados_meteo, by = c("Codigo_postal", "Data_Hora"))

##Kelvin temperature

consumos$Temperatura_Kelvin <- consumos$Temperatura + 273.15

#Data characterization

##Cleaning negative consumption

consumos <- consumos %>%
  filter(!is.na(Energia_ativa_kwh), Energia_ativa_kwh >= 0)

##Consumption and temperature

caracterizacao <- consumos %>%
  group_by(Codigo_postal) %>%
  summarise(
    consumo_min = min(Energia_ativa_kwh, na.rm = TRUE),
    consumo_medio = mean(Energia_ativa_kwh, na.rm = TRUE),
    consumo_max = max(Energia_ativa_kwh, na.rm = TRUE),
    temperatura_min = min(Temperatura, na.rm = TRUE),
    temperatura_media = mean(Temperatura, na.rm = TRUE),
    temperatura_max = max(Temperatura, na.rm = TRUE))

library(openxlsx)

write.xlsx(caracterizacao, "Caracterização dos dados por código postal.xlsx", rowNames = FALSE)

##Socioeconomic characteristics

estatistica_descritiva <- consumos %>% select(Codigo_postal) %>% distinct(Codigo_postal)

estatistica_descritiva <- estatistica_descritiva %>% left_join(tipoareas %>% select(Codigo_postal, Tipo_area), by = "Codigo_postal")
estatistica_descritiva$Tipo_area <- as.factor(estatistica_descritiva$Tipo_area)

estatistica_descritiva <- estatistica_descritiva %>% left_join(popempregada, by = "Codigo_postal")

estatistica_descritiva <- estatistica_descritiva %>% left_join(popprimario, by = "Codigo_postal")

estatistica_descritiva$populacao_setor_primario_percentagem <- (estatistica_descritiva$populacao_setor_primario / estatistica_descritiva$populacao_empregada_total) * 100

estatistica_descritiva <- estatistica_descritiva %>% left_join(popsecundario, by = "Codigo_postal")

estatistica_descritiva$populacao_setor_secundario_percentagem <- (estatistica_descritiva$populacao_setor_secundario /estatistica_descritiva$populacao_empregada_total) * 100

estatistica_descritiva <- estatistica_descritiva %>% left_join(popterciario, by = "Codigo_postal")

estatistica_descritiva$populacao_setor_terciario_percentagem <- (estatistica_descritiva$populacao_setor_terciario / estatistica_descritiva$populacao_empregada_total) * 100

estatistica_descritiva <- estatistica_descritiva %>% left_join(idademedia, by = "Codigo_postal")

estatistica_descritiva <- estatistica_descritiva %>% left_join(ganhomedio, by = "Codigo_postal")

estatistica_descritiva <- estatistica_descritiva %>% left_join(ensino, by = "Codigo_postal")

estatistica_descritiva <- estatistica_descritiva %>% left_join(popresidente, by = "Codigo_postal")

estatistica_descritiva <- estatistica_descritiva %>% left_join(arcondicionado, by = "Codigo_postal")

estatistica_descritiva$populacao_com_ar_condicionado_percentagem <- (estatistica_descritiva$populacao_com_ar_condiconado / estatistica_descritiva$populacao_residente) * 100

tabela_descritiva <- estatistica_descritiva %>%
  select(
    Codigo_postal,
    idade_media,
    ganho_medio,
    ensino_superior_percentagem,
    populacao_setor_primario_percentagem,
    populacao_setor_secundario_percentagem,
    populacao_setor_terciario_percentagem,
    populacao_com_ar_condicionado_percentagem) %>%
  summarise(across(where(is.numeric), list(
    N = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    p25 = ~quantile(., 0.25, na.rm = TRUE),
    p75 = ~quantile(., 0.75, na.rm = TRUE),
    max = ~max(., na.rm = TRUE)),
    .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = everything(), names_to = c("Variavel", ".value"), names_sep = "_(?=[^_]+$)")

write.xlsx(tabela_descritiva, "Estatística descritiva das variáveis socioeconómicas.xlsx", rowNames = FALSE)

##Graphs

library(ggplot2)

Sys.setlocale("LC_TIME", "English")

###Monthly average consumption and temperature and number of rural and urban areas

pdf("Gráficos consumo e temperatura médias mensais.pdf")

grafico1 <- consumos %>%
  mutate(mes = floor_date(Data_Hora, unit = "month")) %>%
  group_by(mes) %>%
  summarise(consumo_medio = mean(Energia_ativa_kwh, na.rm = TRUE)) %>%
  ggplot(aes(x = mes, y = consumo_medio)) +
  geom_col() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Monthly average electrical energy consumption", x = "Month", y = "Average consumption (kWh)") +
  geom_text(aes(label = round(consumo_medio, 0)), vjust = -0.3) +
  theme_minimal()

grafico2 <- consumos %>%
  mutate(mes = floor_date(Data_Hora, unit = "month")) %>%
  group_by(mes) %>%
  summarise(temperatura_media = mean(Temperatura, na.rm = TRUE)) %>%
  ggplot(aes(x = mes, y = temperatura_media)) +
  geom_col() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Monthly average temperature", x = "Month", y = "Temperature (ºC)") +
  geom_text(aes(label = round(temperatura_media, 0)), vjust = -0.3) +
  theme_minimal()

grafico3 <- ggplot(consumos %>% distinct(Codigo_postal, Tipo_area), aes(x = Tipo_area)) +
  geom_bar() +
  labs(title = "Number of rural and urban areas", x = "Area type", y = "Number of postcodes") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  theme_minimal()

print(grafico1)
print(grafico2)
print(grafico3)

dev.off()

###Relationship between temperature and consumption in each postcode

library(patchwork)

cps <- unique(consumos$Codigo_postal)

criar_grafico <- function(cp) {
  df_cp <- consumos %>%
    filter(Codigo_postal == cp) %>%
    mutate(date = as.Date(Data_Hora)) %>%
    group_by(date) %>%
    summarise(
      temp_med = mean(Temperatura, na.rm = TRUE),
      consumo_med = mean(Energia_ativa_kwh, na.rm = TRUE),
      .groups = "drop")
  sf <- max(df_cp$consumo_med, na.rm = TRUE) / max(df_cp$temp_med, na.rm = TRUE)
  ggplot(df_cp, aes(x = date)) +
    geom_line(aes(y = consumo_med, color = "Consumption"), linewidth = 0.7) +
    geom_line(aes(y = temp_med * sf, color = "Temperature"), linewidth = 0.7) +
    scale_y_continuous(name = "Consumption (kWh)", sec.axis = sec_axis(~ . / sf, name = "Temperature (ºC)")) +
    scale_color_manual(values = c("Consumption" = "blue", "Temperature" = "red")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = paste("Daily evolution of consumption and temperature in postcode:", cp), x = "Days", color = "") +
    theme_minimal()}

lista_graficos <- lapply(cps, criar_grafico)

pdf("Gráficos relação entre temperatura e consumo por código postal.pdf", width = 12, height = 8)

for (i in seq(1, length(lista_graficos), by = 4)) {
  grupo <- lista_graficos[i:min(i + 3, length(lista_graficos))]
  painel <- wrap_plots(grupo, ncol = 2, nrow = 2) + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  print(painel)}

dev.off()

#Models:
##Main model

modelo_temp <- consumos %>%
  group_by(Codigo_postal) %>%
  summarise(modelos = list(lm(log(Energia_ativa_kwh) ~ Temperatura_Kelvin + I(Temperatura_Kelvin^2), data = cur_data())))

tabela_resultados <- map_dfr(seq_len(nrow(modelo_temp)), function(i) {
  cp <- modelo_temp$Codigo_postal[i]
  modelo <- modelo_temp$modelos[[i]]
  coef <- summary(modelo)$coefficients
  beta1 <- coef["Temperatura_Kelvin", ]
  sig_beta1 <- case_when(
    beta1["Pr(>|t|)"] < 0.001 ~ "***",
    beta1["Pr(>|t|)"] < 0.01  ~ "**",
    beta1["Pr(>|t|)"] < 0.05  ~ "*",
    beta1["Pr(>|t|)"] < 0.10  ~ ".",
    TRUE ~ "")
  beta2 <- coef["I(Temperatura_Kelvin^2)", ]
  sig_beta2 <- case_when(
    beta2["Pr(>|t|)"] < 0.001 ~ "***",
    beta2["Pr(>|t|)"] < 0.01  ~ "**",
    beta2["Pr(>|t|)"] < 0.05  ~ "*",
    beta2["Pr(>|t|)"] < 0.10  ~ ".",
    TRUE ~ "")
  data.frame(
    Codigo_postal = cp,
    beta1 = beta1["Estimate"],
    p_beta1 = beta1["Pr(>|t|)"],
    sig_beta1,
    beta2 = beta2["Estimate"],
    p_beta2 = beta2["Pr(>|t|)"],
    sig_beta2)})

write.xlsx(tabela_resultados, "Resultados do modelo para cada código postal.xlsx", rowNames = FALSE)

###Análise dos códigos 4780 e 5400

modelo_temp2 <- consumos %>%
  filter(Codigo_postal %in% c(4780, 5400)) %>%
  group_by(Codigo_postal) %>%
  summarise(modelos = list(lm(log(Energia_ativa_kwh) ~ Temperatura_Kelvin, data = cur_data())))

library(modelsummary)

sink("Modelo Temperatura simples.txt")

cat("Modelo: (Energia_ativa_kwh) ~ Temperatura_Kelvin")

for(i in 1:nrow(modelo_temp2)){
  cat("\n\n========================\n")
  cat("Codigo Postal:", as.character(modelo_temp2$Codigo_postal[i]), "\n")
  cat("========================\n\n")
  print(summary(modelo_temp2$modelos[[i]]))}

sink()

#U curve

cps <- cps[!cps %in% c(4780, 5400)]

criar_grafico_u <- function(cp) {
  df_cp <- consumos %>%
    filter(Codigo_postal == cp)
  modelo <- lm(log(Energia_ativa_kwh) ~ Temperatura_Kelvin + I(Temperatura_Kelvin^2), data = df_cp)
  df_cp$previsto <- predict(modelo, newdata = df_cp)
  ggplot(df_cp, aes(x = Temperatura_Kelvin)) +
    geom_point(aes(y = log(Energia_ativa_kwh)), alpha = 0.30, colour = "steelblue") +
    geom_line(aes(y = previsto), colour = "red", linewidth = 1.2) +
    labs(title = paste("U curve of the postcode:", cp), x = "Temperature (K)", y = "log(Consumption)") +
    theme_minimal()}

lista_graficos_u <- lapply(cps, criar_grafico_u)

pdf("Curvas em U por código postal.pdf", width = 12, height = 8)

for (i in seq(1, length(lista_graficos_u), by = 4)) {
  grupo <- lista_graficos_u[i:min(i + 3, length(lista_graficos_u))]
  painel <- wrap_plots(grupo, ncol = 2, nrow = 2)
  print(painel)}

dev.off()

#Temperature values to which consumption is lower

temp_min <- modelo_temp %>%
  mutate(beta1 = map_dbl(modelos, ~ coef(.x)["Temperatura_Kelvin"]),
         beta2 = map_dbl(modelos, ~ coef(.x)["I(Temperatura_Kelvin^2)"])) %>%
  mutate(
    temp_min_kelvin = -beta1/(2*beta2),
    temp_min_celsius = temp_min_kelvin - 273.15) %>%
  select(
    Codigo_postal,
    temp_min_celsius)

#Socioeconomic variables analysis
##Adding the variables

analise_vars_socio <- tabela_resultados %>% filter(!Codigo_postal %in% c(4780, 5400))

analise_vars_socio <- analise_vars_socio %>% left_join(tipoareas %>% select(Codigo_postal, Tipo_area), by = "Codigo_postal")

popprimario_pct <- caracteristicas %>%
  group_by(Codigo_postal) %>%
  summarise(populacao_setor_primario_percentagem = 100 * sum(Populacao_empregada_Setor_primário, na.rm = TRUE) / sum(Populacao_empregada_Total, na.rm = TRUE))
analise_vars_socio <- analise_vars_socio %>% left_join(popprimario_pct, by = "Codigo_postal")

popsecundario_pct <- caracteristicas %>%
  group_by(Codigo_postal) %>%
  summarise(populacao_setor_secundario_percentagem = 100 * sum(Populacao_empregada_Setor_secundário, na.rm = TRUE) / sum(Populacao_empregada_Total, na.rm = TRUE))
analise_vars_socio <- analise_vars_socio %>% left_join(popsecundario_pct, by = "Codigo_postal")

popterciario_pct <- caracteristicas %>%
  group_by(Codigo_postal) %>%
  summarise(populacao_setor_terciario_percentagem = 100 * sum(Populacao_empregada_Setor_terciário, na.rm = TRUE) / sum(Populacao_empregada_Total, na.rm = TRUE))
analise_vars_socio <- analise_vars_socio %>% left_join(popterciario_pct, by = "Codigo_postal")

analise_vars_socio <- analise_vars_socio %>% left_join(idademedia, by = "Codigo_postal")

analise_vars_socio <- analise_vars_socio %>% left_join(ganhomedio, by = "Codigo_postal")

analise_vars_socio <- analise_vars_socio %>% left_join(ensino, by = "Codigo_postal")

pop_ar_condicionados_pct <- caracteristicas %>%
  group_by(Codigo_postal) %>%
  summarise(populacao_com_ar_condicionado_percentagem = 100 * sum(Populacao_residente_Com_ar_condicionado, na.rm = TRUE) / sum(Populacao_residente_Total, na.rm = TRUE))
analise_vars_socio <- analise_vars_socio %>% left_join(pop_ar_condicionados_pct, by = "Codigo_postal")

##Normal distribution of betas

shapiro.test(analise_vars_socio$beta2)

##Testing the variables

t.test(beta2 ~ Tipo_area, data = analise_vars_socio)

variaveis <- c(
  "populacao_setor_primario_percentagem",
  "populacao_setor_secundario_percentagem",
  "populacao_setor_terciario_percentagem",
  "idade_media",
  "ganho_medio",
  "ensino_superior_percentagem",
  "populacao_com_ar_condicionado_percentagem")

resultados <- lapply(variaveis, function(var){
  teste <- cor.test(analise_vars_socio$beta2, analise_vars_socio[[var]], method = "spearman")
  data.frame(Variavel = var, Rho = unname(teste$estimate), p_value = teste$p.value)})

resultados <- do.call(rbind, resultados)

write.xlsx(resultados, "Resultados_Spearman.xlsx", rowNames = FALSE)