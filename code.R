# Bibliotecas
library(readr)
library(tidyverse)
library(ggplot2)
library(evd)
library(ADGofTest)
library(goftest)
library(POT)

# ---------------------------------------------------------------------------- #

convertTime = function(time) {
  timeParts = strsplit(time, ":")
  
  if (length(timeParts[[1]]) == 2) {  # Formato mm:ss.mm
    minute = as.numeric(timeParts[[1]][1])
    second = as.numeric(timeParts[[1]][2])
    return(minute * 60 + second)
  } else if (length(timeParts[[1]]) == 3) {  # Formato hh:mm:ss.mm
    minute = as.numeric(timeParts[[1]][2])
    second = as.numeric(timeParts[[1]][3])
    return(minute * 60 + second)
  } else {
    return("Formato de tempo inválido")
  }
}

convertTimeR = function(time) {
  timeParts = strsplit(time, ":")
  
  if (length(timeParts[[1]]) == 2) {  # Formato mm:ss.mm
    return(time)
  } else if (length(timeParts[[1]]) == 3) {  # Formato hh:mm:ss.mm
    minute = as.numeric(timeParts[[1]][2])
    second = as.numeric(timeParts[[1]][3])
    return(paste0(minute, ":", second))
  }
}

# ---------------------------------------------------------------------------- #

# Leitura dos dados
data = read_csv("Olympic_Swimming_Results_1912to2020.csv")

# Organizando os dados
data = data %>% 
  rename(Distance = `Distance (in meters)`) %>%                # Renomear a coluna 'Distance (in meters)' para 'Distance'
  filter(Distance == "800m", Gender == "Women") %>%            # Filtrar Distance="800m" e Gender="Women"
  mutate(Results = sapply(Results, convertTimeR)) %>%
  mutate(converted_Results = sapply(Results, convertTime)) %>% # Criar uma nova coluna 'converted_Results' aplicando a função 'convertTime' a cada elemento na coluna 'Results'
  select(-`Relay?`)                                            # Retirar a coluna 'Relay?'

# ---------------------------------------------------------------------------- #

### Contagem de registros por país (Team)
team_count = data %>% group_by(Team) %>% summarise(count = n()); team_count

### Os países mais frequentes
top_countries = team_count %>% arrange(desc(count)) %>% head(10); top_countries

# ---------------------------------------------------------------------------- #

# Criar uma coluna para representar a medalha
data$Medal = ifelse(data$Rank == 1, "Ouro", ifelse(data$Rank == 2, "Prata", ifelse(data$Rank == 3, "Bronze", "None")))

# Criar uma tabela resumida do pódio por país
podium_table = data %>%
  group_by(Team, Medal) %>%
  summarise(total_medals = n()) %>%
  pivot_wider(names_from = Medal, values_from = total_medals, values_fill = 0)

# Calcular o total de medalhas por país
podium_table$Total = podium_table$Ouro + podium_table$Prata + podium_table$Bronze

# Ordenar a tabela pelo total de medalhas
podium_table = podium_table %>% arrange(desc(Ouro), desc(Prata), desc(Bronze), desc(Total)) %>% select(Team, Ouro, Prata, Bronze, Total, None)

# Exibir a tabela
print(podium_table)

# ---------------------------------------------------------------------------- #

# Criar uma tabela resumida do pódio por atleta
athlete_table = data %>%
  group_by(Athlete, Medal, Team) %>%
  summarise(total_medals = n()) %>%
  pivot_wider(names_from = Medal, values_from = total_medals, values_fill = 0)

# Calcular o total de medalhas por atleta
athlete_table$Total = athlete_table$Ouro + athlete_table$Prata + athlete_table$Bronze

# Ordenar a tabela pelo total de medalhas
athlete_table = athlete_table %>% arrange(desc(Ouro), desc(Prata), desc(Bronze), desc(Total)) %>% select(Athlete, Team, Ouro, Prata, Bronze, Total, None)

# Exibir a tabela
print(athlete_table)

# ---------------------------------------------------------------------------- #

# Filtrar apenas os países com medalhas
medal_countries = subset(podium_table, Total > 0) %>% select(c(Team, Ouro, Prata, Bronze, Total))

# Reorganizar os dados para o formato longo (tidy data)
data_long = tidyr::pivot_longer(medal_countries, cols = c("Ouro", "Prata", "Bronze"))

# Criar um gráfico de barras do pódio por país (apenas países com medalha)
podium_plot = ggplot(data_long, aes(x = reorder(Team, -Total), y = value, fill = factor(name, levels = c("Ouro", "Prata", "Bronze")))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Ouro" = "#FFD700", "Prata" = "#C0C0C0", "Bronze" = "#CD7F32")) +
  labs(x = "País", y = "Total de Medalhas", fill = "Medalha") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Exibir o gráfico
print(podium_plot)

# ---------------------------------------------------------------------------- #

# Filtrar os dados para obter apenas as primeiras, segundas e terceiras colocações
top_results = data %>% filter(Rank %in% c(1, 2, 3))

# Criar um gráfico de linhas do tempo por ano (apenas tempos das três primeiras)
time_plot = ggplot(top_results, aes(x = Year, y = converted_Results, color = factor(Rank))) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = c(480, 500, 520, 540, 560, 580), linetype = "dashed", color = "gray") +
  scale_x_continuous(breaks = seq(min(top_results$Year), max(top_results$Year), by = 4)) +
  scale_y_continuous(breaks = seq(480, 580, by = 20), limits = c(480, 580)) +
  scale_color_manual(values = c("1" = "#FFD700", "2" = "#C0C0C0", "3" = "#CD7F32")) +
  labs(x = "Ano", y = "Tempo", color = "Colocação") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Exibir o gráfico
print(time_plot)

# ---------------------------------------------------------------------------- #

# Cria um histograma dos resultados convertidos
hist(data$converted_Results, xlab = "Tempo(em s)", ylab = "Frequência", main = "")

# Ajusta um modelo da distribuição generalizada de valores extremos aos dados
modelo = fgev(data$converted_Results)

# Obtém os parâmetros do modelo estimados
loc = modelo$estimate[1]
scale = modelo$estimate[2]
shape = modelo$estimate[3]

# Calcula os valores da distribuição generalizada de valores extremos para um intervalo específico
y = dgev(480:620, loc=loc, scale=scale, shape=shape)

# Cria um data histograma com limites específicos
hist(data$converted_Results, freq=FALSE, xlim=c(480,620), ylim=c(0,0.07), xlab = "Tempo(em s)", ylab = "Densidade", main = "")

# Plota o gráfico da distribuição generalizada de valores extremos sobre o histograma
par(new=T)
plot(480:620, y, type="l", xlim=c(480,620), ylim=c(0,0.07), xlab = "", ylab = "")
par(new=F)

# Realiza o teste Anderson-Darling de ajuste da distribuição generalizada de valores extremos
ad.test(data$converted_Results, pgev, loc=loc, scale=scale, shape=shape)

# Realiza o teste Kolmogorov-Smirnov de ajuste da distribuição generalizada de valores extremos
ks.test(data$converted_Results, pgev, loc=loc, scale=scale, shape=shape)

# Realiza o teste Cramér-von Mises de ajuste da distribuição generalizada de valores extremos
cvm.test(data$converted_Results, pgev, loc=loc, scale=scale, shape=shape)

# Encontra o valor mínimo nos resultados convertidos
min_val = min(data$converted_Results)

# Calcula a probabilidade de ocorrência do valor mínimo na distribuição
p = pgev(min_val, loc=loc, scale=scale, shape=shape)

# Imprime o resultado da probabilidade
print(p)

# ---------------------------------------------------------------------------- #

# Recorde Olímpico
# Encontra o valor mínimo nos resultados convertidos
or = min(data$converted_Results)

# Define o número de observações até o limiar (threshold)
D = 50

# Define a distância (800 metros)
distancia = 800

# Adiciona uma coluna 'count' ao DataFrame para contagem
data$count = 1:length(data[,1])

# Inicializa a contagem
data$count1 = 1

# Loop para calcular a contagem de resultados consecutivos iguais
for (i in 2:length(data[,1])) {
  if (data$converted_Results[i] == data$converted_Results[i-1])
    data$count1[i] = data$count1[i-1] + 1
}

# Ordena os dados pelas contagens decrescentes
contagens <- data[order(data$converted_Results, -data$count1),]
contagens$converted_Results1 <- NULL
contagens$athlete <- NULL
contagens$year <- NULL
contagens$count <- NULL

# Remove duplicatas nas contagens
contagens = contagens[!duplicated(contagens$converted_Results),]

# Mescla os dados originais com as contagens únicas
data1 = merge(data, contagens, by = "converted_Results")
data1$converted_Resultsok = 0

# Loop para calcular os resultados ajustados
for (i in 1:length(data1[,1])) {
  if (data1$count1.y[i] == 1) {
    data1$converted_Resultsok[i] = data1$converted_Results[i]
  } else {
    data1$converted_Resultsok[i] = data1$converted_Results[i] - 0.005 + 0.01 * (2 * data1$count1.x[i] - 1) / (2 * data1$count1.y[i])
  }
}

# Calcula os resultados ajustados em relação à distância
data1$converted_Resultsok1 = distancia / data1$converted_Resultsok

# Cria um histograma dos resultados ajustados
hist(data1$converted_Resultsok1, ylab = "Frequência relativa",
     xlab = "Velocidade (m/s)", main = "800 m livre feminino", freq = FALSE)

# Calcula o limiar (threshold)
thresh = data1$converted_Resultsok1[D] - 0.00005

# Ajusta a distribuição GPD aos dados acima do limiar
scale = fitgpd(data1$converted_Resultsok1, threshold = thresh, "mle")$fitted.values[1]
scale_sd = fitgpd(data1$converted_Resultsok1, threshold = thresh, "mle")$std.err[1]
shape = fitgpd(data1$converted_Resultsok1, threshold = thresh, "mle")$fitted.values[2]
shape_sd = fitgpd(data1$converted_Resultsok1, threshold = thresh, "mle")$std.err[2]

# Imprime os parâmetros estimados
scale
scale_sd
shape
shape_sd

# Calcula o tempo correspondente ao recorde olímpico
distancia / (thresh - scale / shape)

1-pgpd(distancia/or,loc=thresh,scale=scale,shape=shape)

# Calcula a distribuição GPD empírica
x = 1:10000
x = thresh + x / 10000
y = dgpd(x, loc = thresh, scale = scale, shape = shape)

# Cria um histograma dos dados até o limiar
hist(data1$converted_Resultsok1[1:D], freq = FALSE, ylim = c(0,40), xlim = c(1.56,1.66), ylab = "Frequência", xlab = "Tempo", main = "")
par(new = T)

# Plota a distribuição GPD empírica sobre o histograma
plot(x, y, type = "l", ylim = c(0,40), xlim = c(1.56,1.66), ylab = "", xlab = "", main = "")

# Restaura as configurações padrão do gráfico
par(new = F)
