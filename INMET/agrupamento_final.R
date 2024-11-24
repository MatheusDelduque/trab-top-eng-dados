library(lubridate)
library(dplyr)
library(ggplot2)
# Carregar as funções do arquivo funcoes_dataset.R
source("INMET/funcoes_dataset.R")

#--------------------------
# Caminho do arquivo
# -------------------------
file_path_rio_de_janeiro <- "csvs/INMET_SE_RJ_A621_RIO DE JANEIRO - VILA MILITAR_01-01-2023_A_31-12-2023.CSV"
file_path_sao_paulo <- "csvs/INMET_SE_SP_A701_SAO PAULO - MIRANTE_01-01-2023_A_31-12-2023.CSV"

# -------------------------
# 1. Lista de arquivos e cidades
# -------------------------
arquivos <- list(
  rio_de_janeiro = file_path_rio_de_janeiro,
  sao_paulo = file_path_sao_paulo
)

# Lista para armazenar os resultados
resultados <- list()

# -------------------------
# Função para criar agrupamentos temporais
# -------------------------
# Função para criar agrupamentos temporais para múltiplas colunas
agrupar_temporais <- function(df,cidade) {
  
  # Filtrar valores anômalos (caso necessário)
  df <- df %>%
    filter(TEMPERATURA_DO_AR_BULBO_SECO < 60, 
           TEMPERATURA_MAXIMA_NA_HORA_ANT < 60, 
           TEMPERATURA_MINIMA_NA_HORA_ANT < 60)
  df<- df %>%
    mutate(
      TEMPERATURA_DO_AR_BULBO_SECO = as.numeric(TEMPERATURA_DO_AR_BULBO_SECO),
      TEMPERATURA_MAXIMA_NA_HORA_ANT = as.numeric(TEMPERATURA_MAXIMA_NA_HORA_ANT),
      TEMPERATURA_MINIMA_NA_HORA_ANT = as.numeric(TEMPERATURA_MINIMA_NA_HORA_ANT)
    )
  
  # Agrupamento horário (para cada coluna de temperatura)
  horario <- df %>%
    mutate(Hora = as.numeric(substr(Hora_UTC, 1, 2))) %>%
    group_by(Hora) %>%
    summarise(
      Media_Temperatura_Bulbo_Seco = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE),
      Media_Temperatura_Maxima_Anterior = mean(TEMPERATURA_MAXIMA_NA_HORA_ANT, na.rm = TRUE),
      Media_Temperatura_Minima_Anterior = mean(TEMPERATURA_MINIMA_NA_HORA_ANT, na.rm = TRUE)
    )
  
  # Agrupamento diário (para cada coluna de temperatura)
  diario <- df %>%
    mutate(Hora = as.numeric(substr(Hora_UTC, 1, 2))) %>%
    group_by(Data) %>%
    summarise(
      Media_Temperatura_Bulbo_Seco = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE),
      Media_Temperatura_Maxima_Anterior = mean(TEMPERATURA_MAXIMA_NA_HORA_ANT, na.rm = TRUE),
      Media_Temperatura_Minima_Anterior = mean(TEMPERATURA_MINIMA_NA_HORA_ANT, na.rm = TRUE)
    )
  # Agrupamento semanal (para cada coluna de temperatura)
  semanal <- df %>%
    group_by(Semana = floor_date(Data, unit = "week")) %>%
    summarise(
      Media_Temperatura_Bulbo_Seco = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE),
      Media_Temperatura_Maxima_Anterior = mean(TEMPERATURA_MAXIMA_NA_HORA_ANT, na.rm = TRUE),
      Media_Temperatura_Minima_Anterior = mean(TEMPERATURA_MINIMA_NA_HORA_ANT, na.rm = TRUE)
    )
  
  # Agrupamento mensal (para cada coluna de temperatura)
  mensal <- df %>%
    group_by(Mes = floor_date(Data, unit = "month")) %>%
    summarise(
      Media_Temperatura_Bulbo_Seco = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE),
      Media_Temperatura_Maxima_Anterior = mean(TEMPERATURA_MAXIMA_NA_HORA_ANT, na.rm = TRUE),
      Media_Temperatura_Minima_Anterior = mean(TEMPERATURA_MINIMA_NA_HORA_ANT, na.rm = TRUE)
    )
  grafico <- ggplot(mensal, aes(x = Mes, y = Media_Temperatura_Bulbo_Seco)) +
    geom_col(fill = "blue", color = "blue") + 
    labs(
      title = paste("Temperatura do Bulbo Seco ao Longo do Tempo",cidade),
      x = "Data",
      y = "Temperatura do Bulbo Seco (°C)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Ajuste do texto no eixo X
    )
  print(grafico)
  
  
  # Agrupamento bimestral (para cada coluna de temperatura)
  bimestral <- df %>%
    mutate(Bimestre = (month(Data) + 1) %/% 2) %>%
    group_by(Bimestre, Ano = year(Data)) %>%
    summarise(
      Media_Temperatura_Bulbo_Seco = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE),
      Media_Temperatura_Maxima_Anterior = mean(TEMPERATURA_MAXIMA_NA_HORA_ANT, na.rm = TRUE),
      Media_Temperatura_Minima_Anterior = mean(TEMPERATURA_MINIMA_NA_HORA_ANT, na.rm = TRUE)
    )
  
  # Agrupamento trimestral (para cada coluna de temperatura)
  trimestral <- df %>%
    mutate(Trimestre = (month(Data) - 1) %/% 3 + 1) %>%
    group_by(Trimestre, Ano = year(Data)) %>%
    summarise(
      Media_Temperatura_Bulbo_Seco = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE),
      Media_Temperatura_Maxima_Anterior = mean(TEMPERATURA_MAXIMA_NA_HORA_ANT, na.rm = TRUE),
      Media_Temperatura_Minima_Anterior = mean(TEMPERATURA_MINIMA_NA_HORA_ANT, na.rm = TRUE)
    )
  
  # Agrupamento semestral (para cada coluna de temperatura)
  semestral <- df %>%
    mutate(Semestre = (month(Data) - 1) %/% 6 + 1) %>%
    group_by(Semestre, Ano = year(Data)) %>%
    summarise(
      Media_Temperatura_Bulbo_Seco = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE),
      Media_Temperatura_Maxima_Anterior = mean(TEMPERATURA_MAXIMA_NA_HORA_ANT, na.rm = TRUE),
      Media_Temperatura_Minima_Anterior = mean(TEMPERATURA_MINIMA_NA_HORA_ANT, na.rm = TRUE)
    )
  
  # Agrupamento anual (para cada coluna de temperatura)
  anual <- df %>%
    group_by(Ano = year(Data)) %>%
    summarise(
      Media_Temperatura_Bulbo_Seco = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE),
      Media_Temperatura_Maxima_Anterior = mean(TEMPERATURA_MAXIMA_NA_HORA_ANT, na.rm = TRUE),
      Media_Temperatura_Minima_Anterior = mean(TEMPERATURA_MINIMA_NA_HORA_ANT, na.rm = TRUE)
    )
  
  # Retorna uma lista com todas as tabelas
  list(
    horario = horario,
    diario = diario,
    semanal = semanal,
    mensal = mensal,
    bimestral = bimestral,
    trimestral = trimestral,
    semestral = semestral,
    anual = anual
  )
}

# -------------------------
# Exemplo no loop
# -------------------------
for (cidade in names(arquivos)) {
  # Carrega e processa o dataset
  file_path <- arquivos[[cidade]]
  df_inicial <- carregar_dataset(file_path)
  df_reduzido <- reduzir_dataset(df_inicial)
  df_filtrado <- filtrar_invalidos(df_reduzido)
  
  # Executa agrupamentos
  agrupamentos <- agrupar_temporais(df_filtrado, cidade)
  
  # Salva o resultado para a cidade atual
  resultados[[cidade]] <- agrupamentos
  
  cat("\n### Agrupamentos para", cidade, "###\n")
  for (nome_agrupamento in names(agrupamentos)) {
    cat("\n### Agrupamento:", nome_agrupamento, "###\n")
    print(agrupamentos[[nome_agrupamento]])
    # Transformando os dados para o formato longo
  }
}