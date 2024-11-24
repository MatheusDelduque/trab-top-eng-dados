library(lubridate)
library(dplyr)
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
agrupar_temporais <- function(df) {
  
  
  # Agrupamento horário (já feito)
  horario <- df %>%
    mutate(Hora = as.numeric(substr(Hora_UTC, 1, 2))) %>%
    group_by(Data, Hora) %>%
    summarise(Media_Temperatura = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE))
  
  # Agrupamento diário - calcula a média das temperaturas horárias para cada dia
  diario <- horario %>%
    group_by(Data) %>%
    summarise(Media_Temperatura_Diaria = mean(Media_Temperatura, na.rm = TRUE))
  
  # Agrupamento mensal - calcula a média das médias diárias para cada mês
  mensal <- diario %>%
    mutate(Mes = floor_date(Data, "month")) %>%  # Cria uma coluna para o mês
    group_by(Mes) %>%  # Agrupar por mês
    summarise(Media_Temperatura_Mensal = mean(Media_Temperatura_Diaria, na.rm = TRUE))  # Média das médias diárias para cada mês
  
  # Exibir o resultado mensal
  print(diario)
  
  
  
  # # Agrupamento horário
  # horario <- df %>%
  #   mutate(Hora = as.numeric(substr(Hora_UTC, 1, 2))) %>%
  #   group_by(Hora) %>%
  #   summarise(Media_Temperatura = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE))
  # 
  # # Agrupamento diário
  # diario <- df %>%
  #   mutate(Hora = as.numeric(substr(Hora_UTC, 1, 2))) %>%
  #   group_by(Data,Hora) %>%
  #   summarise(Media_Temperatura_Diaria = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE))  # Média das médias horárias para cada dia
  # # Agrupamento semanal
  # semanal <- df %>%
  #   group_by(Semana = floor_date(Data, unit = "week")) %>%
  #   summarise(Media_Temperatura = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE))
  # 
  # # Agrupamento mensal
  # mensal <- df %>%
  #   group_by(Mes = floor_date(Data, unit = "month")) %>%
  #   summarise(Media_Temperatura = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE))
  # 
  # # Agrupamento bimestral
  # bimestral <- df %>%
  #   mutate(Bimestre = (month(Data) + 1) %/% 2) %>%
  #   group_by(Bimestre, Ano = year(Data)) %>%
  #   summarise(Media_Temperatura = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE))
  # 
  # # Agrupamento trimestral
  # trimestral <- df %>%
  #   mutate(Trimestre = (month(Data) - 1) %/% 3 + 1) %>%
  #   group_by(Trimestre, Ano = year(Data)) %>%
  #   summarise(Media_Temperatura = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE))
  # 
  # # Agrupamento semestral
  # semestral <- df %>%
  #   mutate(Semestre = (month(Data) - 1) %/% 6 + 1) %>%
  #   group_by(Semestre, Ano = year(Data)) %>%
  #   summarise(Media_Temperatura = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE))
  # 
  # # Agrupamento anual
  # anual <- df %>%
  #   group_by(Ano = year(Data)) %>%
  #   summarise(Media_Temperatura = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE))
  # 
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
  agrupamentos <- agrupar_temporais(df_filtrado)
  
  # Salva o resultado para a cidade atual
  resultados[[cidade]] <- agrupamentos
  
  cat("\n### Agrupamentos para", cidade, "###\n")
  for (nome_agrupamento in names(agrupamentos)) {
    cat("\n### Agrupamento:", nome_agrupamento, "###\n")
    print(agrupamentos[[nome_agrupamento]])
  }
}