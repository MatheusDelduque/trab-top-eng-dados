# Carregar o pacote Tidyverse
library(tidyverse)

carregar_dataset <- function(file_path) {
  read_delim(
    file_path,
    delim = ",",         # Ajuste o delimitador, se necessário
    locale = locale(encoding = "ISO-8859-1") # Encoding para arquivos brasileiros
  )
} 

# Função para reduzir o dataset às colunas de interesse
reduzir_dataset <- function(df) {
  df %>%
    select(
      Data,
      Hora_UTC,
      TEMPERATURA_DO_AR_BULBO_SECO,
      TEMPERATURA_MAXIMA_NA_HORA_ANT,
      TEMPERATURA_MINIMA_NA_HORA_ANT
    )
}

# Função para filtrar linhas inválidas
filtrar_invalidos <- function(df) {
  df %>%
    filter(
      TEMPERATURA_DO_AR_BULBO_SECO != " ",
      TEMPERATURA_MAXIMA_NA_HORA_ANT != " ",
      TEMPERATURA_MINIMA_NA_HORA_ANT != " ",
      TEMPERATURA_MINIMA_NA_HORA_ANT != "-"
    )
}

# Função para filtrar por critérios específicos


filtrar_criterios_trimestre <- function(df) {
   dados_janeiro <- df %>%
      filter(month(Data) %in% c(1, 2, 3) )
    print(dados_janeiro)
}
filtrar_criterios_horario <- function(df) {
  dados_janeiro <- df %>%
    filter(Hora_UTC == "1000 UTC")  
  print(dados_janeiro)
}
filtrar_temperatura_max <- function(df){
  dados_janeiro <- df %>%
    slice_max(order_by = TEMPERATURA_MAXIMA_NA_HORA_ANT, n = 1)  # Seleciona a maior temperatura
  print(dados_janeiro)
}
filtrar_temperatura_min <- function(df){
  dados_janeiro <- df %>%
    slice_max(order_by = TEMPERATURA_MINIMA_NA_HORA_ANT, n = 1)  # Seleciona a maior temperatura
  print(dados_janeiro)
}
filtrar_temperatura_med <- function(df){
  # Calcula a média
  media_temperatura <- mean(df$TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE)
  print(media_temperatura)
}
