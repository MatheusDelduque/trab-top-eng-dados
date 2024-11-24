library(dplyr)
library(lubridate)

# -------------------------
# Caminhos dos arquivos
# -------------------------
file_path_rio_de_janeiro_2023 <- "csvs/INMET_SE_RJ_A621_RIO DE JANEIRO - VILA MILITAR_01-01-2023_A_31-12-2023.CSV"
file_path_rio_de_janeiro_2022 <- "csvs/INMET_SE_RJ_A621_RIO DE JANEIRO - VILA MILITAR_01-01-2022_A_31-12-2022.CSV"
file_path_sao_paulo_2023 <- "csvs/INMET_SE_SP_A701_SAO PAULO - MIRANTE_01-01-2023_A_31-12-2023.CSV"
file_path_sao_paulo_2022 <- "csvs/INMET_SE_SP_A701_SAO PAULO - MIRANTE_01-01-2022_A_31-12-2022.CSV"

# Lista de arquivos com cidades e anos
arquivos <- list(
  RJ_2022 = file_path_rio_de_janeiro_2022,
  RJ_2023 = file_path_rio_de_janeiro_2023,
  SP_2022 = file_path_sao_paulo_2022,
  SP_2023 = file_path_sao_paulo_2023
)

# Lista para armazenar os resultados
resultados <- list()


# -------------------------
# Função para processar os dados
# Função para carregar e processar o dataset (supondo que já está definida)
calcular_media_anual <- function(file_path) {
  # Carregar o dataset
  df_inicial <- carregar_dataset(file_path)
  df_reduzido <- reduzir_dataset(df_inicial)
  df_filtrado <- filtrar_invalidos(df_reduzido)
  
  # Garantir que as colunas de temperatura são numéricas
  df_filtrado$TEMPERATURA_DO_AR_BULBO_SECO <- as.numeric(df_filtrado$TEMPERATURA_DO_AR_BULBO_SECO)
  df_filtrado$TEMPERATURA_MAXIMA_NA_HORA_ANT <- as.numeric(df_filtrado$TEMPERATURA_MAXIMA_NA_HORA_ANT)
  df_filtrado$TEMPERATURA_MINIMA_NA_HORA_ANT <- as.numeric(df_filtrado$TEMPERATURA_MINIMA_NA_HORA_ANT)
  
  # Tratar valores NA (se necessário, você pode substituir ou remover os NAs)
  df_filtrado <- df_filtrado %>%
    filter(!is.na(TEMPERATURA_DO_AR_BULBO_SECO) &
             !is.na(TEMPERATURA_MAXIMA_NA_HORA_ANT) &
             !is.na(TEMPERATURA_MINIMA_NA_HORA_ANT))
  
  # Calcular médias anuais
  medias_anuais <- df_filtrado %>%
    summarise(
      Media_Temp_Ponto_Orvalho = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE),
      Media_Temp_Max = mean(TEMPERATURA_MAXIMA_NA_HORA_ANT, na.rm = TRUE),
      Media_Temp_Min = mean(TEMPERATURA_MINIMA_NA_HORA_ANT, na.rm = TRUE)
    )
  
  return(medias_anuais)
}

# -------------------------
# Processar os arquivos
# -------------------------
arquivos <- list(
  RJ_2022 = file_path_rio_de_janeiro_2022,
  RJ_2023 = file_path_rio_de_janeiro_2023,
  SP_2022 = file_path_sao_paulo_2022,
  SP_2023 = file_path_sao_paulo_2023
)

resultados <- list()

for (cidade_ano in names(arquivos)) {
  file_path <- arquivos[[cidade_ano]]
  resultados[[cidade_ano]] <- calcular_media_anual(file_path)
}

# Exibir os resultados
cat("\n### Médias Anuais de Temperaturas ###\n")
for (cidade_ano in names(resultados)) {
  cat("\n---", cidade_ano, "---\n")
  print(resultados[[cidade_ano]])
}