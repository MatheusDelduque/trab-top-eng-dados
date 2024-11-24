# Carregar as funções do arquivo funcoes_dataset.R
source("INMET/funcoes_dataset.R")
# 
# # -------------------------
# # Caminho do arquivo
# # -------------------------
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
 # 2. Loop para processar cada arquivo
 # -------------------------
 for (cidade in names(arquivos)) {
   # Carrega e processa o dataset
   file_path <- arquivos[[cidade]]
   df_inicial <- carregar_dataset(file_path)
   df_reduzido <- reduzir_dataset(df_inicial)
   df_filtrado <- filtrar_invalidos(df_reduzido)
   
   # Garantir que as colunas de temperatura sejam numéricas
   df_filtrado <- df_filtrado %>%
     mutate(
       TEMPERATURA_DO_AR_BULBO_SECO = as.numeric(TEMPERATURA_DO_AR_BULBO_SECO),
       TEMPERATURA_MAXIMA_NA_HORA_ANT = as.numeric(TEMPERATURA_MAXIMA_NA_HORA_ANT),
       TEMPERATURA_MINIMA_NA_HORA_ANT = as.numeric(TEMPERATURA_MINIMA_NA_HORA_ANT)
     )
   
   # Tabelas agrupadas (médias por data)
   tabela_ponto <- df_filtrado %>%
     group_by(Data) %>%
     summarise(Total = mean(TEMPERATURA_DO_AR_BULBO_SECO, na.rm = TRUE))
   
   tabela_ponto_max <- df_filtrado %>%
     group_by(Data) %>%
     summarise(Total = mean(TEMPERATURA_MAXIMA_NA_HORA_ANT, na.rm = TRUE))
   
   tabela_ponto_min <- df_filtrado %>%
     group_by(Data) %>%
     summarise(Total = mean(TEMPERATURA_MINIMA_NA_HORA_ANT, na.rm = TRUE))
   
   # Estatísticas básicas para cada tabela
   estatisticas_ponto <- tabela_ponto %>%
     summarise(
       Media = mean(Total, na.rm = TRUE),
       Mediana = median(Total, na.rm = TRUE),
       Moda = calcular_moda(Total),
       Maximo = max(Total, na.rm = TRUE),
       Minimo = min(Total, na.rm = TRUE),
       Desvio_Padrao = sd(Total, na.rm = TRUE),
       Variancia = var(Total, na.rm = TRUE)
     )
   
   estatisticas_ponto_max <- tabela_ponto_max %>%
     summarise(
       Media = mean(Total, na.rm = TRUE),
       Mediana = median(Total, na.rm = TRUE),
       Moda = calcular_moda(Total),
       Maximo = max(Total, na.rm = TRUE),
       Minimo = min(Total, na.rm = TRUE),
       Desvio_Padrao = sd(Total, na.rm = TRUE),
       Variancia = var(Total, na.rm = TRUE)
     )
   
   estatisticas_ponto_min <- tabela_ponto_min %>%
     summarise(
       Media = mean(Total, na.rm = TRUE),
       Mediana = median(Total, na.rm = TRUE),
       Moda = calcular_moda(Total),
       Maximo = max(Total, na.rm = TRUE),
       Minimo = min(Total, na.rm = TRUE),
       Desvio_Padrao = sd(Total, na.rm = TRUE),
       Variancia = var(Total, na.rm = TRUE)
     )
   
   # Salva os resultados em uma lista
   resultados[[cidade]] <- list(
     estatisticas_ponto = estatisticas_ponto,
     estatisticas_ponto_max = estatisticas_ponto_max,
     estatisticas_ponto_min = estatisticas_ponto_min
   )
 }
 
 # -------------------------
 # 3. Exibir os resultados
 # -------------------------
 for (cidade in names(resultados)) {
   cat("### Estatísticas para:", cidade, "###\n")
   
   cat("\n--- Temperatura ambiente ---\n")
   print(resultados[[cidade]]$estatisticas_ponto)
   
   cat("\n--- Temperatura máxima ---\n")
   print(resultados[[cidade]]$estatisticas_ponto_max)
   
   cat("\n--- Temperatura mínima ---\n")
   print(resultados[[cidade]]$estatisticas_ponto_min)
 }