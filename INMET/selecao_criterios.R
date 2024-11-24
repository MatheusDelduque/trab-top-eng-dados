# Carregar as funções do arquivo funcoes_dataset.R
source("INMET/funcoes_dataset.R")

# -------------------------
# Caminho do arquivo
# -------------------------
file_path_rio_de_janeiro <- "csvs/INMET_SE_RJ_A621_RIO DE JANEIRO - VILA MILITAR_01-01-2023_A_31-12-2023.CSV"
file_path_sao_paulo <- "csvs/INMET_SE_SP_A701_SAO PAULO - MIRANTE_01-01-2023_A_31-12-2023.CSV"

# -------------------------
# 1. Carregar e processar o dataset
# -------------------------Rio de janeiro ----------------------------
df_inicial_rj <- carregar_dataset(file_path_rio_de_janeiro)     # Carrega o dataset completo
df_reduzido_rj <- reduzir_dataset(df_inicial_rj)    # Reduz às colunas requeridas
df_filtrado_rj <- filtrar_invalidos(df_reduzido_rj) # Remove linhas inválidas

# ------------------------- Sao paulo --------------------------------
df_inicial_sp <- carregar_dataset(file_path_sao_paulo)     # Carrega o dataset completo
df_reduzido_sp <- reduzir_dataset(df_inicial_sp)    # Reduz às colunas requeridas
df_filtrado_sp <- filtrar_invalidos(df_reduzido_sp) # Remove linhas inválidas


#--------------- definindo critérios ----------------------

#----------------Primeiro trimestre -----------------------
df_criterios_rj <- filtrar_criterios_trimestre(df_filtrado_rj)
df_criterios_sp <- filtrar_criterios_trimestre(df_filtrado_sp)

#----------------horario -----------------------
df_criterios_rj <- filtrar_criterios_horario(df_filtrado_rj)
df_criterios_sp <- filtrar_criterios_horario(df_filtrado_sp)
#----------------- temperatura max/min/med RJ ---------------
df_temperatura_max_rj <- filtrar_temperatura_max(df_filtrado_rj)
df_temperatura_min_rj <- filtrar_temperatura_min(df_filtrado_rj)
df_temperatura_med_rj <- filtrar_temperatura_med(df_filtrado_rj)

#----------------- temperatura max/min/med SP ---------------
df_temperatura_max_sp <- filtrar_temperatura_max(df_filtrado_sp)
df_temperatura_min_sp <- filtrar_temperatura_min(df_filtrado_sp)
df_temperatura_med_sp <- filtrar_temperatura_med(df_filtrado_sp)

#----------------- media/moda/mediana

