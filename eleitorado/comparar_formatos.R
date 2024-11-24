# Carregar as funções do arquivo funcoes_dataset.R
source("eleitorado/funcoes_dataset.R")

# -------------------------
# Caminho do arquivo
# -------------------------
file_path <- "csvs/perfil_eleitor_secao_ATUAL_RR.csv"

# -------------------------
# 1. Carregar o dataset inicial
# -------------------------
df_inicial <- carregar_dataset(file_path)
tamanho_inicial <- dim(df_inicial) # Dimensões do dataset inicial
num_celulas_inicial <- sum(!is.na(df_inicial)) # Células com dados no dataset inicial

# -------------------------
# 2. Reduzir o dataset às colunas requeridas
# -------------------------
df_reduzido <- reduzir_dataset(df_inicial)
tamanho_reduzido <- dim(df_reduzido) # Dimensões do dataset reduzido
num_celulas_reduzidas <- sum(!is.na(df_reduzido)) # Células com dados no dataset reduzido

# -------------------------
# 3. Filtrar linhas inválidas
# -------------------------
df_filtrado <- filtrar_invalidos(df_reduzido)
tamanho_filtrado <- dim(df_filtrado) # Dimensões do dataset filtrado
num_celulas_filtradas <- sum(!is.na(df_filtrado)) # Células com dados no dataset filtrado

# -------------------------
# Comparar os três formatos
# -------------------------
cat("### Comparação dos três formatos ###\n\n")

cat("1. Dataset Inicial:\n")
cat("- Tamanho (linhas, colunas):", tamanho_inicial, "\n")
cat("- Número de células com dados:", num_celulas_inicial, "\n\n")

cat("2. Dataset Reduzido (somente colunas requeridas):\n")
cat("- Tamanho (linhas, colunas):", tamanho_reduzido, "\n")
cat("- Número de células com dados:", num_celulas_reduzidas, "\n\n")

cat("3. Dataset Filtrado (após remoção de linhas inválidas):\n")
cat("- Tamanho (linhas, colunas):", tamanho_filtrado, "\n")
cat("- Número de células com dados:", num_celulas_filtradas, "\n")