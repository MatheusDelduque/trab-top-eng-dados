# Carregar as funções do arquivo funcoes_dataset.R
source("INMET/funcoes_dataset.R")
# editaaaaar
# -------------------------
# Caminho do arquivo
# -------------------------
file_path <- "csvs/perfil_eleitor_secao_ATUAL_RR.csv"

# -------------------------
# 1. Carregar e processar o dataset
# -------------------------
df_inicial <- carregar_dataset(file_path)     # Carrega o dataset completo
df_reduzido <- reduzir_dataset(df_inicial)    # Reduz às colunas requeridas
df_filtrado <- filtrar_invalidos(df_reduzido) # Remove linhas inválidas

# -------------------------
# 2. Agrupamento e Cálculo do Total de Eleitores
# -------------------------

# Agrupamento por município
tabela_municipio <- df_filtrado %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  arrange(desc(Total_Eleitores))

# Agrupamento por grau de instrução
tabela_grau_instrucao <- df_filtrado %>%
  group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  arrange(desc(Total_Eleitores))

# Agrupamento por município e grau de instrução
tabela_municipio_grau_instrucao <- df_filtrado %>%
  group_by(NM_MUNICIPIO, DS_GRAU_ESCOLARIDADE) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  arrange(desc(Total_Eleitores))

# -------------------------
# 3. Exibir Resultados
# -------------------------
cat("### Total de Eleitores por Município ###\n")
print(tabela_municipio)

cat("\n### Total de Eleitores por Grau de Instrução ###\n")
print(tabela_grau_instrucao)

cat("\n### Total de Eleitores por Município e Grau de Instrução ###\n")
print(tabela_municipio_grau_instrucao)