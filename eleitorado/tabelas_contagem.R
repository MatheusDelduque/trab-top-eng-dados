# Carregar as funções do arquivo funcoes_dataset.R
source("eleitorado/funcoes_dataset.R")

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
# 2. Tabelas de Contagem
# -------------------------

# Contagem de eleitores por sexo
tabela_sexo <- df_filtrado %>%
  group_by(DS_GENERO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  arrange(desc(Total_Eleitores))

# Contagem de eleitores por município
tabela_municipio <- df_filtrado %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  arrange(desc(Total_Eleitores))

# Contagem de eleitores por faixa etária
tabela_faixa_etaria <- df_filtrado %>%
  group_by(DS_FAIXA_ETARIA) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  arrange(desc(Total_Eleitores))

# Contagem combinada: Sexo x Faixa Etária
tabela_sexo_faixa <- df_filtrado %>%
  group_by(DS_GENERO, DS_FAIXA_ETARIA) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  arrange(desc(Total_Eleitores))

# Contagem combinada: Município x Gênero
tabela_municipio_sexo <- df_filtrado %>%
  group_by(NM_MUNICIPIO, DS_GENERO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  arrange(desc(Total_Eleitores))

# -------------------------
# 3. Exibir as Tabelas
# -------------------------
cat("### Tabela: Eleitores por Sexo ###\n")
print(tabela_sexo)

cat("\n### Tabela: Eleitores por Município ###\n")
print(tabela_municipio)

cat("\n### Tabela: Eleitores por Faixa Etária ###\n")
print(tabela_faixa_etaria)

cat("\n### Tabela: Eleitores por Sexo x Faixa Etária ###\n")
print(tabela_sexo_faixa)

cat("\n### Tabela: Eleitores por Município x Gênero ###\n")
print(tabela_municipio_sexo)
