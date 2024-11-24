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
# 2. Tabelas agrupadas
# -------------------------

# Tabela de eleitores por sexo
tabela_sexo <- df_filtrado %>%
  group_by(DS_GENERO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# Tabela de eleitores por município
tabela_municipio <- df_filtrado %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# Tabela de eleitores por faixa etária
tabela_faixa_etaria <- df_filtrado %>%
  group_by(DS_FAIXA_ETARIA) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# -------------------------
# 3. Estatísticas Básicas para cada tabela
# -------------------------

# Função para calcular a moda
calcular_moda <- function(vetor) {
  unique_vetor <- unique(vetor)
  unique_vetor[which.max(tabulate(match(vetor, unique_vetor)))]
}

# Estatísticas para eleitores por sexo
estatisticas_sexo <- tabela_sexo %>%
  group_by(DS_GENERO) %>%
  summarise(
    Media = mean(Total_Eleitores),
    Mediana = median(Total_Eleitores),
    Moda = calcular_moda(Total_Eleitores),
    Maximo = max(Total_Eleitores),
    Minimo = min(Total_Eleitores),
    Desvio_Padrao = ifelse(n() > 1, sd(Total_Eleitores, na.rm = TRUE), 0),
    Variancia = ifelse(n() > 1, var(Total_Eleitores, na.rm = TRUE), 0)
  )

# Estatísticas para eleitores por município
estatisticas_municipio <- tabela_municipio %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(
    Media = mean(Total_Eleitores),
    Mediana = median(Total_Eleitores),
    Moda = calcular_moda(Total_Eleitores),
    Maximo = max(Total_Eleitores),
    Minimo = min(Total_Eleitores),
    Desvio_Padrao = ifelse(n() > 1, sd(Total_Eleitores, na.rm = TRUE), 0),
    Variancia = ifelse(n() > 1, var(Total_Eleitores, na.rm = TRUE), 0)
  )

# Estatísticas para eleitores por faixa etária
estatisticas_faixa_etaria <- tabela_faixa_etaria %>%
  group_by(DS_FAIXA_ETARIA) %>%
  summarise(
    Media = mean(Total_Eleitores),
    Mediana = median(Total_Eleitores),
    Moda = calcular_moda(Total_Eleitores),
    Maximo = max(Total_Eleitores),
    Minimo = min(Total_Eleitores),
    Desvio_Padrao = ifelse(n() > 1, sd(Total_Eleitores, na.rm = TRUE), 0),
    Variancia = ifelse(n() > 1, var(Total_Eleitores, na.rm = TRUE), 0)
  )

# -------------------------
# 4. Exibir Resultados
# -------------------------
cat("### Estatísticas: Eleitores por Sexo ###\n")
print(estatisticas_sexo)

cat("\n### Estatísticas: Eleitores por Município ###\n")
print(estatisticas_municipio)

cat("\n### Estatísticas: Eleitores por Faixa Etária ###\n")
print(estatisticas_faixa_etaria)
