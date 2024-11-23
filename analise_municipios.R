# Carregar as funções do arquivo funcoes_dataset.R
source("funcoes_dataset.R")

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
# 2. Agrupamentos e Cálculos
# -------------------------

# Total de eleitores por município, excluindo a capital
tabela_municipio <- df_filtrado %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  filter(NM_MUNICIPIO != "BOA VISTA") %>% # Substitua "CAPITAL" pelo nome real da capital, se necessário
  arrange(Total_Eleitores)

# Total de eleitores por município e gênero, incluindo a capital (opcionalmente exclua também)
tabela_municipio_genero <- df_filtrado %>%
  group_by(NM_MUNICIPIO, DS_GENERO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  filter(NM_MUNICIPIO != "BOA VISTA") %>% # Filtra a capital, se necessário
  arrange(desc(Total_Eleitores))

# Determinar o município com menos eleitores
municipio_menos_eleitores <- tabela_municipio %>%
  slice(1)

# Determinar o município com mais eleitores (após excluir a capital)
municipio_mais_eleitores <- tabela_municipio %>%
  slice_tail(n = 1) # Agora pega o maior município que não é a capital


# Faixa de variação da quantidade de eleitores por município
variacao_eleitores <- tabela_municipio %>%
  summarise(
    Maximo = max(Total_Eleitores),
    Minimo = min(Total_Eleitores),
    Diferenca = Maximo - Minimo
  )

# Análise de eleitores por gênero para o município com mais eleitores
municipio_top_genero <- tabela_municipio_genero %>%
  filter(NM_MUNICIPIO == municipio_mais_eleitores$NM_MUNICIPIO)

# -------------------------
# 3. Exibir Resultados
# -------------------------
cat("### Análise de Eleitores por Município ###\n\n")

cat("1. Município com Menos Eleitores:\n")
print(municipio_menos_eleitores)

cat("\n2. Município com Mais Eleitores (excluindo a capital):\n")
print(municipio_mais_eleitores)

cat("\n3. Faixa de Variação da Quantidade de Eleitores:\n")
print(variacao_eleitores)

cat("\n4. Análise por Gênero no Município com Mais Eleitores:\n")
print(municipio_top_genero)

cat("\n5. Total de Eleitores por Município e Gênero:\n")
print(tabela_municipio_genero)
