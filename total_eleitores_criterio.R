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
# 2. Seleção de critérios específicos
# -------------------------
# Especificar os critérios de filtro
genero <- "MASCULINO"
faixa_etaria <- "35 a 39 anos"
estado_civil <- "CASADO"
grau_escolaridade <- "ENSINO MÉDIO INCOMPLETO"

# Filtrar as linhas que atendem aos critérios
df_criterios <- filtrar_criterios(df_filtrado, genero, faixa_etaria, estado_civil, grau_escolaridade)

# -------------------------
# 3. Cálculo de totais
# -------------------------
# Total geral de eleitores no dataset filtrado
total_geral <- sum(df_filtrado$QT_ELEITORES_PERFIL, na.rm = TRUE)

# Total de eleitores que atendem aos critérios
total_criterios <- sum(df_criterios$QT_ELEITORES_PERFIL, na.rm = TRUE)

# -------------------------
# Exibir resultados
# -------------------------
cat("### Resultados da Seleção ###\n\n")

cat("Total geral de eleitores após remoção de linhas inválidas:", total_geral, "\n")
cat("Total de eleitores que atendem aos critérios:\n")
cat("- Gênero:", genero, "\n")
cat("- Faixa Etária:", faixa_etaria, "\n")
cat("- Estado Civil:", estado_civil, "\n")
cat("- Grau de Escolaridade:", grau_escolaridade, "\n")
cat("Total de eleitores que atendem aos critérios:", total_criterios, "\n")
