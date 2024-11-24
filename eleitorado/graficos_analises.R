# Carregar as funções do arquivo funcoes_dataset.R
source("eleitorado/funcoes_dataset.R")
library(ggplot2)

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
# 2. Agrupamentos
# -------------------------

# Total de eleitores por município
tabela_municipio <- df_filtrado %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  arrange(desc(Total_Eleitores))

# Total de eleitores por grau de instrução
tabela_grau_instrucao <- df_filtrado %>%
  group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  arrange(desc(Total_Eleitores))

# Total de eleitores por município e gênero
tabela_municipio_genero <- df_filtrado %>%
  group_by(NM_MUNICIPIO, DS_GENERO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  arrange(desc(Total_Eleitores))

# -------------------------
# 3. Criar Gráficos
# -------------------------

# Gráfico 1: Eleitores por Município (Barplot)
grafico_municipio <- ggplot(tabela_municipio, aes(x = reorder(NM_MUNICIPIO, Total_Eleitores), y = Total_Eleitores)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Total de Eleitores por Município",
    x = "Município",
    y = "Total de Eleitores"
  ) +
  theme_minimal()

# Gráfico 2: Eleitores por Grau de Instrução (Pie Chart)
grafico_grau_instrucao <- ggplot(tabela_grau_instrucao, aes(x = "", y = Total_Eleitores, fill = DS_GRAU_ESCOLARIDADE)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Distribuição de Eleitores por Grau de Instrução",
    fill = "Grau de Instrução"
  ) +
  theme_void()

# Gráfico 3: Eleitores por Município e Gênero (Stacked Barplot)
grafico_municipio_genero <- ggplot(tabela_municipio_genero, aes(x = reorder(NM_MUNICIPIO, -Total_Eleitores), y = Total_Eleitores, fill = DS_GENERO)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Total de Eleitores por Município e Gênero",
    x = "Município",
    y = "Total de Eleitores",
    fill = "Gênero"
  ) +
  theme_minimal()

# Total de eleitores por faixa etária e estado civil para cada gênero
tabela_faixa_estado_genero <- df_filtrado %>%
  group_by(DS_GENERO, DS_FAIXA_ETARIA, DS_ESTADO_CIVIL) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# -------------------------
# 3. Criar Heatmaps
# -------------------------

# Heatmap para o gênero Masculino
heatmap_masculino <- ggplot(
  tabela_faixa_estado_genero %>% filter(DS_GENERO == "MASCULINO"),
  aes(x = DS_FAIXA_ETARIA, y = DS_ESTADO_CIVIL, fill = Total_Eleitores)
) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Distribuição de Eleitores por Faixa Etária e Estado Civil (Masculino)",
    x = "Faixa Etária",
    y = "Estado Civil",
    fill = "Total de Eleitores"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heatmap para o gênero Feminino
heatmap_feminino <- ggplot(
  tabela_faixa_estado_genero %>% filter(DS_GENERO == "FEMININO"),
  aes(x = DS_FAIXA_ETARIA, y = DS_ESTADO_CIVIL, fill = Total_Eleitores)
) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "pink") +
  labs(
    title = "Distribuição de Eleitores por Faixa Etária e Estado Civil (Feminino)",
    x = "Faixa Etária",
    y = "Estado Civil",
    fill = "Total de Eleitores"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------
# 4. Exibir Gráficos
# -------------------------
print(heatmap_masculino) # Heatmap Masculino
print(heatmap_feminino)  # Heatmap Feminino

# -------------------------
# 4. Exibir Gráficos
# -------------------------
print(grafico_municipio)         # Gráfico 1
print(grafico_grau_instrucao)    # Gráfico 2
print(grafico_municipio_genero)  # Gráfico 3
