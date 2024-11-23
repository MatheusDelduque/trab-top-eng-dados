# Carregar o pacote Tidyverse
library(tidyverse)

# Função para carregar o dataset completo
carregar_dataset <- function(file_path) {
  read_delim(
    file_path,
    delim = ";",         # Ajuste o delimitador, se necessário
    locale = locale(encoding = "ISO-8859-1") # Encoding para arquivos brasileiros
  )
}

# Função para reduzir o dataset às colunas de interesse
reduzir_dataset <- function(df) {
  df %>%
    select(
      NM_MUNICIPIO,
      CD_GENERO,
      DS_GENERO,
      CD_ESTADO_CIVIL,
      DS_ESTADO_CIVIL,
      CD_FAIXA_ETARIA,
      DS_FAIXA_ETARIA,
      CD_GRAU_ESCOLARIDADE,
      DS_GRAU_ESCOLARIDADE,
      QT_ELEITORES_PERFIL
    )
}

# Função para filtrar linhas inválidas
filtrar_invalidos <- function(df) {
  df %>%
    filter(
      DS_FAIXA_ETARIA != "Inválido",
      DS_GENERO != "NÃO INFORMADO",
      DS_ESTADO_CIVIL != "NÃO INFORMADO",
      DS_GRAU_ESCOLARIDADE != "NÃO INFORMADO"
    )
}

# Função para filtrar por critérios específicos
filtrar_criterios <- function(df, genero, faixa_etaria, estado_civil, grau_escolaridade) {
  df %>%
    filter(
      DS_GENERO == genero,
      DS_FAIXA_ETARIA == faixa_etaria,
      DS_ESTADO_CIVIL == estado_civil,
      DS_GRAU_ESCOLARIDADE == grau_escolaridade
    )
}
