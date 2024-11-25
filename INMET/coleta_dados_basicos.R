# Instalar pacotes necessários (se ainda não tiver)
if (!require(readxl)) install.packages("readxl")
if (!require(openxlsx)) install.packages("openxlsx")

# Carregar pacotes
library(readxl)
library(openxlsx)

# Função principal
processar_dataset <- function(arquivo, colunas_interesse = NULL, salvar_como_xlsx = FALSE) {
  
  # Obter tamanho inicial do arquivo
  file_info <- file.info(arquivo)
  tamanho_inicial_kb <- file_info$size / 1024  # Convertendo para KB
  
  # Carregar o dataset dependendo do formato
  if (grepl("\\.csv$", arquivo)) {
    dataset <- read.csv(arquivo)
  } else if (grepl("\\.xlsx$", arquivo) || grepl("\\.xls$", arquivo)) {
    dataset <- read_excel(arquivo)
  } else {
    stop("Formato de arquivo não suportado.")
  }
  
  # Métricas iniciais
  linhas_iniciais <- nrow(dataset)
  colunas_iniciais <- ncol(dataset)
  celulas_iniciais <- linhas_iniciais * colunas_iniciais
  
  # Filtrar colunas de interesse (se fornecidas)
  if (!is.null(colunas_interesse)) {
    dataset_interesse <- dataset[, colunas_interesse, drop = FALSE]
  } else {
    dataset_interesse <- dataset
  }
  
  # Filtrar linhas válidas (remover NA)
  dataset_validos <- dataset_interesse[complete.cases(dataset_interesse), ]
  
  # Métricas com dados de interesse
  linhas_interesse <- nrow(dataset_interesse)
  colunas_interesse <- ncol(dataset_interesse)
  celulas_interesse <- linhas_interesse * colunas_interesse
  
  # Métricas finais (após remover linhas inválidas)
  linhas_validas <- nrow(dataset_validos)
  
  # Salvar o dataset processado em um arquivo temporário
  arquivo_temporario <- tempfile(fileext = ".csv")
  write.csv(dataset_validos, arquivo_temporario, row.names = FALSE)
  
  # Obter o tamanho do arquivo final
  tamanho_final_kb <- file.info(arquivo_temporario)$size / 1024
  
  # Remover o arquivo temporário
  unlink(arquivo_temporario)
  
  # Salvar como Excel, se solicitado
  if (salvar_como_xlsx) {
    write.xlsx(dataset_validos, "dados_interesse.xlsx", asTable = TRUE)
  }
  
  # Retornar um resumo
  resumo <- list(
    "Tamanho inicial (KB)" = tamanho_inicial_kb,
    "Linhas iniciais" = linhas_iniciais,
    "Colunas iniciais" = colunas_iniciais,
    "Células iniciais" = celulas_iniciais,
    "Tamanho com dados de interesse (KB)" = tamanho_final_kb,
    "Linhas com dados de interesse" = linhas_interesse,
    "Colunas com dados de interesse" = colunas_interesse,
    "Células com dados de interesse" = celulas_interesse,
    "Linhas sem dados inválidos" = linhas_validas
  )
  
  return(resumo)
}
arquivo <- "C:/Users/igor_/Downloads/trab-top-eng-dados/csvs/INMET_SE_RJ_A621_RIO DE JANEIRO - VILA MILITAR_01-01-2022_A_31-12-2022.csv"
arquivo2 <- "C:/Users/igor_/Downloads/trab-top-eng-dados/csvs/INMET_SE_RJ_A621_RIO DE JANEIRO - VILA MILITAR_01-01-2023_A_31-12-2023.csv"
colunas <- c("Data", "Hora_UTC", "TEMPERATURA_DO_AR_BULBO_SECO", "TEMPERATURA_MAXIMA_NA_HORA_ANT", "TEMPERATURA_MINIMA_NA_HORA_ANT")
resultado <- processar_dataset(arquivo, colunas_interesse = colunas, salvar_como_xlsx = FALSE)
resultado2 <- processar_dataset(arquivo2, colunas_interesse = colunas, salvar_como_xlsx = FALSE)

# Exibir resultados
print(resultado)
print(resultado2)
