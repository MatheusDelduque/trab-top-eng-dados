library(ggplot2)

# Exemplo para gráfico de temperatura diária
ggplot(agrupamentos$diario, aes(x = Data, y = Media_Temperatura_Diaria)) +
  geom_line(color = "blue") +
  labs(
    title = "Temperatura Diária - Média das Temperaturas",
    x = "Data",
    y = "Média da Temperatura (°C)",
    caption = "Fonte: INMET"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
