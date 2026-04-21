# CaioFelix_RM570459_fase2_cap7

if (!require("readxl")) install.packages("readxl")
if (!require("ggplot2")) install.packages("ggplot2")
library(readxl)
library(ggplot2)

caminho <- "C:/Users/MASTER/Downloads/dados_agronegocio.xlsx"
dados <- read_excel(caminho)

print(head(dados))
str(dados)

area <- dados$area_hectares

cat("Media.....:", round(mean(area), 2), "hectares\n")
cat("Mediana...:", round(median(area), 2), "hectares\n")
moda_area <- as.numeric(names(sort(table(area), decreasing = TRUE))[1])
cat("Moda......:", moda_area, "hectares\n")

cat("Variancia......:", round(var(area), 2), "\n")
cat("Desvio Padrao..:", round(sd(area), 2), "\n")
cat("Amplitude......:", round(max(area) - min(area), 2), "\n")
cat("Coef. Variacao.:", round((sd(area) / mean(area)) * 100, 2), "%\n")

quartis <- quantile(area, probs = c(0.25, 0.50, 0.75))
cat("Q1:", round(quartis[1], 2), "\n")
cat("Q2:", round(quartis[2], 2), "\n")
cat("Q3:", round(quartis[3], 2), "\n")
cat("P10:", round(quantile(area, 0.10), 2), "\n")
cat("P90:", round(quantile(area, 0.90), 2), "\n")

hist(area,
     main = "Distribuicao da Area Plantada (hectares)",
     xlab = "Area (hectares)",
     ylab = "Frequencia",
     col = "seagreen",
     border = "white")

boxplot(area,
        main = "Boxplot: Area Plantada",
        ylab = "Area (hectares)",
        col = "lightgreen")

tabela_cultura <- table(dados$cultura)
print(tabela_cultura)
print(round(prop.table(tabela_cultura) * 100, 2))

barplot(tabela_cultura,
        main = "Distribuicao das Culturas",
        xlab = "Cultura",
        ylab = "Numero de Produtores",
        col = c("goldenrod", "brown", "orange", "darkgreen", "forestgreen"),
        border = "white")

pie(tabela_cultura,
    main = "Proporcao de Culturas Plantadas",
    col = c("goldenrod", "brown", "orange", "darkgreen", "forestgreen"))