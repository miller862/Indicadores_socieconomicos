library(tidyverse)
library(rstatix)
options(scipen=999)
library(readxl)
prov2022 <- read_excel("datos/provincias2022.xlsx")
View(clientes)




prov2022 <- prov2022[, 1:(ncol(prov2022) - 6)]
prov2022 <- prov2022%>% remove_rownames %>% column_to_rownames(var="Provincia")
summary(prov2022)

glimpse(prov2022)


# Si las provincias están como rownames
pbi_share <- prov2022 / rowSums(prov2022, na.rm = TRUE)






# distribución de las variables
par(mfrow = c(1, 2))
hist(clientes$INGRESOS,breaks="Sturges",labels=TRUE, main = "Ingresos", xlab = "Euros", ylab="Frecuencias",col = "gold",
     border="tomato1")
hist(clientes$EDAD,col = "gold",
     border="tomato1", main = "Edad",labels=TRUE, xlab = "Años",ylab="Frecuencias")

# buscando observaciones atípicas
par(mfrow = c(1, 2))
boxplot(clientes$INGRESOS,border="steelblue", col="gold",main = "Ingresos",ylab="Euros")
boxplot(clientes$EDAD,col="gold",border="steelblue",main = "Edad",ylab="Años")

# standarización de las variables 
Zprov2022 <- scale(pbi_share)
library(scales)

# gráfico de dispersión
par(mfrow = c(1, 1))
plot(Zprov2022, col = alpha("steelblue", 0.4), pch = 19, las = 1)
text(Zprov2022, rownames(Zprov2022), pos = 3, cex = .6)

# clusters jerárquicos

d <- dist(Zprov2022, method = "euclidean")
fit <- hclust(d, method="ward.D")
plot(fit, cex = .6, xlab = "", ylab = "Distancia entre grupos", sub = "Cluster jerárquico por el método de Ward provincias") 
Numgrupos <- 5
library(RColorBrewer)
plot(fit, cex = .6, xlab = "", ylab = "Distancia entre grupos", sub = "Cluster jerárquico por el método de Ward para los clientes") 
rect.hclust(fit, k = 5, border = brewer.pal(Numgrupos, "Dark2"))


# partición en conglomerados

grupos3 <- cutree(fit, k = 3)
grupos3

grupos5 <- cutree(fit, k = 5)
grupos5

pbi_share$GRUPO <- factor(grupos5)

colores<-palette(brewer.pal(Numgrupos, "Dark2"))
plot(`Salud pública` ~ `Salud privada`, col = colores, pch = 19, data = pbi_share, las = 1)
text(clientes, rownames(clientes), pos = 3, cex = .6)

colnames(pbi_share)
###############conglomerados no jerarquicos

Data.km <- kmeans(Zprov2022, Numgrupos,5)
Data.km

pbi_share %>%
  group_by(GRUPO) %>%
  get_summary_stats(type="mean_sd")

pbi_share %>% anova_test(`Elaboración de productos alimenticios y bebidas`~GRUPO)

library(dplyr)
library(purrr)
library(rstatix)

variables <- names(pbi_share)[names(pbi_share) != "GRUPO"]

# Recolectamos los resultados en una lista primero
resultados_lista <- map(
  variables,
  ~ {
    formula_txt <- paste0("`", .x, "` ~ GRUPO")
    resultado <- anova_test(data = pbi_share, formula = as.formula(formula_txt))
    resultado$Variable <- .x
    resultado
  }
)

# Luego los combinamos en un solo dataframe
resultados_anova <- bind_rows(resultados_lista)




variables <- names(pbi_share)[names(pbi_share) != "GRUPO"]

# Crear lista de resultados y forzar cada uno a data.frame base
resultados_lista <- map(
  variables,
  ~ {
    formula_txt <- paste0("`", .x, "` ~ GRUPO")
    res <- anova_test(data = pbi_share, formula = as.formula(formula_txt))
    df <- as.data.frame(res)
    df$Variable <- .x
    df
  }
)

# Convertir lista de data.frames en uno solo
resultados_anova <- do.call(rbind, resultados_lista)


resultados_anova_clean <- resultados_anova
class(resultados_anova_clean) <- "data.frame"  # Eliminar clases especiales

resultados_anova_clean %>%
  mutate(p = as.numeric(p)) %>%
  arrange(p) %>%
  View()
