install.packages("lavaan")
install.packages("semPlot")
install.packages("psych")
install.packages("tidyverse")
install.packages("corrplot")   
install.packages("ggpubr")    

library(lavaan)
library(semPlot)
library(psych)
library(tidyverse)
library(corrplot)
library(ggpubr)

head(datos)

items <- paste0("C", 1:14)
datos[items] <- lapply(datos[items], as.integer)

datos$A1 <- factor(datos$A1, 
                   levels = c("<3", "3-5", "6-10", ">10"), 
                   ordered = TRUE)

datos$A2 <- factor(datos$A2,
                   levels = c("1-5", "6-15", "16-30", ">30"),
                   ordered = TRUE)

datos$A3 <- factor(datos$A3)

datos$B1 <- factor(datos$B1)  
datos$B2 <- factor(datos$B2)  


datos$exp_num <- recode(datos$A1,
                        "<3" = 2,
                        "3-5" = 4,
                        "6-10" = 8,
                        ">10" = 15)

datos$n_art <- recode(datos$A2,
                      "1-5" = 3,
                      "6-15" = 10,
                      "16-30" = 22,
                      ">30" = 35)

head(datos)

items <- paste0("C", 1:14)
datos[items] <- lapply(datos[items], as.integer)

datos$A1 <- factor(datos$A1, 
                   levels = c("<3", "3-5", "6-10", ">10"), 
                   ordered = TRUE)

datos$A2 <- factor(datos$A2,
                   levels = c("1-5", "6-15", "16-30", ">30"),
                   ordered = TRUE)

datos$A3 <- factor(datos$A3)

datos$B1 <- factor(datos$B1)  
datos$B2 <- factor(datos$B2)  


datos$exp_num <- recode(datos$A1,
                        "<3" = 2,
                        "3-5" = 4,
                        "6-10" = 8,
                        ">10" = 15)

datos$n_art <- recode(datos$A2,
                      "1-5" = 3,
                      "6-15" = 10,
                      "16-30" = 22,
                      ">30" = 35)

cor_mat <- cor(datos[items], use = "pairwise.complete.obs")

corrplot(cor_mat,
         method = "color",
         type = "upper",
         addCoef.col = NA,
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.5,
         title = "Correlation C1–C14",
         mar = c(0,0,2,0))

alpha_global <- psych::alpha(datos[items])
alpha_global$total$raw_alpha
alpha_global

dim_PFT  <- c("C1", "C2", "C3")
dim_CEPM <- c("C4", "C5", "C6")
dim_EJM  <- c("C7", "C8", "C9")
dim_RLC  <- c("C10", "C11", "C12", "C13", "C14")

alpha_PFT  <- psych::alpha(datos[dim_PFT])
alpha_CEPM <- psych::alpha(datos[dim_CEPM])
alpha_EJM  <- psych::alpha(datos[dim_EJM])
alpha_RLC  <- psych::alpha(datos[dim_RLC])

alpha_PFT$total$raw_alpha
alpha_CEPM$total$raw_alpha
alpha_EJM$total$raw_alpha
alpha_RLC$total$raw_alpha

modelo_deep_sem <- '
  ## Factores de primer orden
  PFT  =~ C1 + C2 + C3
  CEPM =~ C4 + C5 + C6
  EJM  =~ C7 + C8 + C9
  RLC  =~ C10 + C11 + C12 + C13 + C14

  ## Factor de segundo orden: Carencias Epistemológicas Globales
  CEG =~ PFT + CEPM + EJM + RLC

  ## Parte estructural: predictores de CEG
  ## Usamos exp_num (años aprox) y n_art (artículos revisados)
  CEG ~ exp_num + n_art
'


ordered_items <- items  

ajuste_deep_sem <- sem(
  modelo_deep_sem,
  data     = datos,
  ordered  = ordered_items,
  estimator = "WLSMV"   
)

summary(ajuste_deep_sem,
        fit.measures = TRUE,
        standardized = TRUE,
        rsquare      = TRUE)


install.packages("lavaanPlot")
library(lavaanPlot)

lavaanPlot(
  model = ajuste_deep_sem,
  coef  = TRUE,    # muestra coeficientes
  stand = TRUE,    # coeficientes estandarizados
  covs  = TRUE,    # muestra covarianzas entre factores
  stars = "regress" # marca significancia en regresiones (p-value)
)
library(lavaan)
fscores <- lavPredict(ajuste_deep_sem, method = "EBM")  

datos$CEG_score <- fscores[, "CEG"]

head(datos$CEG_score)

library(ggplot2)
theme_set(theme_classic(base_size = 14))
g1 <- ggplot(datos, aes(x = CEG_score)) +
  geom_histogram(aes(y = after_stat(density)),  
                 bins = 30,
                 alpha = 0.6) +
  geom_density(linewidth = 1) +
  labs(
    title = "Distribution of Global Epistemological Deficiencies (GED)",
    x = "Latent GED score",
    y = "Density"
  ) +
  theme_minimal()

g1

g2 <- ggplot(datos, aes(x = B1, y = CEG_score, fill = B1)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Global epistemological deficiencies by area of knowledge",
    x = "Main area (B1)",
    y = "CEG (latent score)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

g2

g3 <- ggplot(datos, aes(x = exp_num, y = CEG_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  labs(
    title = "Relationship between experience as a reviewer and perceived epistemological deficiencies",
    x = "Years of experience as a reviewer (exp_num)",
    y = "CEG (latent score)"
  ) +
  theme_minimal()

g3

install.packages("ggpubr")  
library(ggpubr)

ggarrange(
  g1, g2, g3,
  ncol = 1,
  nrow = 3,
  labels = c("A", "B", "C")
)

labels_B1_en <- c(
  "Ciencias de la Salud"                 = "Health Sciences",
  "Ciencias Naturales / Exactas"        = "Natural and Exact Sciences",
  "Ciencias Sociales y del Comportamiento" = "Social and Behavioural Sciences",
  "Economía y Negocios / Management"    = "Economics and Business / Management",
  "Educación"                           = "Education",
  "Humanidades"                         = "Humanities",
  "Ingeniería y Tecnología"             = "Engineering and Technology",
  "Interdisciplinaria / Multidisciplinaria" = "Interdisciplinary / Multidisciplinary"
)

g2_en <- ggplot(datos, aes(x = B1, y = CEG_score, fill = B1)) +
  geom_boxplot(alpha = 0.7) +
  scale_x_discrete(labels = labels_B1_en) +
  labs(
    title = "Global epistemological deficiencies by disciplinary area",
    x = "Main disciplinary area (B1)",
    y = "CEG latent factor score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "none"
  )

g2_en

