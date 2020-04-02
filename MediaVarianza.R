
library(tidyverse)

# Carga inicial de parametros
data <- tibble(
  bond = c("A","B"),
  R = c(0.15, 0.03),
  V = c(0.08, 0.05)
)

# Portafolios que armamos para el mismo coeficiente de correlacon 
n <- 50

# Matriz de tenencias
w <-  tibble(
  wa = seq(from = 1, to = 0, by = -(1/(n-1))),
  wb = seq(from = 0, to = 1, by = (1/(n-1)))
)


# Confeccion de la matriz grande!
MasterData <- w

MasterData$ERa <- data$R[1]
MasterData$ERb <- data$R[2]

MasterData <- MasterData %>% 
  mutate(
    ERp = wa * ERa + wb * ERb,
    VarA = data$V[1],
    VarB = data$V[2],
    escenario = seq(from= 1, to = n, by = 1)
  )

coefcorrelacion <- seq(from = -1, to = 1,  by = 0.125)
length(coefcorrelacion)


j <- 0
k <- 0
for (i in coefcorrelacion) {
  
  k <- k+1
  # i <- -0.75
  # print(i) 
  
  ColName <- paste0("A", k)
  
  # i <- 0
  MasterData <- MasterData %>% 
    mutate(
      ColName = (wa^2* VarA)+ (wb^2 * VarB) + (2 * wa * wb * i * VarA^(1/2) * VarB^(1/2) )
    )
  
  colnames(MasterData)[9+j] <- ColName    
  
  j <- j+1
}


data_plot <- MasterData %>%
  select(-wa, -wb, -ERa, -ERb, -VarA, -VarB, -escenario) %>%
  gather(-ERp, key = "variable", value = "VarRp")


ggplot()+
  geom_point(
    data        = data_plot,
    mapping     = aes(x=VarRp, y=ERp, color=variable),
    show.legend = FALSE 
  ) +
  geom_point(
    data    = data,
    mapping = aes(x=V, y = R),
    size    = 3,
    color   = "black"
  )+
  annotate(
    geom = 'text',
    x    = data$V[1],
    y    = data$R[1],
    label = "Activo 1",
    hjust = 0,
    vjust = 1
  )+
  annotate(
    geom = 'text',
    x    = data$V[2],
    y    = data$R[2],
    label = "Activo 2",
    hjust = 0,
    vjust = 1
  )+
  scale_x_continuous(
    limits = c(0, max(data$V)+0.01)
  )+
  labs(
    title    = "Representación Gráfica del Entorno Media Varianza",
    subtitle = "2 activos riesgosos",
    caption  = "Preparado en R por Juan Fracisco Gómez, para el curso de Dinero, Crédito y Bancos",
    x = "Var(Rp)",
    y = "E(Rp)"
  )+
  theme_minimal()+
  theme(
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption  = element_text(size = 8),
    axis.title.x  = element_text(hjust = 1, size = 8),
    axis.title.y  = element_text(hjust = 1, size = 8)
    )
