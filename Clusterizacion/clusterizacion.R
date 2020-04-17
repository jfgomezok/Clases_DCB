pacman::p_load(tidyverse, readxl)

# Lectura y limpieza de datos-----------
data <- read_xlsx("Clusterizacion/Oil.xlsx")
data <- data %>% select(Date, contains("px"))
colnames(data) <- str_remove(string = colnames(data), pattern = " Px")

# Subset de variables de interés
df <- data %>%  select(WTI, YPF)

# Algoritmo de clusterizacion 
set.seed(15)
clusters <- kmeans(x = df, centers = 6, iter.max = 300)

# Pego el numero de cluster hallado en el df
df <- cbind(df, cluster = clusters$cluster) %>% as_tibble %>% mutate(cluster = as.factor(cluster))

# Matriz de centros y correlacion
df2 <- tibble(
  cluster = rep(1:6),
  WTI     = clusters$centers[,1],
  YPF     = clusters$centers[,2],
  corr    = df %>% group_by(cluster) %>% summarise(corr = cor(YPF, WTI)) %>%  select(corr) %>% unlist()
  )

# Grafico
ggplot()+
  geom_point(
    data        = df,
    mapping     = aes(x = WTI, y = YPF, color = cluster),
    size        = 1,
    show.legend = FALSE
    )+
  geom_text(
    data    = df2,
    mapping = aes(x = WTI, y = YPF, label = paste0("Corr =",round(corr,1))),
    fontface = "bold" 
    )+
  labs(
    title    = "Clusterización k=6 de YPF&WTI",
    subtitle = "Observaciones diaras desde 11/03/2010 al 09/04/2020",
    caption  = "Elaborado por JFG, en base JG y JMY" 
  ) +
  scale_color_discrete()+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust= 0.5, face = "bold"),
    plot.subtitle = element_text(hjust= 0.5)
  )
