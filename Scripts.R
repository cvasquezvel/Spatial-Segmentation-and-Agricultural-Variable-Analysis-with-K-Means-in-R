### Tema elegante para plots ----

theme_elegante <- function(base_size = 12,
                           base_family = "Raleway"
)
{
  color.background = "#FFFFFF" # Chart Background
  color.grid.major = "#D9D9D9" # Chart Gridlines
  color.axis.text = "#666666" # 
  color.axis.title = "#666666" # 
  color.title = "#666666"
  color.subtitle = "#666666"
  strip.background.color = '#9999CC'
  
  ret <-
    theme_bw(base_size=base_size) +
    
    # Set the entire chart region to a light gray color
    # theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.background = element_rect(fill = "white", 
                                          colour = NA)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    # theme(panel.border=element_rect(color=color.background)) +
    theme(panel.border=element_rect(fill = NA, 
                                    colour = "grey20")) +
    
    # Format the grid
    theme(panel.grid = element_line(colour = color.grid.major)) +
    theme(panel.grid.major=element_line(color=color.grid.major,size=.55, linetype="solid")) +
    # theme(panel.grid.minor=element_line(color=color.grid.major,size=.55, linetype="solid")) +
    theme(panel.grid.minor=element_line(color=color.grid.major, linetype="solid",
                                        size = rel(0.5))) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=base_size-3,color=color.axis.title, family = base_family)) +
    
    theme(strip.text.x = element_text(size=base_size,color=color.background, family = base_family)) +
    theme(strip.text.y = element_text(size=base_size,color=color.background, family = base_family)) +
    #theme(strip.background = element_rect(fill=strip.background.color, linetype="blank")) +
    theme(strip.background = element_rect(fill = "grey70", colour = "grey20")) +
    # theme(panel.border= element_rect(fill = NA, colour = "grey70", size = rel(1)))+
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, 
                                  size=20, 
                                  vjust=1.25, 
                                  family=base_family, 
                                  hjust = 0.5
    )) +
    
    theme(plot.subtitle=element_text(color=color.subtitle, size=base_size+2, family = base_family,  hjust = 0.5))  +
    
    theme(axis.text.x=element_text(size=base_size,color=color.axis.text, family = base_family,
                                   angle=90, hjust=1, vjust = 0.5)) +
    theme(axis.text.y=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(text=element_text(size=base_size, color=color.axis.text, family = base_family)) +
    
    theme(axis.title.x=element_text(size=base_size+2,color=color.axis.title, vjust=0, family = base_family)) +
    theme(axis.title.y=element_text(size=base_size+2,color=color.axis.title, vjust=1.25, family = base_family)) +
    theme(plot.caption=element_text(size=base_size-2,color=color.axis.title, vjust=1.25, family = base_family)) +
    
    # Legend  
    theme(legend.text=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.title=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.key=element_rect(colour = color.background, fill = color.background)) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.title = element_blank(),
          legend.key.width = unit(.75, "cm"),
          legend.key.height = unit(.75, "cm"),
          legend.spacing.x = unit(.25, 'cm'),
          legend.spacing.y = unit(.25, 'cm'),
          legend.margin = margin(t=0, r=0, b=0, l=0, unit="cm")) +
    
    # Plot margins
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
  
  ret
}

# Cambiar el directorio de trabajo ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("C:/Users/Dell/Documents/Danper/4. Capacitaciones")
getwd()

options(scipen = 9999,
        max.print = 9999)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, agricolae, summarytools, car, psych, dplyr)
pacman::p_load(haven, dplyr, tidyverse, labelled, ggthemes, SmartEDA, klaR,
               factoextra, ggplot2, clustMixType, broom, fpc,clusterSim,
               clValid, tidyr, extrafont,patchwork, clustertend, hopkins,
               factoextra, FeatureImpCluster, flexclust, ade4, pca3d,
               tidyr, rJava, RWeka, mlr3, mlr3cluster, recipes)

datos <- read.csv("datos.csv")

data <- datos %>%
  dplyr::mutate(DVI = nir-red,
                SR = nir/red) %>%
  dplyr::select(-c(x,
                   y,
                   # FUNDO,
                   # SEMANA,
                   R_NDVI,
                   R_SAVI,
                   R_MSAVI,
                   R_GNDVI)) %>%
  # dplyr::select(-PC, -`R1/PC`,
  #               -`R2/PC`, -`(R1+R2)/PC`, -`(R1+R2+R3)/PC`,
  #               -`(R2+R3)/PC`)
  recipe() %>%
  update_role(-c(5,6), new_role = "predictor") %>%
  step_nzv(all_predictors()) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  select_if(~ !any(is.na(.)))

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

data <- data %>% 
  mutate_if(is.numeric, normalize) %>%
  dplyr::select(-FUNDO, 
                -SEMANA
                )

colSums(is.na(data))

# str(datos)

###############
# II. K-MEANS #
###############

# I. CLUSTER K-MEANS ----------------------------------------

data.clust <- data

data.final <- datos #%>% 
  # dplyr::filter(VARIEDAD %in% c("BONITA", "MAGICA"), 
  #               FUNDO %in% "VICTORIA",
  #               # CAMPA?A %in% 3
  #               )
  
# 1. Encuentre el n?mero ?ptimo de clusters para un k-means ----

# 1.1. Criterio de la suma de cuadrados ----
# RNGkind(sample.kind = "Rounding")
# set.seed(2021)
fviz_nbclust(data, kmeans, method = "wss", k.max=10) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "criterio de la suma de cuadrados") + theme_bw()

# 1.2. Criterio del Grafico de Silueta  ----

# RNGkind(sample.kind = "Rounding")
# set.seed(2021)
fviz_nbclust(data, kmeans, method = "silhouette", k.max=10) +
  labs(subtitle = "Silhouette method")


# 1.3. Criterio del Gap Statistics ----
# RNGkind(sample.kind = "Rounding")
# set.seed(2021)
fviz_nbclust(data, kmeans, nstart = 25, k.max=15,
             method = "gap_stat",nboot = 20) +
  labs(subtitle = "Gap statistic method")

# 1.4. Eleccion del K optimo - kmeans ----
# RNGkind(sample.kind = "Rounding")
# set.seed(2021)
fviz_nbclust(data.clust, kmeans, method = "wss", k.max=10) +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Metodo Elbow") + theme_bw()

# 1.5. Usando la funcion kmeans() con 2 clusters ----
# RNGkind(sample.kind = "Rounding")
# set.seed(2021)
km <- kmeans(data, 
             centers=4,      # Numero de Cluster
             algorithm = "Hartigan-Wong",
             trace = FALSE,
             iter.max = 100, # Numero de iteraciones maxima
             nstart = 25);km    # Numero de puntos iniciales

# Tama?o de cada cluster
km$size
prop.table(km$size)

# promedios de cada cluster
aggregate(data, by=list(cluster=km$cluster), mean)

# Se visualiza cada elemento con su respectivo cluster
broom::augment(km, data) #%>% View()

# Con el comando tidy podemos ver los principales indicadores por cluster,
# promedio por cluster y variable, el tama?o y la suma de cuadrados.
broom::tidy(km)
broom::glance(km)

# 2. Validacion k mean. ----

# 2.1. Bootstrap ----

kclusters <- fpc::clusterboot(data,
                              B = 20, # Number of resampling runs for each scheme
                              clustermethod = fpc::kmeansCBI,
                              k = 4,
                              # seed = 2022
                              )
kclusters$bootmean

# 2.2. Validacion k mean: indice de Dunn ----

data %>% mutate(grp=km$cluster) -> datos.k
grupo <- as.integer(datos.k$grp)
grupo
kdunn <- clValid::dunn(Data= data, clusters=grupo, distance = NULL)
kdunn

# 2.3. Usando el Criterio del estad?stico Hopkins --------------

# Estad?tico H

# Los valores calculados 0-0,3 indican datos espaciados regularmente. 
# Los valores en torno a 0,5 indican datos aleatorios. 
# Los valores 0,7-1 indican datos agrupados.

hopkins::hopkins(X = data.clust, m = nrow(data.clust)-1) 

# 3. An?lisis de Componentes Principales ----

acp <- dudi.pca(data.clust,scannf=FALSE,nf=ncol(data.clust))
summary(acp)

# Valores propios
acp$eig

inertia.dudi(acp)

# Correlaciones entre las variables y los componentes
acp$co[c(1:5)]

# Gr?fica de Valores propios - ScreePlot
fviz_eig(acp, addlabels=TRUE, hjust = -0.3,
         barfill="white", barcolor ="darkblue",
         linecolor ="red") + ylim(0,85) + theme_minimal()

# Scores o Puntuaciones de cada individuo
acp$li[1:10,1:2]

# Visualizaci?n de cluster por PCA

fviz_cluster(km, data = data.clust, ellipse.type = "convex") +
  theme_elegante()

fviz_cluster(km, data = data.clust, ellipse.type = "convex",
             axes = c(2,3)) +
  theme_elegante()

pca <- prcomp(data)

gr <- factor(broom::augment(km, data) %>% 
               dplyr::select(.cluster) %>% unlist)
summary(gr)

pca3d::pca3d(pca, group=gr)

# 4. Visualizaci?n de los centroides de 1 a 10 clusters ----

kclusts <- 
  tibble(k = 1:10) %>%
  mutate(
    kclust = purrr::map(k, ~kmeans(data, .x)),
    tidied = purrr::map(kclust, broom::tidy),
    glanced = purrr::map(kclust, broom::glance),
    augmented = purrr::map(kclust, broom::augment, data)
  )
kclusts

# Separacion por cada indicador de cluster para cada valor de K
clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

# Separacion de los individuos por cada cluster para cada valor de K
assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

# indicador total para cada valor de K
clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

# Grafico mostrando como se distribuyen los diferentes cluster para cada valor de K
p1 <- 
  ggplot(assignments, aes(x = R1, y = R2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

# Se adiciona el centroide de cada cluster para cada valor de K
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2

# De los graficos anteriores es ahora muy claro el que K tome el valor de 2
# Valor que sera confirmado por el metodo de Elbow.

# Se validara usando el ?ndice de Validaci?n de Davies-Bouldin y el ?ndice de Dunn

set.seed(2021)
db <- numeric()
dunn <- numeric()
for (h in 2:6){
  b          <- kmeans(data,h)
  grupo      <- b$cluster
  indiceDB   <- clusterSim::index.DB(data, grupo, centrotypes = "centroids")
  db[h]      <- indiceDB$DB 
  indiceDUNN <- clValid::dunn(Data = data, clusters = grupo, distance = NULL)
  dunn[h]    <- indiceDUNN
  
}
db
dunn
indices  <- data.frame(cluster = c(2:6),
                       Indice_DB = db[2:6],
                       Indice_Dunn = dunn[2:6]) # a maximizar
indices

# Valores medios por cluster
aggregate(data.final, by=list(km$cluster), mean)

# 5. Importancia de las variables ----

cl <- flexclust::as.kcca(km,data)
x11()
barplot(cl)

Importancia <- FeatureImpCluster::FeatureImpCluster(cl, as.data.table(data))
plot(Importancia)

datos.km <- data.clust %>%
  dplyr::bind_cols(grp=as.factor(km$cluster),
                   data.final %>%
                     dplyr::select(FUNDO, SEMANA))

datos.km.sn <- data.final %>%
  dplyr::bind_cols(grp=as.factor(km$cluster))

datos.km.sn %>% 
  select_if(is.numeric) %>%
  bind_cols(grp=as.factor(km$cluster)) %>%
  group_by(grp) %>% 
  summarise_all(list(mean), na.rm = TRUE) -> medias
medias

datos.km.sn %>%  summarise_if(is.numeric,mean, na.rm = TRUE) -> general
general
general <- cbind(grp="general",general)
general

medias  <- as.data.frame(rbind(medias,general))

# Convirtiendo la data formato tidy (gather y spread)

gathered_datos.km <- pivot_longer(data  = medias, 
                                  -grp,
                                  names_to = "variable",
                                  values_to = "valor")%>%
  dplyr::filter(!is.na(valor))

gathered_datos.km

# 6. CARACTERIZANDO A LOS CLUSTERS ----

# 6.1 Describiendo los clusters usando la funci?n summary() ----
km_results <- data.final %>%
  mutate(cluster = km$cluster) %>%
  group_by(cluster) %>%
  do(resumen = summary(.))
#km_results
km_results$resumen

km_results <- data.final %>%
  mutate(cluster = km$cluster) %>%
  group_by(cluster) %>%
  do(resumen = summarytools::descr(.))
#km_results
km_results$resumen

table_data <- data.final %>%
  mutate(cluster = km$cluster)
table(table_data$cluster, table_data$MODULO)

# 6.2 Diagrama de Cajas de cada variable num?rica seg?n Cluster ----
#     usando el paquete ggplot2

ggplot(datos.km.sn) + aes(x = grp, y = EVI, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "EVI",
       title = "EVI") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g1; g1

ggplot(datos.km.sn) + aes(x = grp, y = NDVI, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "NDVI",
       title = "NDVI") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g2; g2

ggplot(datos.km.sn) + aes(x = grp, y = LAI, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "LAI",
       title = "LAI") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g3; g3

ggplot(datos.km.sn) + aes(x = grp, y = `Longitud R1`, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "cm",
       title = "Longitud R1") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g4; g4

ggplot(datos.km.sn) + aes(x = grp, y = `Longitud R2`, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "cm",
       title = "Longitud R2") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g5; g5

ggplot(datos.km.sn) + aes(x = grp, y = `Longitud R3`, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "cm",
       title = "Longitud R3") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g6; g6

ggplot(datos.km.sn) + aes(x = grp, y = `Diametro R1`, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "mm",
       title = "Diametro R1") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g7; g7

ggplot(datos.km.sn) + aes(x = grp, y = `Diametro R2`, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "mm",
       title = "Diametro R2") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g8; g8

ggplot(datos.km.sn) + aes(x = grp, y = `Diametro R3`, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "mm",
       title = "Diametro R3") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g9; g9

ggplot(datos.km.sn) + aes(x = grp, y = `PC`, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "unidad",
       title = "PC") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g10; g10

ggplot(datos.km.sn) + aes(x = grp, y = `PRIMOCA?AS`, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "unidad",
       title = "Primoca?a") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g11; g11

ggplot(datos.km.sn) + aes(x = grp, y = `SEMANA FENOLOGICA`, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "semana",
       title = "Semana Fenol?gica") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g12; g12

ggplot(datos.km.sn) + aes(x = grp, y = `SEMANA CALENDARIO`, fill = grp) + 
  geom_boxplot() +
  labs(x = "Cluster", y = "semana",
       title = "Semana Calendario") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right")  -> g13; g13

# 6.3 Diagrama de l?neas de promedios por cluster -------------

gathered_datos.km %>% 
  dplyr::filter(!variable %in% c("x","y","SEMANA",
                                 "blue","green","nir","red",
                                 "R_NDVI","R_GNDVI","R_SAVI","R_MSAVI")) %>%
  ggplot() + aes(x=variable,y=valor,color=grp) + 
  geom_point() + geom_line(aes(group = grp)) +
  scale_y_continuous() +
  theme_elegante() +
  theme(legend.position = "bottom") +
  labs(title="Diagrama de líneas de cluster por variable - método K-means",
       x="Variable") +
  scale_colour_discrete("Cluster")

####

datos.km.sn %>% 
  select_if(is.numeric) %>%
  bind_cols(grp=as.factor(km$cluster)) %>%
  group_by(grp, SEMANA) %>% 
  summarise_all(list(mean), na.rm = TRUE) -> medias2
medias2

datos.km.sn %>% group_by(SEMANA) %>%  summarise_if(is.numeric,mean, na.rm = TRUE) -> general2
general2
general2 <- cbind(grp="general",general2)
general2

medias2  <- as.data.frame(rbind(medias2,general2))

# Convirtiendo la data formato tidy (gather y spread)

gathered_datos.km2 <- pivot_longer(data  = medias2, 
                                  -c(grp,SEMANA),
                                  names_to = "variable",
                                  values_to = "valor")%>%
  dplyr::filter(!is.na(valor))

gathered_datos.km2

gathered_datos.km2 %>% 
  dplyr::filter(!variable %in% c("x","y",
                                 "blue","green","nir","red",
                                 "R_NDVI","R_GNDVI","R_SAVI","R_MSAVI")) %>%
  ggplot() + aes(x=SEMANA,y=valor,color=grp) + 
  geom_point() + geom_line(aes(group = grp)) +
  scale_y_continuous() +
  theme_elegante() +
  theme(legend.position = "bottom") +
  labs(title="Diagrama de líneas de cluster por variable - método K-means",
       x="Variable") +
  scale_colour_discrete("Cluster") +
  facet_wrap(vars(variable), scales = "free_y")

# 6.4 Gr?ficos apilados para las variables categ?ricas --------

ggplot(datos.km.sn) + aes(CAMPA?A, fill = grp) + 
  geom_bar(position = position_fill()) + 
  labs(x = "Campa?a", y = "proporci?n",
       fill = "Cluster",
       title = "Campa?a") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g14; g14

ggplot(datos.km.sn) + aes(grp, fill = grp) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Cl?ster", y = "frecuencia",
       fill = "Cluster",
       title = "Cl?sters") +
  theme_elegante()  +
  theme(axis.text.x=element_text(angle=0,hjust=0.5),
        legend.position = "right") -> g15; g15

Figura1 <- (g1 + g2 + g3) / (g4 + g5 + g6) / (g7 + g8 + g9) / (g10 + g11 + g14) / (g12 + g13 + g15) 
Figura1

ggsave(Figura1, filename = "VT OZ BLUE.jpeg", 
       width = 300, 
       height = 240, 
       units = "mm", 
       dpi = 600)
file.show("VT OZ BLUE.jpeg")

write.csv(datos.km.sn %>%
            dplyr::select(x,y,FUNDO, SEMANA, grp),
          "VICTORIA.csv", row.names = F)

library(sf)
library(sp)
library(raster)
library(dplyr)

df = st_as_sf(datos.km.sn %>%
                mutate(grp = factor(grp,
                                    levels = c(3,2,1,4),
                                    labels = c("Crítico",
                                               "Bajo",
                                               "Moderado",
                                               "Alto")),
                       grp.num = as.numeric(grp)) %>%
                dplyr::filter(SEMANA %in% 20), coords = c("x", "y")) %>% st_set_crs(32717)

fundo = read_sf("FUNDOS_DANPER.shp") %>%
  filter(FUNDO %in% "VICTORIA",
         VARIEDAD %in% c("MAGICA","BONITA"))

# with a SpatialPointsDataFrame
# coordinates(datos.km.sn) <- ~x+y
# r <- terra::rast("victoria.tif")
r <- stack("victoria.tif")
r1 <- rasterize(df, r, 'grp.num')
r2 <- raster::mask(r1,fundo)

#r2 <- rasterize(p, r, 'name', fun=max)
# plot(r, r2, cex=0.5)
# a_line = df %>% summarise() %>% st_cast("LINESTRING") # dummy linestring
# x = raster::rasterize(df, 
#                       raster::raster(df, resolution = 0.25))

# ras1 = crop(ras1, extent(x), snap = "near")

# ras1 = resample(ras1, x)
# s <- stack(ras1,x)
# plot(s)
# par(mfrow = c(1,2))
plot(r2, col = RColorBrewer::brewer.pal(8, "Spectral"))

# df2 <- raster::rasterToPoints(r2,XY=T)
df2= raster::as.data.frame(r2, xy = T) %>% na.omit()

pl = ggplot() +
  geom_raster(data = df2 , aes(x = x, y = y, fill = layer)) +
  paletteer::scale_fill_paletteer_c("ggthemes::Red-Green-Gold Diverging",
                         breaks = seq(1, 4, by = 1),
                         limits = c(1, 4),
                         labels = as.character(seq(1, 4, by = 1)))+
  theme_test();pl
# plot(a_line, add = T)
# plot(a_line, add = T)