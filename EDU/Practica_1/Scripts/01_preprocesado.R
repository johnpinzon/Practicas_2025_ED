
# librerias  --------------------------------------------------------------

library(dplyr)
library(data.table)
library(readxl)
library(ggplot2)

options(scipen = 999)


#  carga de datos  --------------------------------------------------------

datos <- fread(input = "Data/CNA2014_ENCABEZADO_15.csv", 
               sep = ",") %>% 
  select(COD_VEREDA,TIPO_UC, P_S5PAUTOS,S05_TENENCIA, P_S7P82, P_S7P84F, P_S7P85B
) %>% 
  filter(TIPO_UC == 1) %>%
  
  mutate(S05_TENENCIA = as.character(S05_TENENCIA))

str(datos)

# limpieza  ---------------------------------------------------------------

t_homologacion_7 <- readxl::read_excel(
  path = "Data/Tablasdehomologacion.xlsx",
  sheet = "Hoja2") %>% 
mutate(S05_TENENCIA = as.character(S05_TENENCIA))

str(t_homologacion_7)

datos_dep <- datos %>% 
  left_join(t_homologacion_7, by = c("S05_TENENCIA"="S05_TENENCIA") ) %>% 
  select(Predominancia, P_S7P85B) %>% 
  na.omit() 
str(datos_dep)

# TDF cualitativa ---------------------------------------------------------

tdf_S05_TENENCIA <- datos_dep %>% 
  group_by(Predominancia) %>% 
  summarise(n_i = n()) %>% 
  arrange(desc(n_i)) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_i = cumsum(f_i))

# Graficas  ---------------------------------------------------------------

barplot(table(datos_dep$Predominancia))

#esquisse
#library(esquisse)
#esquisse::esquisser(viewer = "browser")  

#Gráfico de barras
  
ggplot(datos_dep) +
  aes(x = Predominancia) +
  geom_bar(fill = "#761D1D") +
  labs(title = "Distribución de predominancia") +
  coord_flip() +
  theme_minimal()


# Tdf - variable cuantitativa  ---------------------------------------------------

#leer paquete DT, datatable

#numeros de clase 

k = round(1 + 3.3 * log10(nrow(datos_dep)))
k

#Rango 

rango = max(datos_dep$P_S7P85B, na.rm = T) - 
  min(datos_dep$P_S7P85B, na.rm = T)
rango

#longitud

longitud = rango /k
longitud

#cortes 
cortes = min(datos$P_S7P85B,na.rm = T) + c(seq(0,k,1))*longitud
cortes

#TDF - Leche 

tdf_P_S7P85B <- datos_dep %>% 
  mutate(P_S7P85B_C = as.factor(cut(P_S7P85B,
                          breaks = cortes,
                          levels = cortes,
                          include.lowest = T,
                          dig.lab = 6))) %>% 
  group_by(P_S7P85B_C, .drop = F, .add = F) %>% 
  summarise(n_i = n()) %>% 
mutate(N_1 = cumsum(n_i),
       f_i = n_i/sum(n_i),
       F_i = cumsum(f_i),
       x_i = cortes[1:k] + longitud/2,
       c_i = abs(cortes[1:k] - cortes [2 : (k+1)]),
       d_i = n_i/c_i
       ) 
#










