
#Verificar que tenemos las librerias instaladas
packages <- c("dplyr","readr","ggplot2")

if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{install.packages(setdiff(packages, rownames(installed.packages())))}

#Importar las librerías
library(dplyr)
library(readr)
library(ggplot2)


#Obtener el directorio actual
getwd()

# Cambiar directorio actual donde se encuentra el archivo
setwd("C:/Users/rguerrerop/Documents/Analisis R/Googleplay")

# Vemos los archivos del directorio actual
list.files(getwd())

# cargamos el archivo de Excel en un dataframe que llamaremos "archivo"
archivo <- read_csv("googleplaystore.csv",  col_types = cols(.default = "c"))

# Veamos las primeras filas y columnas del archivo
View(head(archivo))


# Visualizar las aplicaciones que tienen mas de mil millones de instalaciones por categoría
archivo %>%
  select(Category, Installs) %>%
  filter(Installs == "1,000,000,000+") %>%
  group_by(Category) %>%
  arrange(desc(Installs)) %>%
  ggplot(aes(x = Installs, fill = Category)) +
  geom_bar(position = "dodge") + 
  coord_flip()



#  Obtener el top 10 del Conteo de aplicaciones por cada categoría 

archivo %>%
  group_by(Category) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(x = Category, y = count)) + 
  geom_bar(stat = "identity", width=.5, fill= "firebrick4") +
  labs(title =  "Top 10 categorias") + 
  theme(axis.title.x = element_text(angle=65, vjust = 0.6))



# Deshabilitar la notación científica para los números grandes 
options(scipen = 999)

# Quitar el Signo de "+" y la comma para convertir la cantidad de instalaciones en Número y poder graficar
archivo <- archivo %>%
  filter(Installs != "0")
View(archivo)
archivo$Installs <- gsub("," , "", gsub ("\\.", "", archivo$Installs))
archivo$Installs <- as.character(archivo$Installs)
archivo$Installs <- substr(archivo$Installs,1,nchar(archivo$Installs)-1)
archivo$Installs <- as.numeric(archivo$Installs)




# Mostrar en un gráfico el top de Instalaciones hechas por categoría
archivo %>%
  group_by(Category) %>%
  summarize(totalInstalls = sum(Installs)) %>%
  arrange(desc(totalInstalls)) %>%
  head(10) %>%
  ggplot(aes(x = Category, y =totalInstalls, fill= Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10  Categorias") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())




# Cuales son las aplicaciones de pago con más descargas?

archivo %>%
  filter(Type =="Paid") %>%
  group_by(Category) %>%
  summarize(totalInstalls = sum(Installs)) %>%
  arrange(desc(totalInstalls)) %>%
  head(10) %>%
  ggplot(aes(x = Category, y = totalInstalls)) +
  geom_bar(stat = "identity", width = .5, fill="forestgreen") + 
  labs(title=" Top 10 categorías de pago") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))


# De las aplcaciones de paga Familia ¿Cuál es el genero de la categoría que más vende?

archivo %>%
  filter(Category == "FAMILY" & Type =="Paid") %>%
  group_by(Genres) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  ggplot(aes(x = Genres, y = Count)) +
  geom_bar(stat = "identity", width = .5, fill = "gold1") +
  labs(title="Top 10 de géneros de la categoría Familia") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))




# Top 10 categorías gratuitas

  archivo %>%
    filter(Type == "Free") %>%
    group_by(Category) %>%
    summarize(totalInstalls = sum(Installs)) %>%
    arrange(desc(totalInstalls)) %>%
    head(10) %>%
    ggplot(aes(x = Category, y = totalInstalls)) +
    geom_bar(stat = "identity", width = .5, fill="deepskyblue2") + 
    labs(title = "Top 10 categorías gratuitas") +
    theme(axis.text.x = element_text(angle = 65, vjust = 0.6))
  

# Cateogrías más instalaciones de Juegos, cuales son los generos con mayores ins
  archivo %>%
    filter(Category == "GAME" & Type == "Free") %>%
    group_by(Genres) %>%
    summarize(totalInstalls = sum(Installs)) %>%
    arrange(desc(totalInstalls)) %>%
    head(10) %>%
    ggplot(aes(x = Genres, y = totalInstalls)) +
    geom_bar(stat = "identity", width = .5, fill="cyan2") + 
    labs(title = "Top 10 generos más representativos de categoría Juegos (Games)") +
    theme(axis.text.x = element_text(angle = 65, vjust = 0.6))
  


