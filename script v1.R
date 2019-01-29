library(dplyr)
library(readr)
library(ggplot2)

#Obtener el directorio actual

getwd()


# Cambiar directorio actual
setwd("C:/Users/rguerrerop/Documents/Analisis R/Googleplay")

# Vemos los archivos del directorio actual
list.files(getwd())


# Asignamos a una variable el archivo
dataset <- read_csv("googleplaystore.csv")


View(head(dataset))


# Visualizar las aplicaciones más instaladas por categoría
a <- dataset %>%
  select(Category, Installs) %>%
  filter(Installs == "1,000,000,000+") %>%
  group_by(Category) %>%
  arrange(Category)

ggplot(a, aes(x = Installs, fill = Category)) +
  geom_bar(position = "dodge") + 
  coord_flip()



#  TOP 10 Cantidad de aplicaciones por categoría

c <- dataset %>%
  group_by(Category) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

c <- head(c,10)

ggplot(c , aes(x = Category, y = count)) + 
  geom_bar(stat = "identity", width=.5, fill= "firebrick4") +
  labs(title =  "Top 10 categorias") + 
  theme(axis.title.x = element_text(angle=65, vjust = 0.6))


# Obtener las instalaciones por categoría

dataset <- dataset %>%
  filter(Installs != "0")

options(scipen = 999)


# 1:
dataset$Installs <- gsub("," , "", gsub ("\\.", "", dataset$Installs))


# 2:
dataset$Installs <- as.character(dataset$Installs)

dataset$Installs <- substr(dataset$Installs,1,nchar(dataset$Installs)-1)

# 3:
dataset$Installs <- as.numeric(dataset$Installs)

dataset %>%
  group_by(Category) %>%
  summarize(totalInstalls = sum(Installs)) %>%
  arrange(desc(totalInstalls)) %>%
  head(10) %>%
  ggplot(aes(x = Category, y =totalInstalls, fill= Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10  Categorias") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



dataset %>%
  filter(Type =="Paid") %>%
  group_by(Category) %>%
  summarize(totalInstalls = sum(Installs)) %>%
  arrange(desc(totalInstalls)) %>%
  head(10) %>%
  ggplot(aes(x = Category, y = totalInstalls)) +
  geom_bar(stat = "identity", width = .5, fill="forestgreen") + 
  labs(title=" Top 10 categorías de pago") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))


View(dataset)

dataset %>%
  filter(Category == "FAMILY") %>%
  group_by(Genres) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  ggplot(aes(x = Genres, y = Count)) +
  geom_bar(stat = "identity", width = .5, fill = "gold1") +
  labs(title="Top 10 de géneros de la categoría Familia") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))


#Top 10 categorías gratuitas

dataset %>%
  filter(Type == "Free") %>%
  group_by(Category) %>%
  summarize(totalInstalls = sum(Installs)) %>%
  arrange(desc(totalInstalls)) %>%
  head(10) %>%
  ggplot(aes(x = Category, y = totalInstalls)) +
  geom_bar(stat = "identity", width = .5, fill="deepskyblue2") + 
  labs(title = "Top 10 categorías gratuitas") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))


# Cateogrías más representativas de Juegos
dataset %>%
  filter(Category == "GAME") %>%
  group_by(Genres) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  ggplot(aes(x = Genres, y = Count)) +
  geom_bar(stat = "identity", width = .5, fill="cyan2") + 
  labs(title = "Top 10 generos más representativos de categoría Juegos (Games)") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))



