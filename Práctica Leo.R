#EJERCICIO 1.Crea un data frame o tibble a partir de los datos del fichero 
#penguins. Contiene información sobre ejemplares de pingüinos:

getwd()
setwd("C:/Users/guzzm/Desktop/LEO - HERRAMIENTAS DE PROGAMACION PARA LA CIENCIA DE DATOS/TRABAJO LEO")

library(readr)
library(dplyr)
library(janitor)


penguins <- read_csv2("penguins")
penguins

#EJERCICIO 2.Responde a las siguientes preguntas:
#  • ¿Cuántas especies distintas hay?

n_distinct(penguins$species)
unique(penguins$species)

#  • ¿De cuántas islas distintas hay datos?

glimpse(penguins)
n_distinct(penguins$island)

#  • ¿Están todas las especies en todas las islas?

penguins %>% 
  group_by(island) %>% 
  summarise(especies = n_distinct(species))

#EJERCICIO 3. En el ejercicio 2 has visto que en la isla Torgersen solo hay 
#una especie. ¿Cuál es?

penguins %>% 
   filter(island == "Torgersen") %>% 
   summarise(unique(species))

#EJERCICIO 4. Teniendo en cuenta que cada fila es un pingüino, calcula cuántos 
#pingüinos de cada especie hay.

glimpse(penguins)
?count

penguins %>% 
  filter(species == "Adelie") %>% 
nrow()


penguins %>% 
  filter(species == "Chinstrap") %>% 
  nrow()


penguins %>% 
  filter(species == "Gentoo") %>% 
  nrow()

#así lo hace alberto
penguins %>% 
  group_by(species) %>% 
  summarise(numero = n())

#EJERCICIO 5. Calcula la media de la longitud y la profundidad del pico en
#función de cada especie. Pista. Ninguna de las medias es NA.
glimpse(penguins)
#bill_length_mm
#bill_depth_mm

penguins %>% 
  group_by(species) %>% 
  summarise(media = mean(bill_length_mm,na.rm = TRUE), profundidad = mean(bill_depth_mm, na.rm = TRUE))

#EJERCICIO 6. Calcula la mediana de la longitud del ala para la especie Adelie 
#en cada isla y en función del sexo. Quita los casos en los que sex es NA. 
#Replica el siguiente gráfico, que tiene todos esos datos coloreando cada columna
#en función del sexo. Necesitarás jugar con la posicion de las columnas. Explica
#qué conclusiones sacas a la vista del gráfico. Pista. De nuevo, ninguno de estos
#datos es NA.

#flipper_length_mm

df_ejercicio6 <- penguins %>% 
  filter(!is.na(sex)) 

df_ejercicio6 %>%
  group_by(island, sex) %>% 
  filter(species == "Adelie") %>% 
  summarise(mediana = median(flipper_length_mm, na.rm = TRUE))
  
library(ggplot2)

ggplot(df_ejercicio6) + 
  geom_col(aes(x= island, y = flipper_length_mm,fill = sex), position = "dodge")

#EJERCICIO 7. Más adelante vas a hacer un modelo con estos datos, concretamente,
#con las columnas bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g.
#Antes de eso, tienes que reemplazar los NA. Para ello, calcula las medias de las
#cuatro columnas y utilízalas para reemplazar los NA. Puedes hacerlo como quieras,
#pero si lo haces con un bucle, tendrás la máxima puntuación en este ejercicio. 
#Si no, no :)
#Pista. El bucle yo lo he planteado tratanto a las columnas como vectores (o sea,
#no lo planteo con dplyr)

library(purrr)

cuatro_col <- penguins[3:6]

ejercicio7 <- lapply(cuatro_col, funcion_7)

funcion_7 <- function(objeto){
  objeto[is.na(objeto)] <- mean(objeto, na.rm = TRUE)
  return(objeto)
}

funcion_7 

ejercicio7 #este ejercicio es muy importante para el examen final
#el 8 tb 































#CLASE EXTRA LEO
iris

#Crea una función que reste la mediana de una columna que se especifique

#recibe el nombre de la columna -> la detecta en iris -> calcula su mediana
# -> le resta su mediana -> devuelve iris con el cambio

resta_mediana <- function (columna){
  mediana <- median(iris[[columna]])
  iris[[columna]] <- iris[[columna]] - mediana
  
  return(iris)
}
  
resta_mediana("Sepal.Length")


#recibe el nombre de una columna:
resta_mediana("Sepal.Length")

#calculo la mediana:
mediana <- median(iris[["Sepal.Length"]])

#le resto la mediana:
iris[["Sepal.Length"]]) <- iris[["Sepal.Length"]]) - mediana

#todo esto lo meto en la funcion "resta_mediana"

resta_mediana_generica <- function(dataframe_cualquiera, columna_incognita){
  mediana <- median(dataframe_cualquiera[[columna_incognita]])
  dataframe_cualquiera[[columna_incognita]] <- dataframe_cualquiera[[columna_incognita]] - mediana
  return(dataframe_cualquiera)
  }

resta_mediana_generica(iris, "Sepal.Length")



#para el ejercicio 9
grupos <- 2:10

for (i in grupos){
  print(i)
}

fit <- kmeans(penguins, k)
fit$tot.withinss


fit <- kmeans(penguins, 3)
fit$tot.withinss
fit <- kmeans(penguins, 4)
fit$tot.withinss
fit <- kmeans(penguins, 5)
fit$tot.withinss

for (df in list(df1,df2,df3,df4)){
  #hago cierta operación sobre la columna 1 de cada data frame
  df[[1]] - mean(df[[1]])
}


#hola