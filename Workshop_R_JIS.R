# Workshop: R para análisis de Datos -----------
# Jornadas de Informática en Salud
# Fernando Binder - 29/11/2019

  # Introducción: Instalar paquetes necesarios =================
install.packages(c("tidyverse", "lubridate", "forcats", "ggplot2"))

  # Objetos: variables, vectores y data.frames =========
# Variables y comando para asignar
a <- 5
b <- 3

a

a + b
a / b

# Vectores. Clases que veremos en este taller: numeric, character, logical
# Creación de vectores con función "concatenar" c()
v1 <- c(1, 3, 5, 7, 9)
v2 <- c("Lucia", "Rocamadour", "Ronald", "Babs")
v3 <- c(TRUE, FALSE, FALSE)

class(v3)

# Acceder a Elementos de un vector
vect <- c(3,3,4,5,9)
vect
vect[4]
vect[1:3]      # elementos 1, 2 y 3
vect[c(1,4)]   # elementos 1 y 4
vect[-4]       # todos menos el 4to elemento

vect[c(TRUE, TRUE, FALSE, FALSE, FALSE)]   # acceder a elementos usando un vector lógico
vect[c(FALSE, FALSE, FALSE, FALSE, FALSE)]

vect[vect == 3]  # accede a elementos que cumplen la condición
vect[vect > 4]

# Acceder a elementos de una tabla (usaremos un objeto tipo data.frame)

base1 <- data.frame(columna1 = c(3, 3, 2, 3, 3),
                    columna2 = c("m", "f", "f", "f", "f"),
                    columna3 = c(2013, 2014, 2015, 2016, 2017),
                    stringsAsFactors = F)

  # Acceder una columna o variable: $
base1
base1$columna1    # en R "base" (sin usar dplyr), se accede así a variables

  # Operaciones con una variable de una tabla:
base1
mean(base1$columna1)
max(base1$columna3)
table(base1$columna2)


  # Acceder a subsets del data.frame usando [ , ]
base1[3, ]  # código que accesa la 3era fila de la tabla
base1[, 2:3]  # 2nda y 3ra columna de la tabla
base1[base1$columna3 > 2014, ]  # Filas que cumplan con la condición
base1[base1$columna3 > 2014 & base1$columna1 == 3, ]

  # Seleccionar subsets de una tabla usando dplyr y pipes %>%
library(dplyr)
base1
base1 %>% select(1,3)
base1 %>% filter(columna3 > 2015)

base1 %>% filter(columna3 > 2014) %>% select(columna1, columna3)


  # Análisis de datos: Base de consultorio de diabetes ==================
# Cargar paquetes necesarios
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)

# Carga base de datos: consultorios.csv -------
getwd()
consult <- read.csv("consultorios.csv", stringsAsFactors = FALSE)
dim(consult)   # dimensiones: filas columnas

head(consult)  # Primeras filas
str(consult)   # Estructura de base de datos
summary(consult)

# Modificar clases de variables: fechas ---------
consult$hora_presente
lubridate::ymd_hms(consult$hora_presente)
consult$hora_presente <- ymd_hms(consult$hora_presente)

consult$hora_atencion
consult$hora_atencion <- ymd_hms(consult$hora_atencion)

head(consult)

# Crear variables: usando dplyr (pipes), función mutate ------------
# Variable nuliparidad (nuligesta):

consult %>% mutate(nuligest = ifelse(embarazos > 0, 0, 1)) %>% 
   select(embarazos, nuligest) #%>% table()

consult <- consult %>% mutate(nuligest = ifelse(embarazos > 0, 0, 1))

# Variable "minutos de espera":
consult %>% mutate(espera = difftime(hora_atencion,
                                     hora_presente,
                                     units = "mins")) %>% head()

consult <- consult %>% mutate(espera = difftime(hora_atencion,
                                                hora_presente,
                                                units = "mins"))

# Variable "semana del año"
consult %>% mutate(semana = week(hora_presente)) %>% head()

consult <- consult %>% mutate(semana = week(hora_presente))

# Análisis Descriptivo ============
str(consult)
summary(consult)

  # Variable embarazos ---------
summary(consult$embarazos)   # no es lo más apropiado para variable cuantitativa discreta

table(consult$embarazos) %>% addmargins()

consult %>% group_by(embarazos) %>% summarise(n = n(), prop = n()/nrow(consult))

fct_count(factor(consult$embarazos), prop = TRUE) %>% arrange(-n)

  # Variables tiempos de espera --------
summary(consult$espera)
summary(as.numeric(consult$espera))

# tiempos de espera: boxplot por semana del año (paquete ggplot2)
names(consult)

library(ggplot2)
consult %>% ggplot(aes(x = semana, y = espera)) +
   geom_boxplot(aes(group = semana))

consult %>% ggplot(aes(x = semana, y = espera)) +
   geom_boxplot(aes(group = semana), fill = "coral", alpha = 0.5) +
   labs(y = "Espera a la atención médica (mins)",
        x = "Semana del año") +
   theme_minimal() +
   theme(axis.title = element_text(size = 14))


  # Variable glucemia ------------
summary(consult$glucemia)

# Gráficos con paquete ggplot2
  # ggplot(aes(x = ..., y = ...))  define los ejes del gráfico
consult %>% ggplot(aes(x = glucemia, fill = factor(nuligest))) +
   geom_histogram(bins = 30, position = "dodge")

consult %>% ggplot(aes(x = glucemia, group = nuligest)) +
   geom_density(aes(fill = factor(nuligest)), position = "dodge", alpha = 0.4)

consult %>% ggplot(aes(x = nuligest, y = glucemia, group = nuligest)) +
   geom_boxplot(aes(fill = nuligest), alpha = 0.8) +
   geom_jitter(color = "darkslategray", alpha = 0.3, width = 0.2) +
   geom_hline(yintercept = mean(consult$glucemia), linetype = "dotted", size = 1.5) +
   theme_bw() +
   labs(x = "Paridad (sí/no)", y = "Glucemia (mg/dL)")


# Analítico: comparacion de glucemias entre nulíparas y madres --------
t.test(glucemia ~ nuligest, data = consult)        # t-test
wilcox.test(glucemia ~ nuligest, data = consult)   # Mann-Whitney-Wilcoxon Test

# Fin de Workshop R DIS
