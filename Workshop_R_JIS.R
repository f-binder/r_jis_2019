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
ymd_hms(consult$hora_presente)
consult$hora_presente <- ymd_hms(consult$hora_presente)

consult$hora_atencion
consult$hora_atencion <- ymd_hms(consult$hora_atencion)

head(consult)

# Crear variables ------------
# Usaremos dplyr y pipes:
# Variable nuliparidad:

consult %>% mutate(nulipar = ifelse(embarazos > 0, 0, 1)) %>% 
   select(embarazos, nulipar) %>% table()

consult <- consult %>% mutate(nulipar = ifelse(embarazos > 0, 0, 1))

# Variable "minutos de espera":
consult %>% mutate(espera = difftime(hora_atencion, hora_presente, units = "mins")) %>% head()

consult <- consult %>% mutate(espera = difftime(hora_atencion, hora_presente, units = "mins"))

# Análisis Descriptivo ============
str(consult)
summary(consult)

# Variable embarazos: formas de resumir
summary(consult$embarazos)   # no es lo más apropiado para variable cuantitativa discreta

table(consult$embarazos) %>% addmargins()

consult %>% group_by(embarazos) %>% summarise(n = n(), prop = n()/nrow(consult))

fct_count(factor(consult$embarazos), prop = TRUE) %>% arrange(-n)

# Variable glucemia
summary(consult$glucemia)


# Gráficos
consult %>% ggplot(aes(x = glucemia)) +
   geom_histogram(aes(fill = factor(ifelse(consult$embarazos > 0, 1, 0))),
                  bins = 30, position = "dodge")

consult %>% ggplot(aes(x = glucemia, group = nulipar)) +
   geom_density(aes(fill = factor(nulipar)), position = "dodge", alpha = 0.4)

consult %>% ggplot(aes(x = nulipar, y = glucemia, group = nulipar)) +
   geom_boxplot() +
   geom_jitter(color = "darkred", alpha = 0.3, width = 0.2) +
   geom_hline(yintercept = mean(consult$glucemia), linetype = "dotted", size = 1.5) +
   theme_bw() +
   labs(x = "Paridad (sí/no)", y = "Glucemia (mg/dL)")


# Analítico: comparacion de glucemias entre nulíparas y madres --------
t.test(glucemia ~ nulipar, data = consult)        # t-test
wilcox.test(glucemia ~ nulipar, data = consult)   # Mann-Whitney-Wilcoxon Test



