base1 <- data.frame(columna1 = c(3, 3, 2, 3, 3),
                    columna2 = c("m", "f", "f", "f", "f"),
                    columna3 = c(2013, 2014, 2015, 2016, 2017))

data.frame(columna1 = c(3, 3, 2, 3, 3),
           columna2 = c("m", "f", "f", "f", "f"),
           columna3 = c(2013, 2014, 2015, 2016, 2017),
           stringsAsFactors = F)

base1[2, ]
base1[2:4, ]
base1[2, 3]
base1[base1$columna3 > 2014, ]

# Carga de base de datos: diabetes
library(lubridate)
library(dplyr)
library(ggplot2)
diab <- read.csv("diabetes.csv")
head(diab)
summary(diab)
dim(diab)

diab <- diab %>% filter(Pregnancies < 10) %>% select(Pregnancies, Glucose, BloodPressure, BMI, Age)
dim(diab)
data.frame(dias = days(sample(0:4,nrow(diab), replace = T)) + 
   as_date("2019-10-07") + 
   days(sample(0:4, nrow(diab), replace = T)*7)) %>% mutate(dia_sem = wday(dias)) %>% #.$dia_sem %>% table()
ggplot(aes(x = dias)) + geom_bar()


diab$dias <- days(sample(0:4,nrow(diab), replace = T)) + 
   as_date("2019-10-07") + 
   days(sample(0:4, nrow(diab), replace = T)*7)

class(diab$dias)

diab$hora_presente <- as_datetime(diab$dias + hours(8) + 
               hours(floor(runif(nrow(diab), 0, 8)))) + 
   minutes(floor(runif(nrow(diab), 0, 60)))

diab %>% ggplot(aes(x = date(hora_presente))) +
   geom_bar()

class(diab$hora_presente)
diab$hora_presente + minutes(2)

diab$hora_presente <- as.POSIXct(diab$hora_presente)
diab$hora_presente

diab <- diab %>% rowwise() %>%
   mutate(hora_atencion = hora_presente +
             minutes(abs(floor(rnorm(1, mean = 8, sd = 4)))) + 
             minutes(floor(hour(hora_presente)*runif(1,0,3)))) %>%
   mutate(mins_espera = difftime(hora_atencion, hora_presente, units = "mins")) %>%
   ungroup() #%>%
   summarise(min(mins_espera), mean(mins_espera), median(mins_espera), max(mins_espera))

names(diab)
guardar <- diab %>% select(-9)
guardar %>% names()
guardar <- guardar %>% select(-dias)

names(guardar) <- c("embarazos","glucemia","presion_art",
                    "bmi", "edad", "hora_presente","hora_atencion")

head(guardar)

guardar$embarazos <- round(guardar$embarazos / 2)
table(guardar$embarazos)

write.csv(guardar, file = "consultorios.csv", row.names = F)

consultorios <- read.csv("consultorios.csv")

consultorios %>% ggplot(aes(x = hour(as_datetime(hora_presente)),
                            y = difftime(as_datetime(hora_atencion),
                                         as_datetime(hora_presente),
                                         units = "mins"),
                            group = hour(hora_presente))) +
   geom_boxplot()






