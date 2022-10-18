##Punto 2

##Inciso a
require(pacman)
p_load(tidyverse, janitor,skimr, stargazer, ggplot2, rio, dplyr)
##Como estamos trabajando a partir de un proyecto en R en GitHub, no es necesario estableceer un working directory
Encuesta_Empleo <- import("ED67-70 (1).dta")
Encuesta_Empleo_1 <- filter(Encuesta_Empleo, ano %in% c(1967))
##inciso d
tabla1 <- Encuesta_Empleo_1[c("tingresos", "edad", "sexo", "lee_escribe", "grado_universitario", "testudio","num_personas", "ingreso_familiar")]
stargazer(tabla1,
          type= "html",
          title="Tabla 1: Estadisticas descriptivas", 
          out="Descriptivas.doc")
grafica_1 <- ggplot(Encuesta_Empleo_1, aes(x=tingresos))+
  geom_histogram(color="purple4", fill="purple4", alpha=0.75, bins=25, position="identity") +
  scale_x_continuous(name = "Salario") +
  scale_y_continuous(name = "Frecuencia")+
  ggtitle("Histrograma de Salario") 
gráfica_2 <- ggplot(Encuesta_Empleo_1, aes(x=edad))+
  geom_histogram(color="purple4", fill="purple4", alpha=0.75, bins=25, position="identity") +
  scale_x_continuous(name = "edad") +
  scale_y_continuous(name = "Frecuencia")+
  ggtitle("Histrograma de Edad") 
gráfica_2
gráfica_3 <- ggplot(Encuesta_Empleo_1, aes(x=testudio))+
  geom_histogram(color="blue")

