##Punto 2

##Inciso a
require(pacman)
p_load(tidyverse, janitor,skimr, stargazer, ggplot2, rio, dplyr)
##Como estamos trabajando a partir de un proyecto en R en GitHub, no es necesario estableceer un working directory
Encuesta_Empleo <- import("ED67-70 (1).dta")
Encuesta_Empleo_1 <- filter(Encuesta_Empleo, ano %in% c(1967))

##Inciso C
view(Encuesta_Empleo_1)
class(Encuesta_Empleo_1$testudio)
class(Encuesta_Empleo_1$num_personas)
class(Encuesta_Empleo_1$tingresos)
class(Encuesta_Empleo_1$sexo)
class(Encuesta_Empleo_1$ingreso_familiar)


##inciso d
tabla1 <- Encuesta_Empleo_1[c("tingresos", "sexo", "testudio","num_personas", "ingreso_familiar")]
stargazer(tabla1,
          type= "html",
          title="Tabla 1: Estadisticas descriptivas", 
          out="descriptivas.doc")
grafica_1 <- ggplot(Encuesta_Empleo_1, aes(x=tingresos))+
  geom_histogram(color="purple4", fill="purple4", alpha=0.75, bins=25, position="identity") +
  scale_x_continuous(name = "Salario") +
  scale_y_continuous(name = "Frecuencia")+
  ggtitle("Histrograma de Salario") 
gráfica_2 <- ggplot(Encuesta_Empleo_1, aes(x=ingreso_familiar))+
  geom_histogram(color="purple4", fill="purple4", alpha=0.75, bins=25, position="identity") +
  scale_x_continuous(name = "Ingresos del hogar") +
  scale_y_continuous(name = "Frecuencia")+
  ggtitle("Histrograma de Ingresos del hogar") 
gráfica_2
gráfica_3 <- ggplot(Encuesta_Empleo_1, aes(x=testudio))+
  geom_histogram(color="blue", fill="blue", alpha=0.75,bins=25, position="identity")+
  scale_x_continuous(name="años de estudio")+
  scale_y_continuous(name="Frecuencia")+
  ggtitle("Histograma de tiempo de estudio")
gráfica_3

##inciso e

  sum(is.na(Encuesta_Empleo_1[c("tingresos")]))
  sum(is.na(Encuesta_Empleo_1[c("sexo")]))
  sum(is.na(Encuesta_Empleo_1[c("num_personas")]))
  sum(is.na(Encuesta_Empleo_1[c("testudio")]))
  sum(is.na(Encuesta_Empleo_1[c("ingreso_familiar")]))
  
##inciso f
  ##inciso 
  ##La distribución de los datos la podemos encontrar en la gráfica 1. Utilizaremos Boxplot para mirar donde se ubican los valores atípicos
  valores-atípicos <- boxplot(Encuesta_Empleo_1$tingresos)
  Quantil_99 <- quantile(Encuesta_Empleo_1$tingresos,.99, na.rm = TRUE)
  Base_encuesta_3<- Encuesta_Empleo_1 %>% filter(tingresos < Quantil_99)
  hist(Base_encuesta_3$tingresos)
  ##Distribución final
  hist(Base_encuesta_3$tingresos)
  ##inciso G
  reg_1 <- lm(tingresos ~ sexo, data = Base_encuesta_3)
  reg_2 <- lm(tingresos ~ testudio, data = Base_encuesta_3)
  reg_3 <- lm(tingresos ~ grado_universitario, data= Base_encuesta_3)
  reg_4 <- lm(tingresos ~ edad, data= Base_encuesta_3)
  stargazer(reg_1, reg_2, reg_3, reg_4,
            type = "html",
            covariate.labels = c("sexo", "años de estudio", "Grado universitario", "edad"), 
            dep.var.labels = c("Salario del individuo"),
            out = "resultados.doc")
  
  
  
