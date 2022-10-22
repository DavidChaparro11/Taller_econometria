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
  ##distribución previa
  hist(Encuesta_Empleo_1$tingresos, main = "Distribución antes de quitar val. atipicos", xlab = "ingresos individuales")
  ##La distribución de los datos la podemos encontrar en la gráfica 1. Utilizaremos Boxplot para mirar donde se ubican los valores atípicos
  valores-atípicos <- boxplot(Encuesta_Empleo_1$tingresos)
  Quantil_99 <- quantile(Encuesta_Empleo_1$tingresos,.99, na.rm = TRUE)
  Base_encuesta_3<- Encuesta_Empleo_1 %>% filter(tingresos < Quantil_99)
  hist(Base_encuesta_3$tingresos)
  
  ##Distribución final
  hist(Base_encuesta_3$tingresos, main = "Distribución después de quitar val. atipicos", xlab = "ingresos individuales")

  
  ##eliminamos valores atípicos de la variable testudio, esta variable toma valores de 99 cuando no hay información.
  Base_encuesta_4 <- filter(Base_encuesta_3, testudio<99)
  ##Cambiamos valores de testudio de -1 (cuando no estudiaron) por ceros
  Base_encuesta_5 <- Base_encuesta_4 %>% mutate(testudio = ifelse(testudio == -1, 0, testudio))
  hist(Base_encuesta_5$testudio)
##Cambiaremos los valores de edad que toman el valor de -1 (niños menores a un año) por 0
  Base_arreglada <- Base_encuesta_5 %>% mutate(edad = ifelse(edad == -1, 0, edad))
  hist(Base_arreglada$edad)
  ##Hacemos lo mismo para el percentil 99 de la variable ingreso_familiar, que la escogimos para hacer nuestra rergresión
  
quantil_99_familiar <- quantile(Base_arreglada$ingreso_familiar,.99, na.rm=TRUE)  
Base_final <- Base_arreglada %>% filter (ingreso_familiar < quantil_99_familiar)

  ##inciso G
##Depués de limpiar la base, corremos las regresiones relevantes.
  
  reg_1 <- lm(tingresos ~ sexo, data = Base_final)
  reg_2 <- lm(tingresos ~ testudio, data = Base_final)
  reg_3 <- lm(tingresos ~ grado_universitario, data= Base_final)
  reg_4 <- lm(tingresos ~ edad, data= Base_final)
  stargazer(reg_1, reg_2, reg_3, reg_4,
            type = "html",
            covariate.labels = c("sexo", "años de estudio", "Grado universitario", "edad"), 
            dep.var.labels = c("Salario del individuo"),
            out = "resultados.doc")
  
  ##incisio i
  
  ##primero corremos la regresión múltiple conjuntamente
  reg_5 <- lm(tingresos ~ testudio + edad, data= Base_final)
  stargazer(reg_5,
            type= "html",
            covariate.labels = c("años de estudio", "edad"),
            dep.var.labels = c("salario del individuo"),
            out= "reg_multiple_1")
  
  ##Regresión múltiple
  #i
  reg_6 <- lm(testudio ~ edad, data= Base_final)
  resid1 <- resid(reg_6)
  #ii.
  reg_model <- lm(tingresos ~ resid1, data= Base_final)
  summary (reg_model)
  ##iii
  reg_7 <-  lm(tingresos ~ edad, data= Base_final) 
  resid2 <- resid(reg_7)
  #iv.
  reg_part <- lm(resid2 ~ resid1)
  summary (reg_part)
  ##exportar resultados en conjunto
  stargazer(reg_5, reg_part, title="Punto I", type="html", summary=FALSE, rownames=FALSE, out="punto(I).doc")
  
  ##inciso j
  ##creamos la variable testudio^2
  reg_10 <- lm(tingresos ~ sexo + testudio + testudio^2 + edad + dias + grado_universitario, data = Base_final)
  summary(reg_10)
  stargazer(reg_10, title="Regresión lineal múltiple", type="html",
            covariate.labels = c("sexo", "años de estudio", "años de estudio al cuadrado", "edad", "dias trabajados", "titulo universitario"),
            dep.var.labels = c("ingreso del individuo") 
            , out="Regresión lineal múltiple punto J.doc")
  
