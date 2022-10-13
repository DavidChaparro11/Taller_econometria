##Punto 2

##Inciso a
require(pacman)
p_load(tidyverse, janitor,skimr, stargazer, ggplot2, rio, dplyr)
##Como estamos trabajando a partir de un proyecto en R en GitHub, no es necesario estableceer un working directory
Encuesta_Empleo <- import("ED67-70 (1).dta")
Encuesta_Empleo_1 <- filter(Encuesta_Empleo, ano %in% c(1967))
