
library(haven)
library(tidyverse)
library(car)
library(nortest)
library(grid)
library(gridExtra)
library(ggpubr)


data = read_sav("3322.sav")

data <- data %>% select(PREFPTE, EDAD, ESCIDEOL, CONFIANZAOPOSIC, VALORALIDERES_2, VALORALIDERES_4,
                        LIDERESVALORA_1_2, ESCAIDEOLLIDERES_2)
data

data <- filter(data, data$PREFPTE == 2 | data$PREFPTE == 8) #FILTRAMOS LOS CASOS QUE PREFIEREN AYUSO O CASADO COMO PRESIDENTE
data <- subset(data, !ESCIDEOL > 97)#ELIMINAMOS LOS CASOS QUE NO SE UBICAN EN LA ESCALA IDEOLÓGICA
data <- subset(data, !CONFIANZAOPOSIC > 4)
data <- subset(data, !VALORALIDERES_2 >10)
data <- subset(data, !VALORALIDERES_4 >10)
data <- subset(data, !LIDERESVALORA_1_2>10)
data <- subset(data, !ESCAIDEOLLIDERES_2>10)

data



str(data)



#PREFERENCIA PRESIDENTE CASADO O AYUSO VS IDEOLOGÍA
ideologia <- ggplot(data = data, aes(x = as.factor(PREFPTE), y= ESCIDEOL)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="red") +
  labs(title = "",
         subtitle = "",
         caption = "") +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                           size=9),
                axis.text.y = element_text(face="bold", color="black", 
                                           size=9),
        axis.title.x = element_text(color="blue", size=9, face="bold"),
        axis.title.y = element_text(color="blue", size=7, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.caption = element_text(hjust = 1)) +
  scale_x_discrete(name = "", labels=c("2" = "Casado", "8" = "Ayuso"))+
  scale_y_continuous(name="Ubicación ideológica del entrevistado", limits=c(1, 10), breaks = seq(0, 10, by = 1))

ideologia

hist(data$ESCIDEOL)
t.test(data$ESCIDEOL, mu = 7)
qqPlot(data$ESCIDEOL)

lillie.test(data$ESCIDEOL) # si >0.05 sigue distribucion normal :)) y podemos seguir
leveneTest(data$ESCIDEOL,data$PREFPTE) #si >0,05 varianzas iguales (homocedasticidad) | <0.05 varianzas no son iguales (heterocedasticidad)
t.test(data$ESCIDEOL~data$PREFPTE, var.equal = T) # HAY DIFERENCIA CUANDO <0.05 






##PREFERENCIA PRESIDENTE CASADO O AYUSO VS EDAD
edad <- ggplot(data = data, aes(x = as.factor(PREFPTE), y= EDAD)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="red") +
  labs(title = "",
       subtitle = "",
       caption = "") +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=9),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=9),
        axis.title.x = element_text(color="blue", size=9, face="bold"),
        axis.title.y = element_text(color="blue", size=9, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.caption = element_text(hjust = 1)) +
  scale_x_discrete(name = "", labels=c("2" = "Casado", "8" = "Ayuso"))+
  scale_y_continuous(name="Edad del entrevistado", limits=c(18, 99), breaks = seq(18, 99, by = 5))


edad

lillie.test(data$EDAD) # si >0.05 sigue distribucion normal :)) y podemos seguir
leveneTest(data$EDAD,data$PREFPTE) #si >0,05 varianzas iguales (homocedasticidad) | <0.05 varianzas no son iguales (heterocedasticidad)
t.test(data$EDAD~data$PREFPTE, var.equal = T) # si <0.05 

# PREFERENCIA PRESIDENTE CASADO O AYUSO VS VALORACIÓN DE CASADO

valoracion_casado <- ggplot(data = data, aes(x = as.factor(PREFPTE), y= VALORALIDERES_2)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="red") +
  labs(title = "",
       subtitle = "",
       caption = "") +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=9),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=9),
        axis.title.x = element_text(color="blue", size=9, face="bold"),
        axis.title.y = element_text(color="blue", size=9, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.caption = element_text(hjust = 1)) +
  scale_x_discrete(name = "", labels=c("2" = "Casado", "8" = "Ayuso"))+
  scale_y_continuous(name="Valoración a Casado", limits=c(1, 10), breaks = seq(0, 10, by = 1))

valoracion_casado


lillie.test(data$VALORALIDERES_2) # si >0.05 sigue distribucion normal :)) y podemos seguir
leveneTest(data$VALORALIDERES_2,data$PREFPTE) #si >0,05 varianzas iguales (homocedasticidad) | <0.05 varianzas no son iguales (heterocedasticidad)
t.test(data$VALORALIDERES_2~data$PREFPTE, var.equal = T) # HAY DIFERENCIA CUANDO <0.05 

res<-wilcox.test(data$VALORALIDERES_2,data$PREFPTE)
res$statistic

var.test(data$VALORALIDERES_2,data$PREFPTE)


# PREFERENCIA PRESIDENTE CASADO O AYUSO VS VALORACIÓN ABASCAL

valoracion_abascal <- ggplot(data = data, aes(x = as.factor(PREFPTE), y= VALORALIDERES_4)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="red") +
  labs(title = "",
       subtitle = "",
       caption = "") +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=9),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=9),
        axis.title.x = element_text(color="blue", size=9, face="bold"),
        axis.title.y = element_text(color="blue", size=9, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.caption = element_text(hjust = 1)) +
  scale_x_discrete(name = "", labels=c("2" = "Casado", "8" = "Ayuso"))+
  scale_y_continuous(name="Valoración a Abascal", limits=c(1, 10), breaks = seq(0, 10, by = 1))

valoracion_abascal

lillie.test(data$VALORALIDERES_4) # si >0.05 sigue distribucion normal :)) y podemos seguir
leveneTest(data$VALORALIDERES_4,data$PREFPTE) #si >0,05 varianzas iguales (homocedasticidad) | <0.05 varianzas no son iguales (heterocedasticidad)
t.test(data$VALORALIDERES_4~data$PREFPTE, var.equal = T) # HAY DIFERENCIA CUANDO <0.05 





# PREFERENCIA PRESIDENTE CASADO O AYUSO VS ACTUACIÓN COVID CASADO
valoracion_casado_covid <- ggplot(data = data, aes(x = as.factor(PREFPTE), y= LIDERESVALORA_1_2)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="red") +
  labs(title = "",
       subtitle = "",
       caption = "") +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=9),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=9),
        axis.title.x = element_text(color="blue", size=9, face="bold"),
        axis.title.y = element_text(color="blue", size=8, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.caption = element_text(hjust = 1)) +
  scale_x_discrete(name = "", labels=c("2" = "Casado", "8" = "Ayuso"))+
  scale_y_continuous(name="Valoración a Casado por actuación Covid-19", limits=c(1, 10), breaks = seq(0, 10, by = 1))

valoracion_casado_covid

lillie.test(data$LIDERESVALORA_1_2) # si >0.05 sigue distribucion normal :)) y podemos seguir
leveneTest(data$LIDERESVALORA_1_2,data$PREFPTE) #si >0,05 varianzas iguales (homocedasticidad) | <0.05 varianzas no son iguales (heterocedasticidad)
t.test(data$LIDERESVALORA_1_2~data$PREFPTE, var.equal = T) # HAY DIFERENCIA CUANDO <0.05 




# PREFERENCIA PRESIDENTE CASADO O AYUSO VS CONFIANZA CASADO

prueba <- data %>%
  count(PREFPTE, CONFIANZAOPOSIC) %>% 
  group_by(PREFPTE) %>% 
  mutate(Sum=sum(n)) %>% 
  mutate(proportion = n/Sum) 



confianza_casado <- ggplot(data = prueba, aes(x = as.factor(PREFPTE), y = prueba$proportion)) + 
  geom_bar(aes(fill = as.factor(CONFIANZAOPOSIC)), position = "dodge", stat = "identity") +
  #coord_flip()+
  labs(title = "",
       subtitle = "Confianza en Casado como líder de la oposisición",
       caption = "") +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=9),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=9),
        axis.title.x = element_text(color="black", size=9, face="bold"),
        axis.title.y = element_text(color="blue", size=9, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.caption = element_text(hjust = 1),
        legend.position = "right",
        legend.text = element_text(color = "black", size = 6, face = "bold"),
        plot.subtitle = element_text(face = "bold", color = "black", size = 9, hjust = 0.5)) +
  scale_x_discrete(name = "", labels=c("2" = "Casado", "8" = "Ayuso"))+
  scale_y_continuous(name="Proporción", breaks = seq(0, 1, by = 0.1)) +
  scale_fill_discrete(name = "",
                       labels = c("Mucha", "Bastante", "Poca", "Nada")) 


confianza_casado

  
#PARA UNIR LOS GRÁFICOS
figure<- ggarrange(ideologia, edad, valoracion_casado, valoracion_abascal, 
                   valoracion_casado_covid, confianza_casado, ncol = 3, nrow = 2) 

figure <- annotate_figure(figure,
                top = text_grob("Aquellos que prefieren a Casado como Presidente del Gobierno vs prefieren a Ayuso
                  ¿El comienzo de un cisma?", 
                                color = "black", face = "bold"),
                bottom = text_grob("CIS Barómetro Mayo 2021 | @dataR_amateur",
                                   hjust = 1, x = 1, face = "italic", size = 8)) +
  theme(plot.background = element_rect(fill = "white"))

figure



figure + ggsave("media_casado_ayuso.png", width = 13, height = 8.5, dpi = 500)












