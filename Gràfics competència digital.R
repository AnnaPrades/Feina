#ELABORAR GR�FICS PER COMPET�NCIA DIGITAL
setwd("~/Anna/AQU/IL/6A Enquesta 2017/Graus")
#desat a C:\Users\46234665L\Documents\Anna\AQU\IL\6a Enquesta 2017

ls() #veig que tinc objectes pel mig
rm(list=ls()) #per netejar espai

library(foreign)
library(readxl)
enq17 <- read.spss("BDD TIT INTEGRADA (sav).sav",use.value.labels = FALSE, to.data.frame = TRUE)
attr(enq17,"variable.labels") <- sapply(attr(enq17,"variable.labels"), function(x){iconv(x,"UTF-8", "ISO_8859-2")})
names(enq17)
#Seleccionem nom�s Edici� 2017
#dades17<-enq17[enq17$IL_any==2017,]

#Eliminem IL_any --> Dades nom�s del 2017 (Per complir criteris dades d'entrada per a la funci�.)
#dades17$IL_any<-NULL
#names(dades17)

#SUBSET BD AMB VARIABLES QUE VOLEM  AGRUPAR, FILTRAR I GRAFICAR
vars <- c("IL_any", "codi_ens","codi_sa_amp", "codi_a", "ninform_10", "ainform_10" , "dif_inform_10", "funcions_c_antic",  "funcions2_c_antic", "funcions_c", "funcions2_c")
digital <- enq17[colnames(enq17) %in% vars]
head(digital)
summary(digital)
str(digital) ##80512 obs. of  11 variables
class(digital) #data.frame

#Creo factor ordenat
digital$funcions_c_antic <- factor(digital$funcions_c_antic, levels = c(1, 2, 3), labels = c("Espec", "Univ", "No univ"))
table(digital$funcions_c_antic)

#convertir varialbes d'agrupaci� a factors
cols <- c(1:4)
digital[cols] <- lapply(digital[cols],factor)
str(digital)

table(digital$codi_a)
levels(digital$codi_a) <- c("Humanitats", "Socials", "Experimentals", "Salut", "Enginyeries")

table(digital$codi_sa_amp)
levels(digital$codi_sa_amp) <- sa_ampl_noms <- c("Filosofia i Hist�ria","Lleng�es", "Arts i disseny","Mitxes Hum" ,"Economia, Empresa i Turisme","Dret, laboral i pol�tiques","Comunicaci� i Documentaci�","Educaci�","Intervenci� Social","Ci�ncies biol�giques i de la terra","Cc. experimentals i matem�tiques","Infermeria i salut","Psicologia i ter�pia","Medicina i Cc. Biom�diques","Arquitectura i civil","Tecnologies industrials","TIC","Agr�cola, forestal i pesca")
table(digital$codi_sa_amp)


#Treure resultats agregats (group_by) amb summarise, filtrats pels qui fan funcions espec�fiques de titulaci� 
library(plyr) #per contar. Exemple count(bbdd, "name") ->noms i tipus begudes count(bevs, c("name", "drikn"))
library(dplyr)

#detach(package:plyr) -> I believe you've loaded plyr after dplyr, which is why you are getting an overall summary instead of a grouped summary

#t agrupada seleccionat nom�s els que fan funcions espec�fiques per treure mitjana inform�tia
t_agrupada_ambits <-
  digital %>%
  filter(funcions_c_antic == "Espec") %>%
  group_by(codi_a) %>%
  summarise(
    n = length(IL_any),
    x_ninform = round(mean(ninform_10, na.rm = TRUE),2),
    x_ainform = round(mean(ainform_10, na.rm = TRUE),2),
    x_dif_inform = round(mean(dif_inform_10, na.rm = TRUE),2)
    )


t_agrupada_ambits
sum(t_agrupada_ambits$n)

#t agrupada 2017 sub�mbits ampliats seleccionat nom�s els que fan funcions espec�fiques per treure mitjana inform�tia
t_agrup_sa_ampl <- 
  digital %>% 
  filter(IL_any == 2017 & funcions_c_antic == "Espec") %>%
  group_by(codi_sa_amp) %>%
  summarise(
    n = length(IL_any),
    x_ninform = round(mean(ninform_10, na.rm = TRUE),2),
    x_ainform = round(mean(ainform_10, na.rm = TRUE),2),
    x_dif_inform = round(mean(dif_inform_10, na.rm = TRUE),2)
  )
t_agrup_sa_ampl

table(t_agrup_sa_ampl$codi_sa_amp)
levels(t_agrup_sa_ampl$codi_sa_amp) <- sa_ampl_noms <- c("Filosofia i Hist�ria","Lleng�es", "Arts i disseny","Mitxes Hum" ,"Economia, Empresa i Turisme","Dret, laboral i pol�tiques","Comunicaci� i Documentaci�","Educaci�","Intervenci� Social","Ci�ncies biol�giques i de la terra","Cc. experimentals i matem�tiques","Infermeria i salut","Psicologia i ter�pia","Medicina i Cc. Biom�diques","Arquitectura i civil","Tecnologies industrials","TIC","Agr�cola, forestal i pesca")
table(t_agrup_sa_ampl$codi_sa_amp)

#t agrupada seleccionat nom�s els que fan funcions espec�fiques per treure mitjana inform�tia
t_agrupada_ambits_anys <-
  digital %>%
  filter(funcions_c_antic == "Espec") %>%
  group_by(IL_any, codi_a) %>%
  summarise(
    n = length(IL_any),
    x_ninform = round(mean(ninform_10, na.rm = TRUE),2),
    x_ainform = round(mean(ainform_10, na.rm = TRUE),2),
    x_dif_inform = round(mean(dif_inform_10, na.rm = TRUE),2)
  )


t_agrupada_ambits_anys
names(t_agrupada_ambits_anys)
#"IL_any"       "codi_a"       "n"            "x_ninform"    "x_ainform"    "x_dif_inform"

#GR�fic pel 2017 per �mbit
library(ggplot2)
#install.packages("reshape2")



#Gr�fic barres nivell inform�tica per Sa
names(t_agrup_sa_ampl)
t_agrup_sa_ampl %>% 
  ggplot(aes(x= codi_sa_amp, y= x_ninform)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =0.5))+
  ggtitle("Barres verticals text eix x rotat")

t_agrup_sa_ampl %>% 
  ggplot(aes(x= codi_sa_amp, y= x_ninform)) + 
  geom_bar(stat = "identity") +
  coord_flip()+
  ggtitle("Barres horitzontals")

t_agrup_sa_ampl %>% 
  ggplot(aes(x= reorder(codi_sa_amp, x_ninform), y= x_ninform)) + 
  geom_bar(stat = "identity") +
  coord_flip()+
  ggtitle("Barres horitzontals ordenades")
  
t_agrup_sa_ampl %>% 
  ggplot(aes(x= reorder(codi_sa_amp, x_ninform), y= x_ninform)) + 
  geom_bar(stat = "identity", width = 0.60) +
  scale_fill_grey() +
  theme_classic() +
  geom_text(aes(label = x_ninform), nudge_y = 2)+
  coord_flip()+
  ggtitle("Barres horitzontals ordenades i tunejades")



#Gr�fic de punts nivell d'inform�tica, segons �mbit
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x_ninform, ..count..))+
  geom_point(stat = "count", size = 4)+
  facet_grid(codi_a ~ .)

#Gr�fic de barres de nivell d'inform�tica per �mbit
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x= codi_a, y=x_ninform))+
  geom_col() + 
  coord_flip()

#Gr�fic de barres per adequaci� funcions i any
digital %>%
  ggplot(aes(x = IL_any, fill = funcions_c_antic)) +
  geom_bar(position = "fill")

#Gr�fic de barres %funcions per �mbit (com que funcions categ�rica -> bbdd digital)
# position stack (counts)
digital[!is.na(digital$funcions_c_antic), ] %>%
  ggplot(aes(codi_a, fill=funcions_c_antic))+
  geom_bar(position = "stack")

#Gr�fic de barres %funcions per �mbit (com que funcions categ�rica -> bbdd digital)
# position fill (%agrupat)
digital[!is.na(digital$funcions_c_antic), ] %>%
  ggplot(aes(x= codi_a, y=funcions_c_antic, fill=funcions_c_antic,label =funcions_c_antic))+
  geom_bar(stat = "identity")+
  geom_text(position = "fill")

#Gr�fic de barres %funcions per �mbit (com que funcions categ�rica -> bbdd digital)
# position stack (counts)
digital[!is.na(digital$funcions_c_antic), ] %>%
  ggplot(aes(codi_a, fill=funcions_c_antic))+
  geom_bar(position = "dodge")



#Gr�fic d'�mbit
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x=as.factor(codi_a))) + 
  geom_bar() + #tot surt 1
  coord_flip()+ #ho passo a horitzontal!!!!
  

#Gr�fic nivell inform�tica per �mbit, selecci� 2017
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x= codi_a, y=x_ninform)) +
  geom_bar(stat="identity") + #amb vble continua sobre discrta he deposar identity
  theme()
#f you want the heights of the bars to represent values in the data, use geom_col instead. geom_bar uses stat_count by default: it counts the number of cases at each x position. geom_col uses stat_identity
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x= codi_a, y=x_ninform)) +
  geom_col()

#Gr�fic nivell inform�tica per �mbit any 2007
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x=codi_a, y=x_ninform)) +
  geom_col()

#Gr�fic boxplot nivell inform�tica per �mbit any 2007
digital %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x= codi_a, y=ninform_10))+ 
           geom_boxplot(alpha=0) + 
            geom_jitter(alpha=0.3, color="tomato")


#Faig una barra, creant a x un factor buit, que omplo amb geombar
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x=factor(""), fill =codi_a)) + 
  geom_bar()+
  scale_x_discrete("") #amb aquesta l�nia li trec etiqueta factor("")

#Convertir-lo en rotllana
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x=factor(""), fill =codi_a)) + 
  geom_bar()+
  coord_polar(theta = "y")+
  scale_x_discrete("") #amb aquesta l�nia li trec etiqueta factor("")


#But geom_point() displays exactly the same information and doesn't require y_axis to tough zero
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x= codi_a, y=x_ninform)) +
  geom_point()


