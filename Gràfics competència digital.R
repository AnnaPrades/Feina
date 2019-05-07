#ELABORAR GRÀFICS PER COMPETÈNCIA DIGITAL
setwd("~/Anna/AQU/IL/6A Enquesta 2017/Graus")
#desat a C:\Users\46234665L\Documents\Anna\AQU\IL\6a Enquesta 2017

ls() #veig que tinc objectes pel mig
rm(list=ls()) #per netejar espai

library(foreign)
library(readxl)
enq17 <- read.spss("BDD TIT INTEGRADA (sav).sav",use.value.labels = FALSE, to.data.frame = TRUE)
attr(enq17,"variable.labels") <- sapply(attr(enq17,"variable.labels"), function(x){iconv(x,"UTF-8", "ISO_8859-2")})
names(enq17)
#Seleccionem només Edició 2017
#dades17<-enq17[enq17$IL_any==2017,]

#Eliminem IL_any --> Dades només del 2017 (Per complir criteris dades d'entrada per a la funció.)
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

#convertir varialbes d'agrupació a factors
cols <- c(1:4)
digital[cols] <- lapply(digital[cols],factor)
str(digital)

table(digital$codi_a)
levels(digital$codi_a) <- c("Humanitats", "Socials", "Experimentals", "Salut", "Enginyeries")

table(digital$codi_sa_amp)
levels(digital$codi_sa_amp) <- sa_ampl_noms <- c("Filosofia i Història","Llengües", "Arts i disseny","Mitxes Hum" ,"Economia, Empresa i Turisme","Dret, laboral i polítiques","Comunicació i Documentació","Educació","Intervenció Social","Ciències biològiques i de la terra","Cc. experimentals i matemàtiques","Infermeria i salut","Psicologia i teràpia","Medicina i Cc. Biomèdiques","Arquitectura i civil","Tecnologies industrials","TIC","Agrícola, forestal i pesca")
table(digital$codi_sa_amp)


#Treure resultats agregats (group_by) amb summarise, filtrats pels qui fan funcions específiques de titulació 
library(plyr) #per contar. Exemple count(bbdd, "name") ->noms i tipus begudes count(bevs, c("name", "drikn"))
library(dplyr)

#detach(package:plyr) -> I believe you've loaded plyr after dplyr, which is why you are getting an overall summary instead of a grouped summary

#t agrupada seleccionat només els que fan funcions específiques per treure mitjana informàtia
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

#t agrupada 2017 subàmbits ampliats seleccionat només els que fan funcions específiques per treure mitjana informàtia
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
levels(t_agrup_sa_ampl$codi_sa_amp) <- sa_ampl_noms <- c("Filosofia i Història","Llengües", "Arts i disseny","Mitxes Hum" ,"Economia, Empresa i Turisme","Dret, laboral i polítiques","Comunicació i Documentació","Educació","Intervenció Social","Ciències biològiques i de la terra","Cc. experimentals i matemàtiques","Infermeria i salut","Psicologia i teràpia","Medicina i Cc. Biomèdiques","Arquitectura i civil","Tecnologies industrials","TIC","Agrícola, forestal i pesca")
table(t_agrup_sa_ampl$codi_sa_amp)

#t agrupada seleccionat només els que fan funcions específiques per treure mitjana informàtia
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

#GRàfic pel 2017 per àmbit
library(ggplot2)
#install.packages("reshape2")



#Gràfic barres nivell informàtica per Sa
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



#Gràfic de punts nivell d'informàtica, segons àmbit
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x_ninform, ..count..))+
  geom_point(stat = "count", size = 4)+
  facet_grid(codi_a ~ .)

#Gràfic de barres de nivell d'informàtica per àmbit
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x= codi_a, y=x_ninform))+
  geom_col() + 
  coord_flip()

#Gràfic de barres per adequació funcions i any
digital %>%
  ggplot(aes(x = IL_any, fill = funcions_c_antic)) +
  geom_bar(position = "fill")

#Gràfic de barres %funcions per àmbit (com que funcions categòrica -> bbdd digital)
# position stack (counts)
digital[!is.na(digital$funcions_c_antic), ] %>%
  ggplot(aes(codi_a, fill=funcions_c_antic))+
  geom_bar(position = "stack")

#Gràfic de barres %funcions per àmbit (com que funcions categòrica -> bbdd digital)
# position fill (%agrupat)
digital[!is.na(digital$funcions_c_antic), ] %>%
  ggplot(aes(x= codi_a, y=funcions_c_antic, fill=funcions_c_antic,label =funcions_c_antic))+
  geom_bar(stat = "identity")+
  geom_text(position = "fill")

#Gràfic de barres %funcions per àmbit (com que funcions categòrica -> bbdd digital)
# position stack (counts)
digital[!is.na(digital$funcions_c_antic), ] %>%
  ggplot(aes(codi_a, fill=funcions_c_antic))+
  geom_bar(position = "dodge")



#Gràfic d'àmbit
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x=as.factor(codi_a))) + 
  geom_bar() + #tot surt 1
  coord_flip()+ #ho passo a horitzontal!!!!
  

#Gràfic nivell informàtica per àmbit, selecció 2017
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

#Gràfic nivell informàtica per àmbit any 2007
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x=codi_a, y=x_ninform)) +
  geom_col()

#Gràfic boxplot nivell informàtica per àmbit any 2007
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
  scale_x_discrete("") #amb aquesta línia li trec etiqueta factor("")

#Convertir-lo en rotllana
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x=factor(""), fill =codi_a)) + 
  geom_bar()+
  coord_polar(theta = "y")+
  scale_x_discrete("") #amb aquesta línia li trec etiqueta factor("")


#But geom_point() displays exactly the same information and doesn't require y_axis to tough zero
t_agrupada_ambits_anys %>%
  filter(IL_any == 2017) %>%
  ggplot(aes(x= codi_a, y=x_ninform)) +
  geom_point()


