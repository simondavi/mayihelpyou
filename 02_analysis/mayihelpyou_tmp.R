# packages

library(psych)
library(tidyverse)
library(Hmisc)
library(lavaan)

# daten einlesen
# faktoren struktur
# skalen bilden
# missing values
# sem

#### ----------------- (1) Read Data and Data Management ----------------- ####

setwd("C:/Users/simon3/PowerFolders/Forschungsmodul_SS24/03_Datenauswertung/00_Daten")

data <- read.csv("survey_525247_R_data_file.csv")

# Factor vignette, dummy for each vignette
data <- data %>%
  dplyr::mutate(vig = case_when((random == 1) ~ 1,
                                (random == 2) ~ 2,
                                (random == 3) ~ 3,
                                TRUE ~ as.numeric(NA)),
                vig1 = ifelse(vig == 1, 1, 0),
                vig2 = ifelse(vig == 2, 1, 0),
                vig3 = ifelse(vig == 3, 1, 0))


data$vig <- as.factor(data$vig)

### Attribution 

data$int <- data$int_1 # internality
data$sta <- data$sta_1 # stability
data$glo <- data$glo_1 # globality

data$con <- data$kon_1 # controllability


# Attribution style: Int, Sta, Glo, Con
data <- data %>% 
  dplyr::mutate(isgc = rowMeans(subset(data, select = c(int, sta, glo, con)), na.rm = TRUE))

isgc_alpha <- data %>% 
  dplyr::select(int, sta, glo, con) %>%
  psych::alpha(title = "isgc_alpha")

data <- data %>% 
  dplyr::mutate(isc = rowMeans(subset(data, select = c(int, sta, con)), na.rm = TRUE))

isc_alpha <- data %>% 
  dplyr::select(int, sta, con) %>%
  psych::alpha(title = "isc_alpha")

# Negative affect, positive affect
data <- data %>% 
  dplyr::mutate(pos_aff = rowMeans(subset(data, select = c(affekt_1, affekt_3, 
                                                           affekt_4, affekt_6, 
                                                           affekt_10, affekt_11,
                                                           affekt_12, affekt_15)),
                                   na.rm = TRUE),
                neg_aff = rowMeans(subset(data, select = c(affekt_2, affekt_5, 
                                                           affekt_7, affekt_8, 
                                                           affekt_13, affekt_14,
                                                           affekt_16)),
                                   na.rm = TRUE))

data$ang <- data$affekt_5 # Anger
data$gui <- data$affekt_7 # Guilt
data$sha <- data$affekt_14 # Shame

# Notiz:
# Welche Emotionen?

# Self Efficacy
data <- data %>% 
  dplyr::mutate(seff = rowMeans(subset(data, select = c(seffsab_1, seffsab_2, 
                                                        seffsab_20, seffsab_14, 
                                                        seffsab_6, seffsab_28,
                                                        seffsab_48, seffsab_39,
                                                        seffsab_46, seffsab_47,
                                                        seffsab_4, seffsab_29,
                                                        seffsab_44, seffsab_27,
                                                        seffsab_42, seffsab_9)),
                                na.rm = TRUE))

# Self Efficacy, Factor 1: Pedagogical Practices
data <- data %>% 
  dplyr::mutate(seff_f1 = rowMeans(subset(data, select = c(seffsab_1, seffsab_2, 
                                                        seffsab_20, seffsab_14, 
                                                        seffsab_6, seffsab_28,
                                                        seffsab_48, seffsab_39,
                                                        seffsab_46, seffsab_47,
                                                        seffsab_4)),
                                na.rm = TRUE))

# Self Efficacy, Factor 2: Relationships
data <- data %>% 
  dplyr::mutate(seff_f2 = rowMeans(subset(data, select = c(seffsab_29,seffsab_44, 
                                                        seffsab_27,seffsab_42, 
                                                        seffsab_9)),
                                na.rm = TRUE))


# Social Support

## Informational support: "Hinweise und Ratschläge"
data <- data %>% 
  dplyr::mutate(inf_sup = rowMeans(subset(data, select = c(sozunt_inf1, sozunt_inf2, 
                                                           sozunt_inf3, sozunt_inf4, 
                                                           sozunt_inf5)),
                                   na.rm = TRUE),
                
## Instrumental support: "Güter und Materialien"                
              x_ins_sup = rowMeans(subset(data, select = c(sozunt_ins1, sozunt_ins2,
                                                           sozunt_ins3, sozunt_ins4,
                                                           sozunt_ins5)),
                                 na.rm = TRUE),


## Emotional support: "Trost und Nähe"
                emo_sup = rowMeans(subset(data, select = c(# sozunt_emo1, 
                                                           # sozunt_emo2,
                                                           sozunt_emo3, sozunt_emo4,
                                                           sozunt_emo5)),
                                   na.rm = TRUE))

# Reliability 

inf_sup_alpha <- data %>% 
  dplyr::select(sozunt_inf1, sozunt_inf2, sozunt_inf3, sozunt_inf4, 
                sozunt_inf5) %>%
  psych::alpha(title = "sozunt_inf")

emo_sup_alpha <- data %>%  
  dplyr::select(sozunt_emo3, sozunt_emo4, # sozunt_emo1, _emo2 ggf ausschl. (siehe EFA)
                sozunt_emo5) %>%
  psych::alpha(title = "sozunt_emo")

seff_alpha <- data %>%  
  dplyr::select(seffsab_1, seffsab_2, seffsab_20, seffsab_14, seffsab_6, 
                seffsab_28, seffsab_48, seffsab_39, seffsab_46, seffsab_47,
                seffsab_4, seffsab_29, seffsab_44, seffsab_27, seffsab_42, 
                seffsab_9) %>%
  psych::alpha(title = "seff_sab")

# Notiz:
# Auf die Erfassung der *instrumentellen* Unterstützung von Seiten des Mentors 
# wurde verzichtet, da die einschlägige Literatur zum Mentoring diese Form 
# der Unterstützung bisher kaum thematisiert (diese Einschränkung wird im 
# Diskussionsteil angesprochen). 


# check (Kein Misserfolg: 1, 2, 3) - Auschluss von Personen, die keinen ME sahen

# data <- data %>% 
# dplyr::filter(check_1 == 1 | check_1 == 2 | check_1 == 3) 

# data <- data %>% 
#   dplyr::filter(vig == 3) 

# save data
# write.csv(data, "data.csv")

# Mediation

model <- ' # direct effect
             emo_sup ~ c*seff_f2 + seff_f1
           # mediator
             ang ~ a*seff_f2  + seff_f1
             emo_sup ~ b*ang
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)'

model <- ' # direct effect
             inf_sup ~ c*seff_f2 + seff_f1
           # mediator
             isc ~ a*seff_f2  + seff_f1
             inf_sup ~ b*isc
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)'

model <- ' # direct effect 1
             emo_sup ~ cemo*con
           # direct effect 1
             inf_sup ~ cinf*con
             
           # mediator 1: ang
             ang ~ cang*con
             emo_sup ~ angemo*ang
             inf_sup ~ anginf*ang
           # mediator 2: gui
             gui ~ cgui*con
             emo_sup ~ guiemo*gui
             inf_sup ~ guiinf*gui
             
           # indirect effect (ang/gui --> emo)
             indemo_ang := cang*angemo
             indemo_gui := cgui*guiemo
             
           # indirect effect (ang/gui --> inf)
             indinf_ang := cang*anginf
             indinf_gui := cgui*guiinf
             
           # total effect (emo)
             total_angemo := cemo + (cang*angemo)
             total_guiemo := cemo + (cgui*guiemo)
           
           # total effect (inf)
             total_anginf := cinf + (cang*anginf)
             total_guiinf := cinf + (cgui*guiemo)
         '

fit <- lavaan::sem(model, data = data, missing = "FIML")

summary(fit)

# int

model <- ' # direct effect 1
             emo_sup ~ cemo*int
           # direct effect 1
             inf_sup ~ cinf*int
             
           # mediator 1: ang
             ang ~ cang*int
             emo_sup ~ angemo*ang
             inf_sup ~ anginf*ang
           # mediator 2: gui
             gui ~ cgui*int
             emo_sup ~ guiemo*gui
             inf_sup ~ guiinf*gui
             
           # indirect effect (ang/gui --> emo)
             indemo_ang := cang*angemo
             indemo_gui := cgui*guiemo
             
           # indirect effect (ang/gui --> inf)
             indinf_ang := cang*anginf
             indinf_gui := cgui*guiinf
             
           # total effect (emo)
             total_angemo := cemo + (cang*angemo)
             total_guiemo := cemo + (cgui*guiemo)
           
           # total effect (inf)
             total_anginf := cinf + (cang*anginf)
             total_guiinf := cinf + (cgui*guiemo)
         '
