########################################################################
################### Laster inn pakker (setup) ##########################
########################################################################
rm(list = ls()) 

options(scipen=10) 
options(digits=5) 

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(fst))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(modelsummary))
suppressPackageStartupMessages(library(huxtable))
suppressPackageStartupMessages(library(magrittr))

Sys.setlocale(locale="no_NO")

loadfonts()
library("showtext") 
font_add("LM Roman 10", regular = "lmroman10-regular.otf")

########################################################################
########################### Plot Theme #################################
########################################################################


bachelor<-theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "#375B73"),
                plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "#5D6D7E"),
                plot.caption = element_text(family = "LM Roman 10", color = "#5D6D7E"),
                axis.ticks = element_blank(),
                plot.tag = element_text(hjust = 0.5, family = "LM Roman 10", color = "#5D6D7E"),
                axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "#5D6D7E"),
                axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "#5D6D7E"),
                axis.text.x = element_text(angle = 0, size = 7, face = "italic", family = "LM Roman 10", color = "#5D6D7E"),
                axis.text.y = element_text(size = 7, face = "italic", family = "LM Roman 10", color = "#5D6D7E"),
                legend.text = element_text(size = 7, face = "italic", family = "LM Roman 10", color = "#5D6D7E"),
                legend.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "#5D6D7E"))+
  theme(legend.position = "bottom")

########################################################################
##################### Henter ut nødvendig data #########################
########################################################################

NSD3127 <- read_csv("data/Norsk medborgerpanel runde 26, 2023/NSD3127.csv")

# Henter ut alle variabler som begynner på r26 

dataNSD <- NSD3127 %>%
  select(starts_with(c("r26")))

# Velger ut nødvendige veriabler for videre "rydding".        

dataNSD <- dataNSD %>%
  select(r26P1, # Kjønn
         r26_bgciv, # Sivilstatus
         r26P4_2, # Utdanningsnivå
         r26P5_1, # Alder
         r26_bgday, # Arbeidstatus
         r26k2_bginc, # Brutto årsinntekt
         r26_pceco, # Tilfreds med egen økonomi i dag
         r26_pcldi) # Liker/Misliker innvandrere


########################################################################
########### Holdninger til innvandring, Kontinuerlig ###################
########################################################################
# Kategori: Politikk og Politiske holdninger
# Liker/Misliker innvandring.

# fra negativ til positiv (tolkning)
antall_verdier_r26_pcldi <- count(dataNSD, r26_pcldi)
print(antall_verdier_r26_pcldi)

# Filtrerer ut verdi 97 "ikke svart", verdi 98 "ikke spurt"
dataNSD <- dataNSD %>% 
  filter(r26_pcldi != 97 & r26_pcldi != 98)

dataNSD <- dataNSD %>%
  mutate(Holdning_Innvandring = r26_pcldi)

# Teller antall deltakere for hver verdi av Holdning_Innvandring
Teller_Holdning_Innvandring <- count(dataNSD, Holdning_Innvandring)

# Beregner prosentandel av deltakere for hvert svaralternativ
Teller_Holdning_Innvandring$Prosentandel <- (Teller_Holdning_Innvandring$n / sum(Teller_Holdning_Innvandring$n)) * 100

# plot 
ggplot(Teller_Holdning_Innvandring, aes(x = Holdning_Innvandring, y = Prosentandel)) +
  geom_bar(stat = "identity", fill = "#40598A", width = 0.7, position = "identity", alpha = 0.8) + 
  geom_text(aes(label = paste0(round(Prosentandel), "%")), vjust = -0.5, size = 3, color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(0, 7, by = 1)) +
  labs(title = "Var. Holdning_Innvanding",
       subtitle= "Andel svart i prosent",
       x = "Holdninger til innvandring",
       y = "Prosentandel av deltakere",
       caption = "Norsk medborgerpanel runde 26") +
  theme_minimal() +
  bachelor

##########################################################################
##########sosio-økonomiske variabler, demografiske/standard ##############
##########################################################################

##########################################################################
#################### Kjønn, Binær/Dikotom ################################
##########################################################################
# Lager en dummy-variabel for kjønn, kvinne og mann.
# referanse, kvinne.
# Kjønnsdummy (1 for mann, 0 for kvinne)
dataNSD$Kjønn <- dataNSD$r26P1

dataNSD <- dataNSD %>%
  mutate(Kjønn = ifelse(Kjønn == 1, 1, 0))

# Teller antall deltakere for hver verdi av kjønn
# Beregner antall menn og kvinner
Antall_kjønn <- dataNSD %>%
  count(Kjønn) %>%
  mutate(Prosentandel = n / sum(n) * 100)

# Plot
ggplot(Antall_kjønn, aes(x = factor(Kjønn), y = Prosentandel, fill = factor(Kjønn))) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
  geom_text(aes(label = paste0(round(Prosentandel), "%")), 
            position = position_stack(vjust = 1.03), 
            size = 3, 
            color = "#5D6D7E") +
  scale_fill_manual(values = c("#408A88","#40598A")) + 
  labs(title = "Var. Kjønn",
       subtitle= "Fordeling av kjønn i prosent",
       x = "Kjønn",
       y = "Prosentandel",
       fill = "Kvinne = 0\nMann = 1",
       caption = "Norsk medborgerpanel runde 26") +
  theme_minimal()+
  bachelor

########################################################################
################## Utdanningsnivå Faktor 2 nivå ########################
########################################################################
# Vi generaliserer svarene om utdanningsnivået.
# Setter verdiene 1,2,3,4,5,6 som "Lavere" utdanning
# Setter verdiene 7,8,9,10,11 som "Høyere" utdanning,(Høyskole/universitet til Forskernivå (Doktorgrad, Ph.d.))
# "Høyere" settes som referanse.
dataNSD$Utdanningsnivå <- dataNSD$r26P4_2
dataNSD <- subset(dataNSD, Utdanningsnivå != 97 & Utdanningsnivå != 98)

dataNSD <- dataNSD %>%
  mutate(Analyse_Utdanningsnivå = factor(case_when(
    Utdanningsnivå %in% c(1, 2, 3, 4, 5, 6) ~ 'Lavere Utdanning',
    TRUE ~ 'Høyere Utdanning'))) %>% 
  mutate(Analyse_Utdanningsnivå = fct_relevel(Analyse_Utdanningsnivå,'Høyere Utdanning'))


# Teller antall deltakere for hver verdi av Utdanningsnivå
# Beregner antall i hver av verdiene.
Antall_Utdanningsnivå <- dataNSD %>%
  count(Analyse_Utdanningsnivå) %>%
  mutate(Prosentandel = n / sum(n) * 100)

# Plot
ggplot(Antall_Utdanningsnivå, aes(x = factor(Analyse_Utdanningsnivå), y = Prosentandel, fill = factor(Analyse_Utdanningsnivå))) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
  geom_text(aes(label = paste0(round(Prosentandel), "%")), 
            position = position_stack(vjust = 1.03), 
            size = 3, 
            color = "#5D6D7E") +
  scale_fill_manual(values = c("#408A88","#40598A")) + 
  labs(title = "Var. Utdanningsnivå",
       subtitle= "Fordeling av utdanningsnivå i prosent",
       x = "Utdanningsnivå",
       y = "Prosentandel",
       fill = "",
       caption = "Norsk medborgerpanel runde 26") +
  theme_minimal()+
  bachelor

########################################################################
################## Aldersgrupper, Faktor 3 nivå ########################
########################################################################
# Verdiene 1, 2, 3 settes til aldersgruppen født "1959 og tidligere"
# Verdiene 4 og 5 settes til aldersgruppen født "1960-1979"
# Verdiene 6 og 7 settes til aldersgruppen født "1980 og etter"
# Referansen er "født_1960_1979".
dataNSD$Aldersgruppe <- dataNSD$r26P5_1

dataNSD <- dataNSD %>%
  filter(Aldersgruppe != 97 & Aldersgruppe != 98) %>%
  mutate(Analyse_Aldersgruppe = factor(case_when(
    Aldersgruppe %in% c(1, 2, 3) ~ 'Født 1959 og før',
    Aldersgruppe %in% c(4, 5) ~ 'Født 1960-1979',
    Aldersgruppe %in% c(6, 7) ~ 'Født 1980 og etter'))) %>% 
  mutate(Analyse_Aldersgruppe = fct_relevel(Analyse_Aldersgruppe,'Født 1960-1979'))

Antall_Aldersgruppe <- dataNSD %>%
  count(Analyse_Aldersgruppe) %>%
  mutate(Prosentandel = n / sum(n) * 100)

# Plot
ggplot(Antall_Aldersgruppe, aes(x = factor(Analyse_Aldersgruppe), y = Prosentandel, fill = factor(Analyse_Aldersgruppe))) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
  geom_text(aes(label = paste0(round(Prosentandel), "%")), 
            position = position_stack(vjust = 1.03), 
            size = 3, 
            color = "#5D6D7E") +
  scale_fill_manual(values = c("#408A88","#40598A","#63408A")) + 
  labs(title = "Var. Aldersgrupper",
       subtitle= "Fordeling av aldersgrupper i prosent",
       x = "Aldersgrupper",
       y = "Prosentandel",
       fill = "",
       caption = "Norsk medborgerpanel runde 26") +
  theme_minimal()+
  bachelor

########################################################################
################## Arbeidsstatus, Faktor 3 nivå ########################
########################################################################  

# variabel har opprinnelig verdier; 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
# labels for disse er; "Fulltid", "Deltid 30t pluss", "Deltid 30t minus",
# "Selv.Næringsdr","Under Utdanning", "Arb.Ledig Aktiv",
# "Arb.Ledig Ikke Aktiv", "Syk og Ufør","Pensjonert",
# "Militærtjeneste", "Hjemmeværende", "Annet"),
# da blir; 5, 6, 7, 8, 9, 10, 11, 12 = 0, jobber ikke
# da blir; 1, 2, 3, 4 = 1, Jobber
dataNSD$Arbeidstatus <- dataNSD$r26_bgday
dataNSD <- subset(dataNSD, Arbeidstatus != 97 & Arbeidstatus != 98)

dataNSD <- dataNSD %>%
  mutate(Analyse_Arbeidstatus = factor(case_when(
    Arbeidstatus %in% c(5, 6, 7, 8, 10, 11, 12) ~ 'Jobber ikke',
    Arbeidstatus %in% c(9) ~ 'Pensjonert',
    TRUE ~ 'Jobber'))) %>% 
  mutate(Analyse_Arbeidstatus = fct_relevel(Analyse_Arbeidstatus, "Jobber"))

Antall_Arbeidstatus <- dataNSD %>%
  count(Analyse_Arbeidstatus) %>%
  mutate(Prosentandel = n / sum(n) * 100)

# Plot
ggplot(Antall_Arbeidstatus, aes(x = factor(Analyse_Arbeidstatus), y = Prosentandel, fill = factor(Analyse_Arbeidstatus))) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
  geom_text(aes(label = paste0(round(Prosentandel), "%")), 
            position = position_stack(vjust = 1.03), 
            size = 3, 
            color = "#5D6D7E") +
  scale_fill_manual(values = c("#408A88","#40598A","#63408A")) + 
  labs(title = "Var. Arbeidsstatus",
       subtitle= "Fordeling av arbeidsstatus i prosent",
       x = "Arbeidsstatus",
       y = "Prosentandel",
       fill = "",
       caption = "Norsk medborgerpanel runde 26") +
  theme_minimal()+
  bachelor

########################################################################
################# Brutto Årsinntekt, Faktor 2 nivå #####################
########################################################################
# data hadde opprinnelige verdiene; 1, 2, 3, 4, 5, 6, 7, 8
# labels for disse var; "150 000 og mindre", "150 001 - 300 000", "300 001 - 400 000",
# "400 001 - 500 000","500 001 - 600 000", "600 001 - 700 000",
# "700 001 - 1 000 000", "1 000 001 og mer")
# da blir; 1 - 5 = lav inntekt fra 150 000 - 600 000 
# da blir; 6 - 8 = høy inntekt fra 600 001 og oppover. (se kilder, fordi median brutto årsinntekt 2023 var ca 670 000kr)
dataNSD$Bruttoinntekt <- dataNSD$r26k2_bginc
dataNSD <- subset(dataNSD, Bruttoinntekt != 97 & Bruttoinntekt != 98)

dataNSD <- dataNSD %>%
  mutate(Analyse_Bruttoinntekt = factor(case_when(
    Bruttoinntekt %in% c(1, 2, 3, 4, 5) ~ 'Lav inntekt',
    TRUE ~ 'Høy inntekt'))) %>% 
  mutate(Analyse_Bruttoinntekt= fct_relevel(Analyse_Bruttoinntekt,'Høy inntekt'))


Antall_Bruttoinntekt <- dataNSD %>%
  count(Analyse_Bruttoinntekt) %>%
  mutate(Prosentandel = n / sum(n) * 100)

# Plot
ggplot(Antall_Bruttoinntekt, aes(x = factor(Analyse_Bruttoinntekt), y = Prosentandel, fill = factor(Analyse_Bruttoinntekt))) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
  geom_text(aes(label = paste0(round(Prosentandel), "%")), 
            position = position_stack(vjust = 1.03), 
            size = 3, 
            color = "#5D6D7E") +
  scale_fill_manual(values = c("#408A88","#40598A")) + 
  labs(title = "Var. Brutto årsinntekt",
       subtitle= "Fordeling av inntekt i prosent",
       x = "Bruttoinntekt",
       y = "Prosentandel",
       fill = "",
       caption = "Norsk medborgerpanel runde 26") +
  theme_minimal() +
  bachelor

########################################################################
########## Tilfreds med egen økonomi i dag, Kontinuerlig ###############
########################################################################
# Verdier fra 1 til 7, denne går motsatt vei enn hva som er ønskelig da
# verdi en er "svært god". Denne blir denne snudd,
# og den vil begynne fra 1.

dataNSD$Tilfreds_Økonomi <- dataNSD$r26_pceco

dataNSD <- subset(dataNSD, Tilfreds_Økonomi != 97 & Tilfreds_Økonomi != 98)

# Finner antall svar for hver verdi for Tilfreds_Økonomi da verdiene går motsatt vei i forhold til de andre to.
antall_verdier_Tilfreds_Økonomi <- count(dataNSD, Tilfreds_Økonomi)
print(antall_verdier_Tilfreds_Økonomi)

# Snur variabelen Tilfreds_Økonomi og lar den starte på 0.
dataNSD <- dataNSD %>% 
  mutate(Tilfreds_Økonomi = case_when(
    Tilfreds_Økonomi == 7 ~ 1,
    Tilfreds_Økonomi == 6 ~ 2,
    Tilfreds_Økonomi == 5 ~ 3,
    Tilfreds_Økonomi == 4 ~ 4,
    Tilfreds_Økonomi == 3 ~ 5,
    Tilfreds_Økonomi == 2 ~ 6,
    Tilfreds_Økonomi == 1 ~ 7
  )) %>%
  arrange(Tilfreds_Økonomi)

antall_verdier_Tilfreds_Økonomi_ny <- count(dataNSD, Tilfreds_Økonomi)
print(antall_verdier_Tilfreds_Økonomi_ny)

Antall_Tilfreds_Økonomi <- dataNSD %>%
  count(Tilfreds_Økonomi) %>%
  mutate(Prosentandel = n / sum(n) * 100)

#plot
ggplot(Antall_Tilfreds_Økonomi, aes(x = Tilfreds_Økonomi, y = Prosentandel)) +
  geom_bar(stat = "identity", fill = "#40598A", width = 0.7, position = "identity", alpha = 0.8) + 
  geom_text(aes(label = paste0(round(Prosentandel), "%")), vjust = -0.5, size = 3, color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(0, 7, by = 1)) +
  labs(title = "Var. Tilfreds_Økonomi",
       subtitle= "Andel svart i prosent",
       x = "Nivå av tilfredshet med økonomien",
       y = "Prosentandel av deltakere",
       caption = "Norsk medborgerpanel runde 26") +
  theme_minimal() +
  bachelor

########################################################################
######################### Data for Analyse #############################
########################################################################


Analyse_data <- dataNSD%>%
  dplyr::select(Holdning_Innvandring, Kjønn, 
                Analyse_Utdanningsnivå, Analyse_Aldersgruppe, Analyse_Arbeidstatus, 
                Analyse_Bruttoinntekt,Tilfreds_Økonomi)

{r eval=FALSE}
########################################################################
##################### Deskriptiv statistikk ############################
########################################################################
# Lager diskriptiv statistisk tabell med bruk av modellsummary::datasummary() 
# og pakken kableExtra.

Deskriptiv_Tabell <- Analyse_data %>% 
  dplyr::select(Holdning_Innvandring,
                Tilfreds_Økonomi,
                Kjønn) %>% 
  dplyr::rename("Holdning til innvandrere. Numerisk 1-7. Avhengig variabel"="Holdning_Innvandring",
                "Tilfreds med egen økonomiske situasjon. Numerisk 1-7, Forklaringsvariabel"="Tilfreds_Økonomi",
                "Kjønn, 'Dummy' 0 og 1, Ref. Kvinne. Kontrollvariabel."="Kjønn") %>% 
  as.data.frame()

str(Deskriptiv_Tabell_list)
# Denne funksjonen er i bruker-manualen til modelsummary
tomme_kol <- function(x) " "

# Legger inn hva jeg ønsker i tabellen.
datasummary(All(Deskriptiv_Tabell) ~ N + Mean + Min + Max,
            data = Deskriptiv_Tabell, output = "kableExtra",
            title = '') %>%
  row_spec(row = 0, 
           font_size = 18,
           background = "#D5E9F6") %>% 
  kable_paper(full_width = F) %>% 
  footnote("Data hentet fra Medborgerpanelet runde 26, 2023") %>% 
  kable_classic(html_font = "Times New Roman")

# Lager prosenttabell manuelt (huxtable)

Col1 <- c("Holdning Innvandring", "Prosentandel", "Tilfreds med økonomi", "Prosentandel", "Arbeidstatus", "Prosentandel", "Alder", "Prosentandel", "Utdanningsnivå", "Prosentandel", "Bruttoinntekt", "Prosentandel", "Kjønn", "Prosentandel")
Col2 <- c("1", "1%", "1", "1%", "Jobber", "53%", "1959-før", "44%", "Høyere", "56%", "Høy", "41%", "Kvinne", "49%")
Col3 <-c("2", "2%", "2", "2%", "Jobber Ikke", "13%", "1960-1979", "38%", "Lavere", "44%", "Lav", "59%", "Menn", "51%")
Col4 <- c("3", "8%", "3", "6%", "Pensjonist", "34%", "1980-etter", "18%", "", "", "", "", "", "")
Col5 <- c("4", "29%", "4", "12%", "", "", "", "", "", "", "", "", "", "")
Col6 <- c("5", "27%", "5", "17%","", "", "", "", "", "", "", "", "", "" )
Col7 <- c("6", "27%", "6", "48%", "", "", "", "", "", "", "", "", "", "")
Col8 <- c("7", "6%", "7", "13%", "", "", "", "", "", "", "", "", "", "") 

andel_tibble <- data.frame(Col1 = Col1, Col2 = Col2, Col3 = Col3, Col4 = Col4, Col5 = Col5, Col6 = Col6, Col7= Col7, Col8 = Col8)
# Konverterer til huxtable
ht <- as_hux(andel_tibble, add_colnames = FALSE)
ht <- set_bold(ht, evens, 1:8, FALSE)
ht <- set_bold(ht, odds, 1:8, TRUE)
ht <- set_background_color(ht, row = odds, col = 1:8, value = "#D5E9F6")

# Fjerner og fikser vertikale og horisontale linjer
ht <- set_all_borders(ht, 0) # Setter tynne horisontale linjer
ht <- set_outer_borders(ht, 1:14, everywhere, 0.4)
ht <- set_all_borders(ht, row = 1:14, col = everywhere, value = 0.4)
ht <- set_all_borders(ht, row = 1:14, col = everywhere, value = 0)

# Gjør hele kolonne 1 til bold skrift, og alt til times new roman
ht <- set_bold(ht, col = 1, value = TRUE)
ht <- set_font(ht, everywhere, everywhere, "Times New Roman")

# Setter en liten justering
desk_andel_pros <- set_align(ht, everywhere, everywhere, "center")

# Angir til slutt kolonnenavn
colnames(desk_andel_pros) <- c("", "1", "2", "3", "4", "5", "6", "7")

###################################################################
############################# MODELLER ############################
###################################################################

# regresjon lm()
M1 <- lm(Holdning_Innvandring ~ Analyse_Utdanningsnivå+Analyse_Arbeidstatus+Analyse_Bruttoinntekt+Kjønn+Analyse_Aldersgruppe, Analyse_data, na.action = na.exclude)

M2 <- lm(Holdning_Innvandring ~ Analyse_Utdanningsnivå+Analyse_Arbeidstatus+Analyse_Bruttoinntekt+Tilfreds_Økonomi+Kjønn+Analyse_Aldersgruppe, Analyse_data)

M3 <- lm(Holdning_Innvandring ~ Analyse_Utdanningsnivå+Analyse_Arbeidstatus+Analyse_Bruttoinntekt+Tilfreds_Økonomi+
           Analyse_Utdanningsnivå*Tilfreds_Økonomi+
           Analyse_Arbeidstatus*Tilfreds_Økonomi+
           Analyse_Bruttoinntekt*Tilfreds_Økonomi+
           Kjønn+Analyse_Aldersgruppe, Analyse_data)

###################################################################
############################# MODELL 1 ############################
###################################################################
options(encoding = "UTF-8")
#Oppsummerer å sjekker redisdualer
summary(M1)
plot(M1, which = 1, main = "Model fit")
plot(M1, which = 2, main = "Model fit")
plot(M1, which = 3, main = "Model fit")
plot(M1, which = 5, main = "Model fit")
###################################################################
# Multikolinearitet
vif(M1) 

#Lager tabell for resultater av multikolinearitet modell 1
vifM1 <- data.frame(
  Predictor = c("Analyse_Utdanningsniva", "Analyse_Arbeidstatus", "Analyse_Bruttoinntekt", "Kjonn", "Analyse_Aldersgruppe"),
  GVIF = c(1.1217, 3.4305, 1.5212, 1.0901, 2.8141),
  Df = c(1, 2, 1, 1, 2),
  `GVIF^(1/(2*Df))` = c(1.0591, 1.3609, 1.2334, 1.0441, 1.2952)
)
vifM1_tabell <- as_hux(vifM1)
vifM1_tabell <- set_all_borders(vifM1_tabell, 1)
vifM1_tabell <- set_caption(vifM1_tabell, "VIF Resultater Modell 1")
vifM1_tabell <- set_col_width(vifM1_tabell, c(0.4, 0.2, 0.2, 0.2))
vifM1_tabell <- set_italic(final(1), 1)
vifM1_tabell <- set_font(vifM1_tabell, "Arial")

###################################################################
############################# MODELL 2 ############################
###################################################################
# det samme blir gjort for modell 2
summary(M2)
plot(M2, which = 1, main = "Model fit")
plot(M2, which = 2, main = "Model fit")
plot(M2, which = 3, main = "Model fit")
plot(M2, which = 5, main = "Model fit")
vif(M2)
vifM2 <- data.frame(
  Predictor = c("Analyse_Utdanningsniva", "Analyse_Arbeidstatus", "Analyse_Bruttoinntekt", "Tilfreds_Okonomi", "Kjonn", "Analyse_Aldersgruppe"),
  GVIF = c(1.1329, 3.5835, 1.6213, 1.2462, 1.0904, 2.8662),
  Df = c(1, 2, 1, 1, 1, 2),
  `GVIF^(1/(2*Df))` = c(1.0644, 1.3759, 1.2733, 1.1164, 1.0442, 1.3011)
)

vifM2_tabell <- as_hux(vifM2)
vifM2_tabell <- set_all_borders(vifM2_tabell, 1)
vifM2_tabell <- set_caption(vifM2_tabell, "VIF Resultater Modell 2")
vifM2_tabell <- set_col_width(vifM2_tabell, c(0.4, 0.2, 0.2, 0.2))
vifM2_tabell <- set_italic(final(1), 1)
vifM2_tabell <- set_font(vifM2_tabell, "Arial")

###################################################################
############################# MODELL 3 ############################
###################################################################
# og det samme blir gjort for modell 3
summary(M3)
plot(M3, which = 1, main = "Model fit")
plot(M3, which = 2, main = "Model fit")
plot(M3, which = 3, main = "Model fit")
plot(M3, which = 5, main = "Model fit")
vif(M3, type = "predictor")
vifM3<- data.frame(
  Predictor = c("Analyse_Utdanningsniva", "Analyse_Arbeidstatus", "Analyse_Bruttoinntekt", 
                "Tilfreds_Okonomi", "Kjonn", "Analyse_Aldersgruppe"),
  GVIF = c(5.8548, 21.3124, 6.2335, 3.1153, 1.0917, 2.8979),
  Df = c(3, 5, 3, 9, 1, 2),
  `GVIF^(1/(2*Df))` = c(1.3425, 1.3579, 1.3566, 1.0652, 1.0449, 1.3047),
  `Interacts With` = c("Tilfreds_Okonomi", "Tilfreds_Okonomi", "Tilfreds_=konomi", 
                       "Analyse_Utdanningsniva, Analyse_Arbeidstatus, Analyse_Bruttoinntekt", 
                       "--", "--"),
  `Other Predictors` = c("Analyse_Arbeidstatus, Analyse_Bruttoinntekt, Kjonn, Analyse_Aldersgruppe", 
                         "Analyse_Utdanningsniva, Analyse_Bruttoinntekt, Kjonn, Analyse_Aldersgruppe", 
                         "Analyse_Utdanningsniva, Analyse_Arbeidstatus, Kjonn, Analyse_Aldersgruppe", 
                         "Kjonn, Analyse_Aldersgruppe", 
                         "Analyse_Utdanningsniva, Analyse_Arbeidstatus, Analyse_Bruttoinntekt, Tilfreds_Okonomi, Analyse_Aldersgruppe", 
                         "Analyse_Utdanningsniva, Analyse_Arbeidstatus, Analyse_Bruttoinntekt, Tilfreds_Okonomi, Kjønn")
)

vifM3_tabell <- as_hux(vifM3)
vifM3_tabell <- set_all_borders(vifM3_tabell, 1)
vifM3_tabell <- set_caption(vifM3_tabell, "VIF Resultater Modell 3")
vifM3_tabell <- set_col_width(vifM3_tabell, c(0.2, 0.1, 0.1, 0.1, 0.2, 0.3))
vifM3_tabell <- set_italic(final(1), 1)
vifM3_tabell <- set_font(vifM3_tabell, "Arial")

Redusert_Analyse_data1 <- Analyse_data %>% 
  mutate(Utdanning_Nivå = fct_relevel(Analyse_Utdanningsnivå, 'Lavere Utdanning', 'Høyere Utdanning'))

names(coef(M1))
names(coef(M2))
names(coef(M3))

# lager en oppsummert tabell med huxtable for presentasjon av resultater
Resultat_reg_tabell <-huxreg(M1, M2, M3, bold_signif = 0.05,
                             coefs = c("Intercept" ="(Intercept)",
                                       "Obj. Lavere utdanning" = "Analyse_UtdanningsnivåLavere Utdanning",
                                       "Obj. Jobber ikke" = "Analyse_ArbeidstatusJobber ikke",
                                       "Obj. Pensjonert" = "Analyse_ArbeidstatusPensjonert",
                                       "Obj. Lav inntekt" = "Analyse_BruttoinntektLav inntekt",
                                       "Kontr. Mann"="Kjønn",
                                       "Kontr. Aldersgruppe 1959 og tidligere"= "Analyse_AldersgruppeFødt 1959 og før",
                                       "Kontr. Aldersgruppe 1980 og etter"= "Analyse_AldersgruppeFødt 1980 og etter",
                                       "Subj. Tilfredshet m/ økonomi" = "Tilfreds_Økonomi",
                                       "Inter. Lav utdanning vs. Tilfredshet" = "Analyse_UtdanningsnivåLavere Utdanning:Tilfreds_Økonomi",
                                       "Inter. Jobber ikke vs. Tilfredshet" = "Analyse_ArbeidstatusJobber ikke:Tilfreds_Økonomi",
                                       "Inter. Pensjonert vs. Tilfredshet" = "Analyse_ArbeidstatusPensjonert:Tilfreds_Økonomi",
                                       "Inter. Lav inntekt vs. Tilfredshet"= "Analyse_BruttoinntektLav inntekt:Tilfreds_Økonomi"),
                             statistics = c("Observasjoner" = "nobs", 
                                            "R2" = "r.squared", "R2-justert"="adj.r.squared", "F-verdi" = "statistic",
                                            "P-verdi" = "p.value"))

Resultat_reg_tabell<-Resultat_reg_tabell %>% 
  map_background_color(-1, -1, by_regex("\\*" = "#E8FCE4")) %>% 
  set_italic(final(1), 1) %>% 
  set_caption("Resulat Regresjonsanalyser")
quick_pdf(Resultat_reg_tabell)

###################################################################
##################### Interaksjonsplot ############################
###################################################################

int_1<-ggplot(Redusert_Analyse_data1, aes(x = Tilfreds_Økonomi, y = Holdning_Innvandring, 
                                          group = Utdanning_Nivå, color = Utdanning_Nivå)) +
  geom_point(stat = "summary", fun = mean, alpha = 0.7, size=3) + 
  geom_line(stat = "summary", fun = mean, alpha = 0.7) +
  labs(title = "",
       subtitle = "a: Tilfredshet Økonomi og Utdanning",
       x = "Tilfredshet med økonomi",
       y = "Holdning Innvandring",
       color = "Utdanningsnivå") +
  scale_color_manual(values = c("#138D75","#2E86C1", "#D4EC2D")) +
  ylim(0, 6) +
  theme_minimal()+
  bachelor

Redusert_Analyse_data2 <- Analyse_data %>% 
  mutate(Inntekt_Nivå = fct_relevel(Analyse_Bruttoinntekt, 'Lav inntekt', 'Høy inntekt'))

int_2<-ggplot(Redusert_Analyse_data2, aes(x = Tilfreds_Økonomi, y = Holdning_Innvandring, 
                                          group = Inntekt_Nivå, color = Inntekt_Nivå)) +
  geom_point(stat = "summary", fun = mean, width = 0.2, height = 0.2, alpha = 0.5, size=3) +
  geom_line(stat = "summary", fun = mean) +  
  labs(title = "",
       subtitle = "b: Tilfredshet Økonomi og Inntekt",
       y = "",
       x = "Tilfredshet med økonomi",
       color = "Brutto årsinntekt") +
  scale_color_manual(values = c("#138D75","#2E86C1", "#D4EC2D")) + 
  ylim(0, 6) +
  theme_minimal()+
  bachelor+
  theme(plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "#5D6D7E",size=12))

Redusert_Analyse_data3 <- Analyse_data %>% 
  mutate(Arbeids_Nivå = fct_relevel(Analyse_Arbeidstatus, 'Pensjonert', 'Jobber ikke', 'Jobber'))

int_3<-ggplot(Redusert_Analyse_data3, aes(x = Tilfreds_Økonomi, y = Holdning_Innvandring, color = Arbeids_Nivå)) +
  geom_point(stat = "summary", fun = mean, width = 0.2, height = 0.2, alpha = 0.5, size=3) +
  geom_line(stat = "summary", fun = mean) +  
  labs(title = "",
       subtitle = "c: Tilfredshet Økonomi og Arbeidstatus",
       y = "",
       x = "Tilfredshet med økonomi",
       color = "Arbeidsstatus") +
  scale_color_manual(values = c("#138D75","#2E86C1", "#D4EC2D")) +
  ylim(0, 6) +
  theme_minimal()+
  bachelor+
  theme(plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "#5D6D7E",size=12))

grid.arrange(int_1, int_2,int_3, ncol = 3)

###################################################################
################## ILLUSTRASJON Teori #############################
###################################################################
# Denne ble unødvendig lang, men gjorde det jeg ønsket..
# tilbud og etterspørsel data for illustrasjon kompetansegrupper
x <- c(0, 10) 
tilbud <- 0.9 * x + -0.1 
etterspørsel <- -1 * x + 10

# Beregning av skjæringspunktene
skjæringspunktB <- (10 + 0.6) / (1 + 1)  
skjæringspunktA <- (8 + 1) / (1 + 1)    
punktA <- 1 * skjæringspunktA + 1
punktB <- 1 * skjæringspunktB - 0.6
punktC <- (punktA + punktB) / 2
kvantumC <- (skjæringspunktA + skjæringspunktB) / 2
punktD <- (10 + 0.6) / (1 + 1) 

# Plot sjokk i tilbud av arbeidskraft, kompetansegruppe 1
plot1<-ggplot() +
  geom_line(aes(x = x, y = tilbud), color = "#2E86C1", 
            linetype = "dashed", lwd=1.1, alpha = 0.7) +
  geom_line(aes(x = x, y = tilbud + 1.5), color = "#2E50C1", 
            linetype = "solid",lwd=1.1, alpha = 0.7) +
  geom_line(aes(x = x, y = etterspørsel), color = "#138D75", 
            linetype = "solid",lwd=1.1,alpha = 0.7) +
  labs(x = "L", 
       y = "w", 
       title = "",
       subtitle = "1a) Lønns-, og sysselsetningseffekter Kompetansegruppe 1") +
  geom_point(aes(x = skjæringspunktA, y = punktA), color = "#E04B28", size=2, alpha = 0.8) +
  geom_text(aes(x = skjæringspunktA, y = punktA), label = "A", vjust = -0.7,family ="LM Roman 10",size=3,color ="#5D6D7E") +
  geom_point(aes(x = skjæringspunktB, y = punktB), color = "#E04B28", size=2, alpha = 0.8) +
  geom_text(aes(x = skjæringspunktB, y = punktB), label = "B", vjust = -0.7,family ="LM Roman 10",size=3,color ="#5D6D7E") +
  geom_point(aes(x = kvantumC, y = punktC), color = "#E04B28", size=2, alpha = 0.8) +
  geom_text(aes(x = kvantumC, y = punktC), label = "C", vjust = -0.7,family ="LM Roman 10",size=3,color ="#5D6D7E") +
  geom_point(aes(x = 5.75, y = 5.08), color = "#E04B28", size=2, alpha = 0.8) +
  geom_text(aes(x = 5.75, y = 5.08), label = "D", vjust = -0.7,family = "LM Roman 10",size=3,color ="#5D6D7E")+
  geom_segment(aes(x = skjæringspunktA, y = 0, xend = skjæringspunktA, yend = punktA), 
               linetype = "dashed", alpha=0.4, color = "#E04B28") +
  geom_segment(aes(x = 0, y = punktA, xend = skjæringspunktA, yend = punktA), 
               linetype = "dashed",alpha=0.4, color = "#E04B28") +
  geom_segment(aes(x = kvantumC, y = 0, xend = kvantumC, yend = punktC), 
               linetype = "dashed", alpha=0.4, color = "#E04B28") +
  geom_segment(aes(x = 0, y = punktC, xend = kvantumC, yend = punktC), 
               linetype = "dashed", alpha=0.4, color = "#E04B28")+
  geom_segment(aes(x = 5.80, y = 5.1, xend = 0, yend = 5.11), 
               linetype = "dashed", alpha=0.4, color = "#E04B28")+
  geom_text(aes(x = -0.3, y = 5.2), label = "w0", vjust = -1, family = "LM Roman 10",size=3,color ="#5D6D7E")+
  geom_text(aes(x = -0.3, y = 4.75), label = "w1", vjust = 0.4, family = "LM Roman 10",size=3,color ="#5D6D7E")+
  geom_text(aes(x = 4.3, y = -0.6), label = "L0", vjust = 0.5,size=3, family = "LM Roman 10",color ="#5D6D7E")+
  geom_text(aes(x = 5.1, y = -0.6), label = "L1", vjust = 0.5,size=3, family ="LM Roman 10",color ="#5D6D7E")+
  geom_text(aes(x = 9.5, y = 0.5), label = "Ld", vjust = -1,size=3, family = "LM Roman 10",color ="#5D6D7E")+
  geom_text(aes(x = 9.5, y = 9.1), label = "Ls0", vjust = 0.5,size=3, family = "LM Roman 10",color ="#5D6D7E")+
  geom_text(aes(x = 9.5, y = 7.6), label = "Ls1", vjust = 0.5,size=3, family = "LM Roman 10",color ="#5D6D7E")+
  scale_x_continuous(labels = NULL) + 
  scale_y_continuous(labels = NULL) +
  theme_minimal()+
  theme(axis.line.x = element_line(linewidth = 1.2, color = "gray34"),
        axis.line.y = element_line(linewidth =  1.2, color = "gray34"),
        plot.subtitle = element_text(size = 9)) +
  bachelor


# Plot 2, økning i etterspørsel i kompetansegruppe 2
x1 <- c(0, 10) 
tilbud1 <- 0.9 * x1 + -0.1 
etterspørsel1 <- -1 * x1 + 10

# Plot med tilleggene
plot2<-ggplot() +
  geom_line(aes(x = x1, y = etterspørsel1+ 1.5), color = "#139D90", 
            linetype = "dashed", lwd=1.1, alpha = 0.7) +
  geom_line(aes(x = x1, y = tilbud1 + 1.5), color = "#2E50C1", 
            linetype = "solid",lwd=1.1, alpha = 0.7) +
  geom_line(aes(x = x1, y = etterspørsel1), color = "#138D75", 
            linetype = "solid",lwd=1.1,alpha = 0.7) +
  labs(x = "L", 
       y = "w", 
       title = "",
       subtitle = "1b) Lønns-, og sysselsetningseffekter Kompetansegruppe 2") + 
  geom_point(aes(x = 4.54, y = 5.5), color = "#E04B28", size=2, alpha = 0.8)+ 
  geom_text(aes(x = 4.54, y = 5.5), label = "A", vjust = -0.7,size=3,family = "LM Roman 10",color ="#5D6D7E") +
  geom_point(aes(x = 5.31, y = 6.2), color = "#E04B28", size=2, alpha = 0.8) +
  geom_text(aes(x = 5.31, y = 6.2), label = "B", vjust = -0.7,size=3,family = "LM Roman 10",color ="#5D6D7E") +
  geom_segment(aes(x = 4.54, y = 0, xend = 4.54, yend = 5.5), 
               linetype = "dashed", alpha=0.4, color = "#E04B28") +
  geom_segment(aes(x = 0, y = 5.5, xend = 4.54, yend = 5.5), 
               linetype = "dashed",alpha=0.4, color = "#E04B28") +
  geom_segment(aes(x = 5.31, y = 0, xend = 5.31, yend = 6.2), 
               linetype = "dashed",alpha=0.4, color = "#E04B28") +
  geom_segment(aes(x = 0, y = 6.2, xend = 5.31, yend = 6.2),
               linetype = "dashed",alpha=0.4, color = "#E04B28") +
  geom_text(aes(x = -0.3, y = 5.8), label = "w1", vjust = -1,size=3, family ="LM Roman 10",color ="#5D6D7E")+
  geom_text(aes(x = -0.3, y = 5), label = "w0", vjust = 0.2,size=3, family = "LM Roman 10",color ="#5D6D7E")+
  geom_text(aes(x = 4.5, y = -0.6), label = "L0", vjust = 0.5,size=3, family = "LM Roman 10",color ="#5D6D7E")+
  geom_text(aes(x = 5.4, y = -0.6), label = "L1", vjust = 0.5,size=3, family = "LM Roman 10",color ="#5D6D7E")+
  geom_text(aes(x = 9.5, y = 0.6), label = "Ld0", vjust = -1.2,size=3, family ="LM Roman 10",color ="#5D6D7E")+
  geom_text(aes(x = 9.5, y = 2), label = "Ld1", vjust = -1.2,size=3, family = "LM Roman 10",color ="#5D6D7E")+
  geom_text(aes(x = 9.5, y = 9.1), label = "Ls", vjust = -0.2,size=3, family = "LM Roman 10",color ="#5D6D7E")+
  scale_x_continuous(labels = NULL) + 
  scale_y_continuous(labels = NULL) + 
  theme_minimal()+
  theme(axis.line.x = element_line(linewidth = 1, color = "gray34"),
        axis.line.y = element_line(linewidth =  1, color = "gray34"),
        plot.subtitle = element_text(size = 9)) +
  bachelor

# Viser plottene ved siden av hverandre
grid.arrange(plot1, plot2, ncol = 2)





