## https://practical-stats-med-r.netlify.app/wmw_test
## Practical Statistics in Medicine with R
library("tidyverse")
require('DescTools')
library("rstatix")
library("knitr")
library("ggpubr")
###
dA <- read.csv("fertility_rate_2003_2018.csv", sep = ';',
               header=T, na.string="NA");
d2018 <- dA %>% filter(yr==2018)
s2018 <- summary(d2018$frate)
mean2018 <- s2018[["Mean"]]
median2018 <- s2018[["Median"]]
q1.2018 <- quantile(d2018$frate, probs = 0.25)
q3.2018 <- quantile(d2018$frate, probs = 0.75)
sd2018 <-  sd(d2018$frate)
##my_data <- c(15,8,9,15,12,13,2,15,13,8,13,6,7)
Mode2 <- Mode(d2018$frate)
frqMode <- attributes(Mode2)
frqMode$freq

sprintf("%.2f", Mode2)

str(Mode2)

print(Mode2)

median2018
mean2018
install.packages("moments")
library(moments)
Skew(d2018$frate)

(mean2018 - Mode2) / sd2018

3 * (mean2018 - median2018) / sd2018

((q3.2018 - median2018) - (median2018  - q1.2018)) / (q3.2018 -  q1.2018)

skewness(d2018$frate)

##
library("knitr")
vitC <- read.csv(file='vit_C.csv',sep=';',header=T)
narciarze <- table(vitC)
narciarze.table <- addmargins(narciarze)
kable(narciarze.table, col.names = c('nocold', 'cold', 'razem'), booktabs = TRUE)

##
narciarze.total <- sum(narciarze)
narciarze.p <- narciarze/narciarze.total *100

narciarze.table.p <- addmargins(narciarze.p)
kable(digits=2, narciarze.table.p, col.names = c('nocold', 'cold', 'razem'), booktabs = TRUE)
##


narciarze.p <- proportions(narciarze, margin = 1)
narciarze.p.m <- addmargins(narciarze.p, margin = 1)
Sum <- margin.table(narciarze,2) /narciarze.total
narciarze.x <- cbind(rbind(narciarze.p, Sum), c(1,1,1)) * 100
kable(digits=2, narciarze.x, col.names = c('nocold', 'cold', 'razem'), booktabs = TRUE)

### ####################################################################
s0 <- read.csv("depresjaPSW.csv", sep = ';', header = T) |>
  mutate (P = ifelse (praca == 'Szpital', P + 5, P)) |>
  mutate (P = ifelse (P > 30, P - 6, P))

ttest.mp <- s0 %>%  t_test(P ~ praca) %>%
  select(group1, group2, n1, n2, statistic, p)
pval.mp <- ttest.mp$p
kable(ttest.mp, col.names = c('Grupa1', 'Grupa2', 'n1', 'n2', 't', 'p'), booktabs = TRUE)

p1.mp <- s0 %>% filter (praca == 'Szpital') %>% ggplot(aes(sample=P)) + stat_qq() + stat_qq_line() +
  xlab ('Teoretyczny') + ylab ('Zaobserwowany') + ggtitle("Szpital")
p2.mp <- s0 %>% filter (praca == 'Przychodnia') %>% ggplot(aes(sample=P)) + stat_qq() + stat_qq_line() +
  xlab ('Teoretyczny') + ylab ('Zaobserwowany') + ggtitle("Przychodnia")
ggarrange(p1.mp, p2.mp, ncol = 2, nrow = 1)

sw.table <- s0 %>%
  group_by(praca) %>%
  shapiro_test(P) %>% select(praca, statistic, p)

kable(sw.table, col.names = c('m-pracy', 'S-W', 'p'), booktabs = TRUE)

wilcoxp.table <- wilcox_test(P ~ praca, data=s0) %>%
  select(group1, group2, n1, n2, statistic, p)
kable(wilcoxp.table, col.names = c('Grupa1', 'Grupa2', 'n1', 'n2', 'U', 'p'), booktabs = TRUE)
pval.u <- wilcoxp.table$p

s0.mp <- s0 %>%
  group_by(praca) %>%
  summarise(m = mean(P), me=median(P), n=n())
kable(s0.mp, col.names = c('m-pracy', 'średnia', 'mediana', 'n'), booktabs = TRUE)

h.szpital <- s0 |> filter (praca == 'Szpital') |> ggplot(aes(x = P)) +
  geom_histogram(binwidth = 5, fill=default_cyan)
h.szpital
h.przychodnia <- s0 |> filter (praca == 'Przychodnia') |> ggplot(aes(x = P)) +
  geom_histogram(binwidth = 5, fill=default_cyan)
h.przychodnia

###

anova.test <- oneway.test(P ~ staz, data=s0)
p.anova <- anova.test$p.value


anova.test <- anova_test(P ~ staz, data=s0)
anova.test
p.anova <- anova.test$p

sw.table.staz <- s0 %>%
  group_by(staz) %>%
  shapiro_test(P) %>% select(staz, statistic, p)
##sw.table.staz
kable(sw.table.staz, col.names = c('m-pracy', 'S-W', 'p'), booktabs = TRUE)

kw <- kruskal.test(P ~ staz, data = s0)
kw
pval <- kw['p.value']

s0.staz <- s0 %>%
  group_by(staz) %>%
  summarise(m = mean(P), me = median(P), n=n())
kable(s0.staz, col.names = c('staż (kategoria)', 'średnia', 'mediana', 'n'), booktabs = TRUE)

##library("car")
s0 %>% levene_test(P ~ as.factor(staz))

data("ToothGrowth")
df <- ToothGrowth
df$dose <- as.factor(df$dose)
# Compute Levene's Test
lev.test <- df %>% levene_test(len ~ dose)
lev.test
########################################################

s0 <- read.csv("palenie_PSW.csv", sep = ',',  header=F, skip=1, na.string="NA", fileEncoding = "UTF-8",
               col.names = c('time',
                             'status', 'palenie.staz',
                             'palenie.rzucenie',
                             'uzaleznienie',
                             'P5',
                             'P6', 'P7', 'P8', 'P9', 'P10', 'P11', 'P12',
                             'ocena.wiedzy', 'wzorce.palenia', 'plec', 'wiek', 'staz', 'miejsce.pracy'))

levels(as.factor(s0$staz))

s1 <- s0 %>%
  mutate(
    staz=recode(staz,
                '0-3 lat'   = '06 i mniej',
                '4-6 lat'   = '06 i mniej',
                '7-9 lat'   = '07-12',
                '8-12 lat'  = '07-12',
                '10-12 lat' = '07-12',
                '13-15 lat' = '13-18',
                '16-18 lat' = '13-18',
                '19-21 lat' = '19 i więcej',
                '22-24 lat' = '19 i więcej',
                '25 lat i więcej' = '19 i więcej') ) %>%
  #
  ##T12. Uważasz, że bardziej szkodliwe dla zdrowia jest: [JW]
  # "Każda forma kontaktu"
  mutate (P5 = case_when( str_detect(P5, "Każda forma kontaktu") ~ 1, TRUE ~ 0)) %>%
  ##==
  # "Palenie czynne"
  # "Palenie bierne"
  # "Nie wiem"
  #
  ##T16
  mutate (
    P9A = case_when( str_detect(P9, "Przewlekła obturacyjna choroba płuc") ~ 1, TRUE ~ 0),
    P9B = case_when( str_detect(P9, "Astma oskrzelowa") ~ 1, TRUE ~ 0),
    P9C = case_when( str_detect(P9, "Alergie wziewne") ~ 1, TRUE ~ 0),
    P9D = case_when( str_detect(P9, "Przewlekłe zapalenie oskrzeli") ~ 1, TRUE ~ 0),
    P9E = case_when( str_detect(P9, "Infekcje dróg oddechowych") ~ 1, TRUE ~ 0),
    ##
    P9F = case_when( str_detect(P9, "Gruźlica") ~ -1, TRUE ~ 0),
    P9G = case_when( str_detect(P9, "Zapalenie płuc") ~ -1, TRUE ~ 0),
    P9H = case_when( str_detect(P9, "Palenie nie powoduje") ~ -1, TRUE ~ 0),
    P9I = case_when( str_detect(P9, "Nie wiem") ~ -1, TRUE ~ 0),
    #
    #T17
    #
    P10A = case_when( str_detect(P10, "Nadciśnienie tętnicze krwi") ~ 1, TRUE ~ 0),
    P10B = case_when( str_detect(P10, "Zawał mięśnia sercowego") ~ 1, TRUE ~ 0),
    P10C = case_when( str_detect(P10, "Udar mózgu") ~ 1, TRUE ~ 0),
    P10D = case_when( str_detect(P10, "Choroba niedokrwienna serca") ~ 1, TRUE ~ 0),
    P10E = case_when( str_detect(P10, "Miażdżyca tętnic obwodowych") ~ 1, TRUE ~ 0),
    P10F = case_when( str_detect(P10, "Zaburzenie rytmu serca") ~ 1, TRUE ~ 0),
    P10G = case_when( str_detect(P10, "Choroba Buergera") ~ 1, TRUE ~ 0),
    ##
    P10H = case_when( str_detect(P10, "Hipercholesterolemia") ~ -1, TRUE ~ 0),
    P10I = case_when( str_detect(P10, "Tętniak aorty") ~ -1, TRUE ~ 0),
    P10J = case_when( str_detect(P10, "Palenie nie powoduje") ~ -1, TRUE ~ 0)) %>%
  #
  #T18. Czy palenie papierosów powoduje choroby układu pokarmowego? [JW]
  #"Tak"
  #==
  mutate (P11 = case_when( str_detect(P11, "Tak") ~ 1, TRUE ~ 0)) %>%
  #"Nie"
  #"wiem"
  #
  #T19. Jaki według Ciebie ma wpływ palenie papierosów na narządy zmysłów?
  #
  mutate (
    P12A = case_when( str_detect(P12, "Upośledza węch i smak") ~ 1, TRUE ~ 0),
    P12B = case_when( str_detect(P12, "Powoduje podrażnienie spojówek") ~ 1, TRUE ~ 0),
    P12C = case_when( str_detect(P12, "Obniża apetyt") ~ 1, TRUE ~ 0),
    P12D = case_when( str_detect(P12, "Niszczy struny głosowe") ~ 1, TRUE ~ 0),
    P12E = case_when( str_detect(P12, "Zmniejsza ostrość wzroku") ~ 1, TRUE ~ 0),
    ##
    P12F = case_when( str_detect(P12, "Palenie nie ma") ~ -1, TRUE ~ 0)) %>%
  mutate (
    P.total = P5 +
      P9A + P9B + P9C + P9D + P9E + P9F + P9G + P9H + P9I +
      P10A + P10B + P10C + P10D + P10E + P10F + P10G + P10H + P10I +
      P11 +
      P12A + P12B + P12C + P12D + P12E + P12F )

s1.plec <- s1 %>%
  select (plec) %>%
  group_by(plec)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n) * 100 ) |> filter (plec == 'K') |>
  select (prop) |> unlist() |> unname ()



s0.pl <- s1 %>%
  group_by(plec) %>%
  summarise(m = mean(P.total), me =median(P.total), n=n())
kable(s0.pl, col.names = c('płeć', 'średnia', 'mediana', 'n'), booktabs = TRUE)
wilcoxp <- wilcox.test(P.total ~ plec, data=s1)
wilcoxp['p.value']

sw.s1.table.plec <- s1 %>%
  group_by(plec) %>%
  shapiro_test(P.total) %>% select(plec, statistic, p)

##sw.table.staz
kable(sw.s1.table.plec, col.names = c('płeć', 'S-W', 'p'), booktabs = TRUE)

s0.mp <- s1 %>%
  group_by(miejsce.pracy) %>%
  summarise(m = mean(P.total), me=median(P.total), n=n())
kable(s0.mp, col.names = c('m.pracy', 'średnia', 'mediana', 'n'), booktabs = TRUE)

sw.s1.table.praca <- s1 %>%
  group_by(miejsce.pracy) %>%
  shapiro_test(P.total) %>% select(miejsce.pracy, statistic, p)

kable(sw.s1.table.praca, col.names = c('m.pracy', 'S-W', 'p'), booktabs = TRUE)

###

s0.st <- s1 %>%
  group_by(staz) %>%
  summarise(m = mean(P.total), me= median(P.total), n=n())
kable(s0.st, col.names = c('staż', 'średnia', 'mediana', 'n'), booktabs = TRUE)

sw.s1.table.staz <- s1 %>%
  group_by(staz) %>%
  shapiro_test(P.total) %>% select(staz, statistic, p)

kable(sw.s1.table.staz, col.names = c('staż', 'S-W', 'p'), booktabs = TRUE)


#####
#######
s0 <- read.csv("depresja_PSW_2023.csv", sep = ',',  header=F, skip=1, na.string="NA",
               col.names = c('time', 'P1', 'P2', 'P3', 'P4', 'P5',
                             'P6', 'P7', 'P8', 'P9', 'P10',
                             'P11', 'P12', 'P13', 'P14', 'P15',
                             'P16', 'P17', 'P18', 'P19', 'P20', 'P21', 'praca', 'staz', 'plec', 'x1'),
               fileEncoding = "UTF-8") %>%
  mutate(
    staz=recode(staz,
                '0-3 lat'   = '06 i mniej',
                '4-6 lat'   = '06 i mniej',
                '7-9 lat'   = '07-12',
                '8-12 lat'  = '07-12',
                '10-12 lat' = '07-12',
                '13-15 lat' = '13-18',
                '16-18 lat' = '13-18',
                '19-21 lat' = '19 i więcej',
                '22-24 lat' = '19 i więcej',
                '25 lat i więcej' = '19 i więcej'),
    P1=recode(P1,
              'Nie jestem smutny ani przygnębiony.'=0,
              'Odczuwam często smutek, przygnębienie'=1,
              'Przeżywam stale smutek, przygnębienie i nie mogę uwolnić się od tych przeżyć.'=2,
              'Jestem stale tak smutny i nieszczęśliwy, że jest to nie do wytrzymania.'=3),
    ###
    P2=recode(P2,
              'Nie przejmuję się zbytnio przyszłością.'=0,
              'Często martwię się o przyszłość.'=1,
              'Obawiam się, że w przyszłości nic dobrego mnie nie czeka.'=2,
              'Czuję, że przyszłość jest beznadziejna i nic tego nie zmieni.'=3),
    ###
    P3=recode(P3,
              'Sądzę, że nie popełniam większych zaniedbań.'=0,
              'Sądzę, że czynię więcej zaniedbań niż inni.'=1,
              'Kiedy spoglądam na to, co robiłem, widzę mnóstwo błędów i zaniedbań.'=2,
              'Jestem zupełnie niewydolny i wszystko robię źle.'=3),
    ###
    P4=recode(P4,
              'To, co robię, sprawia mi przyjemność.'=0,
              'Nie cieszy mnie to, co robię.'=1,
              'Nic mi teraz nie daje prawdziwego zadowolenia.'=2,
              'Nie potrafię przeżywać zadowolenia i przyjemności; wszystko mnie nuży.'=3),
    ##
    P5=recode(P5,
              'Nie czuję się winnym ani wobec siebie, ani wobec innych.'=0,
              'Dość często miewam wyrzuty sumienia.'=1,
              'Często czuję, że zawiniłem.'=2,
              'Stale czuję się winny.'=3),
    ##
    P6=recode(P6,
              'Sądzę, że nie zasługuję na karę'=0,
              'Sądzę, że zasługuję na karę'=1,
              'Spodziewam się ukarania'=2,
              'Wiem, że jestem karany (lub ukarany)'=3),
    ##
    P7=recode(P7,
              'Jestem z siebie zadowolony'=0,
              'Nie jestem z siebie zadowolony'=1,
              'Czuję do siebie niechęć'=2,
              'Nienawidzę siebie'=3),
    ##
    P8=recode(P8,
              'Nie czuję się gorszy od innych ludzi'=0,
              'Zarzucam sobie, że jestem nieudolny i popełniam błędy'=1,
              'Stale potępiam siebie za popełnione błędy'=2,
              'Winię siebie za wszelkie zło, które istnieje'=3),
    ##
    P9=recode(P9,
              'Nie myślę o odebraniu sobie życia'=0,
              'Myślę o samobójstwie — ale nie mógłbym tego dokonać'=1,
              'Pragnę odebrać sobie życie'=2,
              'Popełnię samobójstwo, jak będzie odpowiednia sposobność'=3),
    ##
    P10=recode(P10,
               'Nie płaczę częściej niż zwykle'=0,
               'Płaczę częściej niż dawniej'=1,
               'Ciągle chce mi się płakać'=2,
               'Chciałbym płakać, lecz nie jestem w stanie'=3),
    ##
    P11=recode(P11,
               'Nie jestem bardziej podenerwowany niż dawniej'=0,
               'Jestem bardziej nerwowy i przykry niż dawniej'=1,
               'Jestem stale zdenerwowany lub rozdrażniony'=2,
               'Wszystko, co dawniej mnie drażniło, stało się obojętne'=3),
    ##
    P12=recode(P12,
               'Ludzie interesują mnie jak dawniej'=0,
               'Interesuję się ludźmi mniej niż dawniej'=1,
               'Utraciłem większość zainteresowań innymi ludźmi'=2,
               'Utraciłem wszelkie zainteresowanie innymi ludźmi'=3),
    P13=recode(P13,
               'Decyzje podejmuję łatwo, tak jak dawniej'=0,
               'Częściej niż kiedyś odwlekam podjęcie decyzji'=1,
               'Mam dużo trudności z podjęciem decyzji'=2,
               'Nie jestem w stanie podjąć żadnej decyzji'=3),
    P14=recode(P14,
               'Sądzę, że wyglądam nie gorzej niż dawniej'=0,
               'Martwię się tym, że wyglądam staro i nieatrakcyjnie'=1,
               'Czuję, że wyglądam coraz gorzej'=2,
               'Jestem przekonany, że wyglądam okropnie i odpychająco'=3),
    P15=recode(P15,
               'Mogę pracować jak dawniej'=0,
               'Z trudem rozpoczynam każdą czynność'=1,
               'Z wielkim wysiłkiem zmuszam się do zrobienia czegokolwiek'=2,
               'Nie jestem w stanie nic zrobić'=3),
    ##
    P16=recode(P16,
               'Sypiam dobrze, jak zwykle'=0,
               'Sypiam gorzej niż dawniej'=1,
               'Rano budzę się 1–2 godziny za wcześnie i trudno jest mi ponownie usnąć'=2,
               'Budzę się kilka godzin za wcześnie i nie mogę usnąć'=3),
    ##
    P17=recode(P17,
               'Nie męczę się bardziej niż dawniej'=0,
               'Męczę się znacznie łatwiej niż poprzednio.'=1,
               'Męczę się wszystkim, co robię.'=2,
               'Jestem zbyt zmęczony, aby cokolwiek robić.'=3),
    ##
    P18=recode(P18,
               'Mam apetyt nie gorszy niż dawniej'=0,
               'Mam trochę gorszy apetyt'=1,
               'Apetyt mam wyraźnie gorszy'=2,
               'Nie mam w ogóle apetytu'=3),
    ##
    P19=recode(P19,
               'Nie tracę na wadze (w okresie ostatniego miesiąca)'=0,
               'Straciłem na wadze więcej niż 2 kg'=1,
               'Straciłem na wadze więcej niż 4 kg'=2,
               'Straciłem na wadze więcej niż 6 kg'=3),
    P20=recode(P20,
               'Nie martwię się o swoje zdrowie bardziej niż zawsze'=0,
               'Martwię się swoimi dolegliwościami, mam rozstrój żołądka, zaparcie, bóle'=1,
               'Stan mojego zdrowia bardzo mnie martwi, często o tym myślę'=2,
               'Tak bardzo martwię się o swoje zdrowie, że nie mogę o niczym innym myśleć'=3),
    P21=recode(P21,
               'Moje zainteresowania seksualne nie uległy zmianom'=0,
               'Jestem mniej zainteresowany sprawami płci (seksu)'=1,
               'Problemy płciowe wyraźnie mniej mnie interesują'=2,
               'Utraciłem wszelkie zainteresowanie sprawami seksu'=3)) %>%
  mutate (P=P1+P2+P3+P4+P5+P6 + P7 + P8 + P9 +P10 +
            P11+P12+P13+P14+P15+P16 + P17 + P18 + P19 +P20 + P21,
          Depresja = case_when( P < 20 ~ "B", P < 26 ~ "Ł", TRUE ~ "C"))


s0 <- read.csv("depresja_PSW_2023.csv", sep = ',',  header=F, skip=1, na.string="NA",
   col.names = c('time', 'P1', 'P2', 'P3', 'P4', 'P5',
   'P6', 'P7', 'P8', 'P9', 'P10',
   'P11', 'P12', 'P13', 'P14', 'P15',
   'P16', 'P17', 'P18', 'P19', 'P20', 'P21', 'praca', 'staz', 'plec', 'x1'),
               fileEncoding = "UTF-8") %>%
   mutate(
staz=recode(staz,
'0-3 lat'   = '06 i mniej',
'4-6 lat'   = '06 i mniej',
'7-9 lat'   = '07-12',
'8-12 lat'  = '07-12',
'10-12 lat' = '07-12',
'13-15 lat' = '13-18',
'16-18 lat' = '13-18',
'19-21 lat' = '19 i więcej',
'22-24 lat' = '19 i więcej',
'25 lat i więcej' = '19 i więcej'),
P1=recode(P1,
 'Nie jestem smutny ani przygnębiony.'=0,
 'Odczuwam często smutek, przygnębienie'=1,
 'Przeżywam stale smutek, przygnębienie i nie mogę uwolnić się od tych przeżyć.'=2,
 'Jestem stale tak smutny i nieszczęśliwy, że jest to nie do wytrzymania.'=3),
###
P2=recode(P2,
 'Nie przejmuję się zbytnio przyszłością.'=0,
 'Często martwię się o przyszłość.'=1,
 'Obawiam się, że w przyszłości nic dobrego mnie nie czeka.'=2,
 'Czuję, że przyszłość jest beznadziejna i nic tego nie zmieni.'=3),
###
P3=recode(P3,
 'Sądzę, że nie popełniam większych zaniedbań.'=0,
 'Sądzę, że czynię więcej zaniedbań niż inni.'=1,
 'Kiedy spoglądam na to, co robiłem, widzę mnóstwo błędów i zaniedbań.'=2,
 'Jestem zupełnie niewydolny i wszystko robię źle.'=3),
###
P4=recode(P4,
 'To, co robię, sprawia mi przyjemność.'=0,
 'Nie cieszy mnie to, co robię.'=1,
 'Nic mi teraz nie daje prawdziwego zadowolenia.'=2,
 'Nie potrafię przeżywać zadowolenia i przyjemności; wszystko mnie nuży.'=3),
##
P5=recode(P5,
 'Nie czuję się winnym ani wobec siebie, ani wobec innych.'=0,
 'Dość często miewam wyrzuty sumienia.'=1,
 'Często czuję, że zawiniłem.'=2,
 'Stale czuję się winny.'=3),
##
P6=recode(P6,
 'Sądzę, że nie zasługuję na karę'=0,
 'Sądzę, że zasługuję na karę'=1,
 'Spodziewam się ukarania'=2,
 'Wiem, że jestem karany (lub ukarany)'=3),
##
P7=recode(P7,
 'Jestem z siebie zadowolony'=0,
 'Nie jestem z siebie zadowolony'=1,
 'Czuję do siebie niechęć'=2,
 'Nienawidzę siebie'=3),
##
P8=recode(P8,
 'Nie czuję się gorszy od innych ludzi'=0,
 'Zarzucam sobie, że jestem nieudolny i popełniam błędy'=1,
 'Stale potępiam siebie za popełnione błędy'=2,
 'Winię siebie za wszelkie zło, które istnieje'=3),
##
P9=recode(P9,
 'Nie myślę o odebraniu sobie życia'=0,
 'Myślę o samobójstwie — ale nie mógłbym tego dokonać'=1,
 'Pragnę odebrać sobie życie'=2,
 'Popełnię samobójstwo, jak będzie odpowiednia sposobność'=3),
##
P10=recode(P10,
 'Nie płaczę częściej niż zwykle'=0,
 'Płaczę częściej niż dawniej'=1,
 'Ciągle chce mi się płakać'=2,
 'Chciałbym płakać, lecz nie jestem w stanie'=3),
##
P11=recode(P11,
 'Nie jestem bardziej podenerwowany niż dawniej'=0,
 'Jestem bardziej nerwowy i przykry niż dawniej'=1,
 'Jestem stale zdenerwowany lub rozdrażniony'=2,
 'Wszystko, co dawniej mnie drażniło, stało się obojętne'=3),
##
P12=recode(P12,
 'Ludzie interesują mnie jak dawniej'=0,
 'Interesuję się ludźmi mniej niż dawniej'=1,
 'Utraciłem większość zainteresowań innymi ludźmi'=2,
 'Utraciłem wszelkie zainteresowanie innymi ludźmi'=3),
P13=recode(P13,
 'Decyzje podejmuję łatwo, tak jak dawniej'=0,
 'Częściej niż kiedyś odwlekam podjęcie decyzji'=1,
 'Mam dużo trudności z podjęciem decyzji'=2,
 'Nie jestem w stanie podjąć żadnej decyzji'=3),
P14=recode(P14,
 'Sądzę, że wyglądam nie gorzej niż dawniej'=0,
 'Martwię się tym, że wyglądam staro i nieatrakcyjnie'=1,
 'Czuję, że wyglądam coraz gorzej'=2,
 'Jestem przekonany, że wyglądam okropnie i odpychająco'=3),
P15=recode(P15,
 'Mogę pracować jak dawniej'=0,
 'Z trudem rozpoczynam każdą czynność'=1,
 'Z wielkim wysiłkiem zmuszam się do zrobienia czegokolwiek'=2,
 'Nie jestem w stanie nic zrobić'=3),
##
P16=recode(P16,
 'Sypiam dobrze, jak zwykle'=0,
 'Sypiam gorzej niż dawniej'=1,
 'Rano budzę się 1–2 godziny za wcześnie i trudno jest mi ponownie usnąć'=2,
 'Budzę się kilka godzin za wcześnie i nie mogę usnąć'=3),
##
P17=recode(P17,
 'Nie męczę się bardziej niż dawniej'=0,
 'Męczę się znacznie łatwiej niż poprzednio.'=1,
 'Męczę się wszystkim, co robię.'=2,
 'Jestem zbyt zmęczony, aby cokolwiek robić.'=3),
##
P18=recode(P18,
 'Mam apetyt nie gorszy niż dawniej'=0,
 'Mam trochę gorszy apetyt'=1,
 'Apetyt mam wyraźnie gorszy'=2,
 'Nie mam w ogóle apetytu'=3),
##
P19=recode(P19,
 'Nie tracę na wadze (w okresie ostatniego miesiąca)'=0,
 'Straciłem na wadze więcej niż 2 kg'=1,
 'Straciłem na wadze więcej niż 4 kg'=2,
 'Straciłem na wadze więcej niż 6 kg'=3),
P20=recode(P20,
 'Nie martwię się o swoje zdrowie bardziej niż zawsze'=0,
 'Martwię się swoimi dolegliwościami, mam rozstrój żołądka, zaparcie, bóle'=1,
 'Stan mojego zdrowia bardzo mnie martwi, często o tym myślę'=2,
 'Tak bardzo martwię się o swoje zdrowie, że nie mogę o niczym innym myśleć'=3),
P21=recode(P21,
'Moje zainteresowania seksualne nie uległy zmianom'=0,
'Jestem mniej zainteresowany sprawami płci (seksu)'=1,
'Problemy płciowe wyraźnie mniej mnie interesują'=2,
'Utraciłem wszelkie zainteresowanie sprawami seksu'=3)) %>%
  mutate (P=P1+P2+P3+P4+P5+P6 + P7 + P8 + P9 +P10 +
            P11+P12+P13+P14+P15+P16 + P17 + P18 + P19 +P20 + P21,
          Depresja = case_when( P < 20 ~ "B", P < 26 ~ "Ł", TRUE ~ "C"))

s0.staz <- s0 %>%
  group_by(staz) %>%
  summarise(m = mean(P), me=median(P), n=n())
kable(s0.staz, col.names = c('staż', 'średnia', 'mediana', 'n'), booktabs = TRUE)

sw.s0.table.staz <- s0 %>%
  group_by(staz) %>%
  shapiro_test(P) %>% select(staz, statistic, p)
kable(sw.s0.table.staz, col.names = c('staż', 'S-W', 'p'), booktabs = TRUE)


sw.s0.table.mp <- s0 %>%
  group_by(praca) %>%
  shapiro_test(P) %>% select(praca, statistic, p)
kable(sw.s0.table.mp, col.names = c('m-pracy.', 'S-W', 'p'), booktabs = TRUE)

####
fb <- read.csv("rwc-2015-2023.csv", sep = ';', dec = ".",  header=T, na.string="NA" ) %>%
  select(year, weight, poscode )
backs <- c('BR', 'CE', 'FB', 'FH', 'WI', 'SH', 'HB', 'BB')
forwards <- c('PR', 'SR', 'HK', 'FF')

fb <- fb %>% mutate (poscode = recode(poscode,
                                      'BR' = 'A',
                                      'CE' = 'A',
                                      'FB' = 'A',
                                      'FH' = 'A',
                                      'WI' = 'A',
                                      'SH' = 'A',
                                      'HB' = 'A',
                                      'BB' = 'A',
                                      'PR' = 'M',
                                      'SR' = 'M',
                                      'HK' = 'M',
                                      'FF' = 'M'
) )

b <- fb %>% filter (poscode == 'A')
b$weight

mean.b <- mean(b$weight, na.rm = TRUE)

median.b <- median(b$weight, na.rm=TRUE)
q1.b <- quantile(b$weight, probs=.25, na.rm  =TRUE)
q3.b <- quantile(b$weight, probs=.75, na.rm =TRUE)
iqr.b <- IQR(b$weight, na.rm = TRUE)
sd.b <- sd(b$weight, na.rm=TRUE)

f <- fb %>% filter (poscode == 'M' )

mean.f <- mean(f$weight, na.rm=TRUE)
median.f <- median(f$weight, na.rm=TRUE)
q1.f <- quantile(f$weight, probs=.25, na.rm =TRUE)
q3.f <- quantile(f$weight, probs=.75, na.rm =TRUE)
iqr.f <- IQR(f$weight, na.rm = TRUE)
sd.f <- sd(f$weight, na.rm=TRUE)

miara <- c('średnia', 'mediana', 'odchyl.st', 'iqr')
atak <- c(mean.b, median.b, sd.b, iqr.b)
mlyn <- c(mean.f, median.f, sd.f, iqr.f)
porownanie <- data.frame(miara, atak, mlyn)

##

s0 <- read.csv("depresja_PSW_2023.csv", sep = ',',  header=F, skip=1, na.string="NA",
               col.names = c('time', 'P1', 'P2', 'P3', 'P4', 'P5',
                             'P6', 'P7', 'P8', 'P9', 'P10',
                             'P11', 'P12', 'P13', 'P14', 'P15',
                             'P16', 'P17', 'P18', 'P19', 'P20', 'P21', 'praca', 'staz', 'plec', 'x1'),
               fileEncoding = "UTF-8")  %>%
  mutate(
    staz=recode(staz,
                '0-3 lat'   = '06 i mniej',
                '4-6 lat'   = '06 i mniej',
                '7-9 lat'   = '07-12',
                '8-12 lat'  = '07-12',
                '10-12 lat' = '07-12',
                '13-15 lat' = '13-18',
                '16-18 lat' = '13-18',
                '19-21 lat' = '19 i więcej',
                '22-24 lat' = '19 i więcej',
                '25 lat i więcej' = '19 i więcej'),
    P1=recode(P1,
              'Nie jestem smutny ani przygnębiony.'=0,
              'Odczuwam często smutek, przygnębienie'=1,
              'Przeżywam stale smutek, przygnębienie i nie mogę uwolnić się od tych przeżyć.'=2,
              'Jestem stale tak smutny i nieszczęśliwy, że jest to nie do wytrzymania.'=3),
    ###
    P2=recode(P2,
              'Nie przejmuję się zbytnio przyszłością.'=0,
              'Często martwię się o przyszłość.'=1,
              'Obawiam się, że w przyszłości nic dobrego mnie nie czeka.'=2,
              'Czuję, że przyszłość jest beznadziejna i nic tego nie zmieni.'=3),
    ###
    P3=recode(P3,
              'Sądzę, że nie popełniam większych zaniedbań.'=0,
              'Sądzę, że czynię więcej zaniedbań niż inni.'=1,
              'Kiedy spoglądam na to, co robiłem, widzę mnóstwo błędów i zaniedbań.'=2,
              'Jestem zupełnie niewydolny i wszystko robię źle.'=3),
    ###
    P4=recode(P4,
              'To, co robię, sprawia mi przyjemność.'=0,
              'Nie cieszy mnie to, co robię.'=1,
              'Nic mi teraz nie daje prawdziwego zadowolenia.'=2,
              'Nie potrafię przeżywać zadowolenia i przyjemności; wszystko mnie nuży.'=3),
    ##
    P5=recode(P5,
              'Nie czuję się winnym ani wobec siebie, ani wobec innych.'=0,
              'Dość często miewam wyrzuty sumienia.'=1,
              'Często czuję, że zawiniłem.'=2,
              'Stale czuję się winny.'=3),
    ##
    P6=recode(P6,
              'Sądzę, że nie zasługuję na karę'=0,
              'Sądzę, że zasługuję na karę'=1,
              'Spodziewam się ukarania'=2,
              'Wiem, że jestem karany (lub ukarany)'=3),
    ##
    P7=recode(P7,
              'Jestem z siebie zadowolony'=0,
              'Nie jestem z siebie zadowolony'=1,
              'Czuję do siebie niechęć'=2,
              'Nienawidzę siebie'=3),
    ##
    P8=recode(P8,
              'Nie czuję się gorszy od innych ludzi'=0,
              'Zarzucam sobie, że jestem nieudolny i popełniam błędy'=1,
              'Stale potępiam siebie za popełnione błędy'=2,
              'Winię siebie za wszelkie zło, które istnieje'=3),
    ##
    P9=recode(P9,
              'Nie myślę o odebraniu sobie życia'=0,
              'Myślę o samobójstwie — ale nie mógłbym tego dokonać'=1,
              'Pragnę odebrać sobie życie'=2,
              'Popełnię samobójstwo, jak będzie odpowiednia sposobność'=3),
    ##
    P10=recode(P10,
               'Nie płaczę częściej niż zwykle'=0,
               'Płaczę częściej niż dawniej'=1,
               'Ciągle chce mi się płakać'=2,
               'Chciałbym płakać, lecz nie jestem w stanie'=3),
    ##
    P11=recode(P11,
               'Nie jestem bardziej podenerwowany niż dawniej'=0,
               'Jestem bardziej nerwowy i przykry niż dawniej'=1,
               'Jestem stale zdenerwowany lub rozdrażniony'=2,
               'Wszystko, co dawniej mnie drażniło, stało się obojętne'=3),
    ##
    P12=recode(P12,
               'Ludzie interesują mnie jak dawniej'=0,
               'Interesuję się ludźmi mniej niż dawniej'=1,
               'Utraciłem większość zainteresowań innymi ludźmi'=2,
               'Utraciłem wszelkie zainteresowanie innymi ludźmi'=3),
    P13=recode(P13,
               'Decyzje podejmuję łatwo, tak jak dawniej'=0,
               'Częściej niż kiedyś odwlekam podjęcie decyzji'=1,
               'Mam dużo trudności z podjęciem decyzji'=2,
               'Nie jestem w stanie podjąć żadnej decyzji'=3),
    P14=recode(P14,
               'Sądzę, że wyglądam nie gorzej niż dawniej'=0,
               'Martwię się tym, że wyglądam staro i nieatrakcyjnie'=1,
               'Czuję, że wyglądam coraz gorzej'=2,
               'Jestem przekonany, że wyglądam okropnie i odpychająco'=3),
    P15=recode(P15,
               'Mogę pracować jak dawniej'=0,
               'Z trudem rozpoczynam każdą czynność'=1,
               'Z wielkim wysiłkiem zmuszam się do zrobienia czegokolwiek'=2,
               'Nie jestem w stanie nic zrobić'=3),
    ##
    P16=recode(P16,
               'Sypiam dobrze, jak zwykle'=0,
               'Sypiam gorzej niż dawniej'=1,
               'Rano budzę się 1–2 godziny za wcześnie i trudno jest mi ponownie usnąć'=2,
               'Budzę się kilka godzin za wcześnie i nie mogę usnąć'=3),
    ##
    P17=recode(P17,
               'Nie męczę się bardziej niż dawniej'=0,
               'Męczę się znacznie łatwiej niż poprzednio.'=1,
               'Męczę się wszystkim, co robię.'=2,
               'Jestem zbyt zmęczony, aby cokolwiek robić.'=3),
    ##
    P18=recode(P18,
               'Mam apetyt nie gorszy niż dawniej'=0,
               'Mam trochę gorszy apetyt'=1,
               'Apetyt mam wyraźnie gorszy'=2,
               'Nie mam w ogóle apetytu'=3),
    ##
    P19=recode(P19,
               'Nie tracę na wadze (w okresie ostatniego miesiąca)'=0,
               'Straciłem na wadze więcej niż 2 kg'=1,
               'Straciłem na wadze więcej niż 4 kg'=2,
               'Straciłem na wadze więcej niż 6 kg'=3),
    P20=recode(P20,
               'Nie martwię się o swoje zdrowie bardziej niż zawsze'=0,
               'Martwię się swoimi dolegliwościami, mam rozstrój żołądka, zaparcie, bóle'=1,
               'Stan mojego zdrowia bardzo mnie martwi, często o tym myślę'=2,
               'Tak bardzo martwię się o swoje zdrowie, że nie mogę o niczym innym myśleć'=3),
    P21=recode(P21,
               'Moje zainteresowania seksualne nie uległy zmianom'=0,
               'Jestem mniej zainteresowany sprawami płci (seksu)'=1,
               'Problemy płciowe wyraźnie mniej mnie interesują'=2,
               'Utraciłem wszelkie zainteresowanie sprawami seksu'=3)) %>%
  mutate (P=P1+P2+P3+P4+P5+P6 + P7 + P8 + P9 +P10 +
            P11+P12+P13+P14+P15+P16 + P17 + P18 + P19 +P20 + P21,
          Depresja = case_when( P < 20 ~ "B", P < 26 ~ "Ł", TRUE ~ "C"))
