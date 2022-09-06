# Utfordring 1.3
# Oppgave 5.

# Fjerner alt fra Global Envirement så ingenting kan klusse til koden senere.
rm(list = ls())

# Laster inn nødvendige pakker.
library(ineq)
library(httr)
library(dplyr)
library(readxl) 
library(rjstat)
library(cowplot)
library(janitor)
library(ggplot2)
library(gglorenz)
library(tidyverse)  
library(gridExtra)
library(hrbrthemes)
library(PxWebApiData)

# Setter lokalet til no-No for å få norsk språk (for å få øæå).
Sys.setlocale(locale="no_NO")

# Setter arbeidsplassen.
setwd("~/")

# Laster ned excel filen "GCIPrawdatatest.xlsx". 
# Leser dataen inn i "decile_data".
# Det er fjernet data fra Norge i 1980-1990 fordi det var "skittent".
decile_data <- read_excel("GCIPrawdatatest.xlsx", 
                          skip = 2)

# Dataen er nå i'tibble'. 
# Vi bruker head funksjonen for å se på de første linjene:
head(decile_data) 

# Nå bruker vi løkker for å fullføre oppgaven vår. Vi begynner med å lage en 
# ny variabel i datasettet vårt, gini, som vi først satte til 0 for alle 
# land-årskombinasjoner.
decile_data$gini <- 0

# Nå bruker vi en løkke for å kjøre gjennom alle radene i datasettet vårt 
# (land-årskombinasjoner). For hver rad vil vi gjenta 
# Gini-koeffisientberegningen fra R walk-through 5.4 og lagre den 
# resulterende verdien i gini-variabelen vi opprettet.
# Funksjonen som beregner Gini-koeffisienter fra en vektor av tall kalles 
# Gini, og vi bruker den på inntektsdesilene for å gi oss antall rader i 
# decil_data
noc <- nrow(decile_data)

for (i in seq(1, noc)){
  # Go to Row I to get the decile data
  decs_i <- unlist(decile_data[i, 3:12])
  decile_data$gini[i] <- Gini(decs_i)
}

# Med denne koden kalkulerer vi 4,799 Gini-koefisienter uten å måtte 
# mannuelt kjøre den samme koden 4,799 ganger. Vi ser nå på noen 
# oppsummerende mål for gini-koeffisienten. Først bruker vi 
# delsettfunksjonen til å velge nordiske land og lagre deres data som 
# temp_data. Som et eksempel har vi valgt fire engelskspråklige land: 
# Storbritannia, USA, Irland og Australia.
temp_data <- subset(
  decile_data, Country %in% c("United States","Sweden","Finland","Norway", 
                              "Denmark"))

# Så plotter vi dataen med ggplot.
ggplot(temp_data, 
       aes(x = Year, y = gini, color = Country)) +
  geom_line(size = 1.5) +
  ylab("Gini") +
  ggtitle("Gini coefficients for Nordic countries") +
  theme_bw()

# Vi har brukt den gitte R-koden for å lage et diagram med Gini-koeffisienter
# for de fire største nordiske landene og USA. Det man kan tydelig se er 
# forskjellen i Gini-koeffisientene mellom de nordiske landene kontra USA.
# USA ligger på en Gini-verdi på ca. 0,35 på det minste og 0,41 på det meste, 
# mens Finnland, Norge, Sverige og Danmark ligger på rundt 0,23 til 0,27.
# Hvorfor er forskjellen mellom de nordiske landene og USA så stor?
# Som man vet er Gini-koeffisienten et mål på inntektsfordelingen 
# (eller formuesfordelingen) for et land som varierer fra 0 til 1. 0 er perfekt 
# likhet, mens 1 er perfekt ulikhet. Så for USA som har en gini på 0,4-0,5 
# vil det si at de har et stort inntekstgap. Altså stor forskjell mellom de 
# rike og de fattige. Gini på dette nivået er ofte forbundet med politisk 
# ustabilitet. Derfor er advarselsnivået til Gini-indeksen 0,4.

# Danmark, Finland, Norge og Sverige på den andre siden har et Gini-gjenomsnitt
# på 0,2 som representerer perfekt inntektslikhet. Altså at forskjellen mellom
# rik og fattig ikke så stor.

# Sletter data-en og verdiene vi ikke trenger mer.
rm(decs_i, i, noc, decile_data)

# Utfordring 1.3
# Oppgave 6.

# Laster in URL-en fra ssb.
URL <- "https://data.ssb.no/api/v0/no/table/12558/"

# Laster in data-en fra ssb.
data <- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:Kommune",
        "values": [
          "5401",
          "1902"
        ]
      }
    },
    {
      "code": "InntektSkatt",
      "selection": {
        "filter": "item",
        "values": [
          "00"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "VerdiDesil"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2020"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

# Slår sammen URL og data til tmp med POST funksjonen.
tmp <- POST(URL, body = data, encode = "json", verbose())

# Gjør om til tibble og rydder navnene.
data.tromso <- fromJSONstat(content(tmp, "text")) %>% 
  clean_names() %>% 
  as_tibble()

rm(data, URL, tmp)

# Fjerner Na.
data.tromso <- na.omit(data.tromso)

# Endrer på kolonne navnene.
names(data.tromso) <- c('Region', 'InntektForEtterSkatt', 'Desil', 'Statistikkvariabel', 'år', 'Value')

# Lager to dataset, en med data fra 2005 og en med data fra 2020.
data.tromso2005 <- filter(data.tromso, år == "2005")
data.tromso2020 <- filter(data.tromso, år == "2020")

# Fjerner 2 verdier vi ikke får bruk for.
data.tromso <- data.tromso %>% 
  select(-'Statistikkvariabel', -'InntektForEtterSkatt')

# Plotter for år 2005 og 2020.
data.tromso %>%
  group_by(Region) %>% 
  filter(år %in% c("2005", "2020")) %>%
  na.omit() %>% 
  ggplot(aes(x = Value, colour = år)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  hrbrthemes::theme_ipsum_rc() +
  labs(x = "Total personer i Tromsø i prosent",
       y = "Total inntekter i Tromsø i prosent",
       title = "Inntektsfordelingen i Tromsø kommune",
       caption = "Source: https://data.ssb.no/api/v0/no/table/12558/")

# Nå har vi brukt R-pakken gglorenz til å tegne Lorenz-kurver for 
# inntektsfodelingen i Tromsø kommune. Man ser tydelig at ulikheten i byen 
# ikke har økt noe særlig fra 2005-2020. Det er snakk om mikroskopiske 
# endringer som er lettere og se om vi stiller begge årene ved siden av
# hverandre.

# Plotter for inntekstfordelingen i Tromsø i 2005.
plot.2005 <- data.tromso2005 %>%
  group_by(Region) %>% 
  filter(InntektForEtterSkatt == "Samlet inntekt") %>%
  na.omit() %>%
  ggplot(aes(x=Value, colour = "2005")) +
  stat_lorenz(desc = FALSE, color ="red") +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  annotate(geom="text", x=0.50, y=0.37, label="29.12%",
           color="darkslategray", size = 4) +
  labs(x = "Total personer i Tromsø i prosent",
       y = "Total inntekter i Tromsø i prosent",
       title = "Inntektsfordelingen i Tromsø kommune år 2005",
       caption = "Source: https://data.ssb.no/api/v0/no/table/12558/",
       colour= "Tromsø Samlet Inntekt") +
  hrbrthemes::theme_ipsum_rc() +
  annotate_ineq(data.tromso2005$Value, 
                measure_ineq = "Gini", color = "black",
                family = theme_get()$text[["family"]],
                size = theme_get()$text[["size"]]/2,
                fontface = "italic",
                x = 0.1,
                y = 1,
                decimals = 4)

# Plotter for inntekstfordelingen i Tromsø i 2020.
plot.2020 <- data.tromso2020 %>%
  group_by(Region) %>% 
  filter(InntektForEtterSkatt == "Samlet inntekt") %>%
  na.omit() %>%
  ggplot(aes(x=Value, colour = "2020")) +
  stat_lorenz(desc = FALSE, color ="red") +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  annotate(geom="text", x=0.50, y=0.37, label="29.32%",
           color="darkslategray", size = 4) +
  labs(x = "Total personer i Tromsø i prosent",
       y = "Total inntekter i Tromsø i prosent",
       title = "Inntektsfordelingen i Tromsø kommune år 2020",
       caption = "Source: https://data.ssb.no/api/v0/no/table/12558/",
       colour= "Tromsø Samlet Inntekt") +
  hrbrthemes::theme_ipsum_rc() +
  annotate_ineq(data.tromso2020$Value, 
                measure_ineq = "Gini", color = "black",
                family = theme_get()$text[["family"]],
                size = theme_get()$text[["size"]]/2,
                fontface = "italic",
                x = 0.1,
                y = 1,
                decimals = 4)

# Plotter de i samme plot.
# plot_grid(plot.2005, plot.2020, labels = "AUTO")
grid.arrange(plot.2005, plot.2020, ncol = 2)

# I 2005 var gini-koeffisienten på 0,2912 / 29,12%, mens i 2020 var den på 
# 0,2932 / 29,32%, den har altså steget med 0,20 %.