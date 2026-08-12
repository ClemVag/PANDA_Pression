# PACKAGES ET FONCTIONS -----
{
  library(conflicted)
  library(cowplot)
  library(dplyr)
  library(flextable)
  library(forcats)
  library(geodata)
  library(ggnewscale) # Package permettant à plusieurs échelles de couleurs de coexister
  library(ggplot2)
  library(ggrepel)
  library(ggspatial)
  library(here)    # rapports
  library(janitor) # doublons
  library(leaflet)
  library(lubridate) # date et heure
  library(mapsf)
  library(mapview)
  library(openxlsx)
  library(osmdata)
  library(plotly)
  library(quarto) # rapports
  library(questionr)
  library(raster)
  library(readr)
  library(readxl)
  library(remotes)
  library(rmarkdown) # rapports
  library(scales)
  library(sf)
  library(spData)
  library(stars)
  library(stringr)
  library(summarytools)
  library(tidyr)
  library(tidyr) # pivot de table
  library(tidyverse)
  library(tmap)
  ## lors d'un export dans excel permet de garder format date YYYY-MM-DD (sinon peut devenir YYYY-DD-MM)
  options(openxlsx.dateFormat = "yyyy-mm-dd")
  conflict_prefer_all("dplyr")
  conflicts_prefer(lubridate::month)
}

# FONCTIONS
`%nin%` <- negate(`%in%`)


# 1. IMPORT DU FICHIER DE PARAMETRAGE ----
Parametrage <- read_excel("Lancement Utilisateur.xlsm")
Parametrage<-Parametrage |>
  select(Variable,Valeur) |>
  filter(!is.na(Variable))

P<-Parametrage |>
  select(Valeur)
row.names(P)<-Parametrage$Variable
Parametrage<-as.data.frame(t(P))


# 2. IMPORT DONNEES CARTOGRAPHIQUES ----

## 2.1. IMPORT DES COUCHES ----
if(Parametrage$TYPE_PERIMETRE[1] == "DT")
{
  Limites_DT<-st_read("R:/Agence/Données SIG/1_DONNEES DE REFERENCE_BASSIN/ADMINISTRATIF_DT/_VERSIONS STABILISEES/DT_2018/AESN_DT_2018_v2023.shp")
  Limites_DT<-Limites_DT |>
    rename("ID"= DT_CODE)
  Limites_DT$ID<-str_to_upper(Limites_DT$ID)
}
if(Parametrage$TYPE_PERIMETRE[1] == "DEPARTEMENT")
{
  Limites_Dpt <-st_read("02_data/CARTO/Departements_AESN.shp")
  Limites_Dpt<-Limites_Dpt %>%
    select(DDEP_L_LIB,geometry) %>%
    rename("ID" = DDEP_L_LIB)
}
if(Parametrage$TYPE_PERIMETRE[1] == "UH")
{
  Limites_UH<-st_read("R:/Agence/Données SIG/1_DONNEES DE REFERENCE_BASSIN/UNITES HYDROGRAPHIQUES/_VERSIONS STABILISEES/2022_SDAGE/UH_actives.shp")
  Limites_UH<-Limites_UH %>%
    select(NOM,geometry) %>%
    rename("ID" = NOM)
}
if(Parametrage$TYPE_PERIMETRE[1] == "EPCI")
{
  Limites_EPCI<-st_read("R:/Agence/Données SIG/1_DONNEES DE REFERENCE_BASSIN/ADMINISTRATIF_BASSIN/_VERSIONS STABILISEES/2025/EPCI_bsn_2026.shp")
  Limites_EPCI<-Limites_EPCI %>%
    select(nom_offi_1,geometry) %>%
    rename("ID" = nom_offi_1)
}
#Limites personnalisées : changer l'emplacement du fichier
if(Parametrage$TYPE_PERIMETRE[1] == "PERSONNALISE")
{
  fichier <- gsub("////", "/", Parametrage$perimetre_custom[1])
  Limites_Custom <-st_read(file.path(fichier))
}

## 2.2. FILTRE GEOGRAPHIQUE SUR LE PERIMETRE DEFINI ----

# On crée un filtre géographique sur la base du périmètre donné pour l'étude
Filtre <- switch(
  Parametrage$TYPE_PERIMETRE[1],
  "DT" = Limites_DT,
  "DEPARTEMENT" = Limites_Dpt,
  "UH"  = Limites_UH,
  "EPCI" = Limites_Communes,
  Limites_Custom  # Valeur par défaut (si TYPE_PERIMETRE n'est ni "DEP" ni "UH" ni "COM")
)
if(Parametrage$TYPE_PERIMETRE[1] %nin% c("PERSONNALISE","DT"))
{
  Filtre<-Filtre %>%
    filter(ID == Parametrage$PERIMETRE[1] )
}
if(Parametrage$TYPE_PERIMETRE[1] == "DT")
{
  Filtre<-Filtre %>%
    filter(ID == Parametrage$DT[1] )
}

## 2.3. AJOUT DU BUFFER ----
Filtre_buffer<-st_buffer(Filtre,ifelse(is.na(Parametrage$TAMPON),0,as.numeric(Parametrage$TAMPON)*1000))

#3. SELECTION DES POINTS DE REJET ----





test_map<-ggplot(data=Filtre_buffer)+
   geom_sf() +
   geom_sf(data=Filtre, colour="red", alpha=0.5)
print(test_map)
