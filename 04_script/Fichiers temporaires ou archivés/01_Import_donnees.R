# SCRIPT 1 : IMPORT DES DONNEES -----


# Ce premier script permet l'import des données d'analyse dans R.
# 
# ETAPE PREALABLE : TELECHARGER LES DONNEES
# Les requêtes suivantes sont à actualiser dans l'entrepôt et à ajouter dans le dossier data :
# Données internes à télécharger depuis l'entrepôt de données :
# - Lister les requêtes
#
# Autres données internes:
#
#
# Données externes :


# PARAMETRAGE ----
# A transférer dans le script-maître
# Type périmètre : 
# - DEP (département) --> Indiquer le n° dans le PERIMETRE
# - UH --> Indiquer le nom en majuscules
# - COM (commune) --> Indiquer le code INSEE
# - CUSTOM (personnalisé) : indique qu'on prend un fichier géographique fourni par le BE
#                           Mettre le fichier géographique dans le dossier data
TYPE_PERIMETRE = "UH"
PERIMETRE = "SERRE"




# 0. PACKAGES ET FONCTIONS ----
# PACKAGES
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




# 1. DONNEES AGENCE ----

## 1.1. DONNEES CARTOGRAPHIQUES ----
### 1.1.1. PERIMETRES D'ETUDE ----
if(TYPE_PERIMETRE == "UH")
{
  Limites_UH<-st_read("R:/Agence/Données SIG/DVO/SIGISTE/_Data_Source/UH/UH_BVO.shp")
  Limites_UH<-Limites_UH %>%
    select(UH,geometry) %>%
    rename("ID" = UH)
}

if(TYPE_PERIMETRE == "DEP")
{
  Limites_Dpt <-st_read("R:/Agence/Données SIG/DVO/SIGISTE/_Data_Source/Administratif/Unités_Administratives/IGN 2018/DVO_BVO/AESN_DEPARTEMENT.shp")
  Limites_Dpt<-Limites_Dpt %>% 
    select(NOM_DEPT,INSEE_DEPT,geometry) %>% 
    rename("ID" = INSEE_DEPT)
}

if(TYPE_PERIMETRE == "COM")
{
  Limites_Communes<-st_read("R:/Agence/Données SIG/DVO/SIGISTE/_Data_Source/Administratif/Adm_Com_Pt_av_lien_EPCI.shp")
  Limites_Communes<-Limites_Communes %>% 
    select(CODE_INSEE,NOM_COM,NOM_EPCI,geometry) %>% 
    rename("ID"=CODE_INSEE)
}


#Limites personnalisées : changer l'emplacement du fichier
if(TYPE_PERIMETRE == "CUSTOM")
{
  Limites_Custom <-st_read("02_data/XXX.shp")
  Limites_Custom<-Limites_Custom |> 
    rename("ID"=fid)
}


### 1.1.2. FILTRE GEOGRAPHIQUE SUR LE PERIMETRE DEFINI ----
# On crée un filtre géographique sur la base du périmètre donné pour l'étude
Filtre <- switch(
  TYPE_PERIMETRE,
  "DEP" = Limites_Dpt,
  "UH"  = Limites_UH,
  "COM" = Limites_Communes,
  Limites_Custom  # Valeur par défaut (si TYPE_PERIMETRE n'est ni "DEP" ni "UH" ni "COM")
)

if(TYPE_PERIMETRE != "CUSTOM")
{
  Filtre<-Filtre %>% 
    filter(ID == PERIMETRE)
}


## 1.2. DONNEES REFERENTIEL OUVRAGES (SITOUREF)  ----


### 1.2.1. IMPLANTATION DES OUVRAGES ----

# Import de la table
ouvrages_amont_et_rejets <- read_excel("02_data/SITOUREF/BET-023-2026_002_Info_points_rejet.xlsx")
# Création d'une table ouvrage et d'une table rejet
# Cela permettra de cibler les points d'AS OU de rejet situés dans un périmètre géographique données
ouvrages<-ouvrages_amont_et_rejets %>% 
  select(`No interne Sitou`,`X (L93)`,`Y (L93)`) %>% 
  rename(         X = `X (L93)`,
                  Y = `Y (L93)`) 

rejets<-ouvrages_amont_et_rejets %>% 
  select(`No interne Sitou`,`Coordonnée X L93`,`Coordonnée Y L93`) %>% 
  rename(         X = `Coordonnée X L93`,
                  Y = `Coordonnée Y L93`)

# Test : les coordonnées géographiques sont-elles bien distinctes entre les ouvrages et les rejets ? 
test_coordonnees<-ouvrages_amont_et_rejets %>% 
  mutate(Coord_amont = paste0(`X (L93)`,`Y (L93)`),
         Coord_rejet = paste0(`Coordonnée X L93`,`Coordonnée Y L93`),
         Comp=ifelse(Coord_amont == Coord_rejet,"IDENTIQUE","DIFFERENT")) %>% 
  select(`No interne Sitou`,Coord_amont,Coord_rejet,Comp) %>% 
  group_by(Comp) %>% 
  summarise(n=n())

# Transformation des virgules en point pour que les coordonnées puissent être lues comme des nombres......
ouvrages$X <- gsub(",",".",ouvrages$X)
ouvrages$Y <- gsub(",",".",ouvrages$Y)
ouvrages$X<-as.numeric(ouvrages$X)
ouvrages$Y<-as.numeric(ouvrages$Y)

rejets$X <- gsub(",",".",rejets$X)
rejets$Y <- gsub(",",".",rejets$Y)
rejets$X<-as.numeric(rejets$X)
rejets$Y<-as.numeric(rejets$Y)

# Conversion en données géographiques
ouvrages<-st_as_sf(ouvrages, coords = c("X","Y"), crs=2154)
rejets<-st_as_sf(rejets, coords = c("X","Y"), crs=2154)


### 1.2.1. CARACTERISTIQUES TECHNIQUES DES OUVRAGES ----
Liste_points_AS_SCL <- read_excel("02_data/SITOUREF/Liste_points_AS_SCL.xlsx")
Liste_points_AS_STEP <- read_excel("02_data/SITOUREF/Liste_points_AS_STEP.xlsx")


### 1.2.3. FILTRAGE SUR LE PERIMETRE GEOGRAPHIQUE ----
#### 1.2.3.1. FILTRAGE DES OUVRAGES DE REJET ET POINTS AS SCL  ----

ouvrages_filtre<-ouvrages %>% 
  st_filter(Filtre) %>% 
  distinct()

rejets_filtre<-rejets %>% 
  st_filter(Filtre) %>% 
  distinct()

# Test : avons-nous des coordonnées identiques ou différentes sur les ouvrages qui ressortent ?
test_coor_post_filtre <- left_join(as.data.frame(ouvrages_filtre),
                                   as.data.frame(rejets_filtre),
                                   by="No interne Sitou") %>% 
  mutate(Comp = paste0(ifelse(geometry.x == geometry.y,"IDENTIQUE","DIFFERENT")))


# Table d'export des données à mettre en entrée dans le script suivant : 
liste_sitous_export <- bind_rows(
  select(ouvrages_filtre, `No interne Sitou`),
  select(rejets_filtre, `No interne Sitou`)
) %>%
  distinct()

donnees_filtrees <-ouvrages_amont_et_rejets %>% 
  filter(`No interne Sitou` %in% liste_sitous_export$`No interne Sitou`)

#' @INFO : Cette ligne de code pourra être supprimée une fois le dévveloppement terminé
#' Il est néanmoins utile dans l'intervalle, d'avoir un fichier Excel intermédiaire pour les tests. 
#' 
write.xlsx(donnees_filtrees,paste0("03_intermediary_data/Sitouref_maillage amont rejet ", PERIMETRE,".xlsx"))

liste_ouvrages<-donnees_filtrees|> 
  select(`No interne Ouvrage amont`,`Identifiant principal Sitou amont`,`No interne Sitou`) |> 
  rename("PP_No_interne_Sitou" =`No interne Sitou`,
         "No_ouvrage_amont" = `No interne Ouvrage amont`,
         "Code_SANDRE"=`Identifiant principal Sitou amont` )

liste_STEP<-liste_ouvrages |> 
  filter(str_sub(No_ouvrage_amont,-3)=="029") |> 
  select(Code_SANDRE)

liste_Pts_SCL<-liste_ouvrages |> 
  filter(str_sub(No_ouvrage_amont,-3)=="242") |> 
  select(No_ouvrage_amont)

#' @SORTIE : liste_STEP et liste_Pts_SCL


## 1.3. DONNEES DU MAILLAGE SITOUREF ----

## 1.4. DONNEES AUTOSURVEILLANCE ---- 





### 1.4.1. DONNEES D'AUTOSURVEILLANCE STEU ----
# Un fichier de données va être importé dans un dataframe "data_all_STEP_0"
# qui va être "épuré" (renommage de colonnes etc) de ses doublons..


#### 1.4.1.1. Fichiers de données ----
data_all_STEP_2022 <- read_excel("02_data/DEQUADO/Données_AS_STEP_DVO_2022.xlsx", sheet = "STEP")
data_all_STEP_2023 <- read_excel("02_data/DEQUADO/Données_AS_STEP_DVO_2023.xlsx", sheet = "STEP")
data_all_STEP_2024 <- read_excel("02_data/DEQUADO/Données_AS_STEP_DVO_2024.xlsx", sheet = "STEP")
data_all_STEP_2025 <- read_excel("02_data/DEQUADO/Données_AS_STEP_DVO_2025.xlsx", sheet = "STEP")
data_all_STEP_0 <- rbind(data_all_STEP_2022, 
                         data_all_STEP_2023,
                         data_all_STEP_2024,
                         data_all_STEP_2025)


#### 1.4.1.2. Renommer les colonnes ----
data_all_STEP_0 <-  data_all_STEP_0 %>%
  rename(
    id_analyse = `Code analyse`,
    Code_SANDRE = `Code SANDRE`,
    Nom_STEU = `Station_Nom Sitou`,
    PP_No_interne_Sitou = `PP_No interne Sitou`,
    Pt_SANDRE = `Pt SANDRE`,
    PP_Nom_Sitou = `PP_Nom Sitou`,
    Step_Capacite_nominale = `Step_Capacité nominale`,
    PP_Identifiant_principal_Sitou = `PP_Identifiant principal Sitou`,
    Date_debut_prel = `Date début prélèvt`,
    Date_depot = `Date du dépôt`,
    Code_sandre_parametre = `Code sandre paramètre`,
    Lib_court_parametre = `Lib court paramètre`,
    Code_unite = `Code unité`,
    Symbole_unite = `Symbole unité`,
    Val_resultat_analyse = `Val résultat analyse`,
    Derniere_qualif_analyse = `Dernière qualification analyse`,
    Code_remarque = `Code remarque`
  )

#### 1.4.1.3. Conserver uniquement les ouvrages qui nous intéressent ----
# On conserve les données de toutes les stations dans la liste STEU
# et comme l'étude est centrée sur les rejets, on ne conserve que les points Sandre
# susceptibles d'en émettre.
data_all_STEP_0 <- data_all_STEP_0 %>%
  filter(Pt_SANDRE %in% c("A2","A4","A5"))


#### 1.4.1.4. Convertir les colonnes d'un type à un type numeric, date ----
data_all_STEP_0 <- data_all_STEP_0 %>%
  mutate(Step_Capacite_nominale = as.numeric(Step_Capacite_nominale)) %>%
  mutate(Date_debut_prel = as.Date(Date_debut_prel)) %>%
  mutate(Date_depot = as.Date(Date_depot))

#### 1.4.1.5. Filtre sur taille de STEP ----
data_all_STEP_1 <- data_all_STEP_0 %>%
  # Ajouter ici un filtre si nécessaire
  # filter(Step_Capacite_nominale < 2000 & Step_Capacite_nominale >= 200) %>%
  filter(Code_remarque != 0)

#### 1.4.1.6. Nettoyage des doublons et des caractères problématiques ----
# TEST : s'il existe plus d'une date de dépôt, conserver uniquement la donnée la plus récente.

# récupérer les lignes doublons pour vérification si besoin
tmp <- get_dupes(
  data_all_STEP_1,
  DT,
  Code_SANDRE,
  Pt_SANDRE,
  Code_sandre_parametre,
  PP_No_interne_Sitou,
  Date_debut_prel
)

# trier les données pour supprimer les doublons après.
data_all_STEP_2 <- data_all_STEP_1 %>%
  arrange(
    DT,
    Code_SANDRE,
    Pt_SANDRE,
    Code_sandre_parametre,
    PP_No_interne_Sitou,
    Date_debut_prel,
    desc(Date_depot)
  )

# supprimer les doublons.
data_all_STEP <- data_all_STEP_2 %>%
  distinct(
    DT,
    Code_SANDRE,
    Pt_SANDRE,
    Code_sandre_parametre,
    PP_No_interne_Sitou,
    Date_debut_prel,
    .keep_all = TRUE
  )


# supprimer les caractères problématiques dans les noms
data_all_STEP$Nom_STEU <- gsub("/", "-", data_all_STEP$Nom_STEU)


#### 1.4.1.7. Remplacement des codes erronnés----
#### Remplacement du code 1098 (Volume) par 1552 (Vol.Moy.J) pour les points Sandre A2,A3, A4, A5, A7
data_all_STEP <- data_all_STEP %>%
  mutate(
    Code_sandre_parametre = case_when(
      Code_sandre_parametre == "1098" &
        Pt_SANDRE %in% c("A2", "A3", "A4", "A5","A7") ~ "1552",
      TRUE ~ Code_sandre_parametre
    ),
    Lib_court_parametre = case_when(
      Code_sandre_parametre == "1552" ~ "Vol.Moy.J.",
      TRUE ~ Lib_court_parametre),
    Code_unite = case_when(
      Code_sandre_parametre == "1552" ~ "120",
      TRUE ~ Code_unite),
    Symbole_unite = case_when(
      Code_sandre_parametre == "1552" ~ "m3/j",
      TRUE ~ Symbole_unite),
  )


#' @SORTIE : data_all_STEP

### 1.4.2. DONNEES D'AUTOSURVEILLANCE SCL ----
# Un fichier de données va être importé dans un dataframe "data_all_SCL_0"
# qui va être "épuré" (renommage de colonnes etc) de ses doublons avant d'être éclaté station par staion.

#### 1.4.2.1. Fichiers de données ----

data_all_SCL_2022 <- read_excel("02_data/DEQUADO/Données_AS_SCL_DVO_2022.xlsx", sheet = "Réseaux")
data_all_SCL_2023 <- read_excel("02_data/DEQUADO/Données_AS_SCL_DVO_2023.xlsx", sheet = "Réseaux")
data_all_SCL_2024 <- read_excel("02_data/DEQUADO/Données_AS_SCL_DVO_2024.xlsx", sheet = "Réseaux")
data_all_SCL_2025 <- read_excel("02_data/DEQUADO/Données_AS_SCL_DVO_2025.xlsx", sheet = "Réseaux")


data_all_SCL_0<-rbind(data_all_SCL_2022,data_all_SCL_2023,data_all_SCL_2024,data_all_SCL_2025)


#### 1.4.2.2. Renommer les colonnes ----
data_all_SCL_0 <-  data_all_SCL_0 %>% 
  rename(id_analyse = `Code analyse`,
         Code_SANDRE = `Code SANDRE`,
         Nom_SCL = `Station_Nom Sitou`,
         PP_No_interne_Sitou = `PP_No interne Sitou`,
         Pt_SANDRE = `Pt SANDRE`,
         PP_Nom_Sitou = `PP_Nom Sitou`,
         Classe_CBPO = `Classe CBPO`,
         Type_reseau = `Type réseau`,
         PP_Identifiant_principal_Sitou = `PP_Identifiant principal Sitou`,
         Date_debut_prel = `Date début prélèvt`,
         Date_depot = `Date du dépôt`,
         Code_sandre_parametre = `Code sandre paramètre`,
         Lib_court_parametre = `Lib court paramètre`,
         Code_unite = `Code unité`,
         Symbole_unite = `Symbole unité`,
         Val_resultat_analyse = `Val résultat analyse`,
         Derniere_qualif_analyse = `Dernière qualification analyse`,
         Code_remarque = `Code remarque`,
         Commentaire_analyse = `Commentaire analyse`
  ) 

#### 1.4.2.3. Conserver uniquement les ouvrages qui nous intéressent ----

# On conserve les données de tous les points d'autosurveillance de la liste liste_Pts_SCL
data_all_SCL_0<-data_all_SCL_0 |> 
  filter(PP_No_interne_Sitou %in% liste_Pts_SCL$No_ouvrage_amont)


#### 1.4.2.4. Convertir les colonnes d'un type à un type numeric, date ----
data_all_SCL_1 <- data_all_SCL_0 %>%
  mutate(Date_debut_prel = as.Date(Date_debut_prel)) %>%
  mutate(Date_depot = as.Date(Date_depot)) %>%
  mutate(Classe_CBPO = as.factor(Classe_CBPO))

#### 1.4.2.5. Nettoyage des doublons ----

#TEST : s'il existe plus d'une date de dépôt, conserver uniquement la donnée la plus récente.

# permet de récupérer les lignes doublons pour vérification si besoin
tmpSCL <- get_dupes(data_all_SCL_1, DT, Code_SANDRE, Pt_SANDRE, Code_sandre_parametre, PP_No_interne_Sitou, Date_debut_prel)

# tri des données pour supprimer les doublons après.
data_all_SCL_2 <- data_all_SCL_1 %>% 
  arrange(DT, Code_SANDRE, Pt_SANDRE,  Code_sandre_parametre, PP_No_interne_Sitou, Date_debut_prel, desc(Date_depot))

# On supprime les doublons.
data_all_SCL <- data_all_SCL_2 %>% 
  distinct(DT, Code_SANDRE, Pt_SANDRE,  Code_sandre_parametre, PP_No_interne_Sitou, Date_debut_prel, .keep_all = TRUE) 


# Nettoyage des noms problématiques
data_all_SCL$Nom_SCL<-gsub("/", "-",data_all_SCL$Nom_SCL)
data_all_SCL$Nom_SCL<-gsub(":", "-",data_all_SCL$Nom_SCL)

#### 1.4.2.6. Nettoyages complémentaires ----
#Suppression des lignes correspondant au code analyse 0 (analyse non faite)
data_all_SCL<-data_all_SCL %>%
  filter(Code_remarque!=0)

### Remplacement du code 1098 (Volume) par 1552 (Vol.Moy.J) le cas échéant
data_all_SCL <- data_all_SCL %>%
  mutate(
    Code_sandre_parametre = case_when(
      Code_sandre_parametre == "1098" ~ "1552",
      TRUE ~ Code_sandre_parametre
    ),
    Lib_court_parametre = case_when(
      Code_sandre_parametre == "1552" ~ "Vol.Moy.J.",
      TRUE ~ Lib_court_parametre),
    Code_unite = case_when(
      Code_sandre_parametre == "1552" ~ "120",
      TRUE ~ Code_unite),
    Symbole_unite = case_when(
      Code_sandre_parametre == "1552" ~ "m3/j",
      TRUE ~ Symbole_unite),
  )
### Réordonnancement de data_SCL_complet$Classe_CBPO et recodage en facteur
data_all_SCL$Classe_CBPO <- factor(data_all_SCL$Classe_CBPO,
                                   levels = c("< 120 kgDBO/j", "de 120 à 600 kgDBO/j", "> 600 kgDBO/j")
)
data_all_SCL$Classe_CBPO<- data_all_SCL$Classe_CBPO %>%
  fct_recode(
    "<120" = "< 120 kgDBO/j",
    "120-600" = "de 120 à 600 kgDBO/j",
    ">600" = "> 600 kgDBO/j"
  )

#' @SORTIE data_all_SCL

### 1.4.3. DONNEES D'AUTOSURVEILLANCE INDUSTRIE ----





## 1.5. DONNEES DE L'ETAT DES LIEUX ----




# 2. DONNEES EXTERNES ----

## 2.1. DONNEES DE DEBIT QMNA5 ----

## 2.2. DONNEES DE MESURES DE QUALITE PHYSICO-CHIMIQUES