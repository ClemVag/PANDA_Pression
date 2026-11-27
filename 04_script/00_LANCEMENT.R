# ------------------------------------------------------------------------- -
#             SCRIPT LANCEMENT PANDA PRESSION  
# ------------------------------------------------------------------------- -

#' Le script de lancement est le script maître de démarrage de PANDA Pression.
#' Il appelle tous les scripts suivants. 
#' Il est agencé de la manière suivante : 
#' - Appel des packages de manière conditionnelle (les packages ne sont pas ré-appelés 
#'   à chaque lancement, on teste s'ils ont bien été chargés)
#' - Appel des fonctions de manière conditionnelle (idem)
#' - Appel des scripts au fur et à mesure avec rappel des variables exportées.

# ------------------------------------------------ -
#' ETAPES PREALABLES AU LANCEMENT 
# ------------------------------------------------ -
#' 
#' - s'assurer que les lecteurs réseaux sont bien connectés (notamment racine$ R)
#' 
#' - Avoir préalablement fait tourner le script "PREP_Liste sitous origine et rejets.R"
#' et avoir vérifié la table de correspondance (notamment pour relier les bons 
#' points de rejets aux bons points d'autosurveillance sur les STEU, qui peuvent avoir des 
#' milieux récepteurs différents entre les A4, A2 et A5).
#' Ne pas hésiter à changer le nom de la table de correspondance à rechercher dans 
#' le script 01.
#' 
#' 
#' /!\ Pour reprendre avec des données déjà chargées et gagner du temps 
#' load("2026-11-24_données travail.RData")
#' 
#' 
{
# AMORCAGE CONDITONNEL ----
if (!exists("lancement"))
{
## PACKAGES -----
  library(pacman)
  p_load(conflicted,
         cowplot, 
         dplyr,
         flextable, 
         forcats,
         geodata,
         ggnewscale,
         ggplot2,
         conflicted,
         cowplot,
         data.table,
         dplyr,
         flextable ,
         forcats,
         geodata,
         ggnewscale, # Package permettant à plusieurs échelles de couleurs de coexister
         ggplot2,
         ggrepel,
         ggspatial,
         here,    # rapports
         janitor, # doublons
         leaflet,
         nngeo,
         leafpop,
         lubridate, # date et heure
         mapsf,
         mapview,
         openxlsx,
         osmdata,
         quarto, # rapports
         questionr,
         readr,
         readxl,
         remotes,
         rmarkdown, # rapports
         scales,
         sf,
         spData,
         stars,
         stringr,
         summarytools,
         tidyr,
         tidyr, # pivot de table
         tidyverse,
         tmap,
         zoo)
  library(plotly)
  ## lors d'un export dans excel permet de garder format date YYYY-MM-DD (sinon peut devenir YYYY-DD-MM)
  options(openxlsx.dateFormat = "yyyy-mm-dd")
  #conflict_prefer_all("dplyr")
  conflicts_prefer(lubridate::month)
  conflicts_prefer(lubridate::year)
  conflicts_prefer(terra::intersect)
  conflicts_prefer(dplyr::filter)
  
  
## FONCTIONS ----
  source(here("04_script/fonctions.R"))
  
lancement <- TRUE

}

# APPEL DES SCRIPTS DE TRAVAIL ----

## 1. SCRIPT 01_PARAMETRAGE_ET_FILTRE_CARTO ----

source(here("04_script/01_PARAMETRAGE_ET_FILTRE_CARTO.R"))

#' VARIABLES DE SORTIE DE SCRIPT, A REUTILISER
#' 
#' - [Filtre] : fichier géographique contenant les contours du périmètre recherché
#' - [liste_points_rejet] : fichier géographique, liste des points de rejets, actifs ou non. 
#' Attention, cette liste est constituée des points de rejets filtrés AVEC LE BUFFER.
#' - [liste_points_rejet_actifs] : dataframe, liste des points de rejets actifs.
#' Attention, cette liste est constituée des points de rejets filtrés AVEC LE BUFFER.
#' - [Parametrage] : dataframe, le paramétrage initial
#' - [table_correspondance] : dataframe, la table de correspondance entre rejet, points 
#' d'autosurveillance, site d'origine et milieu récepteur. 
#' 

 
#' OBJETS DE TEST : 
#' 
#' - [rejets_avec_coordonnees_NA] : liste des points de rejets avec des coordonnées géographiques manquantes
#' - [test_map] : carto test qui s'affiche dans l'onglet Plots, permet de vérifier
#' la configuration générale de ce qu'on va étudier par la suite. 

message("✔ Script 01_PARAMETRAGE_ET_FILTRE_CARTO exécuté avec succès ! ")

}
{
## 2. REJETS INDUSTRIELS ----
source(here("04_script/02_REJET INDUS.R"))

#' VARIABLES DE SORTIE DE SCRIPT, A REUTILISER
#' 
#' - [flux_SRR_PM_export] : dataframe, flux journaliers de polluants par points de mesure SRR
#' - [flux_SRR_PM_mens] : dataframe, flux mensuels de polluants par points de mesure SRR
#' - [flux_forfait_export_condens] : dataframe, flux de polluants journalier par site industriel au forfait
#' - [flux_forfait_export_detail] : dataframe, flux de polluants journalier par site industriel au forfait,
#' détail par code d'activité.
 
message("✔ Script 02_REJET INDUS exécuté avec succès ! ")

# Nettoyage de l'espace mémoire non utilisé
gc()

## 3. REJETS DES SYSTEMES D'ASSAINISSEMENT ----


# Nettoyage de l'espace mémoire non utilisé
#gc()
}


## 4. 