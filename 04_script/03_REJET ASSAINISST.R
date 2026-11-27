
# ──────────────────────────────────────────── ─
# 03. SCRIPT PRESSION ASSAINISSEMENT COLLECTIF ----
# ──────────────────────────────────────────── ─


## 1.1. DIRECTION TERRITORIALE ----
# Indiquer le nom de votre DT à droite de la flèche, en MAJUSCULES, entre guillemets
# Choix : DVO / DVM / DSF / DSAM / DSAV / DBN
# Exemple : DT <- "DVO"
# A remplacer par le paramétrage
#DT <- "DSAV"

## 1.2. CHOIX DU SCRIPT ----

# # Pour le script PRESSION, choix de l'année :
# An <- 2025
#' Note Clémence : on remplace ce paramétrage par le choix des années
#' min et max comme indiqué dans le fichier initial de paramétrage

# ──────────────────────────────────────────── ─
# 2. PACKAGES ET FONCTIONS ----
# ──────────────────────────────────────────── ─

# PACKAGES
# library(readxl)
# library(tidyverse)
# library(dplyr)
# library(summarytools)
# library(conflicted)
# conflict_prefer_all("dplyr")
# library(janitor) # doublons
# library(flextable)
# library(forcats)
# library(lubridate) # date et heure
# library(tidyr) # pivot de table
# library(openxlsx)
# library(data.table)
# library(zoo)
# ## lors d'un export dans excel permet de garder format date YYYY-MM-DD (sinon peut devenir YYYY-DD-MM)
# options(openxlsx.dateFormat = "yyyy-mm-dd")
# conflicts_prefer(lubridate::month)

# ──────────────────────────────────────────── ─
# 3. PREPARATION FICHIER ENTREPOT DE DONNEES ----
# ──────────────────────────────────────────── ─
## 3.1. DONNEES DE ROSEAU----
Roseau_SCL <- read_delim("02_data/DIVERS/Roseau.csv", 
                         delim = ";", escape_double = FALSE,
                         locale = locale(decimal_mark = ",", grouping_mark = "."), 
                         trim_ws = TRUE)
Roseau_SCL<-Roseau_SCL %>% 
  select(`Maître d'ouvrage...88`,
         `Code Sandre du SCL`, 
         `Nom du SCL`,
         `Type de réseau majoritaire du SCL`
  ) %>% 
  rename(Ros_MOA_STEP = `Maître d'ouvrage...88`,
         Ros_Sandre_SCL = `Code Sandre du SCL`,
         Ros_Nom_SCL=`Nom du SCL`,
         Ros_Type_SCL = `Type de réseau majoritaire du SCL`)


## 3.2. TABLE CORRESPONDANCE ROSEAU SITOUREF ----
Table_corresp_Roseau_Sitouref <- read_excel("02_data/SITOUREF/Table_corresp_Roseau_Sitouref.xlsx") %>% 
  rename("Code_SANDRE"=`Code Sandre SCL Sitouref`)

## 3.3. DONNEES DE SITOUREF----
### 3.3.1. Liste brute des points d'autosurveillance ----

# STEP

Liste_points_AS_STEP<-read_excel("02_data/SITOUREF/Liste_points_AS_STEP.xlsx", 
                                 sheet = "STEP")
Liste_points_AS_STEP <-Liste_points_AS_STEP %>% 
  select(DT,Sandre_STEP,Nom_STEP,Capacite_STEP,`No interne Sitou`,PP_Nom_Sitou,LocalisationSitouref) %>% 
  rename(Code_SANDRE = Sandre_STEP,
         Pt_SANDRE = LocalisationSitouref,
         Nom = Nom_STEP,
         PP_No_interne_Sitou = `No interne Sitou`,
         Nom_pt_AS = PP_Nom_Sitou
  ) |> 
  filter(Pt_SANDRE %in% c("A3","A4","A2","A5")) |> 
  mutate(Capacite_STEP = as.numeric(Capacite_STEP))

#SCL 

Liste_points_AS_SCL<-read_excel("02_data/SITOUREF/Liste_points_AS_SCL.xlsx", 
                                sheet = "SCL")
Liste_points_AS_SCL<-Liste_points_AS_SCL %>% 
  select(DT,Code_SANDRE_SCL,Nom_SCL,`No interne Sitou`,`Nom Sitou`,LocalisationSitouref,Classe,`Type réseau`) %>% 
  rename(
    Code_SANDRE = Code_SANDRE_SCL,
    Pt_SANDRE = LocalisationSitouref,
    Nom = Nom_SCL,
    PP_No_interne_Sitou = `No interne Sitou`,
    Nom_pt_AS = `Nom Sitou`,
    Type_reseau = `Type réseau`
  )



### 3.3.2. SELECTION DES OUVRAGES PERTINENTS ----
#### 3.3.2.1. STEU ----
Liste_points_AS_STEP_etudies<-Liste_points_AS_STEP |> 
  filter(PP_No_interne_Sitou %in% table_correspondance$pt_AS)

#### 3.3.2.2. SCL (avec complétion du type de réseau) ----
Liste_points_AS_SCL_etudies<-Liste_points_AS_SCL |> 
  filter(PP_No_interne_Sitou %in% table_correspondance$pt_AS)

for (p in 1:nrow(Liste_points_AS_SCL_etudies))
{  Liste_points_AS_SCL_etudies$Type_reseau[p]<-ifelse(is.na(Liste_points_AS_SCL_etudies$Type_reseau[p]),
                                                      Table_corresp_Roseau_Sitouref$Type_reseau[which(Table_corresp_Roseau_Sitouref$Code_SANDRE==Liste_points_AS_SCL_etudies$Code_SANDRE[p])],
                                                      Liste_points_AS_SCL_etudies$Type_reseau[p])

}

### 3.3.3.RECAP DT ET LISTE OUVRAGES ----
DT_list <- tibble(DT = unique(c(Liste_points_AS_STEP_etudies$DT, Liste_points_AS_SCL_etudies$DT)))

Ouvrages_list<-data.table(Code_SANDRE = c(unique(Liste_points_AS_STEP_etudies$Code_SANDRE),unique(Liste_points_AS_SCL_etudies$Code_SANDRE)),
                          type_ouvrage = c(rep("STEP",length(unique(Liste_points_AS_STEP_etudies$Code_SANDRE))),rep("SCL",length(unique(Liste_points_AS_SCL_etudies$Code_SANDRE)))))



## 3.4. CHOIX DU FICHIER DE DONNEES DE LA DT ----
### 3.4.1. Import fichier de la DT ----
#' Nouvelle requête en entrée spécifique PANDA Pression : critères :
#' --- Si point AS = A3 alors Code Sandre paramètre = 1552 ou 1553
#' --- Si point AS = A2, A4, A5 alors tout Code Sandre paramètre accepté

# Boucle pour nommer les variables en tant que STEP puis SCL pour simplifier le code

ouvrages <- c("STEP","SCL")


for(type in ouvrages) 
{
  # On crée un compteur pour incrémenter le nombre d'itérations pour chaque type d'ouvrage
  compteur_incrementation = 1
  
  for(DT in DT_list)
  {
    
    for (Annee in fenetre_temps_etendue) {
      
      if(DT != "DTSF")
      { data_AS_0 <- read_excel(paste0("02_data/DEQUADO/Données_AS_",type,"_PPRESSION_", DT, "_",Annee,".xlsx"))
      }else
      {# Pour la DTSF, deux fichiers sont nécessaires : fichier DRIF et DPPC
        
        data_AS_01<-read_excel(paste0("02_data/DEQUADO/Données_AS_",type,"_PPRESSION_DSF_DRIF_",Annee,".xlsx"))
        data_AS_02<-read_excel(paste0("02_data/DEQUADO/Données_AS_",type,"_PPRESSION_DSF_DPPC_",Annee,".xlsx"))
        
        colnames(data_AS_02) <- colnames(data_AS_01)
         data_AS_0<-rbind(data_AS_01, data_AS_02)
        
        # Pour DTSF-DRIF, certains fichiers de données peuvent être scindés en deux
        # car il sont particulièrement longs. Dans ce cas, on utilise ce bloc 
        # de code pour lire le 2e onglet du fichier de données. 

        if (length( excel_sheets(paste0("02_data/DEQUADO/Données_AS_",type,"_PPRESSION_DSF_DRIF_",Annee,".xlsx")) ) == 2)
        {
          data_AS_02<-read_excel(paste0("02_data/DEQUADO/Données_AS_",type,"_PPRESSION_DSF_DRIF_",Annee,".xlsx"), sheet = 2, col_names = FALSE)
          colnames(data_AS_02) <- colnames( data_AS_0)
           data_AS_0 <-rbind( data_AS_0, data_AS_02)
        }
        
        
      }  
      
      #' Dans la prochaine étape, on renomme les colonnes et on retient uniquement 
      #' les ouvrages en lien avec les rejets présents sur le périmètre. 
      #' Cela permettra d'aller plus vite sur la suite de l'analyse.
      
      ### 3.4.2. Renommer les colonnes ----  
      data_AS_1<-data_AS_0 |> 
          rename(
            id_analyse = `Code analyse`,
            Code_SANDRE = `Code SANDRE`,
            PP_No_interne_Sitou = `PP_No interne Sitou`,
            Pt_SANDRE = `Pt SANDRE`,
            Date_debut_prel = `Date début prélèvt`,
            Date_depot = `Date du dépôt`,
            Code_sandre_parametre = `Code sandre paramètre`,
            Code_unite = `Code unité`,
            Val_resultat_analyse = `Val résultat analyse`,
            Derniere_qualif_analyse = `Dernière qualification analyse`,
            Code_remarque = `Code remarque`,
            Commentaire_analyse = `Commentaire analyse`  
          )%>%
        #' Ici on conserve uniquement dans la liste des ouvrages reliésà un rejet
        #' présent dans le périmètre identifié
        
        filter(Code_SANDRE %in%  Ouvrages_list[type_ouvrage == type]$Code_SANDRE) %>%
      
      ### 3.4.3. Convertir les colonnes d'un type à un type numeric, date ----
       mutate(Date_debut_prel = as.Date(Date_debut_prel),
                 Date_depot = as.Date(Date_depot))%>%
      
        mutate(
          # Conversion 1 : Remplacement du code 1098 (Volume) par 1552 (Vol.Moy.J) le cas échéant
          Code_sandre_parametre = case_when(
            Code_sandre_parametre == "1098" ~ "1552",
            TRUE ~ Code_sandre_parametre
          ),
          Code_unite = case_when(
            Code_sandre_parametre == "1552" ~ "120",
            TRUE ~ Code_unite),
          
          # Conversion 2 : Harmonisation des unités des paramètres azotés et Phosphore
          Val_resultat_analyse =case_when(
            Code_sandre_parametre == "1335" & Code_unite %in%c("168","395") ~ Val_resultat_analyse*1.2857, #NH4+
            Code_sandre_parametre == "1339" & Code_unite =="168" ~ Val_resultat_analyse*3.2862, #NO2-
            Code_sandre_parametre == "1340" & Code_unite =="168" ~ Val_resultat_analyse*4.4287, #NO3-
            TRUE ~ Val_resultat_analyse),
          
          Code_unite = case_when(Code_sandre_parametre == "1335" & Code_unite %in%c("168","395") ~ "169",
                                 Code_sandre_parametre == "1339" & Code_unite =="168" ~ "171",
                                 Code_sandre_parametre == "1340" & Code_unite =="168" ~ "173",
                                 TRUE ~ Code_unite)
        ) |> 
        # On supprime les paramètres température eau, température air, pH
        filter(Code_sandre_parametre %nin% c("1301","1409","1302"))
      
      ### 3.4.4. Nettoyage des doublons et des caractères problématiques ----
      # TEST : s'il existe plus d'une date de dépôt, conserver uniquement la donnée la plus récente
      
      # récupérer les lignes doublons pour vérification si besoin
      
      # assign(
      #   paste0("tmp_", type), 
      #   get_dupes(
      #     get(paste0("data_all_",type,"_0_",Annee)),
      #     DT,
      #     Code_SANDRE,
      #     Pt_SANDRE,
      #     Code_sandre_parametre,
      #     PP_No_interne_Sitou,
      #     Date_debut_prel,
      #     Date_depot  ))
      
      # trier les données pour supprimer les doublons après.
      data_AS_2 <- data_AS_1 %>%
          arrange(
            DT,
            Code_SANDRE,
            Pt_SANDRE,
            Code_sandre_parametre,
            PP_No_interne_Sitou,
            Date_debut_prel,
            desc(Date_depot))
      
      
      # supprimer les doublons.
      data_AS_3 <- data_AS_2 %>%
          distinct(
            DT,
            Code_SANDRE,
            Pt_SANDRE,
            Code_sandre_parametre,
            PP_No_interne_Sitou,
            Date_debut_prel,
            .keep_all = TRUE
          )
      
      
      ### Concaténation des tables -----
      
      if(compteur_incrementation == 1)
      {
        assign(paste0("data_all_",type),
               data_AS_3)
      } else {
        assign(paste0("data_all_",type),
               rbind(get(paste0("data_all_",type)),data_AS_3))
      }
      
      ### Incrémentation du compteur -----
      compteur_incrementation <- compteur_incrementation+1 
      

     
    
      }
    
  }

  
  ## 3.4.5. CREATION DE LA LISTE DES STEP et SCL ----
  assign(
    paste0("Liste_", type),
    tibble(Code_SANDRE = unique(get(paste0("data_all_",type))$Code_SANDRE)))

  
  ### 3.4.6. Liste des couples points / paramètres et des couples paramètres/unités ----
  assign(
    paste0("ListeParam_", type),
    # Ancienne version (nouvelle annule et remplace)
    # distinct(
    #   tibble(Code_sandre_parametre = get(paste0("data_all_",type))$Code_sandre_parametre)))
    get(paste0("data_all_",type)) |> 
      select(PP_No_interne_Sitou,Code_sandre_parametre,Code_unite) |> 
      distinct() |> 
      filter(Code_sandre_parametre %nin% c("1552","1553")) #On retire les paramètres volume et pluviométrie
  )

  assign(
    paste0("ListeParamUnites_", type),
    get(paste0("data_all_",type))  |> 
      select(Code_sandre_parametre, Code_unite) |> 
      distinct()
    )
  
  
}
### 3.4.7. NETTOYAGE DES VARIABLES INTERMEDIAIRES ----
rm(list = c(
  "data_AS",
  "data_AS_0",
  "data_AS_1",
  "data_AS_2",
  "data_AS_3",
  "Roseau_SCL",
  "Liste_points_AS_SCL",
  "Liste_points_AS_STEP"
  ))



# ──────────────────────────────────────────── ─
# 4. CALCUL DE FLUX ----
# ──────────────────────────────────────────── ─

## 4.1. PARAMETRAGE DU CALCUL DE FLUX  ---- 
params_flux <- read_excel("02_data/BASE/Table_Param_Unites.xlsx") %>% 
  rename("Code_sandre_parametre"=`Code sandre paramètre`,
         "Code_unite"= `Code unité`,
         "Unite_ref" = `Unité référence ?`)  #%>%
# filter(Code_sandre_parametre %in% c("1313","1314","1305","1319","1335","1339","1340","1551","1350"),
#        Unite_ref == "Oui")


code_debit <- "1552"  # Vol.Moy.J. en m3/j
GAP_MAX_JOURS <- 7 # au-delà, un trou de débit n'est plus interpolé
SEUIL_PROP_MANQUANTS <- 0.20 
# au-delà de 20% de jours manquants sur l'année, bascule sur la moyenne annuelle

pts_rejet  <- c("A1", "R1","A2", "A4", "A5")

code_pluvio <- "1553" # pluviométrie en mm

## 4.2. CALCUL FLUX FORFAITAIRES EN A4 ---- 
# Calcul forfaitaire A4 si absence données N et N-1
# forfait dépollution : Traitement biologique sans nitrification ni déphosphatation
conc_forfaitaire_A4 <- tribble(
  ~Code_sandre_parametre, ~C_forfait,
  "1305", 25/365 *0.1,  # / EH MES
  "1313", 22/365 *0.2,  # / EH DBO5
  "1314", 50/365 *0.25,  # / EH DCO
  "1319", 4.4/365*0.7, # / EH NTK
  "1350", 0.7/365*0.7,  # / EH PT
  "1335", NA_real_,
  "1339", NA_real_,
  "1340", NA_real_,
  "1551", NA_real_
)

# débit A4 si absence débit N et N-1 mais présence bilan 
debit_forfaitaire <- tribble(
  ~Code_sandre_parametre, ~d_forfait,
  "1552", 0.150 # / EH m3 (150L/j/hab)
)
#' [TODO] Alternative à voir avec Romain : prendre le volume en A3. 


# Calcul forfait 
base_A4 <- Liste_points_AS_STEP_etudies %>%
  select(
    Code_SANDRE, PP_No_interne_Sitou, Pt_SANDRE, Capacite_STEP
  ) %>%
  filter(Pt_SANDRE == "A4") 

# Flux A4
flux_forfait_A4 <- base_A4 %>%
  cross_join(conc_forfaitaire_A4) %>%
  mutate(
    Flux_kg_j_forfait = Capacite_STEP * C_forfait) %>%
  select(-C_forfait,-Capacite_STEP)

# Débit A4
debit_forfait_A4 <- base_A4 %>%
  cross_join(debit_forfaitaire)%>%
  mutate(Debit_forfait = Capacite_STEP * d_forfait) %>%
  select(-d_forfait, -Capacite_STEP)


## 4.2. PREPARATION DES DONNEES ----
# ──────────────────────────────────────────── ─

### 4.2.1. Pluviométries journalières ----
data_pluvio <- data_all_STEP %>%
#data_pluvio <- get(paste0("data_all_STEP_",Annee)) %>%  # Version originale de Romain
  filter(
    #Pt_SANDRE == "A3",
    Code_sandre_parametre == code_pluvio
  ) %>%
  select(
    Code_SANDRE, 										  
    Pt_SANDRE,
    Date_debut_prel,
    Code_sandre_parametre,
    Val_resultat_analyse, 
    Code_unite,
    Derniere_qualif_analyse, 
  ) %>%
  rename(Pluvio_mmj = Val_resultat_analyse,
         Qualif_pluvio = Derniere_qualif_analyse) 


### 4.2.2. Rejets ----

for (type in ouvrages) {
  #' Note CV : Code redondant avec la nouvelle version de la requête
  # assign(paste0("data_rejet_",type),
  #        get(paste0("data_all_",type,"_",Annee)) %>%
  #          filter(Pt_SANDRE %in% c(pts_rejet,"A3"))
  # )
  
  
  # Débits journaliers (tous les jours disponibles, pas seulement les jours de bilan)
  
  assign(paste0("data_debit_",type),
         get(paste0("data_all_",type)) %>%
           filter(Code_sandre_parametre == code_debit) %>%
           select(
             Code_SANDRE, 										  
             Pt_SANDRE, 
             PP_No_interne_Sitou,
             Date_debut_prel,
             Code_sandre_parametre,
             Val_resultat_analyse, 
             Derniere_qualif_analyse, 
           ) %>%
           rename(Debit_m3j = Val_resultat_analyse,
                  Qualif_debit = Derniere_qualif_analyse) %>%
           mutate(
             Annee       = year(Date_debut_prel),
             Mois        = month(Date_debut_prel),
             Nb_jours_mois = days_in_month(Date_debut_prel) # CV : voir si toujours nécessaire
           )
  )
  
  # Concentrations — uniquement les jours où concentration ET débit sont présents
  # (= jours de bilan 24h / jours de déversements)
  
  assign(paste0("data_conc_",type),
         get(paste0("data_all_",type)) %>%
           # On exclut des concentrations les débits, la pluviométrie, la 
           # températude d'eau (1301), d'air (1409) et le pJ (1302) 
           filter(Code_sandre_parametre %nin% c(code_debit,code_pluvio,1301,1409,1302)) |> 
           select(
             Code_SANDRE,
             Pt_SANDRE,
             PP_No_interne_Sitou,
             Date_debut_prel,
             Code_sandre_parametre, 
             Val_resultat_analyse, 
             Derniere_qualif_analyse,
           ) %>%
           rename(
             Concentration = Val_resultat_analyse,
             Qualif_conc = Derniere_qualif_analyse
           )
  )
  
}

### 4.2.3. DEBITS A4 ----

# Les débits
data_debit_A4_N1 <- data_all_STEP %>%
  filter(year(Date_debut_prel) == min(fenetre_temps_etendue), 
         Pt_SANDRE == "A4", 
         Code_sandre_parametre == code_debit) %>%
  select(Code_SANDRE, PP_No_interne_Sitou, Date_debut_prel, 
         Val_resultat_analyse, Derniere_qualif_analyse) %>%
  rename(Debit_m3j = Val_resultat_analyse,
         Qualif_debit = Derniere_qualif_analyse)

debit_forfait_pt <- debit_forfait_A4 %>%
  distinct(Code_SANDRE, PP_No_interne_Sitou, Debit_forfait)

# Les concentrations
data_conc_A4_N1 <- data_all_STEP %>%
  filter(year(Date_debut_prel) == min(fenetre_temps_etendue), 
         Pt_SANDRE == "A4",
         Code_sandre_parametre %in% params_flux$Code_sandre_parametre,
         !is.na(Val_resultat_analyse)) %>%
  select(Code_SANDRE, PP_No_interne_Sitou, Code_sandre_parametre,
         Date_debut_prel, Val_resultat_analyse, Derniere_qualif_analyse) %>%
  rename(Concentration = Val_resultat_analyse,
         Qualif_conc = Derniere_qualif_analyse)

lib_N  <- c(mesure = "Débit mesuré", moyenne = "Débit estimé - moyenne annuelle",
            interpole = "Débit estimé reconstitué", aucun = "Aucun débit")
lib_N1 <- c(mesure = "Débit estimé (mesure N-1)", moyenne = "Débit estimé (moyenne N-1)",
            interpole = "Débit estimé reconstitué N-1",aucun = "Aucun débit")

# TOUS les points A4 de Sitouref x tous les jours de l'année N ---
points_A4 <- Liste_points_AS_STEP_etudies %>%
  # semi_join(capacite_nominale_STEP %>% filter(Direction ==DT),
  #           by = c("Code_SANDRE" = "Code_SANDRE"))%>%
  filter(Pt_SANDRE == "A4") #%>%
  # distinct(Code_SANDRE = Code_SANDRE, PP_No_interne_Sitou)

cal_A4 <- points_A4 %>%
  crossing(Date_debut_prel = seq.Date(as.Date(paste0(min(fenetre_temps_etendue)+1, "-01-01")),
                                      as.Date(paste0(max(fenetre_temps_etendue), "-12-31")), by = "day"))

#'[REPRENDRE ICI] - Modifier pour intégrer le comblement des débits sur l'année N-1
brut_debit_N <- data_debit_STEP %>%
  filter(Pt_SANDRE == "A4") %>%
  select(Code_SANDRE, PP_No_interne_Sitou, Date_debut_prel, Debit_m3j, Qualif_debit)

# Année N-1 décalé d'un an pour tomber sur le calendrier Année N (le 29/02 N-1 disparaît)
brut_debit_N1 <- data_debit_A4_N1 %>%
  mutate(Date_debut_prel = Date_debut_prel + years(1),
         Qualif_debit    = NA_character_) %>%
  filter(!is.na(Date_debut_prel)) %>%
  select(Code_SANDRE, PP_No_interne_Sitou, Date_debut_prel, Debit_m3j, Qualif_debit)


debit_A4_N  <- combler_debit(brut_debit_N,  cal_A4, lib_N)
debit_A4_N1 <- combler_debit(brut_debit_N1, cal_A4, lib_N1)


# Cascade finale : débit N (mesuré/estimé) sinon débit N-1 sinon forfait sinon aucun
debit_A4_cascade <- debit_A4_N %>%
  left_join(
    debit_A4_N1 %>% select(Code_SANDRE, PP_No_interne_Sitou, Date_debut_prel,
                           Debit_N1 = Debit_m3j, Source_N1 = Source_debit),
    by = c("Code_SANDRE", "PP_No_interne_Sitou", "Date_debut_prel")
  ) %>%
  left_join(debit_forfait_pt,
            by = c("Code_SANDRE", "PP_No_interne_Sitou")) %>%
  mutate(
    Source_debit = case_when(
      !is.na(Debit_m3j)        ~ Source_debit,
      !is.na(Debit_N1)         ~ Source_N1,
      !is.na(Debit_forfait)    ~ "Débit forfaitaire",
      TRUE                     ~ "Aucun débit"
    ),
    Debit_m3j = coalesce(Debit_m3j, Debit_N1, Debit_forfait),
    # la fonction coalesce() retient la première valeur non nulle par ordre de variable
    Pt_SANDRE = "A4"
  ) %>%
  select(-Debit_N1, -Source_N1, -Debit_forfait)

# ──────────────────────────────────────────── ─
## 4.3. CALCUL DES FLUX JOURNALIERS — METHODE DECAPOL ----
#
# Jours AVEC mesure de concentration :
#   Flux_j = Concentration × Debit_j × 1e-3
#
# Jours SANS mesure de concentration :
#   Flux_j = (Flux_moy_bilans / Debit_moy_bilans) × Debit_j × 1e-3
#          = C_pond_bilans × Debit_j × 1e-3
#
# La C_pond est calculée sur TOUS les jours de bilan de l'année
# (flux moyen annuel / débit moyen annuel sur jours de mesure)
# ──────────────────────────────────────────── ─ 

### 4.3.1. Grille complète : tous les jours avec débit × tous les paramètres ----
#' L'opération Crossing fait trop de lignes, remplacement par un full_join. 
#' Adapté pour l'usage macropolluants et micropolluants. 


for (type in ouvrages) {
  assign(paste0("grille_jours_",type),
         get(paste0("data_debit_",type)) %>%
           filter(!is.na(Debit_m3j)) %>%
           select(Code_SANDRE, 
                  Pt_SANDRE, PP_No_interne_Sitou, Date_debut_prel,
                  Debit_m3j,Qualif_debit
           ) %>%
           mutate(Source_debit="Débit mesuré") %>%
           # Ancienne version
           # crossing(params_flux %>% select(Code_sandre_parametre))
           full_join(get(paste0("ListeParam_",type)), by = "PP_No_interne_Sitou", relationship = "many-to-many")
  )
}

  
grille_jours_STEP <- grille_jours_STEP %>%
  filter(Pt_SANDRE %nin% c("A4","A3")) %>%
  bind_rows(
    debit_A4_cascade %>%
      filter(!is.na(Debit_m3j)) %>%
      select(Code_SANDRE, Pt_SANDRE, PP_No_interne_Sitou, Date_debut_prel,
             Debit_m3j, Qualif_debit, Source_debit) %>%
      #crossing(params_flux %>% select(Code_sandre_parametre))
      full_join(get(paste0("ListeParam_",type)), by = "PP_No_interne_Sitou", relationship = "many-to-many")
      )

### 4.3.2. Flux sur les jours de bilan = concentrations x volume ----
for (type in ouvrages) {
  assign(paste0("flux_",type),
         get(paste0("grille_jours_",type)) %>%
           inner_join(
             get(paste0("data_conc_",type)) %>%
               filter(!is.na(Concentration)) %>%
               select(Code_SANDRE, Pt_SANDRE, PP_No_interne_Sitou, Code_sandre_parametre,
                      Date_debut_prel, Concentration, Qualif_conc),
             by = c("Code_SANDRE", "Pt_SANDRE", "PP_No_interne_Sitou", "Code_sandre_parametre", 
                    "Date_debut_prel")
           ) %>%
           mutate(
             Annee     = year(Date_debut_prel),
             Mois      = month(Date_debut_prel),
             Flux_kg_j = Concentration * Debit_m3j * 1e-3,
             Source_flux = "Flux jour Bilan 24h",
             Fiabilite_flux = case_when(
               Qualif_debit == "Incorrecte" & Qualif_conc == "Incorrecte" ~ 0,
               Qualif_debit == "Incorrecte" | Qualif_conc == "Incorrecte" ~ 1,
               Qualif_debit == "Correcte" & Qualif_conc == "Correcte" ~ 3,
               TRUE ~ NA_real_
             )
           )
  )
}
### 4.3.3. Flux réel nul = Jours sans déversements et à débit nul ----
for (type in ouvrages) {
  assign(paste0("flux_debit_nul_",type), 
         get(paste0("grille_jours_",type)) %>%
           anti_join(
             get(paste0("data_conc_",type)) %>%
               filter(!is.na(Concentration)) ,
             by = c("Code_SANDRE", "Pt_SANDRE", "PP_No_interne_Sitou",
                    "Code_sandre_parametre", "Date_debut_prel")
           ) %>%
           filter(Debit_m3j==0,
                  Pt_SANDRE %in% pts_rejet) %>%
           mutate(
             Annee     = year(Date_debut_prel),
             Mois      = month(Date_debut_prel),
             Flux_kg_j = 0,
             Source_flux = "Flux nul (débit nul)",
             Fiabilite_flux = case_when(
               Qualif_debit == "Incorrecte" ~ 0,
               Qualif_debit =="Correcte" ~ 3,
               TRUE ~ NA_real_
             )
           )
  )
}

###  4.3.4. FLUX A4 HORS JOURS DE BILAN — TABLE UNIQUE DES CAS ----
#### 4.3.4.1. Ratios Décapol pour reconstitué les flux à partir des bilans ----

# Ratio Décapol annuel N (bilans à débit MESURÉ uniquement)
ratio_decapol <- flux_STEP %>%
  filter(Pt_SANDRE == "A4") %>%
  group_by(Code_SANDRE, Pt_SANDRE, PP_No_interne_Sitou, Code_sandre_parametre, Annee) %>%
  summarise(
    Flux_moy_bilans  = mean(Flux_kg_j, na.rm = TRUE),
    Debit_moy_bilans = mean(Debit_m3j, na.rm = TRUE),
    C_pond           = Flux_moy_bilans / Debit_moy_bilans * 1e3,
    Nb_bilans_annuel = n(),
    Qualif_base_N = qualite_base(Fiabilite_flux),
    .groups = "drop"
  )

# Ratio Décapol mensuel N : mois éligibles = au moins 4 bilans/mois pour tous les paramètres
nb_param_flux <- n_distinct(params_flux$Code_sandre_parametre)

mois_eligibles_A4 <- flux_STEP %>%
  filter(Pt_SANDRE == "A4", Source_debit == "Débit mesuré",
         Code_sandre_parametre %in% params_flux$Code_sandre_parametre) %>%
  mutate(Mois = month(Date_debut_prel)) %>%
  count(Code_SANDRE, PP_No_interne_Sitou, Annee, Mois, Code_sandre_parametre, name = "Nb_bilans") %>%
  group_by(Code_SANDRE, PP_No_interne_Sitou, Annee, Mois) %>%
  filter(n_distinct(Code_sandre_parametre) == nb_param_flux, min(Nb_bilans) >= 4) %>%
  ungroup() %>%
  distinct(Code_SANDRE, PP_No_interne_Sitou, Annee, Mois)

ratio_decapol_mensuel <- flux_STEP %>%
  filter(Pt_SANDRE == "A4", Source_debit == "Débit mesuré") %>%
  mutate(Mois = month(Date_debut_prel)) %>%
  inner_join(mois_eligibles_A4, by = c("Code_SANDRE", "PP_No_interne_Sitou", "Annee", "Mois")) %>%
  group_by(Code_SANDRE, Pt_SANDRE, PP_No_interne_Sitou, Code_sandre_parametre, Annee, Mois) %>%
  summarise(
    Flux_moy_bilans_mois  = mean(Flux_kg_j, na.rm = TRUE),
    Debit_moy_bilans_mois = mean(Debit_m3j, na.rm = TRUE),
    C_pond_mois           = Flux_moy_bilans_mois / Debit_moy_bilans_mois * 1e3,
    Nb_bilans_mensuel     = n(),
    Qualif_base_mois = qualite_base(Fiabilite_flux),
    .groups = "drop"
  )

# Ratio Décapol année N-1 
# Complétéer les débits mesurés en N-1 avec les interpolations, moyenne et forfait
debit_N1_ratio <- debit_A4_N1 %>%
  select(Code_SANDRE, PP_No_interne_Sitou, Date_debut_prel, Debit_comble = Debit_m3j)%>%
  left_join(debit_forfait_pt, by = c("Code_SANDRE", "PP_No_interne_Sitou")) %>%
  mutate(Debit_m3j = coalesce(Debit_comble, Debit_forfait)) %>%
  filter(!is.na(Debit_m3j)) %>%
  # Qualif debit : la vraie qualification n'existe que pour les jours de débit mesuré
  left_join(
    data_debit_A4_N1 %>%
      mutate(Date_debut_prel = Date_debut_prel + years(1)) %>%
      filter(!is.na(Date_debut_prel)) %>%
      distinct(Code_SANDRE, PP_No_interne_Sitou, Date_debut_prel, Qualif_debit),
    by = c("Code_SANDRE", "PP_No_interne_Sitou", "Date_debut_prel")
  )

# Calcul du ratio N-1
ratio_decapol_N1 <- data_conc_A4_N1 %>%
  mutate(Date_debut_prel = Date_debut_prel + years(1)) %>%
  filter(!is.na(Date_debut_prel)) %>%
  inner_join(debit_N1_ratio,
             by = c("Code_SANDRE", "PP_No_interne_Sitou", "Date_debut_prel")) %>%
  mutate(Annee_N1  = Annee-1,
         Flux_kg_j = Concentration * Debit_m3j * 1e-3,
         Fiabilite_bilan = fiab_bilan(Qualif_debit, Qualif_conc)) %>%
  group_by(Annee_N1, Code_SANDRE, PP_No_interne_Sitou, Code_sandre_parametre) %>%
  summarise(
    Flux_moy_bilans_N1  = mean(Flux_kg_j, na.rm = TRUE),
    Debit_moy_bilans_N1 = mean(Debit_m3j, na.rm = TRUE),
    C_pond_N1           = Flux_moy_bilans_N1 / Debit_moy_bilans_N1 * 1e3,
    Qualif_base_N1 = qualite_base(Fiabilite_bilan),
    .groups = "drop"
  )

#### 4.3.4.2. Table unique des cas ----

cles_jour <- c("Code_SANDRE", "PP_No_interne_Sitou", "Code_sandre_parametre", "Date_debut_prel")

jours_deja_traites <- bind_rows(
  flux_STEP           %>% filter(Pt_SANDRE == "A4") %>% select(all_of(cles_jour)),
  flux_debit_nul_STEP %>% filter(Pt_SANDRE == "A4") %>% select(all_of(cles_jour))
) %>% distinct()

flux_A4_cas <- debit_A4_cascade %>%
  crossing(params_flux %>% select(Code_sandre_parametre)) %>%
  mutate(Annee    = Annee,
         Mois     = month(Date_debut_prel),
         Annee_N1 = Annee - 1) %>%
  anti_join(jours_deja_traites, by = cles_jour) %>%
  left_join(ratio_decapol_mensuel %>%
              select(Code_SANDRE, PP_No_interne_Sitou, Code_sandre_parametre, Annee, Mois, C_pond_mois, Qualif_base_mois),
            by = c("Code_SANDRE", "PP_No_interne_Sitou", "Code_sandre_parametre", "Annee", "Mois")) %>%
  left_join(ratio_decapol %>%
              select(Code_SANDRE, PP_No_interne_Sitou, Code_sandre_parametre, Annee, C_pond, Qualif_base_N),
            by = c("Code_SANDRE", "PP_No_interne_Sitou", "Code_sandre_parametre", "Annee")) %>%
  left_join(ratio_decapol_N1 %>%
              select(Code_SANDRE, PP_No_interne_Sitou, Code_sandre_parametre, Annee_N1, C_pond_N1, Qualif_base_N1),
            by = c("Code_SANDRE", "PP_No_interne_Sitou", "Code_sandre_parametre", "Annee_N1")) %>%
  left_join(flux_forfait_A4 %>%
              select(Code_SANDRE, PP_No_interne_Sitou, Code_sandre_parametre, Flux_kg_j_forfait),
            by = c("Code_SANDRE", "PP_No_interne_Sitou", "Code_sandre_parametre")) %>%
  mutate(
    Debit_dispo = !is.na(Debit_m3j),
    
    # UN SEUL ENDROIT où l'ordre de priorité est défini
    Cas = case_when(
      !is.na(C_pond_mois) & Debit_dispo ~ "1_ratio_mois_N",
      !is.na(C_pond)      & Debit_dispo ~ "2_ratio_annee_N",
      !is.na(C_pond_N1)   & Debit_dispo ~ "3_ratio_annee_N1",
      !is.na(Flux_kg_j_forfait)         ~ "4_forfait",
      TRUE                              ~ "5_aucune"
    ),
    Concentration = case_when(
      Cas == "1_ratio_mois_N"   ~ C_pond_mois,
      Cas == "2_ratio_annee_N"  ~ C_pond,
      Cas == "3_ratio_annee_N1" ~ C_pond_N1,
      TRUE                      ~ NA_real_
    ),
    Source_flux = case_when(
      Cas == "1_ratio_mois_N"   ~ "Flux estimé reconstitué (mois N)",
      Cas == "2_ratio_annee_N"  ~ "Flux estimé reconstitué (année N)",
      Cas == "3_ratio_annee_N1" ~ "Flux estimé (année N-1)",
      Cas == "4_forfait"        ~ "Flux forfaitaire",
      TRUE                      ~ NA_character_
    ),
    Flux_kg_j = case_when(
      Cas == "4_forfait" ~ Flux_kg_j_forfait,
      Cas == "5_aucune"  ~ NA_real_,
      TRUE               ~ Concentration * Debit_m3j * 1e-3  #pour le flux reconstitué
    ),
    Qualif_base = case_when(
      Cas == "1_ratio_mois_N"   ~ Qualif_base_mois,
      Cas == "2_ratio_annee_N"  ~ Qualif_base_N,
      Cas == "3_ratio_annee_N1" ~ Qualif_base_N1,
      TRUE                      ~ NA_character_
    ),
    Fiabilite_flux = case_when(
      is.na(Qualif_base)                                          ~ NA_real_,  # forfait, aucune
      Qualif_base == "incorrecte" & Qualif_debit == "Incorrecte"  ~ 0,
      Qualif_base %in% c("incorrecte", "mixte") |
        Qualif_debit == "Incorrecte"                              ~ 1,
      Qualif_base == "correcte" & Qualif_debit == "Correcte"      ~ 2,
      TRUE                                                        ~ NA_real_
    )
  )

#### 4.3.4.3. DIAGNOSTIC : où sont les cas manquants ? ----

diag_cas_A4 <- flux_A4_cas %>%
  count(Cas, Source_debit, name = "Nb_jours_x_param") %>%
  arrange(Cas, Source_debit)

diag_aucune_A4 <- flux_A4_cas %>%
  filter(Cas == "5_aucune") %>%
  count(Code_SANDRE, PP_No_interne_Sitou, Code_sandre_parametre, Source_debit, name = "Nb_jours")

diag_sans_debit_A4 <- flux_A4_cas %>%
  group_by(Code_SANDRE, PP_No_interne_Sitou) %>%
  summarise(Prop_sans_debit = mean(!Debit_dispo), .groups = "drop") %>%
  filter(Prop_sans_debit > 0) %>%
  arrange(desc(Prop_sans_debit))

#### 4.3.4.4. Sortie (même rôle que l'ancien flux_repli_A4, y compris les STEP absentes en N) ----

flux_repli_A4 <- flux_A4_cas %>%
  filter(!is.na(Flux_kg_j)) %>%
  select(Code_SANDRE, Pt_SANDRE, PP_No_interne_Sitou, Date_debut_prel,
         Code_sandre_parametre, Debit_m3j, Qualif_debit, Source_debit,
         Annee, Mois, Concentration, Flux_kg_j, Source_flux, Fiabilite_flux)


### 4.3.5 Estimation en cas de déversements sans charges polluantes transmises ----
#Jours sans déversements mais à débit non nul = flux à estimer avec A3 

### 1.Calcul des différents cas d'estimation des flux rejetés ----

# Priorité 1 = Bilan A3 si jours de bilans 

conc_A3_jour <- data_conc_STEP %>%
  filter(Pt_SANDRE == "A3", !is.na(Concentration)) %>%
  mutate(Annee = year(Date_debut_prel)) %>%
  select(Annee, Date_debut_prel, Code_SANDRE, PP_No_interne_Sitou, 
         Code_sandre_parametre, Concentration)

#  Priorité 2 = Moyenne [A3] année N temps de pluie (Pluvio >0) 

# Définition temps de pluie

temps_pluie <- data_pluvio %>%
  filter(Pluvio_mmj >0)

# Fréquence pluviométrique pour appliquer Priorité 2 uniquement les jours de pluie si 365 pluvio

frequence_pluvio_ok <- data_pluvio %>%
  filter(!is.na(Pluvio_mmj)) %>%
  mutate(Annee = year(Date_debut_prel)) %>%
  group_by(Annee, Code_SANDRE) %>%
  summarise(Frequence = n(), .groups="drop") %>%
  filter(Frequence >= 365)

# Qualification pluviométrique pour appliquer Priorité 2 uniquement si 
# aucune qualification de pluie d'incorrect dans l'année
pluvio_fiable_annee <- data_pluvio %>%
  filter(!is.na(Pluvio_mmj)) %>%
  mutate(Annee = year(Date_debut_prel)) %>%
  group_by(Annee, Code_SANDRE) %>%
  summarise (
    Pluvio_fiable = !any(Qualif_pluvio == "Incorrecte", na.rm = TRUE),
    .groups ="drop"
  ) %>%
  filter(Pluvio_fiable == TRUE)

# Calcul concentration moyenne annuelle A3 temps de pluie 

conc_A3_moy_pluie <- conc_A3_jour %>%
  filter(
    Date_debut_prel %in% temps_pluie$Date_debut_prel,
    Code_SANDRE %in% frequence_pluvio_ok$Code_SANDRE,
    Code_SANDRE %in% pluvio_fiable_annee$Code_SANDRE
  ) %>%
  group_by(Annee, Code_SANDRE, Code_sandre_parametre) %>%
  summarise(conc_moy_A3_pluie_N = mean(Concentration, na.rm =TRUE), .groups="drop")


# Priorité 3 = Moyenne [A3] année N, tout temps confondu (sec et pluie)

conc_A3_moy_N <- conc_A3_jour %>%
  group_by(Annee, Code_SANDRE, Code_sandre_parametre) %>%
  summarise(conc_A3_moy_N = mean(Concentration, na.rm=TRUE), .groups="drop")

# Priorité 4 = Moyenne [A3] année N-1, tout temps confondu 

conc_A3_moy_N1 <- get(paste0("data_all_STEP_",Annee-1)) %>%
  filter(
    Pt_SANDRE == "A3",
    Code_sandre_parametre %in% params_flux$Code_sandre_parametre,
    !is.na(Val_resultat_analyse)
  ) %>%
  mutate(Annee_A3 = year(Date_debut_prel)) %>%
  group_by(Annee_A3, Code_SANDRE, Code_sandre_parametre) %>%
  summarise(conc_moy_A3_N1 = mean(Val_resultat_analyse, na.rm=TRUE), .groups="drop")


### 2. Application estimation aux différents jours pour A1/R1/A2/A5 ----

# Extraction des jours sans concentration avec débit > 0

base_A1_R1 <- grille_jours_SCL %>%
  anti_join(
    data_conc_SCL %>% filter(!is.na(Concentration)),
    by = c("Code_SANDRE", "Pt_SANDRE", "PP_No_interne_Sitou", "Code_sandre_parametre",
           "Date_debut_prel")
  ) %>%
  filter(Debit_m3j > 0)

base_A2_A5 <- grille_jours_STEP %>%
  filter(Pt_SANDRE %in% c("A2", "A5")) %>%
  anti_join(
    data_conc_STEP %>% filter(!is.na(Concentration)),
    by = c("Code_SANDRE", "Pt_SANDRE", "PP_No_interne_Sitou", "Code_sandre_parametre", 
           "Date_debut_prel")
  ) %>%
  filter(Debit_m3j > 0)

flux_repli_deversements <- bind_rows(base_A1_R1, base_A2_A5) %>%
  mutate(
    Annee = year(Date_debut_prel),
    Mois = month(Date_debut_prel),
    Annee_N1 = Annee -1
  ) %>%
  
  # Récupération du code SANDRE STEP correspondant pour A1/R1
  
  left_join(
    Table_corresp_Roseau_Sitouref %>%
      select(Code_SANDRE, `Code Sandre STEU Sitouref`),
    by ="Code_SANDRE"
  ) %>%
  rename(Code_SANDRE_corresp = `Code Sandre STEU Sitouref`) %>%
  mutate(
    Code_SANDRE = case_when(
      Pt_SANDRE %in% c("A1", "R1") ~ Code_SANDRE_corresp,
      Pt_SANDRE %in% c("A2", "A5") ~ Code_SANDRE,
      TRUE ~ NA_character_
    )
  ) %>%
  select(-Code_SANDRE_corresp) %>%
  
  # Priorité 1 : Jour de bilan A3 à la date exacte
  
  left_join(
    conc_A3_jour %>%
      select(Code_SANDRE, Code_sandre_parametre, 
             Date_debut_prel, Concentration)
    %>%
      rename(conc_A3_jour_exact = Concentration),
    by = c("Code_SANDRE" = "Code_SANDRE",
           "Code_sandre_parametre" = "Code_sandre_parametre",
           "Date_debut_prel")
  ) %>%
  
  # Priorité 2 : Pluvio du jour + fréquence de la STEP
  
  left_join(
    conc_A3_moy_pluie %>%
      select(Code_SANDRE, Code_sandre_parametre,
             Annee,  conc_moy_A3_pluie_N),
    by = c("Code_SANDRE" = "Code_SANDRE",
           "Code_sandre_parametre" = "Code_sandre_parametre",
           "Annee"
    )
  ) %>%
  
  # Priorités 3 et 4 : Moyennes annnuelles A3 (N, N pluie, N-1)
  
  left_join(
    conc_A3_moy_N, 
    by= c("Code_SANDRE" = "Code_SANDRE",
          "Code_sandre_parametre",
          "Annee" = "Annee")) %>%
  left_join(
    conc_A3_moy_N1,
    by = c("Code_SANDRE" = "Code_SANDRE",
           "Code_sandre_parametre",
           "Annee_N1" = "Annee_A3")) %>%
  
  # Décision unique : quel cas s'applique à cette ligne ?
  
  mutate(
    Cas = case_when(
      !is.na(conc_A3_jour_exact) ~ "exact",
      !is.na(conc_moy_A3_pluie_N)  ~ "pluie_N",
      !is.na(conc_A3_moy_N) ~ "moyenne_N",
      !is.na(conc_moy_A3_N1) ~ "moyenne_N-1",
      TRUE ~ "aucune"
    ),
    
    # ── Concentration et source dérivées du même Cas ──
    
    Concentration = case_when(
      Cas == "exact"      ~ conc_A3_jour_exact,
      Cas == "pluie_N"    ~ conc_moy_A3_pluie_N,
      Cas == "moyenne_N"  ~ conc_A3_moy_N,
      Cas == "moyenne_N-1" ~ conc_moy_A3_N1,
      TRUE                ~ NA_real_
    ),
    Source_flux = case_when(
      Cas == "exact"      ~ "Flux estimé (bilan A3 jour exact)",
      Cas == "pluie_N"    ~ "Flux estimé (moyenne A3 temps de pluie)",
      Cas == "moyenne_N"  ~ "Flux estimé (moyenne A3, tout temps)",
      Cas == "moyenne_N-1" ~ "Flux estimé (moyenne A3 année N-1, tout temps)",
      TRUE                ~ "Débit non nul - concentration A3 indisponible"
    ),
    Flux_kg_j = case_when(
      !is.na(Concentration) ~ Concentration * Debit_m3j * 1e-3,
      TRUE ~ NA_real_ 
    ) ,
    Fiabilite_flux = case_when(
      Cas %in% c("exact", "pluie_N", "moyenne_N", "moyenne_N-1") & 
        Qualif_debit == "Correcte" ~ 2,
      Cas %in% c("exact", "pluie_N", "moyenne_N", "moyenne_N-1") & 
        Qualif_debit == "Incorrecte" ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # suppression des variables non nécessaires dans le tableau final
  select(-Cas, -conc_A3_jour_exact, -conc_A3_moy_N, -conc_moy_A3_pluie_N, -conc_moy_A3_N1,
         -Code_SANDRE, -Annee_N1)

flux_repli_SCL <- flux_repli_deversements %>% filter(Pt_SANDRE %in% c("A1","R1"))
flux_repli_STEP <- flux_repli_deversements %>% filter(Pt_SANDRE %in% c("A2", "A5"))

### 4.3.6 Assemblage final----
# STEP   
data_flux_j_STEP <- flux_STEP %>%
  bind_rows(flux_debit_nul_STEP, flux_repli_A4, flux_repli_STEP) %>%
  left_join(Liste_points_AS_STEP %>% select(PP_No_interne_Sitou, Nom, Nom_pt_AS), 
            by = "PP_No_interne_Sitou") %>% 
  # compléter les codes unités à partir du tableau code sandre parametre/code unite
  left_join(
    params_flux %>%
      distinct(Code_sandre_parametre, Code_unite),
    by= "Code_sandre_parametre"
  ) %>%
  select(
    Code_SANDRE, Nom, PP_No_interne_Sitou, Nom_pt_AS, Pt_SANDRE, Date_debut_prel, 
    Code_sandre_parametre, Code_unite, Fiabilite_flux, everything(), 
    -Qualif_debit, -Qualif_conc, -Annee, -Mois
  ) %>% 
  filter(Pt_SANDRE != "A3") 

# SCL
data_flux_j_SCL <- flux_SCL %>%
  bind_rows(flux_debit_nul_SCL, flux_repli_SCL) %>%
  left_join(Liste_points_AS_SCL %>% select(PP_No_interne_Sitou, Nom, Nom_pt_AS),
            by = "PP_No_interne_Sitou") %>%
  # compléter les codes unités à partir du tableau code sandre parametre/code unite
  left_join(
    params_flux %>%
      distinct(Code_sandre_parametre, Code_unite),
    by= "Code_sandre_parametre"
  ) %>%
  select (Code_SANDRE, Nom, PP_No_interne_Sitou, Nom_pt_AS, Pt_SANDRE, Date_debut_prel, 
          Code_sandre_parametre, Code_unite, Fiabilite_flux, everything(), 
          -Qualif_debit,  -Qualif_conc, -Annee, -Mois
  )


# supprimer les variables nécessaires aux tests mais non nécessaires dans tableau final
# comme débit et concentration

# ──────────────────────────────────────────── ─
# 5. EXPORT DES FLUX ----
# ──────────────────────────────────────────── ─

## 5.1. FLUX MENSUEL ----
# ──────────────────────────────────────────── ─
for (type in ouvrages) {
  assign(paste0("flux_mois_",type), 
         get(paste0("data_flux_j_", type)) %>%
           mutate(
             Annee = year(Date_debut_prel),
             Mois = month(Date_debut_prel)
           )%>%
           group_by(Code_SANDRE, Nom, PP_No_interne_Sitou, Nom_pt_AS, Pt_SANDRE,
                    Annee, Mois, Code_sandre_parametre)  %>%
           summarise(
             Flux_kg_mois = sum(Flux_kg_j, na.rm = TRUE),
             Nb_jours_deversements = if_else(
               unique(Pt_SANDRE) %in% c("A1", "R1", "A2", "A5"),
               sum(Flux_kg_j>0, na.rm =TRUE),
               NA_integer_
             ), .groups="drop"
           ) %>%
           left_join(
             ListeParamUnites_STEP %>%
               distinct(Code_sandre_parametre, Code_unite),
             by = "Code_sandre_parametre"
           ) %>%
           relocate(Code_unite, .before=Flux_kg_mois)
  )
}


## 5.2. FLUX ANNUEL ----
# ──────────────────────────────────────────── ─

for (type in ouvrages) {
  assign(paste0("flux_annuel_",type), 
         get(paste0("data_flux_j_", type)) %>%
           mutate(Annee = year(Date_debut_prel))%>%
           group_by(Code_SANDRE, Nom, PP_No_interne_Sitou, Nom_pt_AS, Pt_SANDRE,
                    Annee, Code_sandre_parametre)  %>%
           summarise(
             Flux_kg_an = sum(Flux_kg_j, na.rm = TRUE),
             Nb_jours_deversements = if_else(
               unique(Pt_SANDRE) %in% c("A1", "R1", "A2", "A5"),
               sum(Flux_kg_j>0, na.rm =TRUE),
               NA_integer_
             ), .groups="drop"
           ) %>%
           left_join(
             ListeParamUnites_STEP %>%
               distinct(Code_sandre_parametre, Code_unite),
             by = "Code_sandre_parametre"
           ) %>%
           relocate(Code_unite, .before=Flux_kg_an)
  )
}


## 5.3. EXPORT EXCEL ----
# ──────────────────────────────────────────── ─

dir.create("resultat", showWarnings = FALSE)


### ── Fichier flux journalier par département ----

# STEP
data_flux_j_STEP <- data_flux_j_STEP %>%
  mutate(Departement = substr(Code_SANDRE, 3,4))

liste_dept_STEP <- unique(data_flux_j_STEP$Departement)

for (dept in liste_dept_STEP) {
  data_dept_STEP <- data_flux_j_STEP %>%
    filter(Departement == dept) %>%
    select(-Departement)
  
  nom_fichier_dept_STEP <- paste0(
    "resultat/STEP/Flux_journaliers_STEP_Dep_",dept,"_",DT,"_",Annee,".xlsx"
  )
  
  wb_dept <- createWorkbook()
  ecrire_onglet(wb_dept,"Flux_journaliers", data_dept_STEP)
  
  saveWorkbook(wb_dept, nom_fichier_dept_STEP, overwrite = TRUE)
  message("✔ Export terminé : ", nom_fichier_dept_STEP)
}

# Réseaux 
data_flux_j_SCL <- data_flux_j_SCL %>%
  mutate(Departement      = substr(Code_SANDRE, 3, 4))

liste_dept_SCL <- unique(data_flux_j_SCL$Departement)

for (dept in liste_dept_SCL) {
  data_dept_SCL <- data_flux_j_SCL %>%
    filter(Departement == dept) %>%
    select(-Departement)
  
  nom_fichier_dept_SCL <- paste0(
    "resultat/SCL/Flux_journaliers_SCL_Dep_",dept,"_",DT,"_",Annee,".xlsx"
  )
  
  wb_dept <- createWorkbook()
  ecrire_onglet(wb_dept,"Flux_journaliers", data_dept_SCL)
  
  saveWorkbook(wb_dept, nom_fichier_dept_SCL, overwrite = TRUE)
  message("✔ Export terminé : ", nom_fichier_dept_SCL)
}

### ── Fichier flux mensuel ──────────────────────
for (type in ouvrages) {
  fichier_flux_mois <- paste0("resultat/",type,"/Flux_mois_",type,"_",DT,"_",Annee,".xlsx")
  
  wb <- createWorkbook()
  ecrire_onglet(wb, "Flux_mensuels", get(paste0("flux_mois_",type)))
  saveWorkbook(wb, 
               fichier_flux_mois, 
               overwrite = TRUE)
  message("✔ Export terminé : ", fichier_flux_mois)
  
}


### ── Fichier flux annuel ──────────────────────
for (type in ouvrages) {
  fichier_flux_annuel <- paste0("resultat/",type,"/Flux_annuel_",type,"_",DT,"_",Annee,".xlsx")
  
  wb <- createWorkbook()
  ecrire_onglet(wb, "Flux_annuels", get(paste0("flux_annuel_",type)))
  saveWorkbook(wb, 
               fichier_flux_annuel, 
               overwrite = TRUE)
  message("✔ Export terminé : ", fichier_flux_annuel)
  
}



