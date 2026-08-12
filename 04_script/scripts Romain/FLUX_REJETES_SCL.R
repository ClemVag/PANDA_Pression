# ─────────────────────────────────────────────
# 0. BLOC EN-TÊTE ----
# ─────────────────────────────────────────────


# ─────────────────────────────────────────────
# 1. PARAMETRAGE ----
# ─────────────────────────────────────────────


## 1.1. DIRECTION TERRITORIALE ----
# Indiquer le nom de votre DT à droite de la flèche, en MAJUSCULES, entre guillemets
# Choix : DVO / DVM / DSF / DSAM / DSAV / DBN / TOUTES
# Exemple : DT <- "DVO"
DT <- "DSAV"

## 1.2. CHOIX DU SCRIPT ----

# Indiquer le nom du script choisi en MAJUSCULES, entre guillemets
# Choix : PANDA / PANDARAMIS / COMPLETUDE / DCO / SIMULATEUR
# Exemple : script <- "PANDA"
script <- "PRESSION"
{
# ─────────────────────────────────────────────
# 2. PACKAGES ET FONCTIONS ----
# ─────────────────────────────────────────────

  # PACKAGES
    library(readxl)
    library(tidyverse)
    library(dplyr)
    library(summarytools)
    library(conflicted)
    conflict_prefer_all("dplyr")
    library(janitor) # doublons
    library(flextable)
    library(forcats)
    library(lubridate) # date et heure
    library(tidyr) # pivot de table
    library(openxlsx)
    ## lors d'un export dans excel permet de garder format date YYYY-MM-DD (sinon peut devenir YYYY-DD-MM)
    options(openxlsx.dateFormat = "yyyy-mm-dd")
    conflicts_prefer(lubridate::month)
  
# ─────────────────────────────────────────────
# 3. PREPARATION FICHIER ENTREPOT DE DONNEES ----
# ─────────────────────────────────────────────
    
  ## 3.1. DONNEES DE ROSEAU----
    Roseau_SCL <- read_delim("data/Roseau.csv", 
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
    Table_corresp_Roseau_Sitouref <- read_excel("data/Table_corresp_Roseau_Sitouref.xlsx") %>% 
      rename("Code_SANDRE"=`Code Sandre SCL Sitouref`)
    
  ## 3.3. DONNEES DE SITOUREF----
  ### Liste des points d'autosurveillance ----
    Liste_points_AS_SCL<-read_excel("data/Liste_points_AS_SCL.xlsx", 
                                    sheet = "SCL")
    Liste_points_AS_SCL<-Liste_points_AS_SCL %>% 
      select(DT,Code_SANDRE_SCL,Nom_SCL,`No interne Sitou`,`Nom Sitou`,`Identifiant principal Sitou`,LocalisationSitouref,Classe, `Type réseau`) %>% 
      rename("Pt_SANDRE" = LocalisationSitouref,
             PP_No_interne_Sitou = `No interne Sitou`,
             NOM = `Nom Sitou`,
             PP_Identifiant_principal_Sitou = `Identifiant principal Sitou`,
             Type_reseau = `Type réseau`)
    
    Liste_points_AS_SCL$Classe <- factor(Liste_points_AS_SCL$Classe,
                                         levels = c("< 120 kgDBO/j", "de 120 à 600 kgDBO/j", "> 600 kgDBO/j")
    )
    
    Liste_points_AS_SCL$Classe<-Liste_points_AS_SCL$Classe %>% 
      fct_recode(
        "<120" = "< 120 kgDBO/j",
        "120-600" = "de 120 à 600 kgDBO/j",
        ">600" = "> 600 kgDBO/j"
      )
    
    for (p in 1:nrow(Liste_points_AS_SCL))
    {  Liste_points_AS_SCL$Type_reseau[p]<-ifelse(is.na(Liste_points_AS_SCL$Type_reseau[p]),
                                                  Table_corresp_Roseau_Sitouref$Type_reseau[which(Table_corresp_Roseau_Sitouref$Code_SANDRE==Liste_points_AS_SCL$Code_SANDRE_SCL[p])],
                                                  Liste_points_AS_SCL$Type_reseau[p])
    
    }
  ## 3.4. CHOIX DU FICHIER DE DONNEES DE LA DT ----
  # Choisir la requête sur l'entrepot de données utilisée pour PANDA
  ### 3.4.1. Import fichier de la DT ----
    if (DT == "DVO")
    {
      data_all_SCL_0 <- read_excel("data/Données_AS_SCL_DVO.xlsx", sheet = "Réseaux")
    }
    if (DT == "DVM")
    {
      data_all_SCL_0 <- read_excel("data/Données_AS_SCL_DVM.xlsx", sheet = "Réseaux")
    }
    if (DT == "DBN")
    {
      data_all_SCL_0 <- read_excel("data/Données_AS_SCL_DBN.xlsx", sheet = "Réseaux")
    }
    if (DT == "DSAM")
    {
      data_all_SCL_0 <- read_excel("data/Données_AS_SCL_DSAM.xlsx", sheet = "Réseaux")
    }
    if (DT == "DSAV")
    {
      data_all_SCL_0 <- read_excel("data/Données_AS_SCL_DSAV.xlsx", sheet = "Réseaux")
    }
    if (DT == "DSF")
    {
      data_all_SCL_01 <- read_excel("data/Données_AS_SCL_DSF_DRIF.xlsx", sheet = "Réseaux")
      data_all_SCL_02 <- read_excel("data/Données_AS_SCL_DSF_DPPC.xlsx",sheet = "Réseaux")
      colnames(data_all_SCL_02) <- colnames(data_all_SCL_01)
      data_all_SCL_0 <- rbind(data_all_SCL_01, data_all_SCL_02)
    }
    
    if (DT=="TOUTES")
    {
      data_all_SCL_01 <- read_excel("data/Données_AS_SCL_DVO.xlsx", sheet = "Réseaux")
      data_all_SCL_02 <- read_excel("data/Données_AS_SCL_DVM.xlsx", sheet = "Réseaux")
      data_all_SCL_03 <- read_excel("data/Données_AS_SCL_DBN.xlsx", sheet = "Réseaux")
      data_all_SCL_04 <- read_excel("data/Données_AS_SCL_DSAM.xlsx", sheet = "Réseaux")
      data_all_SCL_05 <- read_excel("data/Données_AS_SCL_DSAV.xlsx", sheet = "Réseaux")
      data_all_SCL_06 <- read_excel("data/Données_AS_SCL_DSF_DRIF.xlsx", sheet = "Réseaux")
      data_all_SCL_07 <- read_excel("data/Données_AS_SCL_DSF_DPPC.xlsx", sheet = "Réseaux")
      data_all_SCL_0 <- rbind(data_all_SCL_01, 
                               data_all_SCL_02,
                               data_all_SCL_03,
                               data_all_SCL_04,
                               data_all_SCL_05,
                               data_all_SCL_06,
                               data_all_SCL_07)
    }
    
    ### 3.4.2. Renommer les colonnes ----
    data_all_SCL_0 <-  data_all_SCL_0 %>% 
      rename(id_analyse = `Code analyse`,
             Code_SANDRE = `Code SANDRE`,
             SCL_Nom_Sitou = `Station_Nom Sitou`,
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
    ### 3.4.3. Convertir les colonnes d'un type à un type numeric, date ----
    data_all_SCL_1 <- data_all_SCL_0 %>%
      mutate(Date_debut_prel = as.Date(Date_debut_prel)) %>%
      mutate(Date_depot = as.Date(Date_depot)) %>%
      mutate(Classe_CBPO = as.factor(Classe_CBPO))
    
    ### 3.4.4 Nettoyage des doublons et des caractères problématiques ----
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
    data_all_SCL$SCL_Nom_Sitou<-gsub("/", "-",data_all_SCL$SCL_Nom_Sitou)
    data_all_SCL$SCL_Nom_Sitou<-gsub(":", "-",data_all_SCL$SCL_Nom_Sitou)
    data_all_SCL$SCL_Nom_Sitou<-gsub("SAINT-SYMPHORIEN-LE-VALOIS-La-Haye-du-Puits", "SAINT-SYMPHORIEN-LE-VALOIS",data_all_SCL$SCL_Nom_Sitou)
    
    
    ### 3.4.5. Nettoyages complémentaires ----
    #Suppression des lignes correspondant au code analyse 0 (analyse non faite)
    data_all_SCL<-data_all_SCL %>%
      filter(Code_remarque!=0)
    
    ### Remplacement du code 1098 (Volume) par 1552 (Vol.Moy.J) le cas échéant ----
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
    ### Réordonnancement de data_SCL_complet$Classe_CBPO et recodage en facteur ----
    data_all_SCL$Classe_CBPO <- factor(data_all_SCL$Classe_CBPO,
                                       levels = c("< 120 kgDBO/j", "de 120 à 600 kgDBO/j", "> 600 kgDBO/j")
    )
    data_all_SCL$Classe_CBPO<- data_all_SCL$Classe_CBPO %>%
      fct_recode(
        "<120" = "< 120 kgDBO/j",
        "120-600" = "de 120 à 600 kgDBO/j",
        ">600" = "> 600 kgDBO/j"
      )
    
    ## 3.4.6. Liste des paramètres remontés et des unités ----
    ListeParam_SCL<-distinct(data.frame(data_all_SCL_1$Code_sandre_parametre,data_all_SCL_1$Lib_court_parametre))
    colnames(ListeParam_SCL)<-c("Code_sandre_parametre","Lib_court_parametre")
    ListeParamUnites_SCL<-distinct(data.frame(data_all_SCL_1$Code_sandre_parametre,
                                              data_all_SCL_1$Lib_court_parametre,
                                              data_all_SCL_1$Code_unite,
                                              data_all_SCL_1$Symbole_unite))
    colnames(ListeParamUnites_SCL)<-c("Code_sandre_parametre","Lib_court_parametre","Code_unite","Symbole_unite")
    
    
    
    ## 3.5. LISTE DE SCL ----
    Liste_SCL_nom <- distinct(data.frame(data_all_SCL$SCL_Nom_Sitou,data_all_SCL$Code_SANDRE))  #Voir si on conserve
    colnames(Liste_SCL_nom)<-c("Nom_SCL","Code_SANDRE")
    
    # Attention un bout du code a été scrappé : création d'une table sur la 
    # base d'une requête pluriannuelle concernant exclusivement les déversoirs
    # d'une taille supérieure à 600 kg DBO5/j pour compter le nombre de 
    # déversements en moyenne quinquennale. A ajouter au besoin. 
    
    ## 3.6. LISTE DES PARAMETRES SELON ARRETE DU 21/07/2015 ----
    Liste_param_attendus<-read_excel("data/Paramètres_SCL.xlsx", 
                                     col_types = c("text", "text", "text"))
    
    
    ## 3.7. PARAMETRAGE SORTIE EXCEL ----
    entete <- createStyle(
      fontSize = 12,           # Taille de la police
      textDecoration = "bold",     # Texte en gras
      halign = "center",       # Alignement horizontal centré
      valign = "center",       # Alignement vertical centré
      fgFill = "steelblue1",      
      wrapText = TRUE )
    
    autres_lignes<- createStyle(
      fontSize = 12,           # Taille de la police
      halign = "left",       # Alignement horizontal à gauche
      valign = "center",       # Alignement vertical centré
      wrapText = TRUE )
    
    # Bordure inférieure fine
    bordure_inferieure <- createStyle(
      border = "bottom",   # Ajoute une bordure inférieure
      borderColour = "black",  # Couleur de la bordure
      borderStyle = "thin")   # Style de la bordure (fin)
    
    # Bordure inférieure épaisse
    bordure_thick <- createStyle(border = "bottom", borderColour = "black", borderStyle = "thick")
    
    # Surlignement rouge
    surlign_rouge <- createStyle(
      textDecoration = "bold",     # Texte en gras
      fgFill = "red")
    
    
    
    ## 3.8. PARAMETRES DATES -----
    nb_jours = ifelse(leap_year(date = data_all_SCL$Date_debut_prel[1]), 366, 365)
}
# ─────────────────────────────────────────────
# 4. SCRIPT PRESSION ----
# ─────────────────────────────────────────────
if(script == "PRESSION")
{
  # ─────────────────────────────────────────────
  # 4.1. PARAMETRES DE FLUX ----
  # ─────────────────────────────────────────────
  
  params_flux <- tribble(
    ~Code_sandre_parametre, ~Lib_court_parametre,  ~Lib_long,
    "1313",                 "DBO5",                "DBO5 (kg/j)",
    "1314",                 "DCO",                 "DCO (kg/j)",
    "1305",                 "MES",                 "MES (kg/j)",
    "1319",                 "NTK",                 "NTK (kg/j)",
    "1335",                 "NH4",                 "NH4 (kg/j)",
    "1339",                 "NO2",                 "NO2 (kg/j)",
    "1340",                 "NO3",                 "NO3 (kg/j)",
    "1551",                 "NGL",                 "NGL (kg/j)",
    "1350",                 "PT",                  "PT (kg/j)",
    "1433",                 "PO4",                 "PO4 (kg/j)"
  )
  
  code_debit <- "1552"  # Vol.Moy.J. en m3/j
  pts_rejet  <- c("A1","R1")
  
  # ─────────────────────────────────────────────
  # 4.2. PREPARATION DES DONNEES ----
  # ─────────────────────────────────────────────
  
  data_rejet <- data_all_SCL %>%
    filter(Pt_SANDRE %in% pts_rejet)
  
  # Débits journaliers (tous les jours disponibles, pas seulement les jours de bilan)
  data_debit <- data_rejet %>%
    filter(Code_sandre_parametre == code_debit) %>%
    select(
      DT, Code_SANDRE, SCL_Nom_Sitou, Classe_CBPO,
      Pt_SANDRE, PP_Identifiant_principal_Sitou, PP_Nom_Sitou,
      Date_debut_prel,Val_resultat_analyse, Code_remarque, 
      Derniere_qualif_analyse, Commentaire_analyse
    ) %>%
    rename(
      Debit_m3j = Val_resultat_analyse,
      Identifiant_pt_AS = PP_Identifiant_principal_Sitou,
      Nom_pt_AS = PP_Nom_Sitou
      ) %>%
    mutate(
      Annee       = year(Date_debut_prel),
      Mois        = month(Date_debut_prel),
      Nb_jours_mois = days_in_month(Date_debut_prel)
    )
  
  # Concentrations — uniquement les jours où concentration ET débit sont présents
  # (= jours de bilan 24h)
  data_conc <- data_rejet %>%
    filter(Code_sandre_parametre %in% params_flux$Code_sandre_parametre) %>%
    select(
      DT, Code_SANDRE, SCL_Nom_Sitou, Classe_CBPO,
      Pt_SANDRE, PP_Identifiant_principal_Sitou, PP_Nom_Sitou,
      Date_debut_prel,
      Code_sandre_parametre, Lib_court_parametre, Symbole_unite,
      Val_resultat_analyse, Code_remarque, Derniere_qualif_analyse,
      Commentaire_analyse
    ) %>%
    rename(
      Concentration = Val_resultat_analyse,
      Identifiant_pt_AS = PP_Identifiant_principal_Sitou,
      Nom_pt_AS = PP_Nom_Sitou
      )
  # ─────────────────────────────────────────────
  # 4.3. CALCUL DES FLUX JOURNALIERS — METHODE DECAPOL ----
  #
  # Jours AVEC mesure de concentration :
  #   Flux_j = Concentration × Debit_j × 1e-3
  #
  ##### ATTENTE METHODE ESTIMATION QUAND PAS CONCENTRATION, 
  ##### OK AU-DESSUS
  # Jours SANS mesure de concentration :
  #   Flux_j = (Flux_moy_bilans / Debit_moy_bilans) × Debit_j × 1e-3
  #          = C_pond_bilans × Debit_j × 1e-3
  #
  # La C_pond est calculée sur TOUS les jours de bilan de l'année
  # (flux moyen annuel / débit moyen annuel sur jours de mesure)
  # ─────────────────────────────────────────────
  
  # Grille complète : tous les jours avec débit × tous les paramètres
  grille_jours <- data_debit %>%
    filter(!is.na(Debit_m3j)) %>%
    select(DT, Code_SANDRE, SCL_Nom_Sitou, Classe_CBPO,
           Pt_SANDRE, Identifiant_pt_AS,Nom_pt_AS,
           Date_debut_prel, Debit_m3j, Derniere_qualif_analyse,
           Commentaire_analyse) %>%
    crossing(params_flux %>% select(Code_sandre_parametre, Lib_court_parametre))
  
  # Flux sur les jours de bilan = concentrations x volume
  flux_bilans <- grille_jours %>%
    inner_join(
      data_conc %>%
        filter(!is.na(Concentration)) %>%
        select(Code_SANDRE, Pt_SANDRE,
               Code_sandre_parametre,
               Date_debut_prel, Concentration, Symbole_unite),
      by = c("Code_SANDRE", "Pt_SANDRE", "Code_sandre_parametre", "Date_debut_prel")
    ) %>%
    mutate(
      Annee     = year(Date_debut_prel),
      Mois      = month(Date_debut_prel),
      Flux_kg_j = Concentration * Debit_m3j * 1e-3,
      Source_concentration = "Flux jour déversement"
    )
  
  # Jours sans bilan mais à débit nul = flux réel nul
  flux_debit_nul <- grille_jours %>%
    anti_join(
      data_conc %>% filter(!is.na(Concentration)),
      by = c("Code_SANDRE", "Pt_SANDRE","Code_sandre_parametre",
             "Date_debut_prel")) %>%
    filter(Debit_m3j==0) %>%
    mutate(
      Annee = year(Date_debut_prel),
      Mois = month(Date_debut_prel),
      Concentration = NA_real_,
      Flux_kg_j =0,
      Source_concentration = "Flux réel nul (débit nul)",
    )
  
  # Jours sans bilan mais à débit non nul = flux à estimer avec A3
  conc_manquantes <- grille_jours %>%
    anti_join(
      data_conc %>% filter(is.na(Concentration)),
      by = c("Code_SANDRE", "Pt_SANDRE","Code_sandre_parametre",
             "Date_debut_prel")) %>%
    filter(Debit_m3j>0) %>%
    mutate(
      Annee = year(Date_debut_prel),
      Mois = month(Date_debut_prel),
      Concentration = NA_real_,
      Flux_kg_j =0,
      Source_concentration = "Débit non nul - Flux à estimer avec A3 step"
    )
  
  # Assemblage final
  data_flux_j <- flux_bilans %>%
    bind_rows(flux_debit_nul, conc_manquantes) %>%
    arrange(Code_SANDRE, Pt_SANDRE, Identifiant_pt_AS,Nom_pt_AS, Code_sandre_parametre, Date_debut_prel)

  
  # ─────────────────────────────────────────────
  # 4.6. FLUX ANNUEL ----
  # ─────────────────────────────────────────────

  # Flux annuel par somme des flux journaliers
flux_an <- data_flux_j %>%
    group_by(
    DT, Code_SANDRE, SCL_Nom_Sitou, Classe_CBPO,
    Pt_SANDRE, Identifiant_pt_AS,Nom_pt_AS, Code_sandre_parametre, Lib_court_parametre,
    Annee
  ) %>%
  
  summarise(
    # Nombre de jours avec un flux déversé
    Nb_jours_deversements        = sum(!is.na(Flux_kg_j)),
    
    
    # Somme des flux journaliers sur l'année entière (kg/an)
    # na.rm = TRUE : les jours sans débit ou sans C_pond (NA) sont ignorés
    Flux_annuel_kg_an    =  sum(Flux_kg_j, na.rm = TRUE),
    .groups = "drop"
  )
flux_an <- flux_an %>%
  mutate(Departement=substr(Code_SANDRE,3,4)) %>%
  relocate(Departement, .after=SCL_Nom_Sitou)
 
  # ─────────────────────────────────────────────
  # 4.4. EXPORT EXCEL ----
  # ─────────────────────────────────────────────
annee_data <- unique(flux_an$Annee)

dir.create("resultat", showWarnings = FALSE)

ecrire_onglet <- function(wb, nom, data) {
  addWorksheet(wb, nom)
  writeData(wb, nom, data, headerStyle = entete)
  addStyle(wb, nom, style = autres_lignes,
           rows = 2:(nrow(data) + 1), cols = 1:ncol(data),
           gridExpand = TRUE)
  setColWidths(wb, nom, cols = 1:ncol(data), widths = "auto")
  freezePane(wb, nom, firstRow = TRUE)
}

# ── Fichier flux annuel ──────────────────────
fichier_flux_an <- paste0(
  "resultat/SCL/Flux_rejet_an_SCL_", DT, "_", annee_data, ".xlsx"
)
wb <- createWorkbook()
ecrire_onglet(wb, "Flux_annuels", flux_an)
saveWorkbook(wb, fichier_flux_an, overwrite = TRUE)
message("✔ Export terminé : ", fichier_flux_an)

# ── Fichier flux journalier par département ───
data_flux_j <- data_flux_j %>%
  mutate(Departement      = substr(Code_SANDRE, 3, 4))

liste_dept <- unique(data_flux_j$Departement)

for (dept in liste_dept) {
  data_dept <- data_flux_j %>%
    filter(Departement == dept)
  
  nom_fichier_dept <- paste0(
    "resultat/SCL/Flux_journaliers_SCL_Dep_",dept,"_",DT,"_",annee_data,".xlsx"
  )
  
  wb_dept <- createWorkbook()
  ecrire_onglet(wb_dept,"Flux_journaliers", data_dept)
  
  saveWorkbook(wb_dept, nom_fichier_dept, overwrite = TRUE)
  message("✔ Export terminé : ", nom_fichier_dept)
}
}

