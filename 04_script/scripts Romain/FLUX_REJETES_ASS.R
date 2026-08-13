
# ─────────────────────────────────────────────
# 0. BLOC EN-TÊTE ----
# ─────────────────────────────────────────────


# ─────────────────────────────────────────────
# 1. PARAMETRAGE ----
# ─────────────────────────────────────────────


## 1.1. DIRECTION TERRITORIALE ----
# Indiquer le nom de votre DT à droite de la flèche, en MAJUSCULES, entre guillemets
# Choix : DVO / DVM / DSF / DSAM / DSAV / DBN
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
      ### 3.4.1. Import fichier de la DT ----
      # Choisir la requête sur l'entrepot de données utilisée pour PANDA
    
        if (DT %in% c("DVO", "DVM", "DBN", "DSAM", "DSAV")) {
          data_all_STEP_0 <- read_excel(paste0("data/Données_AS_STEP_", DT, ".xlsx"), sheet = "STEP")
          data_all_SCL_0  <- read_excel(paste0("data/Données_AS_SCL_", DT, ".xlsx"), sheet = "Réseaux")
          
        } else if (DT == "DSF") {
          data_all_STEP_01 <- read_excel("data/Données_AS_STEP_DSF_DRIF.xlsx", sheet = "STEP")
          data_all_STEP_02 <- read_excel("data/Données_AS_STEP_DSF_DPPC.xlsx", sheet = "STEP")
          colnames(data_all_STEP_02) <- colnames(data_all_STEP_01)
          data_all_STEP_0 <- rbind(data_all_STEP_01, data_all_STEP_02)
          
          data_all_SCL_01 <- read_excel("data/Données_AS_SCL_DSF_DRIF.xlsx", sheet = "Réseaux")
          data_all_SCL_02 <- read_excel("data/Données_AS_SCL_DSF_DPPC.xlsx", sheet = "Réseaux")
          colnames(data_all_SCL_02) <- colnames(data_all_SCL_01)
          data_all_SCL_0 <- rbind(data_all_SCL_01, data_all_SCL_02)
        }
    
      
      # Boucle pour nommer les variables en tant que STEP puis SCL pour simplifier le code
    
        ouvrages <- c("STEP","SCL")
        
        for(type in ouvrages) 
          {
          
      ### 3.4.2. Renommer les colonnes ----  
          
          assign(
            paste0("data_all_", type,"_0"), 
            get(paste0("data_all_",type,"_0")) %>%
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
                ))
        
      ### 3.4.3. Convertir les colonnes d'un type à un type numeric, date ----
          assign(
            paste0("data_all_", type,"_0"), 
            get(paste0("data_all_",type,"_0")) %>%
              mutate(Date_debut_prel = as.Date(Date_debut_prel)) %>%
              mutate(Date_depot = as.Date(Date_depot))
              ) 

      ### 3.4.4. Nettoyage des doublons et des caractères problématiques ----
      # TEST : s'il existe plus d'une date de dépôt, conserver uniquement la donnée la plus récente
          
      # récupérer les lignes doublons pour vérification si besoin
          
          assign(
            paste0("tmp_", type), 
            get_dupes(
                get(paste0("data_all_",type,"_0")),
                DT,
                Code_SANDRE,
                Pt_SANDRE,
                Code_sandre_parametre,
                PP_No_interne_Sitou,
                Date_debut_prel,
                Date_depot  ))
    
      # trier les données pour supprimer les doublons après.
      
          assign(
            paste0("data_all_", type,"_1"), 
            get(paste0("data_all_",type,"_0")) %>%
              arrange(
                DT,
                Code_SANDRE,
                Pt_SANDRE,
                Code_sandre_parametre,
                PP_No_interne_Sitou,
                Date_debut_prel,
                desc(Date_depot)  ))
        
      
      # supprimer les doublons.
        
          assign(
            paste0("data_all_", type), 
            get(paste0("data_all_",type,"_1")) %>%
              distinct(
                DT,
                Code_SANDRE,
                Pt_SANDRE,
                Code_sandre_parametre,
                PP_No_interne_Sitou,
                Date_debut_prel,
                .keep_all = TRUE
                ))
      
    
    ### 3.4.5. Remplacement des codes erronnés----
    #Suppression des lignes correspondant au code analyse 0 (analyse non faite)
    ##### ***** SUR SCRIPT ORIGINAL N'EXISTAIT QUE POUR SCL, ICI APPLIQUE AUSSI A STEP ******
        assign(
          paste0("data_all_", type), 
          get(paste0("data_all_",type)) %>%
            filter(Code_remarque !=0)
          )

    
    #### Remplacement du code 1098 (Volume) par 1552 (Vol.Moy.J) pour les points Sandre A2,A3, A4, A5, A7
   
        assign(
          paste0("data_all_", type), 
          get(paste0("data_all_",type)) %>%
            mutate(
              Code_sandre_parametre = case_when(
                Code_sandre_parametre == "1098" &
                  Pt_SANDRE %in% c("A1", "R1", "A2", "A3", "A4", "A5","A7") ~ "1552",
                TRUE ~ Code_sandre_parametre
              ),
              Code_unite = case_when(
                Code_sandre_parametre == "1552" ~ "120",
                TRUE ~ Code_unite)
            )
        )

    
    ### 3.4.6. Liste des paramètres et des couples paramètres/unités ----
        assign(
          paste0("ListeParam_", type), 
          distinct(
            data.frame(
              Code_sandre_parametre = get(paste0("data_all_",type))$Code_sandre_parametre
          ))) 
        
        assign(
          paste0("ListeParamUnites_", type), 
          distinct(
            data.frame(
              Code_sandre_parametre = get(paste0("data_all_",type))$Code_sandre_parametre,
              Code_unite = get(paste0("data_all_",type))$Code_unite
          ))) 
        
}
    
    ### 3.4.7. CREATION DE LA LISTE DES STEP----
    Liste_STEP_nom <- distinct(
      data.frame(
        data_all_STEP$Station_Nom_Sitou,
        data_all_STEP$Code_SANDRE,
        data_all_STEP$Step_Capacite_nominale
      )
    )  #Voir si on conserve
    colnames(Liste_STEP_nom) = c("Nom", "Code_SANDRE", "Capa")
    
    Liste_SCL_nom <- distinct(data.frame(data_all_SCL$SCL_Nom_Sitou,data_all_SCL$Code_SANDRE))  
    
    #Voir si on conserve
    colnames(Liste_SCL_nom)<-c("Nom_SCL","Code_SANDRE")
    
    ### 3.4.8. MISE EN FORME SORTIE EXCEL ----
    # En-tête
    entete <- createStyle(
      fontSize = 12,            # Taille de la police
      textDecoration = "bold",  # Texte en gras
      halign = "center",        # Alignement horizontal centré
      valign = "center",        # Alignement vertical centré
      fgFill = "steelblue1",
      wrapText = TRUE
    )
    
    autres_lignes <- createStyle(
      fontSize = 12,     # Taille de la police
      halign = "left",   # Alignement horizontal à gauche
      valign = "center", # Alignement vertical centré
      wrapText = TRUE
    )
    
    # Bordure inférieure fine
    bordure_sep <- createStyle(border = "bottom", borderColour = "black")
    
    # Bordure inférieure épaisse
    bordure_thick <- createStyle(border = "bottom", borderColour = "black", borderStyle = "thick")
    
    # Surlignement rouge
    surlign_rouge <- createStyle(
      textDecoration = "bold",     # Texte en gras
      fgFill = "red")
    
    # Surlignement rose
    surlign_rose <- createStyle(
      textDecoration = "bold",     # Texte en gras
      fgFill = "lightpink")
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
    "1350",                 "PT",                  "PT (kg/j)"
  )
  
  code_debit <- "1552"  # Vol.Moy.J. en m3/j
  pts_rejet  <- c("A1", "R1","A2", "A4", "A5")
  
  # ─────────────────────────────────────────────
  # 4.2. PREPARATION DES DONNEES ----
  # ─────────────────────────────────────────────
  
  data_rejet_STEP <- data_all_STEP %>%
    filter(Pt_SANDRE %in% pts_rejet) %>%
    rename(Commentaire_analyse=`Commentaire analyse`)
  
  data_rejet_SCL <- data_all_SCL %>%
    filter(Pt_SANDRE %in% pts_rejet)
  
  # Débits journaliers (tous les jours disponibles, pas seulement les jours de bilan)
  data_debit_STEP <- data_rejet_STEP %>%
    filter(Code_sandre_parametre == code_debit) %>%
    select(
      DT, Code_SANDRE, Station_Nom_Sitou, Step_Capacite_nominale,											  
      Pt_SANDRE, Date_debut_prel,
      Val_resultat_analyse, Code_remarque, Derniere_qualif_analyse, Commentaire_analyse
    ) %>%
    rename(Debit_m3j = Val_resultat_analyse) %>%
    mutate(
      Annee       = year(Date_debut_prel),
      Mois        = month(Date_debut_prel),
      Nb_jours_mois = days_in_month(Date_debut_prel)
    )
  
  data_debit_SCL <- data_rejet_SCL %>%
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
  # (= jours de bilan 24h / jours de déversements)
  data_conc_STEP <- data_rejet_STEP %>%
    filter(Code_sandre_parametre %in% params_flux$Code_sandre_parametre) %>%
    select(
      DT, Code_SANDRE, Station_Nom_Sitou, Step_Capacite_nominale,
      Pt_SANDRE, Date_debut_prel,
      Code_sandre_parametre, Lib_court_parametre, Symbole_unite,
      Val_resultat_analyse, Code_remarque, Derniere_qualif_analyse,
	    Commentaire_analyse
    ) %>%
    rename(Concentration = Val_resultat_analyse)
  
  data_conc_SCL <- data_rejet_SCL %>%
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
  # Jours SANS mesure de concentration :
  #   Flux_j = (Flux_moy_bilans / Debit_moy_bilans) × Debit_j × 1e-3
  #          = C_pond_bilans × Debit_j × 1e-3
  #
  # La C_pond est calculée sur TOUS les jours de bilan de l'année
  # (flux moyen annuel / débit moyen annuel sur jours de mesure)
  # ─────────────────────────────────────────────
  
  # Grille complète : tous les jours avec débit × tous les paramètres
  grille_jours_STEP <- data_debit_STEP %>%
    filter(!is.na(Debit_m3j)) %>%
    select(DT, Code_SANDRE, Station_Nom_Sitou, Step_Capacite_nominale,
           Pt_SANDRE, Date_debut_prel, Debit_m3j,
           Derniere_qualif_analyse,
           Commentaire_analyse) %>%
    crossing(params_flux %>% select(Code_sandre_parametre, Lib_court_parametre))
  
  grille_jours_SCL <- data_debit_SCL %>%
    filter(!is.na(Debit_m3j)) %>%
    select(DT, Code_SANDRE, SCL_Nom_Sitou, Classe_CBPO,
           Pt_SANDRE, Identifiant_pt_AS,Nom_pt_AS,
           Date_debut_prel, Debit_m3j, Derniere_qualif_analyse,
           Commentaire_analyse) %>%
    crossing(params_flux %>% select(Code_sandre_parametre, Lib_court_parametre))
  
  # Flux sur les jours de bilan = concentrations x volume
  flux_STEP <- grille_jours_STEP %>%
    inner_join(
      data_conc_STEP %>%
        filter(!is.na(Concentration)) %>%
        select(Code_SANDRE, Pt_SANDRE, Code_sandre_parametre,
               Date_debut_prel, Concentration, Symbole_unite),
      by = c("Code_SANDRE", "Pt_SANDRE", "Code_sandre_parametre", "Date_debut_prel")
    ) %>%
    mutate(
      Annee     = year(Date_debut_prel),
      Mois      = month(Date_debut_prel),
      Flux_kg_j = Concentration * Debit_m3j * 1e-3,
      Source_concentration = "Flux jour Bilan 24h"
    )
  
  flux_SCL <- grille_jours_SCL %>%
    inner_join(
      data_conc_SCL %>%
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
  
  # flux hors jours de bilans = (flux moyen annuel jours de bilans / débit moyen annuel sur les jours de bilan)
  # * volume
  # = concentration moyenne pondérée par le débit, à l'échelle annuelle
  ratio_decapol <- flux_STEP %>%
    filter(Pt_SANDRE=="A4")%>%
    group_by(
      Code_SANDRE, Pt_SANDRE, Code_sandre_parametre,
      Symbole_unite, Lib_court_parametre, Annee
    ) %>%
    summarise(
      Flux_moy_bilans   = mean(Flux_kg_j,   na.rm = TRUE),  # kg/j moyen
      Debit_moy_bilans  = mean(Debit_m3j,   na.rm = TRUE),  # m3/j moyen
      C_pond    = Flux_moy_bilans / Debit_moy_bilans * 1e3, # mg/L
      Nb_bilans_annuel  = n(),
      .groups = "drop"
    )
  
  # Jours sans déversements et à débit nul = flux réel nul
  flux_debit_nul <- grille_jours_SCL %>%
    anti_join(
      data_conc_SCL %>% filter(!is.na(Concentration)),
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
  
  # Jours sans bilan : appliquer le ratio Decapol × débit du jour
  flux_repli_A4 <- grille_jours_STEP %>%
    # Isoler les jours sans mesure de concentration
    anti_join(
      data_conc_STEP %>% filter(!is.na(Concentration)),
      by = c("Code_SANDRE", "Pt_SANDRE", "Code_sandre_parametre", "Date_debut_prel")
    ) %>%
    # Extraire l'année et le mois nécessaires au calcul à l'échelle annuelle
    mutate(
      Annee = year(Date_debut_prel),
      Mois  = month(Date_debut_prel)
    ) %>%
    # Pour chaque combinaison (STEP, point AS, paramètre, année),
    # on récupère les 3 grandeurs calculées dans ratio_decapol :
    #   • Flux_moy_bilans :   flux moyen journalier sur les jours de bilan (kg/j)
    #   • Debit_moy_bilans :  débit moyen les jours de bilans (m3/j)
    #   • C_pond :            concentration pondérée = Flux_moyen / Debit_moy * 1000 (mg/L)
    
    # La jointure sur Annee garantir qu'on utilise les bilans de la même année civile
    # pour imputer les jours manquants (pas de mélange inter-annuel)
    left_join(
      ratio_decapol %>% select(
        Code_SANDRE, Pt_SANDRE, Code_sandre_parametre,
        Symbole_unite, Annee,
        Flux_moy_bilans, Debit_moy_bilans, C_pond
      ),
      by = c("Code_SANDRE", "Pt_SANDRE", "Code_sandre_parametre", "Annee")
    ) %>%
    
  # Imputation et calcul du flux journalier estimé
    mutate(
      Concentration        = C_pond,
      Source_concentration = case_when(
        !is.na(C_pond) & Debit_m3j > 0 ~"Flux estimé reconstitué",
        TRUE ~NA_character_
      ), #exclu la notification flux estimé lorsque données nulles
      # permet de retracer flux bilans, des flux estimés
      Derniere_qualif_analyse = NA_character_,
      Code_remarque        = NA_character_,
      # Calcul flux journalier estimé
      Flux_kg_j = case_when(
        !is.na(C_pond) & Debit_m3j > 0 #exclu les années sans bilans
        ~ C_pond * Debit_m3j * 1e-3,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-Flux_moy_bilans, -Debit_moy_bilans, -C_pond)
  
  # Jours sans déversements mais à débit non nul = flux à estimer avec A3
  flux_repli_SCL <- grille_jours_SCL %>%
    anti_join(
      data_conc_SCL %>% filter(is.na(Concentration)),
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
  data_flux_j_STEP <- flux_STEP %>%
    bind_rows(flux_repli_A4) %>%
    arrange(Code_SANDRE, Pt_SANDRE, Code_sandre_parametre, Date_debut_prel)
  
  data_flux_j_SCL <- flux_SCL %>%
    bind_rows(flux_debit_nul, flux_repli_SCL) %>%
    arrange(Code_SANDRE, Pt_SANDRE, Identifiant_pt_AS,Nom_pt_AS, Code_sandre_parametre, Date_debut_prel)
  
  
  # ─────────────────────────────────────────────
  # 4.4. FLUX ANNUEL ----
  # ─────────────────────────────────────────────

  # Flux annuel par somme des flux journaliers STEP
flux_an_STEP <- data_flux_j_STEP %>%
    group_by(
    DT, Code_SANDRE, Station_Nom_Sitou, Step_Capacite_nominale,
    Pt_SANDRE, Code_sandre_parametre, Lib_court_parametre,
    Annee
  ) %>%
  
  summarise(
    # Nombre de jours avec un flux calculé (bilans 24h + flux reconstitué)
    Nb_jours_flux        = sum(!is.na(Flux_kg_j)),
    
    # Nombre de vrais prélèvements dans l'année (hors repli)
    Nb_bilans_reels      = sum(Source_concentration == "Flux jour Bilan 24h",
                               na.rm = TRUE),
    
    # Somme des flux journaliers sur l'année entière (kg/an)
    # na.rm = TRUE : les jours sans débit ou sans C_pond (NA) sont ignorés
    Flux_annuel_kg_an    =  sum(Flux_kg_j, na.rm = TRUE),
    .groups = "drop"
  )
flux_an_STEP <- flux_an_STEP %>%
  mutate(Departement=substr(Code_SANDRE,3,4)) %>%
  relocate(Departement, .after=Station_Nom_Sitou)

    # Flux annuel par somme des flux journaliers SCL

flux_an_SCL <- data_flux_j_SCL %>%
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
flux_an_SCL <- flux_an_SCL %>%
  mutate(Departement=substr(Code_SANDRE,3,4)) %>%
  relocate(Departement, .after=SCL_Nom_Sitou)
 
# ─────────────────────────────────────────────
# 4.4.bis STATIONS SANS BILAN ----
# ─────────────────────────────────────────────
Liste_toutes_STEP_0 <- read_excel("data/SITOUREF_STEP_bassin.xlsx", 
                                  sheet = "Liste des sitous (autre format)")


if(DT == "DBN")
  {gestionnaire_DT = "Pollution Bocages Normands"}
if(DT == "DSAV")
  {gestionnaire_DT = "Pollution Seine Aval"}
if(DT == "DVO")
  {gestionnaire_DT = "Pollution Vallées d'Oise"}
if(DT == "DVM")
  {gestionnaire_DT = "Pollution Vallées de Marne"}
if(DT == "DSAM")
  {gestionnaire_DT = "Pollution Seine Amont"}
if(DT == "DSF")
  {gestionnaire_DT = 
    c("Pollution Ile de France",
      "Pollution Petite Couronne")
}

Liste_toutes_STEP <- Liste_toutes_STEP_0 %>%
  filter(
    IDTYPE_SITUATION != "CL",
    `UNITE GESTIONNAIRE`== gestionnaire_DT
         )%>%
  select(Code_SANDRE=VALEUR_FCT_PRINCIPAL, NOM, 
         MO_ROSEAU=`MOA (Roseau)`,
         Situation=IDTYPE_SITUATION, 
         capacite=`29.CAPACIT - Capacité nominale`) 


# garder les step non présentes dans flux_an où il y a des données
stations_sans_bilans <- Liste_toutes_STEP %>%
  anti_join(flux_an_STEP, by = c("Code_SANDRE"))

  # ─────────────────────────────────────────────
  # 4.5. EXPORT EXCEL ----
  # ─────────────────────────────────────────────
annee_data <- unique(flux_an_STEP$Annee)

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
fichier_flux_an_STEP <- paste0(
  "resultat/STEP/Flux_rejet_an_STEP_", DT, "_", annee_data, ".xlsx"
)
wb <- createWorkbook()
ecrire_onglet(wb, "Flux_annuels", flux_an_STEP)
ecrire_onglet(wb, "Stations_sans_bilans", stations_sans_bilans)
saveWorkbook(wb, fichier_flux_an_STEP, overwrite = TRUE)
message("✔ Export terminé : ", fichier_flux_an_STEP)

fichier_flux_an_SCL <- paste0(
  "resultat/SCL/Flux_rejet_an_SCL_", DT, "_", annee_data, ".xlsx"
)
wb <- createWorkbook()
ecrire_onglet(wb, "Flux_annuels", flux_an_SCL)
saveWorkbook(wb, fichier_flux_an_SCL, overwrite = TRUE)
message("✔ Export terminé : ", fichier_flux_an_SCL)

# ── Fichier flux journalier par département ───

# STEP
data_flux_j_STEP <- data_flux_j_STEP %>%
  mutate(Departement = substr(Code_SANDRE, 3,4))

liste_dept_STEP <- unique(data_flux_j_STEP$Departement)

for (dept in liste_dept_STEP) {
  data_dept_STEP <- data_flux_j_STEP %>%
    filter(Departement == dept)
  
  nom_fichier_dept_STEP <- paste0(
    "resultat/STEP/Flux_journaliers_STEP_Dep_",dept,"_",DT,"_",annee_data,".xlsx"
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
    filter(Departement == dept)
  
  nom_fichier_dept_SCL <- paste0(
    "resultat/SCL/Flux_journaliers_SCL_Dep_",dept,"_",DT,"_",annee_data,".xlsx"
  )
  
  wb_dept <- createWorkbook()
  ecrire_onglet(wb_dept,"Flux_journaliers", data_dept_SCL)
  
  saveWorkbook(wb_dept, nom_fichier_dept_SCL, overwrite = TRUE)
  message("✔ Export terminé : ", nom_fichier_dept_SCL)
}
}

