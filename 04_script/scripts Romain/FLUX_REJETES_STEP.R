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
    
  ### 3.1. CHOIX DU FICHIER DE DONNEES DE LA DT ----
  # Choisir la requête sur l'entrepot de données utilisée pour PANDA
    
    if (DT == "DVO")
    {
      data_all_STEP_0 <- read_excel("data/Données_AS_STEP_DVO.xlsx", sheet = "STEP")
    }
    if (DT == "DVM")
    {
      data_all_STEP_0 <- read_excel("data/Données_AS_STEP_DVM.xlsx", sheet = "STEP")
    }
    if (DT == "DBN")
    {
      data_all_STEP_0 <- read_excel("data/Données_AS_STEP_DBN.xlsx", sheet = "STEP")
    }
    if (DT == "DSAM")
    {
      data_all_STEP_0 <- read_excel("data/Données_AS_STEP_DSAM.xlsx", sheet = "STEP")
    }
    if (DT == "DSAV")
    {
      data_all_STEP_0 <- read_excel("data/Données_AS_STEP_DSAV.xlsx", sheet = "STEP")
    }
    if (DT == "DSF")
    {
      data_all_STEP_01 <- read_excel("data/Données_AS_STEP_DSF_DRIF.xlsx", sheet = "STEP")
      data_all_STEP_02 <- read_excel("data/Données_AS_STEP_DSF_DPPC.xlsx",sheet = "STEP")
      colnames(data_all_STEP_02) <- colnames(data_all_STEP_01)
      data_all_STEP_0 <- rbind(data_all_STEP_01, data_all_STEP_02)
    }
    
    if (DT=="TOUTES")
    {
      data_all_STEP_01 <- read_excel("data/Données_AS_STEP_DVO.xlsx", sheet = "STEP")
      data_all_STEP_02 <- read_excel("data/Données_AS_STEP_DVM.xlsx", sheet = "STEP")
      data_all_STEP_03 <- read_excel("data/Données_AS_STEP_DBN.xlsx", sheet = "STEP")
      data_all_STEP_04 <- read_excel("data/Données_AS_STEP_DSAM.xlsx", sheet = "STEP")
      data_all_STEP_05 <- read_excel("data/Données_AS_STEP_DSAV.xlsx", sheet = "STEP")
      data_all_STEP_06 <- read_excel("data/Données_AS_STEP_DSF_DRIF.xlsx", sheet = "STEP")
      data_all_STEP_07 <- read_excel("data/Données_AS_STEP_DSF_DPPC.xlsx", sheet = "STEP")
      data_all_STEP_0 <- rbind(data_all_STEP_01, 
                               data_all_STEP_02,
                               data_all_STEP_03,
                               data_all_STEP_04,
                               data_all_STEP_05,
                               data_all_STEP_06,
                               data_all_STEP_07)
    }
    
    ### 3.2. Renommer les colonnes ----
    data_all_STEP_0 <-  data_all_STEP_0 %>%
      rename(
        id_analyse = `Code analyse`,
        Code_SANDRE = `Code SANDRE`,
        Station_Nom_Sitou = `Station_Nom Sitou`,
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
    ### 3.3. Convertir les colonnes d'un type à un type numeric, date ----
    data_all_STEP_0 <- data_all_STEP_0 %>%
      mutate(Step_Capacite_nominale = as.numeric(Step_Capacite_nominale)) %>%
      mutate(Date_debut_prel = as.Date(Date_debut_prel)) %>%
      mutate(Date_depot = as.Date(Date_depot))
    
    ### 3.4. Nettoyage des doublons et des caractères problématiques ----
    # TEST : s'il existe plus d'une date de dépôt, conserver uniquement la donnée la plus récente.
    
    # récupérer les lignes doublons pour vérification si besoin
    tmp <- get_dupes(
      data_all_STEP_0,
      DT,
      Code_SANDRE,
      Pt_SANDRE,
      Code_sandre_parametre,
      PP_No_interne_Sitou,
      Date_debut_prel
    )
    
    # trier les données pour supprimer les doublons après.
    data_all_STEP_1 <- data_all_STEP_0 %>%
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
    data_all_STEP <- data_all_STEP_1 %>%
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
    data_all_STEP$Station_Nom_Sitou <- gsub("/", "-", data_all_STEP$Station_Nom_Sitou)
    
    
    ### 3.5. Remplacement des codes erronnés----
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
    
    ### 3.6. Liste des paramètres et des couples paramètres/unités ----
    Liste_parametres <- distinct(
      data.frame(
        data_all_STEP$Code_sandre_parametre,
        data_all_STEP$Lib_court_parametre
      )
    )
    colnames(Liste_parametres) <- c("Code_sandre_parametre", "Lib_court_parametre")
    
    
    Liste_parametres_unites <- distinct(
      data.frame(
        data_all_STEP$Code_sandre_parametre,
        data_all_STEP$Lib_court_parametre,
        data_all_STEP$Code_unite,
        data_all_STEP$Symbole_unite
      )
    )
    colnames(Liste_parametres_unites) <- c("Code_sandre_parametre",
                                           "Lib_court_parametre",
                                           "Code_unite",
                                           "Symbole_unite")
    
    
    ## 3.7. CREATION DE LA LISTE DES STEP----
    Liste_STEP_nom <- distinct(
      data.frame(
        data_all_STEP$Station_Nom_Sitou,
        data_all_STEP$Code_SANDRE,
        data_all_STEP$Step_Capacite_nominale
      )
    )  #Voir si on conserve
    colnames(Liste_STEP_nom) = c("Nom", "Code_SANDRE", "Capa")
    
    ## 3.8. MISE EN FORME SORTIE EXCEL ----
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
    "1350",                 "PT",                  "PT (kg/j)",
    "1433",                 "PO4",                 "PO4 (kg/j)"
  )
  
  code_debit <- "1552"  # Vol.Moy.J. en m3/j
  pts_rejet  <- c("A2", "A4", "A5")
  
  # ─────────────────────────────────────────────
  # 4.2. PREPARATION DES DONNEES ----
  # ─────────────────────────────────────────────
  
  data_rejet <- data_all_STEP %>%
    filter(Pt_SANDRE %in% pts_rejet)
  
  # Débits journaliers (tous les jours disponibles, pas seulement les jours de bilan)
  data_debit <- data_rejet %>%
    filter(Code_sandre_parametre == code_debit) %>%
    select(
      DT, Code_SANDRE, Station_Nom_Sitou, Step_Capacite_nominale,
      Pt_SANDRE, Date_debut_prel,
      Val_resultat_analyse, Code_remarque, Derniere_qualif_analyse
    ) %>%
    rename(Debit_m3j = Val_resultat_analyse) %>%
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
      DT, Code_SANDRE, Station_Nom_Sitou, Step_Capacite_nominale,
      Pt_SANDRE, Date_debut_prel,
      Code_sandre_parametre, Lib_court_parametre, Symbole_unite,
      Val_resultat_analyse, Code_remarque, Derniere_qualif_analyse
    ) %>%
    rename(Concentration = Val_resultat_analyse)
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
  grille_jours <- data_debit %>%
    filter(!is.na(Debit_m3j)) %>%
    select(DT, Code_SANDRE, Station_Nom_Sitou, Step_Capacite_nominale,
           Pt_SANDRE, Date_debut_prel, Debit_m3j) %>%
    crossing(params_flux %>% select(Code_sandre_parametre, Lib_court_parametre))
  
  # Flux sur les jours de bilan = concentrations x volume
  flux_bilans <- grille_jours %>%
    inner_join(
      data_conc %>%
        filter(!is.na(Concentration)) %>%
        select(Code_SANDRE, Pt_SANDRE, Code_sandre_parametre,
               Date_debut_prel, Concentration, Symbole_unite,
               Code_remarque, Derniere_qualif_analyse),
      by = c("Code_SANDRE", "Pt_SANDRE", "Code_sandre_parametre", "Date_debut_prel")
    ) %>%
    mutate(
      Annee     = year(Date_debut_prel),
      Mois      = month(Date_debut_prel),
      Flux_kg_j = Concentration * Debit_m3j * 1e-3,
      Source_concentration = "Flux jour Bilan 24h"
    )
  
  # flux hors jours de bilans = (flux moyen annuel jours de bilans / débit moyen annuel sur les jours de bilan)
  # * volume
  # = concentration moyenne pondérée par le débit, à l'échelle annuelle
  ratio_decapol <- flux_bilans %>%
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
  
  # Jours sans bilan : appliquer le ratio Decapol × débit du jour
  flux_repli <- grille_jours %>%
    # Isoler les jours sans mesure de concentration
    anti_join(
      data_conc %>% filter(!is.na(Concentration)),
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
  
  # Assemblage final
  data_flux_j <- flux_bilans %>%
    bind_rows(flux_repli) %>%
    arrange(Code_SANDRE, Pt_SANDRE, Code_sandre_parametre, Date_debut_prel)
}
  
  # ─────────────────────────────────────────────
  # 4.4. FLUX ANNUEL ----
  # ─────────────────────────────────────────────

  # Flux annuel par somme des flux journaliers
flux_an <- data_flux_j %>%
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
flux_an <- flux_an %>%
  mutate(Departement=substr(Code_SANDRE,3,4)) %>%
  relocate(Departement, .after=Station_Nom_Sitou)
 
# ─────────────────────────────────────────────
# 4.4.bis STATIONS SANS BILAN ----
# ─────────────────────────────────────────────

Liste_toutes_STEP_0 <- read_excel("data/SITOUREF_STEP_bassin.xlsx", 
                                sheet = "Liste des sitous (autre format)")

Liste_toutes_STEP <- Liste_toutes_STEP_0 %>%
  filter(IDTYPE_SITUATION != "CL")%>%
  select(Code_SANDRE=VALEUR_FCT_PRINCIPAL, NOM, 
         MO_ROSEAU=`MOA (Roseau)`,
         Situation=IDTYPE_SITUATION, 
         capacite=`29.CAPACIT - Capacité nominale`) 

# garder les step non présentes dans flux_an où il y a des données
stations_sans_bilans <- Liste_toutes_STEP %>%
  anti_join(flux_an, by = c("Code_SANDRE"))
  # ─────────────────────────────────────────────
  # 4.5. EXPORT EXCEL ----
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
  "resultat/STEP/Flux_rejet_an_STEP_", DT, "_", annee_data, ".xlsx"
)
wb <- createWorkbook()
ecrire_onglet(wb, "Flux_annuels", flux_an)
ecrire_onglet(wb, "Stations_sans_bilans", stations_sans_bilans)
saveWorkbook(wb, fichier_flux_an, overwrite = TRUE)
message("✔ Export terminé : ", fichier_flux_an)

# ── Fichier flux journalier par département ───
data_flux_j <- data_flux_j %>%
  mutate(Departement = substr(Code_SANDRE, 3,4))

liste_dept <- unique(data_flux_j$Departement)

for (dept in liste_dept) {
  data_dept <- data_flux_j %>%
    filter(Departement == dept)
  
  nom_fichier_dept <- paste0(
    "resultat/STEP/Flux_journaliers_STEP_Dep_",dept,"_",DT,"_",annee_data,".xlsx"
  )
  
  wb_dept <- createWorkbook()
  ecrire_onglet(wb_dept,"Flux_journaliers", data_dept)
  
  saveWorkbook(wb_dept, nom_fichier_dept, overwrite = TRUE)
  message("✔ Export terminé : ", nom_fichier_dept)
}

