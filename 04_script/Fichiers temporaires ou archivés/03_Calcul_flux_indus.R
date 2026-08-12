



tab_param_unites<-read_excel("02_data/BASE/Table_Param_Unités.xlsx", 
                             col_types = c("text", "text", "text", 
                                           "text", "text", "text", "text", "text", 
                                           "numeric", "text", "text"))

tab_param_unites<-tab_param_unites |> 
  rename("Code_sandre_parametre" =`Code sandre paramètre`,
         "Lib_parametre" = `Lib court paramètre`,
         "Code_unite" = `Code unité`,
         "Lib_unite" = `Lib unité`,
         "Param_ref" = `Paramètre référence`, # Ce paramètre est-il le paramètre de référence ?
         "Forcer_param" = `Forcer paramètre`, # Si non, quelle unité doit être utilisée ? 
         "Unite_ref" = `Unité référence ?`, # Cette unité est-elle celle de référence ?
         "Facteur_conv" = `Facteur conversion vers unité référence`, # Si non, utiliser ce facteur de conversion
         "Forcer_unite" = `Forcer unité`, # Changement d'unité
         "Lib_param_unite" = `Libellé court Param+unité`) # Pour l'affichage sur les fichiers finaux 


# Ecriture d'une fonction pour standardiser les unités
# Arg 1 = data : le fichier contenant les données
# Arg 2 = col_Code_sandre_parametre : le nom de la colonne contenant le code sandre du paramètre


standardisation_param_unites<-function(data){

  
    liste_param_unit_data <- distinct(as.data.frame(data$Code_sandre_parametre,data$Code_unite))
  
  
}


standardisation_param_unites(data_SRR_brut)


data_SRR_brut <- read_excel("02_data/ARAMIS/BET-023-2026_003_Analyses_SRR.xlsx", 
                                            col_types = c("text", "text", "date", 
                                                          "text", "text", "numeric"))


data_SRR_brut<-data_SRR_brut |> 
  rename("site" = `N° Site SRR-PE`,
         "PP_No_interne_Sitou" = `N° PME`,
         "Date_debut_prel" = `Date de mesure`,
         "Code_sandre_parametre" = `Code Sandre`,
         "Code_unite" = `Unité Sandre`,
         "Val_resultat_analyse" = `Valeur mesurée`)


## TODO: Intégrer le tableau des paramètres Unités (dossier BASE à ajouter et numéroter)


# ─────────────────────────────────────────────
# 4.2. PREPARATION DES DONNEES ----
# ─────────────────────────────────────────────


# Débits journaliers (tous les jours disponibles, pas seulement les jours de bilan)
data_debit_SRR <- data_SRR_brut %>%
  filter(Code_sandre_parametre == "1552") %>%
  rename(Debit_m3j = Val_resultat_analyse) %>%
  mutate(
    Annee       = year(Date_debut_prel),
    Mois        = month(Date_debut_prel),
    Nb_jours_mois = days_in_month(Date_debut_prel)
  )

# Concentrations — uniquement les jours où concentration ET débit sont présents
# (= jours de bilan 24h)
data_conc_SRR <- data_SRR_brut %>%
  filter(Code_sandre_parametre !="1552") %>%
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
grille_jours <- data_debit_SRR %>%
  filter(!is.na(Debit_m3j)) %>%
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
