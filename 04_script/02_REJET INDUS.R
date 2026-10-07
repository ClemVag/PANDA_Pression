# ------------------------------------------- -
# |     REJETS INDUSTRIELS SRR & FORFAIT      |
# ------------------------------------------- -

system.time(
{
  #0  PACKAGES ET FONCTIONS ----
  ## 0.1. PACKAGES ----
  {
    library(tidyverse)
    library(dplyr)
    library(janitor)
    library(DiagrammeR)
    library(readxl)
    library(writexl)
    library(rsvg)
    
    library(openxlsx)
    library(stringr)
    library(rmarkdown)
    library(quarto)
    library(here)
    library(conflicted)
    conflict_prefer("filter", "dplyr")
    conflict_prefer("write.xlsx", "openxlsx")
  }
  
  ## 0.2. FONCTIONS ----
  `%nin%` <- negate(`%in%`)  
  
  
  ecrire_onglet <- function(wb, nom, data) {
    addWorksheet(wb, nom)
    writeData(wb, nom, data, headerStyle = entete)
    addStyle(wb, nom, style = autres_lignes,
             rows = 2:(nrow(data) + 1), cols = 1:ncol(data),
             gridExpand = TRUE)
    setColWidths(wb, nom, cols = 1:ncol(data), widths = "auto")
    freezePane(wb, nom, firstRow = TRUE)
  }
  
  ## 0.3. PARAMETRAGE ----
  # Mise en forme fichier sortie
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
  
  
  # 1. IMPORT DONNEES BRUTES ----
  ## 1.1. REFERENTIELS SANDRE ----
  parametres_sandre <-read_excel("02_data/BASE/parametres.xlsx")
  parametres_sandre<-parametres_sandre |> 
    select(code_sandre_parametre,lib_parametre)
  
  unites_sandre <-read_excel("02_data/BASE/unites.xlsx")
  unites_sandre<-unites_sandre |> 
    select(code_sandre_unite,lib_unite,symbole_unite)
  
  tab_param_unites<-read_excel("02_data/BASE/Table_Param_Unites.xlsx")
  tab_param_unites<-tab_param_unites |> 
    rename("code_sandre_parametre" = `Code sandre paramètre`,
           "lib_parametre" = `Lib court paramètre`,
           "code_sandre_unite" = `Code unité`, 
           "facteur_conv_flux" = `Facteur conversion flux`,
           "unite_flux" = `Libellé flux`) |> 
    select(code_sandre_parametre, lib_parametre, code_sandre_unite,facteur_conv_flux,unite_flux)
  
  
  
  
  ## 1.2. DONNEES SRR ----
  file <- "02_data/ARAMIS/BET-023-2026_003_Analyses_SRR.xlsx"
  sheets <- excel_sheets(file)
  
  for (i in 1:length(sheets))
  {
    onglet<-read_excel(file, 
                       sheet = sheets[i],
                       col_types = c("text", "text", "date", 
                                     "text", "text", "numeric"))
    if(i==1)
    {data_SRR_00<-onglet}
    else
    { colnames(onglet)<-colnames(data_SRR_00)
      data_SRR_00 <-bind_rows(data_SRR_00,onglet)}
    
  }
  
  # data_SRR_00 <- read_excel("02_data/ARAMIS/BET-023-2026_003_Analyses_SRR.xlsx", 
  #                           col_types = c("text", "text", "date", 
  #                                         "text", "text", "numeric"))
  data_SRR_01<-data_SRR_00 |> 
    rename("site_origine" = `N° Site SRR-PE`,
           "pt_AS" =`N° PME`,
           "date" =`Date de mesure`,
           "code_sandre_parametre"=`Code Sandre`,
           "code_sandre_unite"=`Unité Sandre`,
           "conc"=`Valeur mesurée`) |> 
    mutate(mois = month(date),
           annee = year(date)) |> 
    # On supprime les données de l'année en cours pour s'assurer d'avoir des données complètes
    filter(annee!=year(today())) |> 
    # On conserve uniquement les mesures reliées à des couples paramètres-unités qui nous intéressent
    filter(code_sandre_parametre %in% tab_param_unites$code_sandre_parametre & 
             code_sandre_unite %in% tab_param_unites$code_sandre_unite)
  
  ## 1.3. CALENDRIER ----
  calendrier<-crossing(as.data.frame(c(1:12)), as.data.frame(min(data_SRR_01$annee):max(data_SRR_01$annee)))
  colnames(calendrier) = c("mois","annee")
  
  calendrier_j<-as.data.frame(as_date(c(as_date(paste0(min(data_SRR_01$annee),"-01-01")):as_date(paste0(max(data_SRR_01$annee),"-12-31")))))
  colnames(calendrier_j)=c("date")
  calendrier_j<-calendrier_j |> 
    mutate(mois = month(date),
           annee = year(date))
  
  
  ## 1.4. LISTE POINTS DE MESURE SRR ----
  liste_points_mesure <-data_SRR_01 |> 
    select(site_origine,pt_AS) |> 
    distinct() |> 
    slice_head(n=20) # Commande à supprimer pour le script final 
  
  
  ## 1.5. FLUX FORFAITAIRES MENSUELS ----
  file <- "02_data/ARAMIS/BET-023-2026_004_Analyses_Forfait.xlsx"
  sheets <- excel_sheets(file)
  
  for (i in 1:(length(sheets)-1))
  {
    onglet<-read_excel(file, 
                       sheet = sheets[i])
    if(i==1)
    {flux_forfait<-onglet}
    else
    {colnames(onglet)<-colnames(flux_forfait)
    flux_forfait <-bind_rows(flux_forfait,onglet)}
    
  }
  flux_forfait<-flux_forfait |> 
  filter(Année!=year(today())) |> 
  mutate(`Code SANDRE paramètre` = ifelse(Paramètre == "SDE", "7818",`Code SANDRE paramètre`))
  #' Note : dans le fichier d'extraction Flux forfait, le code Sandre 1031 (chaleur)
  #' a été retiré. 
  #' Le code Sandre des SDE (7818) est ajouté, il apparaissait à 0 sur l'extraction d'origine.
  
  CGM<-read_excel(file, sheet = sheets[length(sheets)])
  CGM<-CGM |> 
    mutate(`Année d'application` = as.numeric(`Année d'application`),
            `Année d'application` = ifelse(`Année d'application` > year(today()), NA,`Année d'application`))
  
  
  # 2. CALCUL FLUX SRR ----
  ## LOOP ----
  # Intégrer ici le filtre géographique
  for (i in 1:nrow(liste_points_mesure))
  {
    data_SRR_PM_00 <- data_SRR_01 |> 
      filter(pt_AS ==liste_points_mesure$pt_AS[i])
    
    annee_min = min(data_SRR_PM_00$annee)
    annee_max = max(data_SRR_PM_00$annee)
    
    data_SRR_PM_000 <- data_SRR_PM_00 |> 
      select(date,code_sandre_parametre,code_sandre_unite,conc)
    
    couple_parametre_unite_PM<-data_SRR_PM_00 |> 
      select(code_sandre_parametre,code_sandre_unite) |> 
      distinct()
    
    calendrier_param_unite<-crossing(calendrier, couple_parametre_unite_PM)
    
    #'  On compte le nombre de mesures par couple paramètre / unité, mois et année, 
    #'  puis on fait une jointure avec la table calendrier pour s'assurer d'avoir toute
    #'  la période.
    data_SRR_PM_01<-data_SRR_PM_00|> 
      group_by(code_sandre_parametre,code_sandre_unite,mois,annee) |> 
      summarise(nb_mens = n(),
                moy_mens = round(mean(conc),digits = 3))
    
    data_SRR_PM_02<-data_SRR_PM_00 |> 
      group_by(code_sandre_parametre,code_sandre_unite,annee) |> 
      summarise(nb_ann = n(),
                moy_ann = round(mean(conc),digits = 3))
    
    data_SRR_PM_03<-data_SRR_PM_00 |> 
      mutate(annee_suiv=annee+1) |> 
      group_by(code_sandre_parametre,code_sandre_unite,annee_suiv) |> 
      summarise(nb_ann_prec = n(),
                moy_ann_prec = round(mean(conc),digits = 3)) |> 
      rename("annee" = annee_suiv)
    
    data_SRR_PM_04<-data_SRR_PM_00 |> 
      group_by(code_sandre_parametre,code_sandre_unite) |> 
      summarise(nb_pluri = n(),
                moy_pluri = round(mean(conc),digits = 3))
    
    
    data_SRR_PM_05<-calendrier_param_unite |> 
      left_join(data_SRR_PM_01, by = c("code_sandre_parametre","code_sandre_unite","mois","annee")) |> 
      left_join(data_SRR_PM_02, by = c("code_sandre_parametre","code_sandre_unite","annee")) |> 
      left_join(data_SRR_PM_03, by = c("code_sandre_parametre","code_sandre_unite","annee")) |> 
      left_join(data_SRR_PM_04, by = c("code_sandre_parametre","code_sandre_unite")) |> 
      arrange("code_sandre_parametre","année","mois") |> 
      mutate(conc_moy = case_when(nb_mens>=4 ~ moy_mens,
                                  nb_ann>=1 ~ moy_ann,
                                  nb_ann_prec>=1 ~ moy_ann_prec,
                                  TRUE ~ moy_pluri),
             origine_conc = case_when(nb_mens>=4 ~ "moy_mens",
                                      nb_ann>=1 ~ "moy_ann",
                                      nb_ann_prec>=1 ~ "moy_ann_prec",
                                      TRUE ~ "moy_pluri"))
    
    data_SRR_PM_06<-data_SRR_PM_05 |> 
      select(mois, annee, code_sandre_parametre,code_sandre_unite,conc_moy,origine_conc)
    
    data_SRR_PM_vol<-data_SRR_PM_06 |> 
      filter(code_sandre_parametre =="1552") |> 
      rename("vol_moy"=conc_moy,
             "origine_vol" = origine_conc) |> 
      select(mois,annee,vol_moy,origine_vol)
    
    # Calcul de flux
    data_SRR_PM_08<-data_SRR_PM_00 |> 
      filter(code_sandre_parametre =="1552") |> 
      right_join(calendrier_j, by = c("date","mois","annee")) |> 
      select(-code_sandre_parametre,-code_sandre_unite) |> 
      rename("vol" = conc) |> 
      left_join(data_SRR_PM_vol, by = c("mois","annee")) |> 
      left_join(data_SRR_PM_06, by=c("mois","annee"), relationship = "many-to-many") |> 
      left_join(data_SRR_PM_000, by = c("date","code_sandre_parametre","code_sandre_unite")) |> 
      filter(code_sandre_parametre !="1552") |> 
      left_join(tab_param_unites, by = c("code_sandre_parametre", "code_sandre_unite")) |> 
      filter(annee %in% c(annee_min:annee_max)) |> 
      mutate(
        # On remplit les cellules vides pour le site d'origine et le pt d'AS 
        site_origine = liste_points_mesure$site_origine[i],
        pt_AS = liste_points_mesure$pt_AS[i],
        
        #' On prend le volume le plus pertinent pour le calcul :
        #' - si c'est un volume réel (vol) : on prend celui-ci en priorité
        #' - s'il n'est pas disponible : on prend un volume moyen vol_moy
        
        volume= case_when(
          !is.na(vol) ~ vol,
          is.na(vol) ~ vol_moy),
        
        #' Calcul de flux :
        #' On utilise le volume précédemment déterminé et un facteur de conversion. 
        #' Pour la valeur de concentration : 
        #' - si c'est une valeur mesurée ce jour (conc) : on prend celle-ci en priorité
        #' - si elle n'est pas disponible : on prend une concentration moyenne conc_moy
        Flux_j = case_when(!is.na(conc) ~ volume*conc*facteur_conv_flux,
                           is.na(conc) ~ volume * conc_moy*facteur_conv_flux,
                           TRUE ~ volume * conc_moy * facteur_conv_flux),
        
        
        
        Source_flux = case_when(
          !is.na(vol) & vol==0 ~ "Flux nul (débit nul)",
          !is.na(vol) & vol!=0 & !is.na(conc) ~ "Flux jour Bilan 24h",
          !is.na(vol) & vol!=0 & origine_conc == "moy_mens" ~ "Flux estimé reconstitué (mois N)",
          !is.na(vol) & vol!=0 & origine_conc == "moy_ann" ~ "Flux estimé reconstitué (année N)",
          !is.na(vol) & vol!=0 & origine_conc == "moy_ann_prec" ~ "Flux estimé (conc année N-1)",
          !is.na(vol) & vol!=0 & origine_conc == "moy_pluri" ~ "Flux estimé (conc pluriannuelle)",
          is.na(vol) & origine_vol == "moy_mens" ~ "Flux estimé reconstitué (mois N)",
          is.na(vol) & origine_vol == "moy_ann" ~ "Flux estimé reconstitué (année N)",
          is.na(vol) & origine_vol == "moy_ann_prec" ~ "Flux estimé (vol année N-1)",
          is.na(volume) ~ "Aucun débit, calcul de flux impossible",
          TRUE ~ "A vérifier"
        ), 
        
        Fiabilite_flux = case_when(
          !is.na(vol) & !is.na(conc) ~ "3", # Flux calculé à partir d'un débit et d'une concentration mesurés
          !is.na(vol) & is.na(conc) ~ "2", # Flux calculé à partir d'un débit mesuré et d'une concentration moyenne
          is.na(vol) & origine_vol=="moy_mens"& !is.na(conc) ~ "2", #Flux calculé à partir d'un débit moyen mensuel et d'une concentration mesurée (peu de données manquantes)
          TRUE ~ "1" #Tous les autres cas
        )

        
      ) |> 
      distinct()
    
    if(i==1)
    {data_SRR_PM <- data_SRR_PM_08}
    
    data_SRR_PM<-bind_rows(data_SRR_PM,data_SRR_PM_08)
  }
  
  data_SRR_PM_export<- data_SRR_PM |> 
    distinct() |> 
    select(site_origine,pt_AS,date,code_sandre_parametre,lib_parametre,code_sandre_unite,Flux_j,unite_flux,Source_flux,Fiabilite_flux)
  
  data_SRR_PM_mens<-data_SRR_PM |> 
    distinct() |> 
    group_by(site_origine,pt_AS,annee,mois,code_sandre_parametre,lib_parametre,code_sandre_unite,unite_flux) |> 
    summarise(Flux_m=sum(Flux_j))
  
  write.xlsx(data_SRR_PM_export,"03_intermediary_data/data_SRR_PM_export.xlsx")

  
  
  
  # 3. MISE EN FORME FLUX FORFAIT ----
CGM_01<-CGM |> 
    filter(!is.na(`Date de mesure`)) |> 
    mutate(annee_CGM = year(as.Date(`Date de mesure`))) |> 
    select(-`Date de mesure`,-`Année d'application`) |> 
    distinct() |> 
    rename("site_origine" = `N° Ouvrage`,
         "Année" = `Année de redevance`) |> 
    arrange(Année, site_origine,`Code Activité`, desc(annee_CGM)) |> 
    distinct(Année, site_origine,`Code Activité`, .keep_all = TRUE)

flux_forfait_01 <-flux_forfait |>
  rename("frequence" = `Fréquence Activité`) |> 
  left_join(CGM_01, by = c("Année","site_origine", "Code Activité")) |> # , relationship = "many-to-many"
  mutate(Année=as.numeric(Année),
         Fiabilite_flux = ifelse(!is.na(annee_CGM) & (annee_CGM+10)>= Année,"2","3"),
         Source_flux = case_when(
           frequence == "Mensuel" & (!is.na(annee_CGM) & (annee_CGM+10)>= Année) ~ "Forfait, reconstitution sur la base des flux mensuels, CGM récente",
           frequence == "Annuel"  & (!is.na(annee_CGM) & (annee_CGM+10)>= Année) ~ "Forfait, reconstitution sur la base des flux annuels, CGM récente",
           frequence == "Mensuel" & (!is.na(annee_CGM)) ~ "Forfait, reconstitution sur la base des flux mensuels, CGM ancienne",
           frequence == "Annuel" & (!is.na(annee_CGM)) ~ "Forfait, reconstitution sur la base des flux annuels, CGM ancienne",
           frequence == "Mensuel" & is.na(annee_CGM) ~ "Forfait, reconstitution sur la base des flux mensuels sans CGM",
           frequence == "Annuel" & is.na(annee_CGM) ~ "Forfait, reconstitution sur la base des flux annuels sans CGM",
           TRUE ~ "A vérifier"
         ),
         nb_j_mois = case_when(Mois %in% c("01","03","05","07","08","10","120") ~ 31,
                               Mois %in% c("04","06","09","11") ~ 30,
                               TRUE ~ 28),#On reste sur 28 jours par défaut pour février
         Flux_kg_j_activite = `PRM brute` / nb_j_mois
  ) |> 
  rename("Code_sandre_parametre" = `Code SANDRE paramètre`)

flux_forfait_export_detail<-flux_forfait_01 |> 
  select(-c(`PRM brute`,`Unité paramètre`,frequence,annee_CGM, nb_j_mois)) |> 
  relocate(Flux_kg_j_activite, .before = Fiabilite_flux)

flux_forfait_export_condens<-flux_forfait_export_detail |> 
  group_by(Année,Mois,site_origine,Code_sandre_parametre) |> 
  summarise(Flux_kg_j = sum(Flux_kg_j_activite))
  

write_xlsx(flux_forfait_export_detail, "03_intermediary_data/flux_forfait_export_detail.xlsx")
write_xlsx(flux_forfait_export_condens, "03_intermediary_data/flux_forfait_export_condens.xlsx")

write_csv2(
  flux_forfait_export_detail,
  "03_intermediary_data/flux_forfait_export_detail.xlsx",
  na = "NA")

write_csv2(
  flux_forfait_export_condens,
  "03_intermediary_data/flux_forfait_export_condens.xlsx",
  na = "NA")

})

