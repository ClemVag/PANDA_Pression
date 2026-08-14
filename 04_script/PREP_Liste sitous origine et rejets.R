#0  PACKAGES ET FONCTIONS ----
## 0.1. PACKAGES ----
{
  library(tidyverse)
  library(dplyr)
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


  
  # 1. FICHIERS DE DONNEES ----
  # Le premier fichier de données est une extraction de Sitouref, qui contient sur un onglet l'ensemble des liaisons, 
  # sur un autre onglet l'ensemble des Sitous avec leur caractéristique du type de site. 
  # L'extraction provient de l'entrepôt de données. 

  ## 1.1. LIENS ----
  l_base <- read_excel("02_data/SITOUREF/OUTIL-016-2025_Maillage_sitous.xlsx", sheet = "Liaisons")
  l_base <- l_base |> 
    select(-`Liaison_Sitou-Amont`)
  
  ## 1.2. NOEUDS ----
  n_base <- read_excel("02_data/SITOUREF/OUTIL-016-2025_Maillage_sitous.xlsx", sheet = "Sitous")
  
  n_DT<-n_base |> 
    select(DT,No_Sitou) |> 
    rename(identifiant = "No_Sitou")
  
  n_base<-n_base |>  
    select(-`Libellé UG`,-DT) |>  
    rename(identifiant = "No_Sitou")

  
  ## 1.4. LISTE DES POINTS D'AUTOSURVEILLANCE STEU----
  Pts_AS_STEU <- read_excel("02_data/SITOUREF/PandaPression_Liste_points_AS_STEP.xlsx")

# 2. CREATION DES TABLES OBJETS ET LIAISONS ----


# Pour constituer la liste des sites à étudier, on récupère tous les sitous
# correspondant au type de sitou attendu. 
  type_site = "026" # Point de rejet
  liste_sites <- n_base |>  
  filter(No_Type_Sitou == type_site)  |>  
  distinct() |> 
  select(identifiant, Nom_Sitou)


### 2.1. CREATION DE LA TABLE DES LIENS ----
l_base <- l_base  |> 
  mutate(liaison = paste0(str_sub(No_Sitou_Am, -3), "-", str_sub(No_Sitou, -3))) |>  
  distinct()


## 2.3. LISTE DES OBJETS "DEBUT","FIN" DE MAILLAGE (BREAK) ----
#' Une liste est utilisée pour indiquer les sitous de "bout de chaîne" après 
#' lesquels on va "couper" la recherche de généalogie.

  break_list_amont<-c("029", # STEU
                      "012", # Site indus
                      "243", # SCL
                      "013") # Exploitation agricole
  



# 3. LOOP - CREATION DE LA LISTE DES SITES ----

 table_site_rejet <- n_site <- data.frame(
    rejet = character(),
    site_origine = character(),
    rang = character()
  )  
   

for (k in 1:nrow(liste_sites)) #nrow(liste_sites)
{
  code_site <- liste_sites$identifiant[k]

  
  n_site <- data.frame(
    identifiant = character(),
    Nom_Sitou = character(),
    No_Type_Sitou = character()
  )
  n_breaklist_amont<-n_site

  n_site_add <- n_base |> 
    filter(identifiant == code_site) 
  n_site <- n_site_add
  n_site_aval <- n_site
  n_site_amont<-n_site
  
  n1 <- 1
  n2 <- 0
  rang_variable = 0
  
  n_site_rang <- n_site_add |> 
    mutate(rang = 0)
  
  ### On cherche les ascendants du Sitou en s'arrêtant sur la liste de site de la breaklist
  while (n2 != n1)
  {
    rang_variable = rang_variable + 1
    
    n1 <- nrow(n_site_amont)
    
    if(n1>1) #On n'exécute pas cette étape sur la première itération pour éviter d'écarter le sitou central.
    {
      # On crée une liste de noeuds avec les codes issus de la "breaklist" 
      # issus de l'itération précédente 
      n_breaklist_temp<-n_site_amont |>  
        filter(No_Type_Sitou %in% break_list_amont)
      
      # On la compile avec la breaklist de l'itération précédente
      n_breaklist_amont<-distinct(rbind(n_breaklist_amont,n_breaklist_temp))
      
      
      # On retire les sitous issus de la breaklist de la liste de noeuds à considérer.
      n_site_amont<-n_site_amont |>  
        filter(No_Type_Sitou %nin% break_list_amont)
    }
    
    lien_site <- l_base |> 
      filter(No_Sitou %in% n_site_amont$identifiant) |> 
      select(No_Sitou_Am) |> 
      rename("No_Sitou" = No_Sitou_Am)
    
    
    ### On ajoute les sitous liés à la liste des sitous noeuds
    n_site_add <- n_base |> 
      filter(identifiant %in% lien_site$No_Sitou)
    
    
    n_site_add_rang <-n_site_add |> 
      mutate(rang = rang_variable)
      
    n_site_amont <- distinct(rbind(n_site_amont, n_site_add))
    n2 <- nrow(n_site_amont)
    
    n_site_rang <-rbind(n_site_rang, n_site_add_rang)
    
  }
  #' On ajoute (en retirant les doublons) les noeuds dans la breaklist pour avoir
  #' une liste finale de sitous amont.
  n_site_amont<-distinct(rbind(n_site_amont,n_breaklist_amont))
  
  #' On reprend la liste des sites avec leurs rangs, en supprimant les éventuels doublons
  #' (surtout valable pour les sites industriels)
  n_site_rang<-n_site_rang |> 
    filter(No_Type_Sitou %in% c(type_site,break_list_amont)) |>
    arrange(desc(rang)) |> 
    distinct(identifiant,No_Type_Sitou,Nom_Sitou,.keep_all=TRUE)
  
  #' Pour éviter de retrouver TOUS les sites étant ascendants d'un point de rejet 
  #' (par ex, site industriel connecté à un SSCL connecté à un point de rejet),
  #' on récupère uniquement les sites issus de la liste "finale" avec le rang le plus petit
  n_sites_breaklist = sum(n_site_rang$No_Type_Sitou %in% break_list_amont)
  rangs_sites_breaklist <-n_site_rang |> 
    select(rang) |> 
    filter(rang>0)
  rang_max = max(rangs_sites_breaklist$rang)
  
  if(n_sites_breaklist >1 & any(rangs_sites_breaklist < rang_max)) 
  {
    n_site_rang<-n_site_rang |> 
      filter(n_site_rang$rang != rang_max)
  }
  
  #' Et enfin, on compile avec la table globale
  table_stack <- n_site_rang |> 
    filter(No_Type_Sitou != "026") |> 
    select(identifiant,rang) |> 
    mutate(rejet = code_site) |> 
    rename("site_origine" = identifiant) |> 
    relocate(rejet, .before = everything())
  
  table_site_rejet<-rbind(table_site_rejet,table_stack)
  
}
table_site_rejet<-distinct(table_site_rejet)

liste_sites_origine <- liste_sites |> 
left_join(table_site_rejet, by = c("identifiant" = "rejet")) |> 
  left_join(n_base, by = c("site_origine" = "identifiant")) |> 
  left_join(n_DT,  by = "identifiant") |>  
  select(-No_Type_Sitou) |> 
  rename("nom_rejet" = Nom_Sitou.x,
         "nom_site_origine" = Nom_Sitou.y,
         "rejet" = identifiant ) |> 
  select(DT,rang,site_origine,nom_site_origine, rejet, nom_rejet)
  



# 4. RECHERCHE DU SITOU POINT DE MESURE D'AUTOSURVEILLANCE ASSOCIE ----

## 4.1. POINTS D'AUTOSURVEILLANCE SCL ----

liste_PAS_SCL_00<-table_site_rejet |> 
  filter(str_sub(site_origine,-3)=="243") |> 
  left_join(l_base,by=c("rejet" = "No_Sitou"), relationship="many-to-many") |> 
  filter(str_sub(No_Sitou_Am,-3)=="242") |> 
  rename("pt_AS" = No_Sitou_Am) |> 
  select(-liaison,-rang)

liste_PAS_SCL <-liste_sites_origine |> 
  filter(str_sub(site_origine,-3)=="243") |> 
  left_join(liste_PAS_SCL_00, by=c("rejet","site_origine")) |> 
  left_join(n_base, by=c("pt_AS"="identifiant")) |> 
  select(-No_Type_Sitou) |> 
  rename("nom_pt_AS" = Nom_Sitou) |> 
  relocate(c(pt_AS, nom_pt_AS),.before = "rejet")


## 4.2. POINTS D'AUTOSURVEILLANCE STEU ----
liste_PAS_STEU_00<-Pts_AS_STEU |> 
  select(`No interne Sitou amont`,`No interne Sitou`,LocalisationSitouref) |> 
  filter(LocalisationSitouref %in% c("A2","A5","A4")) |> 
  group_by(`No interne Sitou amont`) |> 
  #mutate(nb_pts = n()) |> 
  ungroup() |> 
  rename("site_origine" = `No interne Sitou amont`,
         "pt_AS" = `No interne Sitou`,
         "nom_pt_AS" = LocalisationSitouref)

liste_PAS_STEU<-liste_sites_origine |> 
  filter(str_sub(site_origine,-3)=="029") |> 
  left_join(liste_PAS_STEU_00, by="site_origine", relationship="many-to-many")|> 
  relocate(c(pt_AS, nom_pt_AS),.before = "rejet")


## 4.3. POINTS D'AUTOSURVEILLANCE INDUSTRIELS ----
liste_PAS_I_00 <-l_base |> 
  filter(liaison %in% c("085-224",        # Atelier indus --> Point d'AS 
                        "025-224")) |>    # STEU indus  --> Point d'AS
  select(-liaison) |> 
  rename("pt_AS" = No_Sitou)


liste_PAS_I_01 <-l_base |> 
  filter(liaison %in% c("085-026",       # Atelier indus --> Point de rejet 
                        "025-026"))|>    # STEU indus  --> Point de rejet
  select(-liaison) |> 
  rename("rejet" = No_Sitou)


liste_PAS_I_002<-liste_PAS_I_00 |> 
  left_join(liste_PAS_I_01, by = "No_Sitou_Am", relationship = "many-to-many") |> 
  filter(!is.na(rejet)) |> 
  left_join(n_base,by=c("pt_AS"="identifiant")) |> 
  select(-No_Sitou_Am, -No_Type_Sitou) |> 
  rename("nom_pt_AS" = Nom_Sitou)

liste_PAS_I<-liste_sites_origine |> 
  filter(str_sub(site_origine,-3)=="012") |> 
  left_join(liste_PAS_I_002, by="rejet", relationship="many-to-many")|> 
  relocate(c(pt_AS, nom_pt_AS),.before = "rejet")


## 4.4. POINTS DE REJETS RESTANTS -----
liste_restant<-liste_sites_origine |> 
  filter(rejet %nin% c(liste_PAS_STEU$rejet, 
                       liste_PAS_SCL$rejet,
                       liste_PAS_I$rejet)) #|>
#' Les lignes suivantes permettent de vérifier le nombre de liaisons de chaque sitou dans 
#' la liste des 'restants'
#'
#   rowwise() |> 
#   mutate(nb_liaisons = sum(rejet %in% l_base$No_Sitou | rejet %in% l_base$No_Sitou_Am))
#   
# max(liste_restant$nb_liaisons)

## 4.5. CONCATENATION DES TABLES
table_correspondance_00<-bind_rows(liste_PAS_STEU,
                        liste_PAS_SCL,
                        liste_PAS_I,
                        liste_restant) |> 
  left_join(l_base,by= c("rejet" = "No_Sitou_Am"), relationship = "many-to-many") |> 
  left_join(n_base, by = c("No_Sitou" = "identifiant")) |> 
  select(-liaison, -No_Type_Sitou) |> 
  rename("milieu_recepteur" = No_Sitou,
         "nom_milieu_recepteur" = Nom_Sitou)

#' On a la table de correspondance complète.
#' On va faire quelques tests pour vérifier les informations.
#' 
#' 
  
# 5. EXPORT FICHIER VUE D'ENSEMBLE -----

wb <- createWorkbook()
# ecrire_onglet(wb,"liste_PM_SCL",liste_PM_SCL)
# ecrire_onglet(wb,"liste_PM_STEU",liste_PM_STEU)
# ecrire_onglet(wb,)
saveWorkbook(wb, "03_intermediary_data/Table_correspondance.xlsx", overwrite = TRUE)
