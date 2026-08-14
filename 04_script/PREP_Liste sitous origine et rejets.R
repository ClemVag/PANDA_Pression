#  PACKAGES ET FONCTIONS ----
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

# FONCTIONS
`%nin%` <- negate(`%in%`)  

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

ecrire_onglet <- function(wb, nom, data) {
  addWorksheet(wb, nom)
  writeData(wb, nom, data, headerStyle = entete)
  addStyle(wb, nom, style = autres_lignes,
           rows = 2:(nrow(data) + 1), cols = 1:ncol(data),
           gridExpand = TRUE)
  setColWidths(wb, nom, cols = 1:ncol(data), widths = "auto")
  freezePane(wb, nom, firstRow = TRUE)
}

# PARAMETRAGE 
type_site = "026"
  
  
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
  n_base<-n_base |>  
    select(-`Libellé UG`) |>  
    rename(identifiant = "No_Sitou")
  
  n_DT<-n_base |> 
    select(DT,identifiant)
  
  n_base<-n_base |> 
    select(-DT)
  
  ## 1.3. LISTE DE TYPES DE SITOUS ----
  
  types_sitou <- read_excel("02_data/SITOUREF/Type_sitous.xlsx")
  # Recodage du type Sitou sur 3 caractères
  types_sitou$No_Type_Sitou <- case_when(
    nchar(types_sitou$No_Type_Sitou) == 1 ~ paste0("00", types_sitou$No_Type_Sitou),
    nchar(types_sitou$No_Type_Sitou) == 2 ~ paste0("0", types_sitou$No_Type_Sitou),
    TRUE ~ types_sitou$No_Type_Sitou
  )

  # # Création d'un vecteur contenant uniquement les n° de sitous pour la table récapitulative
  # num_type <- data.frame(types_sitou$No_Type_Sitou)
  # colnames(num_type) = "base"
  
  ## 1.4. LISTE DES POINTS D'AUTOSURVEILLANCE STEU----
  Pts_AS_STEU <- read_excel("02_data/SITOUREF/PandaPression_Liste_points_AS_STEP.xlsx")

# 2. CREATION DES TABLES OBJETS ET LIAISONS ----


# Pour constituer la liste des sites à étudier, on récupère tous les sites de la DT
# correspondant au type de site attendu. 
liste_sites <- n_base |>  
  filter(No_Type_Sitou == type_site)  |>  
  distinct() |> 
  select(identifiant, Nom_Sitou)
# filter(identifiant %in% c("HR202A_211"))

# # On retire la colonne DT de la table des noeuds pour l'alléger. 
# n_base<-n_base |>  
#   select(-DT)

### 2.1. CREATION DE LA TABLE DES LIENS ----
l_base <- l_base  |> 
  mutate(liaison = paste0(str_sub(No_Sitou_Am, -3), "-", str_sub(No_Sitou, -3))) |>  
  distinct()


##2.3. LISTE DES OBJETS "DEBUT","FIN" DE MAILLAGE (BREAK) ET EXCLUS ----
# Une liste est utilisée pour indiquer les sitous avant / après lesquels on va "couper" la recherche 
# de généalogie.

### 2.3.1. ASSAINISSEMENT ----

  break_list_amont<-c("029", # STEU
                      "012", # Site indus
                      "243", # SCL
                      "013") # Exploitation agricole
  
 table_site_rejet <-   n_site <- data.frame(
  rejet = character(),
  site_origine = character(),
  rang = character()
)


# 3. LOOP SUR LA LISTE DES SITES ----
 
## 3.1. RECHERCHE DU SITOU A L'ORIGINE DES POLLUTIONS ----
 
for (k in 1:nrow(liste_sites)) #
  #for (k in 1:50)
  #k=60
  
{
  ### 3.1.1. INFO POINT DE REJET ----

  # Informations relatives au site et à son futur chemin d'export
  code_site <- liste_sites$identifiant[k]

  ### 3.1.2. TABLE DES NOEUDS ----
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
  # On ajoute (en retirant les doublons) les noeuds dans la breaklist pour avoir
  # une liste finale de sitous amont.
  n_site_amont<-distinct(rbind(n_site_amont,n_breaklist_amont))
  
  n_site_rang<-n_site_rang |> 
    filter(No_Type_Sitou %in% c(type_site,break_list_amont)) |>
    arrange(desc(rang)) |> 
    distinct(identifiant,No_Type_Sitou,Nom_Sitou,.keep_all=TRUE)
  
  
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
  relocate(rang,.before = site_origine) |> 
  left_join(n_DT,  by = c("site_origine" = "identifiant")) |>  
  relocate(DT, .before = everything()) |> 
  select(-No_Type_Sitou)



## 3.2. RECHERCHE DU SITOU POINT DE MESURE D'AUTOSURVEILLANCE ASSOCIE ----

### 3.2.1. POINTS D'AUTOSURVEILLANCE SCL ----
liste_PM_SCL<-table_site_rejet |> 
  filter(str_sub(site_origine,-3)=="243") |> 
  left_join(l_base,by=c("rejet" = "No_Sitou"), relationship="many-to-many") |> 
  filter(str_sub(No_Sitou_Am,-3)=="242") |> 
  rename("point_mesure" = No_Sitou_Am) |> 
  select(-liaison)


### 3.2.2. POINTS D'AUTOSURVEILLANCE STEU ----
Pts_AS_rejets_STEU<-Pts_AS_STEU |> 
  select(`No interne Sitou amont`,`No interne Sitou`,LocalisationSitouref) |> 
  filter(LocalisationSitouref %in% c("A2","A5","A4")) |> 
  group_by(`No interne Sitou amont`) |> 
  mutate(nb_pts = n()) |> 
  ungroup() |> 
  rename("site_origine" = `No interne Sitou amont`,
         "point_mesure" = `No interne Sitou`)

liste_PM_STEU<-liste_sites_origine |> 
  filter(str_sub(site_origine,-3)=="029") |> 
  left_join(Pts_AS_rejets_STEU, by="site_origine", relationship="many-to-many")


### 3.2.3. POINTS D'AUTOSURVEILLANCE INDUSTRIELS ----

#### V1 -----

# Création de la table de base 
liste_PM_indus_base<-l_base |> 
  filter(liaison %in% c("085-224",    # Atelier indus --> Point d'AS 
                        "025-224")) |>  # STEU indus  --> Point d'AS
  select(No_Sitou)

liste_PM_indus<-n_base |> 
  filter(identifiant %in% liste_PM_indus_base$No_Sitou) |> 
  select(-No_Type_Sitou)

table_site_Pt_AS <- data.frame(
  pt_AS = character(),
  site_origine = character()
)

#### Recherche des ascendants ----
#' 
#' 
#' 
# for (k in 1:10)
for (k in 1:400)
{
  code_point <- liste_PM_indus$identifiant[k]
  

  
  n_site <- data.frame(
  identifiant = character(),
  Nom_Sitou = character(),
  No_Type_Sitou = character()
)
n_breaklist_amont<-n_site


n_site_add <- n_base |> 
  filter(identifiant == code_point) 
n_site <- n_site_add
n_site_aval <- n_site
n_site_amont<-n_site

n1 <- 1
n2 <- 0


### On cherche avec quels autres sitou notre liste de sitous a des liens
# D'abord les liens amont
while (n2 != n1)
{
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
  

  n_site_amont <- distinct(rbind(n_site_amont, n_site_add))
  n2 <- nrow(n_site_amont)
  
}
# On ajoute (en retirant les doublons) les noeuds dans la breaklist pour avoir
# une liste finale de sitous amont.
n_site_amont<-distinct(rbind(n_site_amont,n_breaklist_amont))


table_stack <- n_site_amont |> 
  filter(No_Type_Sitou != "224") |> 
  select(identifiant) |> 
  mutate(pt_AS = code_point) |> 
  rename("site_origine" = identifiant) |> 
  relocate(pt_AS, .before = everything())

table_site_Pt_AS<-rbind(table_site_Pt_AS,table_stack)

}
table_site_Pt_AS<-distinct(table_site_Pt_AS)

table_site_Pt_AS_01<- table_site_Pt_AS|> 
  mutate(type_sitou = str_sub(site_origine,-3)) |> 
  pivot_wider(names_from = type_sitou,
              values_from = site_origine,
              values_fill = NA)

liste_Pt_AS_indus_origine <- liste_PM_indus |> 
  left_join(table_site_Pt_AS, by = c("identifiant" = "pt_AS")) |> 
  left_join(n_base, by = c("site_origine" = "identifiant")) |> 
  left_join(n_DT,  by = c("site_origine" = "identifiant")) |>  
  relocate(DT, .before = everything()) |> 
  select(-No_Type_Sitou)


#### V2 ----
liste_PAS_I_00 <-l_base |> 
  filter(liaison %in% c("085-224",    # Atelier indus --> Point d'AS 
                        "025-224")) |>    # STEU indus  --> Point d'AS
  select(-liaison) |> 
  rename("pt_AS" = No_Sitou)


liste_PAS_I_01 <-l_base |> 
  filter(liaison %in% c("085-026",    # Atelier indus --> Point de rejet 
                        "025-026"))|>    # STEU indus  --> Point de rejet
  select(-liaison) |> 
  rename("rejet" = No_Sitou)


liste_PAS_I_02<-liste_PAS_I_00 |> 
  left_join(liste_PAS_I_01, by = "No_Sitou_Am", relationship = "many-to-many") |> 
  filter(!is.na(rejet)) |> 
  select(-No_Sitou_Am) |> 
  left_join(n_base,by=c("pt_AS"="identifiant"))


# 4. EXPORT FICHIER VUE D'ENSEMBLE -----

wb <- createWorkbook()
ecrire_onglet(wb,"liste_PM_SCL",liste_PM_SCL)
ecrire_onglet(wb,"liste_PM_STEU",liste_PM_STEU)
ecrire_onglet(wb,)
saveWorkbook(wb, "03_intermediary_data/Table_correspondance.xlsx", overwrite = TRUE)
