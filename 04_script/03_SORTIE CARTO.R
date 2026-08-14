# Attention, pour le dev, lancer le script 01 avant toute modification !
Liste_points_AS_SCL<-read_excel("02_data/SITOUREF/Liste_points_AS_SCL.xlsx")

Liste_points_AS_SCL<-Liste_points_AS_SCL |> 
  rename("pt_AS" = `No interne Sitou`,
         "nom_pt_AS" = `Nom Sitou`
  )
Liste_points_AS_SCL$Classe <- factor(Liste_points_AS_SCL$Classe,
                                     levels = c("< 120 kgDBO/j", "de 120 à 600 kgDBO/j", "> 600 kgDBO/j")
)

Liste_points_AS_SCL$Classe<-Liste_points_AS_SCL$Classe %>% 
  fct_recode(
    "<120" = "< 120 kgDBO/j",
    "120-600" = "de 120 à 600 kgDBO/j",
    ">600" = "> 600 kgDBO/j"
  )



table_correspondance_00 <- read_excel("03_intermediary_data/Table_correspondance.xlsx")

table_correspondance_01 <-table_correspondance_00 |> 
  filter(rejet %in% liste_points_rejet$`No interne Sitou`) |> 
  mutate(type_rejet = case_when(str_sub(site_origine, -3)== "029" ~ "STEU",                                                                                    str_sub(site_origine, -3)== "029" ~ "SCL",
                                str_sub(site_origine, -3)== "243" ~ "SCL",
                                str_sub(site_origine, -3)== "012" ~ "INDUSTRIE",
                                TRUE ~ "AUTRE"))





# Ajout d'information au niveau du type de point de rejet


XY_rejets_filtre<-data_geo_01 |> 
  filter(`No interne Sitou`%in% table_correspondance_01$rejet)|> 
  rename("rejet" = `No interne Sitou`,
         "nom_rejet" = `Nom Sitou`) |> 
  st_filter(Filtre_buffer) |> 
  left_join(table_correspondance_01, by = c("rejet","nom_rejet" )) 
# Ajouter ici la jointure sur les tables d'information des flux.


XY_rejets_SCL<-XY_rejets_filtre |> 
  filter(type_rejet == "SCL") |> 
  left_join(Liste_points_AS_SCL, by=c("pt_AS","nom_pt_AS")) |> 
  select(rejet,pt_AS, nom_pt_AS, Classe, `Type réseau`)


XY_site_origine<-data_geo_01 |> 
  filter(`No interne Sitou`%in% table_correspondance_01$site_origine &
           `No interne Sitou`!="243" )|> #Pas de coordonnées géographiques associés aux SCL
  st_filter(Filtre_buffer)

# CONFIGURATION DE LA CARTOGRAPHIE DE SORTIE

carte_interactive <- leaflet() |> 
  #addTiles() %>%
  addProviderTiles(providers$OpenStreetMap.France) |> 
  addMarkers(lng = 2.790025858133278 , lat = 49.398777487570484, popup = "Vous êtes ici !")


carte_interactive
htmltools::save_html(carte_interactive, file = "07_output/carte_rejet.html")

