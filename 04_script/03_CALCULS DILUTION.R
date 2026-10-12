# 1. IMPORT DES DONNEES ----

## 1.1. CHARGEMENT DE LA CARTE DES QMNA5 ----
carte_QMNA5_00<-st_read("02_data/CARTO/QMNA5/Q5_finaux_Decembre2012SN.shp")

carte_QMNA5<-carte_QMNA5_00 |> 
  # Les QMNA5 sont exprimés en m3/s, on va les convertir en m3/j
  mutate(QMNA5_j = Q5MOY_MN * 24 * 3600) |> 
  # On restreint la quantité de colonnes pour ne conserver que ce qui nous intéresse
  select(CODE_HYDRO,TOPONYME1,TOPONYME2,QMNA5_j,ROBUSTES_1) |> # Ecartés : ID_BDCARTH, C_HYD_CDO
  st_filter(Filtre) |> 
  st_segmentize(dfMaxLength = 100)

## 1.2. CARTES EDL ----
### 1.2.1. CARTE EDL MESU ----

carte_EDL_MESU_00<-st_read("02_data/CARTO/Masses d'eau rivières (EDL 2025)/TronconElemMasseDEauRiviere_EDL2025.shp")
carte_EDL_MESU<-carte_EDL_MESU_00 |> 
  st_filter(Filtre)


### 1.2.2. CARTE EDL BV ----
carte_EDL_BV_00<-st_read("02_data/CARTO/Etat écologique ME (EDL 2025)/Etat_eco-EDL_2025.shp")
carte_EDL_BV<-carte_EDL_BV_00 |> 
  st_filter(Filtre)

## 1.3. LIMITES DE QUALITE ----
### 1.3.1. TABLEAU DES NQE (micropolluants) -----
NQE_micro_00 <-read_excel("02_data/EDL/Valeurs Seuils_NQE_Ineris_v20241216.xlsx", sheet = "Toutes valeurs seuils_202407")

#' On transforme le tableau de base pour ne retenir que les codes sandre des 
#' paramètres et leur limite de qualité. 
#' Attention, plusieurs limites de qualité existent, vérifier avec Grace si c'est
#' bien celle-ci qui s'applique.
#' 
NQE_micro <- NQE_micro_00 |> 
  rename("lib_parametre" = Substance,
    "code_sandre_parametre" = `CODE\r\nSANDRE`,
    "seuil_NQE" = `Valeur\r\nréglementaire\r\nPS/PHS\r\nAA-EQS FW\r\n(Dir. 2013/39/CE)\r\n(µg/L)`) |> 
  select(code_sandre_parametre,seuil_NQE) |> 
  mutate(seuil_NQE = as.numeric(seuil_NQE)) |> 
  filter(!is.na(seuil_NQE))



### 1.3.2. TABLEAU DES CLASSES D'ETAT (macropolluants) ----
classes_etat_macro <-read_excel("02_data/EDL/Classes d'état DCE.xlsx")






#' [DEV UNIQUEMENT] Création d'un échantillon test de points de rejet
#' - Indus SRR
#' - Indus au forfait 
#' 

ech_SRR<-flux_SRR_PM_export |> 
  select(site_origine,pt_AS) |> 
  distinct() #|>
  # head(10)

# ech_forfait<-as_tibble_col(flux_forfait_export_condens$site_origine, column_name = "site_origine") |> 
#   distinct() |> 
#   head(5)

## 2. CALCUL DE DILUTION SUR LES POINTS DE REJET SRR ----

#'  On commence par le SRR
#'  Etape 1 : trouver le point de rejet et le cours d'eau de raccordement.
#'  Pour cela, on fait le lien avec la table de correspondance

traitement_SRR_01<-left_join(ech_SRR,table_correspondance, by = c("site_origine","pt_AS")) |> 
  filter(exutoire_rejet == "COURS D'EAU") |> 
  select(-nb_pt_AS_par_rejet, -nb_pt_AS_par_site, -nb_rejet_par_site,-a_verifier)

#' Etape 2 : on cherche les coordonnées géographiques associées au point de rejet

traitement_SRR_02<-st_as_sf(left_join(traitement_SRR_01,liste_points_rejet, by = "rejet"))

#' Etape 3 : on cherche à snapper le cours d'eau avec le QMNA5 le plus proche :

traitement_SRR_03<-st_snap(traitement_SRR_02,carte_QMNA5, tolerance = 100) #|> 

  # st_join(carte_QMNA5) |> 
  # st_intersection(carte_EDL_BV)

points_rejet<-traitement_SRR_02
cours_eau <-carte_QMNA5


# Avec le package nngeo (Merci Claude !) - A nettoyer
 
points_projetes <- st_nn(points_rejet, cours_eau, 
                         returnDist = TRUE, 
                         k = 1, 
                         progress = FALSE)

# Obtenir l'index du cours d'eau le plus proche
index_proche <- unlist(points_projetes$nn)

# Projeter chaque point sur sa ligne la plus proche
points_sur_lignes <- st_nearest_points(points_rejet, cours_eau[index_proche,], pairwise = TRUE)

# Extraire le point sur la ligne
points_finaux <- lwgeom::st_endpoint(points_sur_lignes)
# Il faudra trouver comment rattraper le nom du cours d'eau récepteur. 





  
# Attention, vérifier les dénominations des cours d'eau "snappés" avant d'aller plus loin



# On crée un dataframe pour regarder de plus près les points qui n'ont pas été snappés. 
rejets_unsnapped<-traitement_SRR_03 |> 
  filter(is.na(CODE_HYDRO))


#' On va rapprocher le QMNA5_j et la mesure des rejets dans le SRR pour calculer 
#' la PEC (Predicted Environmental Concentration) = calcul de dilution.

traitement_SRR_04<-flux_SRR_PM_export |> 
  filter(pt_AS %in% ech_SRR$pt_AS) |> 
  left_join(traitement_SRR_03, by = c("site_origine",
                                      "pt_AS" )) |> 
  mutate(PEC = (Flux_j/QMNA5_j)*10^6,
         unite_PEC = ifelse(unite_flux == "kg", "mg/m3","A vérifier")) |> 
  left_join(NQE_micro, by = "code_sandre_parametre") |> 
  mutate(intens_pression_micro = case_when(!is.na(seuil_NQE) & PEC < seuil_NQE/10 ~ "-", 
                                       !is.na(seuil_NQE) & PEC >= seuil_NQE/10  &
                                         !is.na(seuil_NQE) & PEC < seuil_NQE  ~ "Moyen",
                                       !is.na(seuil_NQE) & PEC >= seuil_NQE ~ "Fort",
                                       is.na(seuil_NQE) ~ "Pas de seuil de NQE pour comparaison",
                                       TRUE ~ "A vérifier"
                                       ),
         nv_pression_micro = PEC/seuil_NQE
         )





# Carto test
# test_map_QMNA5<-ggplot(data=Filtre)+
#   geom_sf() +
#   geom_sf(data=carte_EDL_BV, fill = "orchid", alpha=0.5)+
#   geom_sf(data=Filtre, colour="red", alpha=0.5)+
#   geom_sf(data=carte_QMNA5, colour="blue", alpha=0.5)+
#   geom_sf(data=traitement_SRR_02, colour= "black")+
#   geom_sf(data=traitement_SRR_03, colour= "darkgreen")
# 
# print(test_map_QMNA5)
# carte_EDL_BV4326<-st_transform(carte_EDL_BV,crs = 4326)
# test_map_interactive<-leaflet() |> 
#   addPolygons(data=carte_EDL_BV,
#               color = "black",
#               fillColor = "orchid",
#               fillOpacity = 0.5,
#               label = ~NomMasseDE)



# Créer un lien entre les points d'origine et reprojetés sur le cours d'eau à QMNA5 le plus proche. 
rejet_non_projete <- traitement_SRR_02 |> 
  select(rejet,geometry) |> 
  cbind(st_coordinates(traitement_SRR_02)) |> 
  st_drop_geometry()


rejet_projete <-traitement_SRR_03 |> 
  select(rejet,geometry) |> 
  cbind(st_coordinates(traitement_SRR_03)) |> 
  st_drop_geometry() |> 
  rename("X_proj" = "X",
         "Y_proj" = "Y")

liens_rejet <-left_join(rejet_non_projete, rejet_projete, by = "rejet") |> 
  mutate(geom = sprintf("LINESTRING(%s %s, %s %s)",X, Y, X_proj, Y_proj))

test2<-st_as_sf(liens_rejet, wkt = "geom", crs = 2154)

# CARTE TEST ----
mapview(carte_EDL_BV,
        label = "NomMasseDE",
        col.region = "orchid",
        alpha.region=0.25, # Transparence
        map.types="OpenStreetMap")+
  mapview(carte_QMNA5,
          label = "TOPONYME1",
          color = "blue")+
  mapview(traitement_SRR_02,
          label = "nom_rejet",
          color = "black")+
  mapview(points_finaux,
          color = "red")
  # mapview(test2,
  #         label = "rejet",
  #         color = "black")




