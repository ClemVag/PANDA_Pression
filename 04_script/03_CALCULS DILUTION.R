# 1. IMPORT DES DONNEES ----

## 1.1. CHARGEMENT DE LA CARTE DES QMNA5 ----
carte_QMNA5_00<-st_read("02_data/CARTO/QMNA5/Q5_finaux_Decembre2012SN.shp")

carte_QMNA5<-carte_QMNA5_00 |> 
  # Les QMNA5 sont exprimés en m3/s, on va les convertir en m3/j
  mutate(QMNA5_j = Q5MOY_MN * 24 * 3600) |> 
  # On restreint la quantité de colonnes pour ne conserver que ce qui nous intéresse
  select(ID_BDCARTH,CODE_HYDRO,C_HYD_CDO,TOPONYME1,TOPONYME2,QMNA5_j,ROBUSTES_1) |> 
  st_filter(carte_QMNA5,Filtre)



## 1.2. TABLEAU DES NQE (micropolluants) -----
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



## 1.3. TABLEAU DES CLASSES D'ETAT (macropolluants) ----
classes_etat_macro <-read_excel("02_data/EDL/Classes d'état DCE.xlsx")






#' [DEV UNIQUEMENT] Création d'un échantillon test de points de rejet
#' - Indus SRR
#' - Indus au forfait 
#' 

ech_SRR<-flux_SRR_PM_export |> 
  select(site_origine,pt_AS) |> 
  distinct() |> 
  head(5)

ech_forfait<-as_tibble_col(flux_forfait_export_condens$site_origine, column_name = "site_origine") |> 
  distinct() |> 
  head(5)

## 2. CALCUL DE DILUTION SUR LES POINTS DE REJET SRR ----

#'  On commence par le SRR
#'  Etape 1 : trouver le point de rejet et le cours d'eau de raccordement.
#'  Pour cela, on fait le lien avec la table de correspondance

traitement_SRR_01<-left_join(ech_SRR,table_correspondance, by = c("site_origine","pt_AS")) |> 
  filter(exutoire_rejet == "COURS D'EAU")

#' Etape 2 : on cherche les coordonnées géographiques associées au point de rejet

traitement_SRR_02<-st_as_sf(left_join(traitement_SRR_01,liste_points_rejet, by = "rejet"))

#' Etape 3 : on cherche à snapper le cours d'eau avec le QMNA5 le plus proche :

traitement_SRR_03<-st_snap(traitement_SRR_02,carte_QMNA5_local, tolerance = 200) |> 
  st_join(carte_QMNA5_local)
# Attention, vérifier les dénominations des cours d'eau "snappés" avant d'aller plus loin



# On crée un dataframe pour regarder de plus près les points qui n'ont pas été snappés. 
unsnapped<-traitement_SRR_03 |> 
  filter(is.na(ID_BDCARTH))


# On va rapprocher le QMNA5_j et la mesure des rejets dans le SRR
traitement_SRR_04<-flux_SRR_PM_export |> 
  filter(pt_AS %in% ech_SRR$pt_AS) |> 
  left_join(traitement_SRR_03, by = c("site_origine",
                                      "pt_AS" )) |> 
  mutate(dilution = (Flux_j/QMNA5_j)*10^6,
         unite_dilution = ifelse(unite_flux == "kg", "mg/m3","A vérifier")) |> 
  left_join(NQE_micro, by = "code_sandre_parametre") |> 
  mutate(pression_significative = ifelse(!is.na(seuil_NQE) & dilution > seuil_NQE/10, "Oui", "Non"))





# Carto test
test_map_QMNA5<-test_map<-ggplot(data=Filtre)+
  geom_sf() +
  geom_sf(data=Filtre, colour="red", alpha=0.5)+
  geom_sf(data=carte_QMNA5_local, colour="blue", alpha=0.5)+
  geom_sf(data=traitement_SRR_02, colour= "black")+
  geom_sf(data=traitement_SRR_02, colour= "darkgreen")

print(test_map_QMNA5)

