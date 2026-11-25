# 1. IMPORT DES DONNEES ----

## 1.1. CHARGEMENT DE LA CARTE DES QMNA5 ----
carte_QMNA5_00<-st_read("02_data/CARTO/QMNA5/Q5_finaux_Decembre2012SN.shp")

carte_QMNA5<-carte_QMNA5_00 |> 
  # Les QMNA5 sont exprimés en m3/s, on va les convertir en m3/j
  mutate(QMNA5_j = Q5MOY_MN * 24 * 3600) |> 
  # On restreint la quantité de colonnes pour ne conserver que ce qui nous intéresse
  select(ID_BDCARTH,TOPONYME1,TOPONYME2,QMNA5_j,ROBUSTES_1) |> # Ecartés : CODE_HYDRO, C_HYD_CDO
  st_filter(Filtre)

#' On conserve l'identifiant ID_BDCARTH comme identifiant pour faire la jointure 
#' avec les tables ultérieurement. Cet identifiant est unique par rapport au QMNA5.


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
#' L'import des NQE / Valeurs seuils sera à actualiser avec le tableau unique des NQE
NQE_00<-read_excel("02_data/EDL/Parametres_EDL.xlsx")
NQE<- NQE_00 |> 
  rename("code_sandre_parametre" = `CODE PARAMETRE`,
          "NQE" = `NQE-CMA  (μg/L)`) |> 
  select(code_sandre_parametre,CATEGORIE,FAMILLE,NQE) |> 
  filter(!is.na(NQE))


## 1.4. FICHIERS DE DONNEES ----

#' Rappel des variables utilisées
#' - [flux_SRR_PM_export] : dataframe, flux journaliers de polluants par points de mesure SRR
#' - [flux_forfait_export_condens] : dataframe, flux de polluants journalier par site industriel au forfait


#' On va uniformiser les fichiers pour permettre de les traiter les uns après les 
#' autres par la suite.Chaque fichier va être transformé pour avoir ces colonnes 
#' empilé sur les autres
#' - origine_pollution -- Colonne rajoutée pour indiquer d'où vient la pollution.
#' - site_origine          
#' - pt_AS                 
#' - date  
#' - code_sandre_parametre 
#' - lib_parametre     
#' - Flux_j
#' - unite_flux
#' - Source_flux
#' - Fiabilite_flux

source_SRR<-flux_SRR_PM_export |> 
  mutate(origine_pollution="Industriel SRR") |> 
  relocate(origine_pollution, .before = "site_origine")

source_forfait <- flux_forfait_export_condens |> 
  mutate(origine_pollution = "Industriel forfait",
         unite_flux = "kg",
         pt_AS = NA,
         date = as.Date(ym(paste0(Année,"-",Mois)))) |> 
  rename("Flux_j" = Flux_kg_j) |> 
  ungroup() |> 
  select(origine_pollution,
         site_origine,
         pt_AS,
         date,
         code_sandre_parametre,
         #lib_parametre,
         unite_flux)

## 1.5. NETTOYAGE DE L'ENVIRONNEMENT ----

rm(list = c("NQE_00",
            "carte_QMNA5_00",
            "carte_EDL_MESU_00",
            "carte_EDL_BV_00"))



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



# 2. CALCUL DE LA PEC SUR LE REJETS ----

## 2.1. APPAIRAGE REJET / COURS D'EAU RACCORDEMENT (SITOUREF) ----

#'  Etape 1 : trouver le point de rejet et le cours d'eau de raccordement.
#'  Pour cela, on fait le lien avec la table de correspondance

traitement_01<-table_correspondance |> 
  filter(exutoire_rejet == "COURS D'EAU") |> 
  select(-nb_pt_AS_par_rejet, -nb_pt_AS_par_site, -nb_rejet_par_site,-a_verifier)

#' Etape 2 : on cherche les coordonnées géographiques associées au point de rejet

traitement_02<-st_as_sf(left_join(traitement_01,liste_points_rejet, by = "rejet"))


## 2.2. APPAIRAGE REJET / QMNA5 (PROJECTION) ----

#' Etape 3 : on cherche à accrocher le point de rejet sur le cours d'eau avec 
#' le QMNA5 le plus proche :

points_rejet<-traitement_02

# Avec le package nngeo 

points_projetes <- st_nn(points_rejet, carte_QMNA5, 
                         returnDist = TRUE, 
                         k = 1, 
                         progress = FALSE)

# Obtenir l'index du cours d'eau le plus proche
index_proche <- unlist(points_projetes$nn)
distance_projection<- round(unlist(points_projetes$dist), digits = 1)

# Projeter chaque point sur sa ligne la plus proche
# Cette commande crée aussi la liaison entre les deux points. 
points_sur_lignes <- st_nearest_points(points_rejet, carte_QMNA5[index_proche,], pairwise = TRUE)

# Extraire le point sur la ligne
points_finaux <- lwgeom::st_endpoint(points_sur_lignes) 

# Table de comparaison milieu récepteur - A vérifier 

table_comparaison_projection <- cbind(points_rejet,index_proche, distance_projection) |> 
  mutate(nom_milieu_recepteur_projete = carte_QMNA5$TOPONYME1[index_proche],
         QMNA5_j = carte_QMNA5$QMNA5_j[index_proche]) |> 
  select(-index_proche) |> 
  relocate(QMNA5_j, .after = last_col()) |> 
  st_drop_geometry()

traitement_03<-table_comparaison_projection


## 2.3. CALCUL DE LA PEC  ----

#' L'évaluation de la significativité de la pression dépend de l'état de
#' départ de la masse d'eau réceptrice. 
#' On va intersecter le point de rejet avec le bassin versant pour récupérer
#' l'état de la masse d'eau réceptrice. 

etat_BV <- traitement_02 |> 
  select(rejet) |> 
  st_join(carte_EDL_BV) |> 
  st_drop_geometry() |>
  select(rejet, EtatEcoMe) |> 
  distinct() 

#' On va rapprocher le QMNA5_j et la mesure des rejets dans le SRR pour calculer 
#' la PEC (Predicted Environmental Concentration) = calcul de dilution.

traitement_04<-flux_SRR_PM_export |> 
  filter(pt_AS %in% ech_SRR$pt_AS) |> 
  left_join(traitement_03, by = c("site_origine",
                                      "pt_AS" )) |> 
  mutate(PEC = (Flux_j/QMNA5_j)*10^6,
         unite_PEC = ifelse(unite_flux == "kg", "mg/m3","A vérifier")) |> 

  # mutate(intens_pression_micro = case_when(!is.na(seuil_NQE_micro) & PEC < seuil_NQE_micro/10 ~ "-", 
  #                                          !is.na(seuil_NQE_micro) & PEC >= seuil_NQE_micro/10  &
  #                                            !is.na(seuil_NQE_micro) & PEC < seuil_NQE_micro  ~ "Moyen",
  #                                          !is.na(seuil_NQE_micro) & PEC >= seuil_NQE_micro ~ "Fort",
  #                                          is.na(seuil_NQE_micro) ~ "Pas de seuil de NQE pour comparaison",
  #                                          TRUE ~ "A vérifier"
  # ),
  # pourcentage_pression_micro = round(PEC/seuil_NQE_micro*100, digits = 2)
  # )

  left_join(etat_BV, by = "rejet") |> 
  left_join(NQE, by = "code_sandre_parametre") |>
  mutate(intens_pression = case_when(
    # Cas 0 : hors classe
    is.na(CATEGORIE) | is.na(PEC) ~ NA,
    
    # Cas 1 : l'état écologique est bon ou très bon et la PEC est inférieure à 50% du seuil : pression faible
    CATEGORIE != "Etat CHIMIQUE" & !is.na(EtatEcoMe) & EtatEcoMe %in% c("Bon","Tres bon") & PEC < 0.5*NQE ~  "-",
    
    # # Cas 2 : l'état écologique est bon ou très bon et la PEC est supérieure à 50% du seuil : pression forte
    CATEGORIE != "Etat CHIMIQUE" & !is.na(EtatEcoMe) & EtatEcoMe %in% c("Bon","Tres bon") & PEC >= 0.5*NQE ~  "Fort",
    
    # Cas 3 : pour tous les autres états écologiques, la PEC est inférieure à 10 % du seuil : pression faible
    PEC < 0.1*NQE ~  "-",
    
    # # Cas 4 : pour tous les autres états écologiques, la PEC est comprise entre 10 % et 50 % du seuil : pression moyenne
    PEC >= 0.1*NQE & PEC < 0.5*NQE ~  "Moyen",
    
    # Cas 5 : pour tous les autres états écologiques, la PEC est comprise entre 10 % et 50 % du seuil : pression forte
    PEC >= 0.5*NQE ~  "Fort",
    
    # Cas 6 : tous les autres cas
    TRUE ~ "A vérifier" ),
    
    pourcentage_pression = round(PEC/NQE*100, digits = 2),
    
    # On crée un indice d'existence de pression
    exist_pression_significative = ifelse(intens_pression %in% c("Moyen", "Fort"),"Oui","Non")
    
  )



# 3. IDENTIFICATION DES REJETS AVEC DES PRESSIONS SIGNIFICATIVES ----
rejet_avec_PS <- traitement_04 |> 
  select(site_origine,rejet,date,code_sandre_parametre,lib_parametre,intens_pression,exist_pression_significative) |> 
  filter(exist_pression_significative == "Oui") 


frequence_PS<-rejet_avec_PS|> 
  mutate(annee = year(date)) |> 
  group_by(site_origine,rejet,annee,code_sandre_parametre,lib_parametre) |> 
  summarize(nb_jours_PS = n()) 

param_declass <-frequence_PS |> 
  mutate(param_freq = paste0(lib_parametre, " (", nb_jours_PS,")")) |> 
  group_by(rejet,annee) |>
  summarize(parametre_freq_PS = list(param_freq)) |> 
  mutate_if(is.list, ~ if(is.data.frame(.x)) do.call(paste, .x) else map_chr(.x, toString)) |> 
  # La ligne précédente permet de passer d'un vecteur dans une liste à une valeur de type character dans un vecteur
  pivot_wider(id_cols = rejet,
              names_from = annee,
              values_from = parametre_freq_PS)

# 4. REPRESENTATION GRAPHIQUE ----
## 4.1. AJOUT A LA CARTE ----
couche_carto_pression <- traitement_02 |> 
  select(-DT,-TYPE) |> 
  filter(rejet %in% param_declass$rejet) |> 
  left_join(param_declass, by = "rejet") |> 
  relocate(geometry, .after=last_col())

## 4.2. PRODUCTION DE GRAPHIQUES ----
# On ne produit des graphes que pour les couples rejets - paramètres en PS
rejet_param_declass<-frequence_PS |> 
  ungroup() |> 
  select(rejet,code_sandre_parametre) |> 
  distinct()

traitement_07<-traitement_04 |> 
  select(site_origine,nom_site_origine,nom_pt_AS,rejet,date,code_sandre_parametre,lib_parametre,Fiabilite_flux,PEC) |> 
  filter(rejet %in% rejet_param_declass$rejet & code_sandre_parametre %in%rejet_param_declass$code_sandre_parametre) |> 
  mutate(Fiabilite_flux = as.factor(Fiabilite_flux))


for (k in 1:nrow(rejet_param_declass)) # erreur sur k = 11 : investiguer. 
{
  
  traitement_08 <-traitement_07 |> 
    filter(rejet == rejet_param_declass$rejet[k] & code_sandre_parametre == rejet_param_declass$code_sandre_parametre[k])
  
  NQE = NQE$NQE[which(NQE$code_sandre_parametre==rejet_param_declass$code_sandre_parametre[k])]
  NQE_10 = NQE/10
  NQE_50 = NQE/2
  
  graphe <- ggplot(traitement_08,aes(x=date,y=PEC, colour=Fiabilite_flux))+
    geom_point(aes(color = Fiabilite_flux, alpha = 0.6))+
    scale_colour_manual(values=c("1"="orange","2"="yellowgreen", "3"="darkgreen"))+
    geom_line(color="gray",size=0.02)+
    scale_x_date(breaks = waiver(), date_breaks="1 year",date_labels = "%Y")+
    theme(
      axis.text.x = element_text(hjust = 0, size = 6),
      axis.text.y = element_text(hjust = 1, size = 6),
      axis.title.x = element_text(size = 6, face="bold", colour="black"),
      axis.title.y = element_text(size = 6, face="bold", colour="black"),
      axis.line = element_line(color = "black"),  # Couleur des lignes des axes
      axis.text = element_text(color = "black"),  # Couleur du texte des axes
      axis.ticks = element_line(color = "black"), # Couleur des ticks des axes
      panel.background = element_blank(),         # Retirer le fond gris
      plot.background = element_blank(),           # Retirer le fond du graphique
      panel.grid.minor = element_blank(),
      panel.grid.major=element_line(color="lightgrey")
    ) +
    labs(title = traitement_08$nom_site_origine[1],
         subtitle = paste0(traitement_08$nom_pt_AS[1], " - rejet : ", traitement_08$rejet[1],"\n Paramètre : ",traitement_08$lib_parametre),
         x = "Date",
         y = "Concentration de polluant induit par le rejet dans le milieu (mg / m3)")+
    new_scale_color() +
    scale_colour_manual(name="", # Nouveau titre de la légende
                        values=c("NQE"="green4","NQE_10"="darkorange", "NQE_50"="purple"),
                        labels = c( "NQE"="NQE ou seuil de bon état",
                                    "NQE_10" = "10 % NQE (limite pression significative)",
                                    "NQE_50" = "50 % NQE ou seuil de bon état")
    )+
    geom_hline(aes(yintercept = NQE, color="NQE"), linetype="dashed", linewidth = 0.7)+
    geom_hline(aes(yintercept = NQE_10, color="NQE_10"), linetype="dashed", linewidth = 0.7)+
    geom_hline(aes(yintercept = NQE_50, color="NQE_50"), linetype="dashed", linewidth = 0.7)+

    # # Legend
    guides(size = "none") + # Supprime la légende pour la taille des points, ne laissant que la légende pour la couleur.
    theme(legend.position = "bottom", legend.text = element_text(size = 8),
          legend.key.size = unit(0.5, "cm"),
          legend.key.height = unit(0.5, "cm"),  # Ajuster la hauteur des cases dans la légende
          legend.box.spacing = unit(0.2, "cm"), # Ajuste l'espacement entre le bloc légende et le graphe
          legend.spacing.x = unit(0.2, "cm"),  # Réduire l'espacement horizontal entre les éléments de la légende
          legend.spacing.y = unit(0.1, "cm"),  # Réduire l'espacement vertical entre les éléments de la légende)
          legend.background = element_rect(fill = "white", color = "grey", size = 0)
    )
  
  print(graphe)
  
  ggsave(file.path(paste0("06_figs/",rejet_param_declass$rejet[k],"_",rejet_param_declass$code_sandre_parametre[k],".png")),
         width = 12,
         height = 10,
         units = "cm")
  
  
}






# CARTE TEST ----
carte_demo<-mapview(carte_EDL_BV,
        label = "NomMasseDE",
        col.region = "orchid",
        alpha.region=0.25, # Transparence
        map.types="OpenStreetMap")+
  mapview(carte_QMNA5,
          label = "TOPONYME1",
          color = "blue3")+
  mapview(points_rejet,
          label = "nom_rejet",
          color = "black")+
  mapview(points_finaux,
          label = "nom_rejet",
          color = "darkgreen")+
  mapview(couche_carto_pression,
          color = "red",
          lwd = 3
  )+
  mapview(points_sur_lignes,
          color = "darkgrey")
# mapview(test2,
#         label = "rejet",
#         color = "black")


carte_demo

saveRDS(carte_demo,"carte_demo.rds")
# Pour charger la carte dans R 
# readRDS("carte_demo.rds")

