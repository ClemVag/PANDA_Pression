# DONNEES DE BASE ----
Desc_parametres <- data.frame(
  code_sandre_parametre = c("1301","1302","1305","1313","1314","1319","1335","1339","1340","1350","1551","1552","1553","1782"),
  lib_parametre = c("Temp. eau","pH","MES","DBO5","DCO","NKJ","NH4+","NO2-","NO3-","P total","NGL","Vol.Moy.J.","Haut.Préc.","Temps"),
  code_sandre_unite = c("27","264","162","175","175","168","169","171","173","177","168","120","184","203"),
  lib_sandre_unite = c("°C","unité pH","mg/L","mg(O2)/L","mg(O2)/L","mg(N)/L","mg(NH4)/L","mg(NO2)/L","mg(NO3)/L","mg(P)/L","mg(N)/L","m3/j","mm","min")
  
  
)

# FONCTIONS GENERALISTES ----
### FONCTION "NOT IN" ---- 
`%nin%` <- negate(`%in%`)

## MISE EN FORME FICHIERS EXCEL EN SORTIE ----
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


## ECRIRE ONGLET DANS FICHIER EXCEL ----

ecrire_onglet <- function(wb, nom, data) {
  if(!exists(wb)) stop("Ecriture arrêtée : veuillez créer le classeur (workbook) avant d'écrire un onglet")
  if(!exists(entete)) stop("Ecriture arrêtée : veuillez définir une mise en page pour l'en-tête (entete)")
  if(!exists(autres_lignes)) stop("Ecriture arrêtée : veuillez définir une mise en page pour le corps du fichier (autres_lignes)")
  
  addWorksheet(wb, nom)
  writeData(wb, nom, data, headerStyle = entete)
  addStyle(wb, nom, style = autres_lignes,
           rows = 2:(nrow(data) + 1), cols = 1:ncol(data),
           gridExpand = TRUE)
  setColWidths(wb, nom, cols = 1:ncol(data), widths = "auto")
  freezePane(wb, nom, firstRow = TRUE)
}
# QUALIFICATION DE LA FIABILITE DES DONNEES  ----
# Fiabilité d'un bilan = règle déjà utilisée sur le script Assainissement en 4.3.2
fiab_bilan <- function(qual_debit, qual_conc) {
  case_when(
    qual_debit == "Incorrecte" & qual_conc == "Incorrecte" ~ 0,
    qual_debit == "Incorrecte" | qual_conc == "Incorrecte" ~ 1,
    qual_debit == "Correcte"   & qual_conc == "Correcte"   ~ 3,
    TRUE ~ NA_real_)
}
# Qualité de l'ensemble des bilans qui servent de base à un ratio
qualite_base <- function(fiab) {
  case_when(
    all(fiab %in% 3)       ~ "correcte",
    all(fiab %in% 0)       ~ "incorrecte",
    any(fiab %in% c(0, 1)) ~ "mixte",
    TRUE                   ~ "non_qualifiee")
}

# COMBLEMENT DE DEBITS PAR INTERPOLATION -----
combler_debit <- function(brut, calendrier, lib) {
  calendrier %>%
    left_join(brut, by = c("Code_SANDRE", "PP_No_interne_Sitou", "Date_debut_prel")) %>%
    arrange(Code_SANDRE, PP_No_interne_Sitou, Date_debut_prel) %>%
    group_by(Code_SANDRE, PP_No_interne_Sitou) %>%
    mutate(
      Prop_manquants   = mean(is.na(Debit_m3j)),
      Debit_moy_annuel = if (all(is.na(Debit_m3j))) NA_real_ else mean(Debit_m3j, na.rm = TRUE),
      Debit_interpole  = if (sum(!is.na(Debit_m3j)) >= 2) {
        na.approx(Debit_m3j, x = Date_debut_prel, maxgap = GAP_MAX_JOURS, na.rm = FALSE)
      } else NA_real_
    ) %>%
    ungroup() %>%
    mutate(
      Cas_debit = case_when(
        !is.na(Debit_m3j)                                                ~ "mesure",
        Prop_manquants > SEUIL_PROP_MANQUANTS & !is.na(Debit_moy_annuel) ~ "moyenne",
        !is.na(Debit_interpole)                                          ~ "interpole",
        !is.na(Debit_moy_annuel)                                         ~ "moyenne",
        TRUE                                                             ~ "aucun"
      ),
      Debit_m3j = case_when(
        Cas_debit == "mesure"    ~ Debit_m3j,
        Cas_debit == "interpole" ~ Debit_interpole,
        Cas_debit == "moyenne"   ~ Debit_moy_annuel,
        TRUE                     ~ NA_real_
      ),
      Source_debit = unname(lib[Cas_debit])
    ) %>%
    select(Code_SANDRE, PP_No_interne_Sitou, Date_debut_prel,
           Debit_m3j, Qualif_debit, Source_debit)
}

# EDITION DE GRAPHIQUES  ----

## Graphique concentration d'un paramètre au fil du temps avec statistiques ----
# Ce graphique prend en compte la qualification des données (réelle ou préparée via PANDA)

graph_temporel_conc_stat <- function(data,
                                     data_stat,
                                     nom_colonne_point_AS,
                                     identifiant_point_AS,
                                     nom_colonne_code_sandre,
                                     code_sandre_parametre)
{ 
  data_graph <- data |> 
    filter(get(nom_colonne_point_AS) == identifiant_point_AS & 
             get(nom_colonne_code_sandre) == code_sandre_parametre) |> 
    select(Date_debut_prel,Val_resultat_analyse,Derniere_qualif_analyse)
  
  if(nrow(data_graph)==0)
  {break
    print(paste0("Aucune donnée sur ce point pour le paramètre",Desc_parametres$lib_parametre[which(Desc_parametres$code_sandre_parametre == code_sandre_parametre)]))}
  
  data_stat_graph<-data_stat |>
    filter(get(nom_colonne_point_AS) == identifiant_point_AS & 
             get(nom_colonne_code_sandre) == code_sandre_parametre)
  
  
  graphe<-ggplot(data_graph,aes(x=Date_debut_prel,y=Val_resultat_analyse, colour=Derniere_qualif_analyse))+
    geom_point(aes(size = Derniere_qualif_analyse))+
    geom_line(color="gray",size=0.5)+
    scale_x_date(breaks=datebreaks,labels=scales::date_format("%d/%m"), limits = c(debut_annee, fin_annee))+
    xlab("")+
    ylab("Concentration")+
    # Customize the color of the horizontal lines in the legend
    scale_colour_manual(name="", # Nouveau titre de la légende
                        values=c("Correcte"="green4",
                                 "Incertaine"="darkorange",
                                 "Incorrecte"="firebrick2", 
                                 "Moyenne"="slateblue2", 
                                 "Médiane"="turquoise4", 
                                 "Moyenne-2SD"="darkgreen", 
                                 "Moyenne+2SD"="darkgreen"),
                        labels = c( "Moyenne-2SD" = "Moyenne +/- 2 Standard Deviation", 
                                    "Moyenne+2SD" = "")) +
    scale_size_manual(values=c(1,1.5,1.5))+
    geom_hline(aes(yintercept = data_stat_graph$val_moy[1], colour="Moyenne"), linetype="dashed")+
    geom_hline(aes(yintercept = data_stat_graph$val_med[1], colour="Médiane"), linetype="dashed")+
    geom_hline(aes(yintercept = data_stat_graph$val_moy[1]-2*data_stat_graph$val_sd, colour="Moyenne-2SD"), linetype="dashed")+
    geom_hline(aes(yintercept = data_stat_graph$val_moy[1]+2*data_stat_graph$val_sd, color="Moyenne+2SD"), linetype="dashed", show.legend = FALSE)+
    guides(size = "none") + # Supprime la légende pour la taille des points, ne laissant que la légende pour la couleur.
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, size = 7),
      axis.line = element_line(color = "black"),  # Couleur des lignes des axes
      axis.text = element_text(color = "black"),  # Couleur du texte des axes
      axis.ticks = element_line(color = "black"), # Couleur des ticks des axes
      panel.background = element_blank(),         # Retirer le fond gris
      plot.background = element_blank(),           # Retirer le fond du graphique
      panel.grid.minor = element_blank(),
      panel.grid.major=element_line(color="lightgrey"),
      legend.position = "bottom", legend.text = element_text(size = 8), 
      legend.key.size = unit(0.5, "cm"), 
      legend.key.height = unit(0, "cm"),  # Ajuster la hauteur des cases dans la légende
      legend.box.spacing = unit(0, "cm"),
      legend.spacing.x = unit(0.2, "cm"),  # Réduire l'espacement horizontal entre les éléments de la légende
      legend.spacing.y = unit(0.1, "cm"),  # Réduire l'espacement vertical entre les éléments de la légende)
      legend.background = element_rect(fill = "white", color = "grey", size = 0)  
    )  
  
  
  
  graphe<- graphe +plot_annotation(title = paste0(identifiant_point_AS," - ",Desc_parametres$lib_parametre[which(Desc_parametres$code_sandre_parametre == code_sandre_parametre)] ))
  
  print(graphe)
}

## Graphique concentration d'un paramètre au fil du temps simple ----
# Ce graphique prend en compte la qualification des données (réelle ou préparée via PANDA)

graph_temporel_conc <- function(data,
                                nom_colonne_point_AS,
                                identifiant_point_AS,
                                nom_colonne_code_sandre,
                                code_sandre_parametre)
{ 
  data_graph <- data |> 
    filter(get(nom_colonne_point_AS) == identifiant_point_AS & 
             get(nom_colonne_code_sandre) == code_sandre_parametre) |> 
    select(Date_debut_prel,Val_resultat_analyse,Derniere_qualif_analyse)
  
  if(nrow(data_graph)==0)
  {break
  print(paste0("Aucune donnée sur ce point pour le paramètre",Desc_parametres$lib_parametre[which(Desc_parametres$code_sandre_parametre == code_sandre_parametre)]))}
 
  graphe<-ggplot(data_graph,aes(x=Date_debut_prel,y=Val_resultat_analyse, colour=Derniere_qualif_analyse))+
    geom_point(aes(size = Derniere_qualif_analyse))+
    geom_line(color="gray",size=0.5)+
    scale_x_date(breaks=datebreaks,labels=scales::date_format("%d/%m"), limits = c(debut_annee, fin_annee))+
    xlab("")+
    ylab("Concentration")+
    # Customize the color of the horizontal lines in the legend
    scale_colour_manual(name="", # Nouveau titre de la légende
                        values=c("Correcte"="green4",
                                 "Incertaine"="darkorange",
                                 "Incorrecte"="firebrick2"))+
    scale_size_manual(values=c(1,1.5,1.5))+
    guides(size = "none") + # Supprime la légende pour la taille des points, ne laissant que la légende pour la couleur.
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, size = 7),
      axis.line = element_line(color = "black"),  # Couleur des lignes des axes
      axis.text = element_text(color = "black"),  # Couleur du texte des axes
      axis.ticks = element_line(color = "black"), # Couleur des ticks des axes
      panel.background = element_blank(),         # Retirer le fond gris
      plot.background = element_blank(),           # Retirer le fond du graphique
      panel.grid.minor = element_blank(),
      panel.grid.major=element_line(color="lightgrey"),
      legend.position = "bottom", legend.text = element_text(size = 8), 
      legend.key.size = unit(0.5, "cm"), 
      legend.key.height = unit(0, "cm"),  # Ajuster la hauteur des cases dans la légende
      legend.box.spacing = unit(0, "cm"),
      legend.spacing.x = unit(0.2, "cm"),  # Réduire l'espacement horizontal entre les éléments de la légende
      legend.spacing.y = unit(0.1, "cm"),  # Réduire l'espacement vertical entre les éléments de la légende)
      legend.background = element_rect(fill = "white", color = "grey", size = 0)  
    )  
  graphe<- graphe +plot_annotation(title = paste0(identifiant_point_AS," - ",Desc_parametres$lib_parametre[which(Desc_parametres$code_sandre_parametre == code_sandre_parametre)] ))
  
  print(graphe)
}

## Graphique volume au fil du temps ----
graph_temporel_volume_avec_ref <- function(data,
                                           nom_colonne_point_AS,
                                           identifiant_point_AS,
                                           nom_colonne_code_sandre,
                                           ligne_ref=NULL)
{
  data_graph<- data |> 
    filter(get(nom_colonne_point_AS) == identifiant_point_AS & 
             get(nom_colonne_code_sandre) == "1552") |> 
    select(Date_debut_prel,Val_resultat_analyse,Derniere_qualif_analyse)
  
  
  
  graphe<-ggplot(data_graph,aes(x=Date_debut_prel,y=Val_resultat_analyse, colour=Derniere_qualif_analyse))+
    geom_line(color="gray",size=0.5)+
    geom_point(aes(size = Derniere_qualif_analyse))+
    scale_x_date(breaks=datebreaks,labels= scales::date_format("%d/%m"), limits = c(debut_annee, fin_annee))+
    xlab("")+
    ylab(paste0("Volume ",identifiant_point_AS," (m3/j)"))+
    scale_colour_manual(name="", # Nouveau titre de la légende
                        values=c("Correcte"="green4",
                                 "Incertaine"="darkorange",
                                 "Incorrecte"="firebrick2",
                                 "Débit de référence"="turquoise4"))+
    scale_size_manual(values=c(1,1,1.5))+
    guides(size = "none") + # Supprime la légende pour la taille des points, ne laissant que la légende pour la couleur.
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, size = 7),
      axis.line = element_line(color = "black"),  # Couleur des lignes des axes
      axis.text = element_text(color = "black"),  # Couleur du texte des axes
      axis.ticks = element_line(color = "black"), # Couleur des ticks des axes
      panel.background = element_blank(),         # Retirer le fond gris
      plot.background = element_blank(),           # Retirer le fond du graphique
      panel.grid.minor = element_blank(),
      panel.grid.major=element_line(color="lightgrey"),
      legend.position = "bottom", legend.text = element_text(size = 8), 
      legend.key.size = unit(0.5, "cm"), 
      legend.key.height = unit(0, "cm"),  # Ajuster la hauteur des cases dans la légende
      legend.box.spacing = unit(0, "cm"),
      legend.spacing.x = unit(0.2, "cm"),  # Réduire l'espacement horizontal entre les éléments de la légende
      legend.spacing.y = unit(0.1, "cm"),  # Réduire l'espacement vertical entre les éléments de la légende)
      legend.background = element_rect(fill = "white", color = "grey", size = 0)  
    )  
  
  if(!is.null(ligne_ref))
  {graphe <- graphe +
    geom_hline(aes(yintercept = get(ligne_ref), colour="Débit de référence"), linetype="dashed")}
  
  print(graphe) 
}


## Graphiques volumes / pluviométrie (A1 / A2 / A5)----
graphe_volume_pluviometrie<-function(data,
                                     nom_colonne_point_AS,
                                     identifiant_point_AS,
                                     nom_colonne_code_sandre)
{
  data_volume<- data |> 
    filter(get(nom_colonne_point_AS) == identifiant_point_AS & 
             get(nom_colonne_code_sandre) == "1552") |> 
    select(Date_debut_prel,Val_resultat_analyse,Derniere_qualif_analyse)

  
  data_pluvio <-data |> 
    filter(get(nom_colonne_code_sandre) == "1553") |> 
    select(Date_debut_prel,Val_resultat_analyse)
  
  graphe_volume <- ggplot(data_volume, aes(x = Date_debut_prel, y = Val_resultat_analyse)) +
    geom_point(aes(color = Derniere_qualif_analyse)) +  # Utilisation de 'color' sans 'values' ici
    scale_color_manual(values = c("Incorrecte" = "red", "Incertaine" = "darkorange","Correcte"=alpha("white",0))) +
    geom_line(color = "lightsteelblue4", size = 0.5) +
    scale_x_date(breaks = datebreaks, labels = scales::date_format("%m"), limits = c(debut_annee, fin_annee)) +
    theme_no_legend()+
    scale_y_continuous(expand = c(0, 0),limits = c(0,max(data_volume$Val_resultat_analyse)*1.05)) +
    xlab("") +
    ylab("Volume déversé (m3)")+
    theme(
      axis.text.x = element_blank(),
      axis.line = element_line(color = "black"),  # Couleur des lignes des axes
      axis.text = element_text(color = "black"),  # Couleur du texte des axes
      axis.ticks = element_line(color = "black"), # Couleur des ticks des axes
      panel.background = element_blank(),         # Retirer le fond gris
      plot.background = element_blank(),           # Retirer le fond du graphique
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "lightgrey"),
      legend.position = "none"
    ) 
  
  graphe_pluvio <- ggplot(data_pluvio,aes(x=Date_debut_prel,y=Val_resultat_analyse))+
    geom_line(color="royalblue1",size=0.5)+
    scale_x_date(breaks=datebreaks,labels=scales::date_format("%B"), limits = c(debut_annee, fin_annee))+
    scale_y_continuous(trans="reverse",expand=c(0,0),limits = c(0,max(data_pluvio$Val_resultat_analyse)*1.05))+
    theme_no_legend()+
    xlab("")+
    ylab("Précipitations (mm)")+
    theme(
      axis.text.x = element_blank(),
      axis.line = element_line(color = "black"),  # Couleur des lignes des axes
      axis.text = element_text(color = "black"),  # Couleur du texte des axes
      axis.ticks = element_line(color = "black"), # Couleur des ticks des axes
      panel.background = element_blank(),         # Retirer le fond gris
      plot.background = element_blank(),           # Retirer le fond du graphique
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "lightgrey"),
      legend.position = "none"
    ) 
 
  
  graphe_complet <- graphe_volume/graphe_pluvio + plot_annotation(
    title = paste0("Déversements du point ",identifiant_point_AS," et précipitations"))   
  
  print(graphe_complet)
}

## Graphiques déversements / pluviométrie (A1 / A2 / A5)----
graphe_nb_dev_selon_pluviometrie<-function(data,
                                     nom_colonne_point_AS,
                                     identifiant_point_AS,
                                     nom_colonne_code_sandre)
  {
  data_volume<- data |> 
    filter(get(nom_colonne_point_AS) == identifiant_point_AS & 
             get(nom_colonne_code_sandre) == "1552") |> 
    select(Date_debut_prel,Val_resultat_analyse)
  colnames(data_volume) = c("Date","vol")
  
   data_pluvio <-data |> 
    filter(get(nom_colonne_code_sandre) == "1553") |> 
    select(Date_debut_prel,Val_resultat_analyse)
  colnames(data_pluvio)=c("Date","pluvio")
  
  dates_annee <- data.frame(Date = seq.Date(from = debut_annee, to = fin_annee, by = "day"))
  

  data_dev_pluvio<-left_join(dates_annee, data_volume,by="Date") |> 
                    left_join(data_pluvio,by="Date")|> 
    na.omit() |> 
    mutate(classe_pluvio=case_when(pluvio == 0 ~ "0",
                                   pluvio <= 3 ~ "0-3",
                                   pluvio <= 6 ~ "3-6",
                                   pluvio <= 9 ~ "6-9",
                                   pluvio <= 12 ~ "9-12",
                                   pluvio <= 15 ~ "12-15",
                                   pluvio <= 18 ~ "15-18",
                                   pluvio <= 21 ~ "18-21",
                                   TRUE ~ "21+"
    )) |>
    mutate(dev=case_when(vol==0 ~ "non",
                         TRUE ~ "oui" ))
  
  
  data_dev_pluvio$classe_pluvio <- as_factor(data_dev_pluvio$classe_pluvio) 
  data_dev_pluvio$classe_pluvio <-data_dev_pluvio$classe_pluvio %>%
    fct_relevel(
      "0", "0-3", "3-6", "6-9", "9-12", "12-15", "15-18", "18-21",
      "21+"
    )
  
  
  data_dev_pluvio <- data_dev_pluvio %>%
    group_by(classe_pluvio) %>%
    summarise(Nb_pl = n()) %>%
    left_join(data_dev_pluvio %>%
                filter(vol > 0) %>%
                group_by(classe_pluvio) %>%
                summarise(Nb_dev = n()), by = "classe_pluvio")
  
  # Convertir les données en format long
  data_dev_pluvio_long <- data_dev_pluvio %>%
    pivot_longer(cols = c(Nb_pl, Nb_dev), 
                 names_to = "Variable", 
                 values_to = "Valeur") |>
    mutate(Variable = if_else(Variable == "Nb_pl", "Jours de pluie", "Jours de déversement"))
  
  
  cat("\n ")
  # Créer le graphique en barres
  graphe <-ggplot(data_dev_pluvio_long, aes(x = classe_pluvio, y = Valeur, fill = Variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(subtitle = "Analyse des déversements selon la hauteur de précipitation",
         x = "Classe Pluviométrie (mm)",
         y = "Nombre de jours",
         fill = "Légende") +
    scale_fill_manual(values=c("Jours de pluie"="royalblue1",
                               "Jours de déversement"="lightsteelblue4"))+
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_text(aes(label = Valeur), vjust = -0.3,size = 3, position = position_dodge(width=1))
  
  print(graphe)
  
  
  
}

