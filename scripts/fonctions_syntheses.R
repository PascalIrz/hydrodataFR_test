# -------------------------------------------------------------
# fonction interne pour récupérer la synthèse d'une station en texte
# -------------------------------------------------------------

get_1_sta <- function(syntheses, indexes_debut, indexes_fin, i, nom_liste = "liste") {

  sta <- list()

  sta_data <- syntheses[indexes_debut[i]:indexes_fin[i]] #%>%
  # as.list()

  sta[[1]] <- sta_data

  sta_id <- sta_data[[2]] %>%
    strsplit(";") %>%
    .[[1]] %>%
    .[[1]]

  sta[[2]] <- sta_id

  sta

}

# yo <- get_1_sta(syntheses = syntheses,
#                 indexes_debut = indexes_debut,
#                 indexes_fin = indexes_fin,
#                 i = 1)
# -------------------------------------------------------------
# fonction pour scinder les synthèses par station
# -------------------------------------------------------------

scinder_syntheses <- function(syntheses) {

  indexes_debut <- syntheses %>%
    stringi::stri_detect_fixed(pattern = "Code station") %>%
    which()

  nb_stations <- length(indexes_debut)

  indexes_fin <- (indexes_debut[2:nb_stations] - 1) %>%
    c(length(syntheses))

  map(.x = 1:nb_stations,
      .f = get_1_sta,
      syntheses = syntheses,
      indexes_debut = indexes_debut,
      indexes_fin = indexes_fin)

}

#liste <- scinder_syntheses(syntheses = syntheses)

# -------------------------------------------------------------
# fonction interne de nommage des synthèses par les codes stations
# -------------------------------------------------------------

nommer_liste <- function(liste) {

  noms_liste <- map(.x = liste,
                    .f = function(synthese) synthese[[2]]) %>%
    unlist()

  names(liste) <- noms_liste

  map(.x = liste,
      .f = function(synthese) synthese[1])

}

#liste <- nommer_liste(liste)

# -------------------------------------------------------------
# Extraire un bloc de la synthèse d'une station
# Le nb de lignes s'entend ligne de titre déduite
# -------------------------------------------------------------

# extraire_bloc <- function(synthese, pattern_debut, nb_lignes) {
#
#   ligne_debut <- synthese %>%
#     stringi::stri_detect_fixed(pattern = pattern_debut) %>%
#     which()
#
#   data <- synthese[(1 + ligne_debut):(nb_lignes + ligne_debut)] %>%
#     str_split(pattern = ";")
#
# }


extraire_bloc2 <- function(synthese, pattern_debut, nb_lignes) {

  ligne_debut <- synthese %>%
    stringi::stri_detect_fixed(pattern = pattern_debut) %>%
    which()

  data <- synthese[(1 + ligne_debut):(nb_lignes + ligne_debut)] %>%
    str_split(pattern = ";") %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()

}

# -------------------------------------------------------------
# Mettre en forme un des dataframes thématiques d'une station
# -------------------------------------------------------------

mef_df <- function(df, noms_colonnes, noms_lignes, suffixer = FALSE)

{
  df <- df %>%
    slice(2:n()) %>%
    mutate_all(as.numeric) %>%
    purrr::set_names(noms_colonnes) %>%
    purrr::discard( ~ all(is.na(.)))

  if(length(df) == 0)  {df <- NA

        }else{

  nb_cols <- length(df)

      if(suffixer == TRUE)

          {

    noms_colonnes_candidats <- noms_colonnes %>%
      .[.!=''] %>%
      suffixer_colonnes()

          df <- df %>%
              purrr::set_names(noms_colonnes_candidats[1:length(df)])

          }

  if('Année' %in% names(df))

          {

          df <- df %>% select(-`Année`)

          }

  df <- df %>%
    magrittr::set_rownames(noms_lignes)

  }

  df

}



# -------------------------------------------------------------
# Ajouter des suffixes aux noms de colonnes (cas des intervalles de confiance)
# -------------------------------------------------------------
suffixer_colonnes <- function(noms_colonnes, suffixes = c("est", "min", "max"))

  {

  map(.x = noms_colonnes,
      .f = paste,
      suffixes) %>%
    unlist()


}




# -------------------------------------------------------------
# -------------------------------------------------------------
# PARSER UNE STATION D'UN COUP
# -------------------------------------------------------------
# -------------------------------------------------------------
parser_1sta <- function(liste, sta_id) {

  ma_synthese <- liste[sta_id] %>%
    unlist()

  # -------------------------------------------------------------
  # ecoulements mensuels
  # -------------------------------------------------------------
  nb_lignes <- 4

  em <- extraire_bloc2(synthese = ma_synthese,
                       pattern_debut = "Ecoulements mensuels",
                       nb_lignes = nb_lignes)

  noms_colonnes <- em[1,]

  noms_lignes <- em %>%
    pull(V1) %>%
    .[.!=""]

  em <- em %>%
    mef_df(noms_colonnes = noms_colonnes,
           noms_lignes = noms_lignes)

  # -------------------------------------------------------------
  # modules interannuels
  # -------------------------------------------------------------
  nb_lignes <- 2
  mi <- extraire_bloc2(synthese = ma_synthese,
                       pattern_debut = "Modules interannuels",
                       nb_lignes = nb_lignes)

  noms_colonnes <- mi[1,]

  noms_lignes <- mi %>%
    pull(V1) %>%
    .[.!=""]

  mi <- mi %>%
    mef_df(noms_colonnes = noms_colonnes,
           noms_lignes = noms_lignes,
           suffixer = TRUE)

  # données module (moyenne)
  mm <- extraire_bloc2(synthese = ma_synthese,
                       pattern_debut = "Module (moyenne)",
                       nb_lignes = 0)

  # bidouillage noms col pour que le nb de noms corresponde au nb de colonnes
  noms_colonnes <- mm[2,1] %>%
    paste(c("est", "min", "max"))

  noms_colonnes <- c("V1", noms_colonnes, "V2")

  noms_lignes <- "Débits (m3/s)"

  mm <- mm %>%
    mef_df(noms_colonnes = noms_colonnes,
           noms_lignes = noms_lignes,
           suffixer = FALSE)

  # -------------------------------------------------------------
  # Basses eaux
  # -------------------------------------------------------------
  # Comme les 2 premières ligens ne contiennent pas le même nb d'éléments que
  # les suivantes, besoin de procéder en deux temps

  ### Lignes du haut
  nb_lignes <- 3
  be <- extraire_bloc2(synthese = ma_synthese,
                       pattern_debut = "Basses eaux",
                       nb_lignes = nb_lignes)

  noms_colonnes <- be[1,]

  noms_lignes <- be %>%
    #  as.data.frame() %>%
    pull(V1) %>%
    .[.!=""]

  be <- be %>%
    mef_df(noms_colonnes = noms_colonnes,
           noms_lignes = noms_lignes,
           suffixer = TRUE)

  ### Lignes du bas
  nb_lignes <- 5
  be2 <- extraire_bloc2(synthese = ma_synthese,
                        pattern_debut = "Basses eaux",
                        nb_lignes = nb_lignes) %>%
    .[c(1, 4, 5),]

  noms_colonnes <-  be2[1,]

  noms_lignes <- be2 %>%
    pull(V1) %>%
    .[.!=""]

  be2 <- be2 %>%
    mef_df(noms_colonnes = noms_colonnes,
           noms_lignes = noms_lignes,
           suffixer = FALSE)
  # -------------------------------------------------------------
  # Crues
  # -------------------------------------------------------------
  # Comme les 2 premières ligens ne contiennent pas le même nb d'éléments que
  # les suivantes, besoin de procéder en deux temps

  ### Lignes du haut
  nb_lignes <- 3
  cr <- extraire_bloc2(synthese = ma_synthese,
                       pattern_debut = "Crues",
                       nb_lignes = nb_lignes)

  noms_colonnes <- cr[1,]

  noms_lignes <- cr %>%
    pull(V1) %>%
    .[.!=""]

  cr <- cr %>%
    mef_df(noms_colonnes = noms_colonnes,
           noms_lignes = noms_lignes,
           suffixer = FALSE)


  ### Lignes du bas
  # On retient 8 lignes car la centennale n'est jamais remplie
  nb_lignes <- 8
  cr2 <- extraire_bloc2(synthese = ma_synthese,
                        pattern_debut = "Crues",
                        nb_lignes = nb_lignes) %>%
    .[c(1, 4:8),]

  noms_colonnes <- cr2[1,]

  noms_lignes <- cr2 %>%
    pull(V1) %>%
    .[.!=""] %>%
    .[.!="NA"]

  cr2 <- cr2 %>%
    mef_df(noms_colonnes = noms_colonnes,
           noms_lignes = noms_lignes,
           suffixer = TRUE)

  # -------------------------------------------------------------
  # Maximums connus
  # -------------------------------------------------------------
  nb_lignes <- 2
  dc <- extraire_bloc2(synthese = ma_synthese,
                       pattern_debut = "Débits classés",
                       nb_lignes = nb_lignes)

  noms_colonnes <- paste("f_", dc[1,])

  noms_lignes <- 'Débit (m3/s)'

  dc <- dc %>%
    mef_df(noms_colonnes = noms_colonnes,
           noms_lignes = noms_lignes,
           suffixer = FALSE)

  # -------------------------------------------------------------
  # assemblage par station
  # -------------------------------------------------------------
  sortie <- list(em, mi, mm, be, be2, cr, cr2, dc)

  sortie

}
