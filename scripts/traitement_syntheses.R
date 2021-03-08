library(tidyverse)

# Lecture du fichier texte
syntheses <- readLines(con = "raw_data/5590_4_synthese.csv")

# Création d'une liste contenant un élément par station et nommé d'après le code station
liste <- scinder_syntheses(syntheses = syntheses) %>%
  nommer_liste()

# essais sur un des éléments de la liste
ma_synthese <- liste["J0621610"] %>%
  unlist()

# -------------------------------------------------------------
# ecoulements mensuels
# -------------------------------------------------------------
nb_lignes <- 4

em_txt <- extraire_bloc(synthese = ma_synthese,
                        pattern_debut = "Ecoulements mensuels",
                        nb_lignes = nb_lignes)

noms_colonnes <- c('Jan', 'Fév', 'Mars', 'Avr', 'Mai', 'Juin', 'Juil', 'Aout', 'Sept', 'Oct', 'Nov', 'Dec')

noms_lignes <- map(.x = em_txt[2:nb_lignes],
                   .f = function(x) x[1]) %>%
  unlist()

em <- parser_bloc(bloc = em_txt,
                  noms_colonnes = noms_colonnes,
                  noms_lignes = noms_lignes,
                  total_annuel = TRUE)

rm(em_txt)

# -------------------------------------------------------------
# modules interannuels
# -------------------------------------------------------------
nb_lignes <- 2
mi_txt <- extraire_bloc(synthese = ma_synthese,
                        pattern_debut = "Modules interannuels",
                        nb_lignes = nb_lignes)

# noms des colonnes. Nb il y a les intervalles de confiance => 3 valeurs / nom
noms_colonnes <- mi_txt[[1]] %>%
  .[.!=''] %>%
  suffixer_colonnes()# suppression des vides

noms_lignes <- map(.x = mi_txt[2:nb_lignes],
                   .f = function(x) x[1]) %>%
  unlist()

mi <- parser_bloc(bloc = mi_txt,
                  noms_colonnes = noms_colonnes,
                  noms_lignes = noms_lignes)

rm(mi_txt)

# données module (moyenne)
mm_txt <- extraire_bloc(synthese = ma_synthese,
                        pattern_debut = "Module (moyenne)",
                        nb_lignes = 0)

noms_colonnes <- mm_txt[[2]][1] %>%
  .[.!=''] %>%
  suffixer_colonnes() # suppression des vides

noms_lignes <- "Débits (m3/s)"

mm <- parser_bloc(bloc = mm_txt,
                  noms_colonnes = noms_colonnes,
                  noms_lignes = noms_lignes)

rm(mm_txt)

# -------------------------------------------------------------
# Basses eaux
# -------------------------------------------------------------
# Comme les 2 premières ligens ne contiennent pas le même nb d'éléments que
# les suivantes, besoin de procéder en deux temps

### Lignes du haut
nb_lignes <- 3
be_txt <- extraire_bloc(synthese = ma_synthese,
                        pattern_debut = "Basses eaux",
                        nb_lignes = nb_lignes)

# noms des colonnes. Nb il y a les intervalles de confiance => 3 valeurs / nom
noms_colonnes <- be_txt[[1]] %>%
  .[.!=''] %>% # suppression des vides
  suffixer_colonnes()

noms_lignes <- map(.x = be_txt[2:nb_lignes],
                   .f = function(x) x[1]) %>%
  unlist()

be <- parser_bloc(bloc = be_txt,
                  noms_colonnes = noms_colonnes,
                  noms_lignes = noms_lignes)

### Lignes du bas
nb_lignes <- 5
be_txt <- extraire_bloc(synthese = ma_synthese,
                        pattern_debut = "Basses eaux",
                        nb_lignes = nb_lignes) %>%
  .[c(1, 4, 5)]

# noms des colonnes. Nb il y a les intervalles de confiance => 3 valeurs / nom
noms_colonnes <- be_txt[[1]] %>%
  .[.!='']

noms_lignes <- map(.x = be_txt[2:length(be_txt)],
                   .f = function(x) x[1]) %>%
  unlist()

be2 <- parser_bloc(bloc = be_txt,
                  noms_colonnes = noms_colonnes,
                  noms_lignes = noms_lignes)

rm(be_txt)

# -------------------------------------------------------------
# Crues
# -------------------------------------------------------------
# Comme les 2 premières ligens ne contiennent pas le même nb d'éléments que
# les suivantes, besoin de procéder en deux temps

### Lignes du haut
nb_lignes <- 3
cr_txt <- extraire_bloc(synthese = ma_synthese,
                        pattern_debut = "Crues",
                        nb_lignes = nb_lignes)

# noms des colonnes. Nb il y a les intervalles de confiance => 3 valeurs / nom
noms_colonnes <- cr_txt[[1]] %>%
  .[.!=''] # suppression des vides

noms_lignes <- map(.x = cr_txt[2:nb_lignes],
                   .f = function(x) x[1]) %>%
  unlist()

cr <- parser_bloc(bloc = cr_txt,
                  noms_colonnes = noms_colonnes,
                  noms_lignes = noms_lignes)

### Lignes du bas
# On retient 8 lignes car la centennale n'est jamais remplie
nb_lignes <- 8
cr_txt <- extraire_bloc(synthese = ma_synthese,
                        pattern_debut = "Crues",
                        nb_lignes = nb_lignes) %>%
  .[c(1, 4:8)]

# noms des colonnes. Nb il y a les intervalles de confiance => 3 valeurs / nom
noms_colonnes <- cr_txt[[1]] %>%
  .[.!=''] %>%
  suffixer_colonnes()


noms_lignes <- map(.x = cr_txt[2:length(cr_txt)],
                   .f = function(x) x[1]) %>%
  unlist()

cr2 <- parser_bloc(bloc = cr_txt,
                   noms_colonnes = noms_colonnes,
                   noms_lignes = noms_lignes)

rm(cr_txt)

# -------------------------------------------------------------
# Maximums connus
# -------------------------------------------------------------
nb_lignes <- 2
dc_txt <- extraire_bloc(synthese = ma_synthese,
                        pattern_debut = "Débits classés",
                        nb_lignes = nb_lignes)

# noms des colonnes. Nb il y a les intervalles de confiance => 3 valeurs / nom
noms_colonnes <- dc_txt[[1]] %>%
  .[.!=''] %>%
  .[2:(length(dc_txt[[1]])-1)]

noms_colonnes <- paste0("f_", noms_colonnes)

noms_lignes <- map(.x = dc_txt[2:nb_lignes],
                   .f = function(x) x[1]) %>%
  unlist()

dc <- parser_bloc(bloc = dc_txt,
                  noms_colonnes = noms_colonnes,
                  noms_lignes = noms_lignes)

rm(dc_txt)

sortie <- list(em, mi, mm, be, be2, cr, cr2, dc)


