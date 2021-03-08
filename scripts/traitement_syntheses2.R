rm(list=ls())

unloadNamespace("hydrodataFR")
#rm(list="print.object", envir = get(".__S3MethodsTable__.", envir = baseenv()))

#detach("package:hydrodataFR", unload=TRUE)
library(hydrodataFR)

syntheses <- bh_sy_parser(fichier = "raw_data/5590_4_synthese.csv")

bh_sy_ind_sta(syntheses = syntheses,
              sta_id = "J0626610",
              indicateur = "em")

ma_station <- "J0626610"

bh_sy_ind_sta(syntheses = syntheses,
              sta_id = ma_station,
              indicateur = "em") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("Mois") %>%
  mutate(Mois = fct_inorder(Mois)) %>%
  ggplot(aes(x = Mois, y = `Débits (m3/s)`)) +
      geom_bar(stat = "identity", fill = "#54b5c5") +
      labs(x = "", title = ma_station)

prov <- donnees %>%
  arrange(frequence) %>%
         frequence_non_cumulee = frequence - lag(frequence, 1),
         debit_diff = (`Débit (m3/s)` - lag(`Débit (m3/s)`, 1)),
         hauteur_barre = frequence_non_cumulee / debit_diff) %>%
  group_by()


ggplot(prov, aes(x = debit_moy, y = frequence_non_cumulee)) +
  geom_histogram(aes(y=..density..))

