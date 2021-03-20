#' Mettre en couleur les colonnes des valeurs estimées
#'
#' @param ft Une flextable
#' @param couleur Caractère. Couleur choisie
#'
#' @return Une flextable mise en forme
#' @export
#'
#' @importFrom stringr str_subset
#' @importFrom flextable color
#'
#' @examples \dontrun{
#' color_est (ft, couleur  ="red")
#' }
color_est <- function(ft, couleur) {

  noms_colonnes <- ft %>%
    .[["header"]] %>%
    .[["dataset"]] %>%
    names() %>%
    stringr::str_subset(pattern = "est")

  color(ft, j = noms_colonnes, color = couleur, part = "all")

}



mef <- function(ft)

{
  ft %>%
    theme_zebra() %>%
    align(align = "center", part = "all")
}









