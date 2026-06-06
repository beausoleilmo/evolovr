#' Fonction pour augmenter la précision à chaque itération
#' Utilisation de w (compteur) comme valeur qui va augmenter la précision
#' @param x Nombre
#'
#' @description
#' Retourne valeur d'une fonction (m1 : 1/x, m2 : exponentielle, m3 :  1/x^2)
#'
#' @returns
#' Valeur de sortie selon calcul
#' @export
#'
#' @examples
#' more_prec(1:10)
#'
#' # Graphique pour montrer la diminution des valeurs de x.
#' matplot(do.call(cbind, more_prec(1:10)),
#'   type = 'l',
#'   xlab = "Indice",
#'   ylab = "Valeurs", main = "more_prec")
#' legend("topright",
#'   legend = c("m1", "m2", "m3"),
#'   col = 1:3,
#'   lty = 1:3,
#'   bty = "o")

more_prec <- function(x) {
  m1 <- 2 * x^(-1) / 10 # 2* pour aller plus vite (faster decay)
  m2 <- exp(-x)
  m3 <- (x + 1)^-2
  # m4 = -log(x)
  # m5 = -sqrt(x)
  return(list(
    m1 = m1,
    m2 = m2,
    m3 = m3
  ))
}
