#' @title Procedury skalowania egzaminow.
#' @description
#' Funkcja przeprowadza skalowanie wyników sprawdzianu.
#' @param daneWzorcowe data frame zawierający dane do skalowania wzorcowego
#' @param daneWszyscy data frame zawierający dane wszystkich zdających (do wyliczenia
#' oszacowań umiejętności na podstawe parametrów ze skalowania wzorcowego)
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return
#' lista z elementami:
#' \itemize{
#'   \item{\code{usunieteKryteria} wektor tekstowy z nazwami (pseudo)kryteriów, które
#'         zostały usunięte podczas skalowania wzorcowego.}
#'   \item{\code{parametry} data frame z wyestymowanymi parametrami modelu w jego
#'         ostatecznej postaci (tj. takiej, jak w ostatnim kroku skalowania wzorcowego
#'         i w jedynym kroku skalowania na wszystkich zdających).}
#'   \item{\code{oszacowania} data frame zawierający id_obserwacji i wyliczone
#'         oszacowania umiejętności dla wszystkich zdających.}
#'   \item{\code{rzetelnoscEmpiryczna} rzetelność wyliczona na podstawie oszacowań ze
#'         skalowania wzorcowego (jako wariancja oszacowań EAP).}
#' }
#' @seealso \code{\link{skaluj}}, \code{\link{procedura_1k_1w}}
#' @examples
#' # chwilowo brak
#' @export
skaluj_spr = function(daneWzorcowe, daneWszyscy, processors=2) {
  # sprawdzanie, czy zgadzają sie zestawy (pseudo)kryteriów
  zmienneKryteria = list(
    wzorcowe = names(daneWzorcowe)[grepl("^[kp]_[[:digit:]]+$", names(daneWzorcowe))],
    wszyscy  = names(daneWszyscy )[grepl("^[kp]_[[:digit:]]+$", names(daneWszyscy ))])
  if (!all(zmienneKryteria$wzorcowe %in% zmienneKryteria$wszyscy) |
        !all(zmienneKryteria$wszyscy %in% zmienneKryteria$wzorcowe)) {
    stop("Niezgodność zestawu zmiennych do skalowania pomiędzy argumentami 'daneWzorcowe' i 'daneWszyscy'.")
  }
  # wyciągnimy rok do nazw plików i podpisów
  if ("rok" %in% names(daneWzorcowe)) {
    if (length(unique(daneWzorcowe$rok)) == 1) {
      tytulWzorcowe = paste0("spr", daneWzorcowe$rok[1], " wzor")
      tytulWszyscy  = paste0("spr", daneWzorcowe$rok[1], " wszyscy")
    } else {
      tytulWzorcowe = "spr wzor"
      tytulWszyscy  = "spr wszyscy"
    }
  } else {
    tytulWzorcowe = "spr wzor"
    tytulWszyscy  = "spr wszyscy"
  }
  # wyrzucamy wszystko, co niepotrzebne do skalowania (rypanie po dysku zajmuje potem cenny czas)
  zmienneKryteria = zmienneKryteria[[1]]
  daneWzorcowe = daneWzorcowe[, c("id_obserwacji", "id_testu", zmienneKryteria)]
  daneWszyscy  =  daneWszyscy[, c("id_obserwacji", "id_testu", zmienneKryteria)]

  # skalowanie jako takie
  message("\n### Skalowanie wzorcowe ###\n")
  opisWzorcowe = procedura_1k_1w(zmienneKryteria, "s", processors=processors)
  sprWzorcowe  = skaluj(daneWzorcowe, opisWzorcowe, "id_obserwacji", tytul=tytulWzorcowe, zmienneDolaczaneDoOszacowan="id_testu")
  # wyliczanie rzetelności empirycznej
  rzetelnoscEmpiryczna = sprWzorcowe[[1]][[length(sprWzorcowe[[1]])]]$zapis[["s"]]
  rzetelnoscEmpiryczna = var(rzetelnoscEmpiryczna)

  message("\n### Wyliczanie oszacowań dla wszystkich zdających ###\n")
  wartosciZakotwiczone = sprWzorcowe[[1]][[length(sprWzorcowe[[1]])]]$parametry$surowe
  wartosciZakotwiczone = wartosciZakotwiczone[!(wartosciZakotwiczone$typ %in% c("mean", "variance")), ]
  zmienneKryteriaPoUsuwaniu = wartosciZakotwiczone$zmienna2[wartosciZakotwiczone$typ == "by"]
  opisWszyscy  = procedura_1k_1w(zmienneKryteriaPoUsuwaniu, "s", wartosciZakotwiczone, processors=processors)
  sprWszyscy   = skaluj(daneWszyscy , opisWszyscy , "id_obserwacji", tytul=tytulWszyscy , zmienneDolaczaneDoOszacowan="id_testu")

  return(list(
    usunieteKryteria = zmienneKryteria[!(zmienneKryteria %in% zmienneKryteriaPoUsuwaniu)],
    parametry = wartosciZakotwiczone,
    oszacowania = sprWszyscy[[1]][[length(sprWszyscy[[1]])]]$zapis,
    rzetelnoscEmpiryczna = rzetelnoscEmpiryczna
  ))
}
