#' @title Procedury skalowania egzaminow.
#' @description
#' Funkcja przeprowadza skalowanie wyników egzaminu gimnazjalnego.
#'
#' Argumenty (poza ostatnim) muszą być listami, których elementy mają nazwy ze zbioru:
#' \code{"gh", "gh_h", "gh_p", "gm", "gm_p", "gm_m"} i są data frame'ami zawierającymi
#' dane z wynikami odpowiednich część egzaminu gimnazjalnego.
#'
#' W przypadku nowej struktury egzaminu gimnazjalnego wystarczy podać wyniki testów,
#' dane do wyskalowania części jako całości zostaną połączone automatycznie, wewnątrz
#' funkcji.
#' @param daneWzorcowe lista data frame'ów zawierających dane do skalowania wzorcowego
#' @param daneWszyscy lista data frame'ów zawierających zawierający dane wszystkich
#' zdających (do wyliczenia oszacowań umiejętności na podstawe parametrów ze skalowania
#' wzorcowego)
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return
#' lista z elementami:
#' \itemize{
#'   \item{\code{usunieteKryteria} lista wektorów tekstowych z nazwami (pseudo)kryteriów,
#'    które zostały usunięte podczas skalowania wzorcowego.}
#'   \item{\code{parametry} lista data frame'ów z wyestymowanymi parametrami modeli w ich
#'         ostatecznej postaci (tj. takiej, jak w ostatnim kroku skalowania wzorcowego
#'         i w jedynym kroku skalowania na wszystkich zdających).}
#'   \item{\code{oszacowania} lista data frame'ów zawierających id_obserwacji i wyliczone
#'         oszacowania umiejętności dla wszystkich zdających.}
#'   \item{\code{rzetelnoscEmpiryczna} rzetelność wyliczona na podstawie oszacowań ze
#'         skalowania wzorcowego (jako wariancja oszacowań EAP).}
#' }
#' @seealso \code{\link{skaluj}}, \code{\link{procedura_1k_1w}}
#' @examples
#' # chwilowo brak
#' @export
skaluj_egz_gimn = function(daneWzorcowe, daneWszyscy, processors=2) {
  stopifnot(length(daneWzorcowe) == length(daneWszyscy))
  stopifnot(!is.null(names(daneWzorcowe)), !is.null(names(daneWszyscy)))
  stopifnot(all(names(daneWzorcowe) %in% c("gh", "gh_h", "gh_p", "gm", "gm_p", "gm_m")),
            all(names(daneWszyscy ) %in% c("gh", "gh_h", "gh_p", "gm", "gm_p", "gm_m")))
  # wyciągnimy rok do nazw plików i podpisów
  lata = lapply(daneWzorcowe, function(x) {return(unique(x$rok))})
  if (length(unique(unlist(lata))) != 1) {
    stop("Dane pochodzą z różnych lat.")
  } else {
    rok = lata[[1]]
  }
  for (i in 1:length(daneWzorcowe)) {
    # sprawdzanie, czy zgadzają sie zestawy (pseudo)kryteriów
    zmienneKryteria = list(
      wzorcowe = names(daneWzorcowe[[i]])[grepl("^[kp]_[[:digit:]]+$", names(daneWzorcowe[[i]]))],
      wszyscy  = names(daneWszyscy[[i]] )[grepl("^[kp]_[[:digit:]]+$", names(daneWszyscy[[i]] ))])
    if (!all(zmienneKryteria$wzorcowe %in% zmienneKryteria$wszyscy) |
          !all(zmienneKryteria$wszyscy %in% zmienneKryteria$wzorcowe)) {
      stop("Niezgodność zestawu zmiennych do skalowania pomiędzy ", i, ". elementami argumentów 'daneWzorcowe' i 'daneWszyscy'")
    }
    # wyrzucamy wszystko, co niepotrzebne do skalowania (rypanie po dysku zajmuje potem cenny czas)
    zmienneKryteria = zmienneKryteria[[1]]
    daneWzorcowe[[i]] = daneWzorcowe[[i]][, c("id_obserwacji", zmienneKryteria)]
    daneWszyscy[[i]]  =  daneWszyscy[[i]][, c("id_obserwacji", zmienneKryteria)]
  }

  # ew. dopisywanie części zbierających po dwa testy z nowej formuły
  if (all(c("gh_h", "gh_p") %in% names(daneWzorcowe)) & !("gh" %in% names(daneWzorcowe))) {
    daneWzorcowe$gh = merge(daneWzorcowe$gh_h, daneWzorcowe$gh_p)
     daneWszyscy$gh = merge( daneWszyscy$gh_h,  daneWszyscy$gh_p)
  }
  if (all(c("gm_p", "gm_m") %in% names(daneWzorcowe)) & !("gm" %in% names(daneWzorcowe))) {
    daneWzorcowe$gm = merge(daneWzorcowe$gm_p, daneWzorcowe$gm_m)
     daneWszyscy$gm = merge( daneWszyscy$gm_p,  daneWszyscy$gm_m)
  }
  # przemieszczanie gh i gm na koniec list z danymi, żeby można było do nich wprowadzić poprawki (wyrzucić (pseudo)kryteria) na podstawie skalowania testów składowych
  kolejnosc = c(grep("^g[hm]_", names(daneWzorcowe)), grep("^g[hm]$", names(daneWzorcowe)))
  daneWzorcowe = daneWzorcowe[kolejnosc]
   daneWszyscy =  daneWszyscy[kolejnosc]
  # skalowanie jako takie
  wyniki = setNames(vector(mode="list", length=length(daneWzorcowe)), names(daneWzorcowe))
  for (i in 1:length(daneWzorcowe)) {
    tytulWzorcowe = paste0(names(daneWzorcowe)[i], rok, " wzor")
    tytulWszyscy  = paste0(names(daneWzorcowe)[i], rok, " wszyscy")
    zmienneKryteria = names(daneWzorcowe[[i]])[grepl("^[kp]_[[:digit:]]+$", names(daneWzorcowe[[i]]))]

    # sztuczka, żeby przy skalowaniu gh i gm w nowej formule już nie usuwał (pseudo)kryteriów
    if ( ((names(daneWzorcowe)[i] == "gh") & all(c("gh_h", "gh_p") %in% names(daneWzorcowe))) |
           ((names(daneWzorcowe)[i] == "gm") & all(c("gm_p", "gm_m") %in% names(daneWzorcowe))) ) {
      # dajemy tu data frame, żeby nie było usuwania kryteriów, ale wtedy trzeba zadać w nim wartość oczekiwaną i wariancję
      wartosciZakotwiczone = data.frame(typ=c("mean", "variance"), zmienna1=names(daneWzorcowe)[i], zmienna2="", wartosc=c(0, 1), stringsAsFactors=FALSE)
    } else {
      wartosciZakotwiczone = NULL
    }
    message("### Skalowanie wzorcowe ", names(daneWzorcowe)[i], " ###\n")
    opisWzorcowe = procedura_1k_1w(zmienneKryteria          , names(daneWzorcowe)[i], wartosciZakotwiczone, processors=processors)
    egWzorcowe   = skaluj(daneWzorcowe[[i]], opisWzorcowe, "id_obserwacji", tytul=tytulWzorcowe)
    # wyliczanie rzetelności empirycznej
    rzetelnoscEmpiryczna = egWzorcowe[[1]][[length(egWzorcowe[[1]])]]$zapis[[names(daneWzorcowe)[i]]]
    rzetelnoscEmpiryczna = var(rzetelnoscEmpiryczna)

    message("### Wyliczanie oszacowań dla wszystkich zdających ", names(daneWzorcowe)[i], " ###\n")
    wartosciZakotwiczone = egWzorcowe[[1]][[length(egWzorcowe[[1]])]]$parametry$surowe
    wartosciZakotwiczone = wartosciZakotwiczone[!(wartosciZakotwiczone$typ %in% c("mean", "variance")), ]
    zmienneKryteriaPoUsuwaniu = wartosciZakotwiczone$zmienna2[wartosciZakotwiczone$typ == "by"]
    if ( ((names(daneWzorcowe)[i] == "gh") & all(c("gh_h", "gh_p") %in% names(daneWzorcowe))) ) {
      usunieteKryteria = c(wyniki$gh_h$usunieteKryteria, wyniki$gh_p$usunieteKryteria)
    } else if ( (names(daneWzorcowe)[i] == "gm") & all(c("gm_p", "gm_m") %in% names(daneWzorcowe))) {
      usunieteKryteria = c(wyniki$gm_p$usunieteKryteria, wyniki$gm_m$usunieteKryteria)
    } else {
      usunieteKryteria = zmienneKryteria[!(zmienneKryteria %in% zmienneKryteriaPoUsuwaniu)]
    }

    opisWszyscy  = procedura_1k_1w(zmienneKryteriaPoUsuwaniu, names(daneWzorcowe)[i], wartosciZakotwiczone, processors=processors)
    egWszyscy    = skaluj(daneWszyscy[[i]] , opisWszyscy , "id_obserwacji", tytul=tytulWszyscy )

    wyniki[[i]] = list(
      usunieteKryteria = usunieteKryteria,
      parametry = wartosciZakotwiczone,
      oszacowania = egWszyscy[[1]][[length(egWszyscy[[1]])]]$zapis,
      rzetelnoscEmpiryczna = rzetelnoscEmpiryczna
    )
    # ew. wyrzucanie (pseudo)kryteriów z gh i gm na podstawie tego, co wyszło w poszczególnych testach
    if ( (names(daneWzorcowe)[i] %in% c("gh_h", "gh_p")) & ("gh" %in% names(daneWzorcowe)) ) {
      daneWzorcowe$gh = daneWzorcowe$gh[, !(names(daneWzorcowe$gh) %in% wyniki[[i]]$usunieteKryteria)]
      daneWzorcowe$gh = daneWzorcowe$gh[, !(names(daneWzorcowe$gh) %in% wyniki[[i]]$usunieteKryteria)]
    }
    if ( (names(daneWzorcowe)[i] %in% c("gm_p", "gm_m")) & ("gm" %in% names(daneWzorcowe)) ) {
      daneWzorcowe$gm = daneWzorcowe$gm[, !(names(daneWzorcowe$gm) %in% wyniki[[i]]$usunieteKryteria)]
      daneWzorcowe$gm = daneWzorcowe$gm[, !(names(daneWzorcowe$gm) %in% wyniki[[i]]$usunieteKryteria)]
    }
  }
  return(wyniki)
}
# Poniżej procedury z 2013 r., wobec zmiany ogólnych procedur pobierania i przetwarzania
# danych, obecnie bez znaczenia.
# #' @title Procedury skalowania egzaminow.
# #' @description
# #' Procedura skalowania egzaminu gimnazjalnego - w formule od 2012 r.
# #' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
# #' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
# #' @param parametryS0 data.frame z parametrami zadań ze sprawdzianu dla uczniów o standardowej długości toku kształcenia
# #' @param parametryS1 data.frame z parametrami zadań ze sprawdzianu dla uczniów o toku kształcenia wydłużonym o rok
# #' @param processors liczba rdzeni do wykorzystania przy estymacji
# #' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
# #' @seealso \code{\link{skaluj}}
# #' @examples
# #' # chwilowo brak
# #' @export
# procedura_eg = function(nazwyZmiennych, parametryS0, parametryS1, processors=3) return(list(
#   "sprawdzian" = list(
#     czescPomiarowa = list(
#       s0 = list(
#         zmienne = parametryS0$zmienna2[parametryS0$typ == "by" & parametryS0$zmienna1 == "s0"],
#         var1 = FALSE,
#         rasch = FALSE,
#         kryteriaUsuwania = NULL,
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = parametryS0[, c("typ", "zmienna1", "zmienna2", "wartosc")]
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   ),
#   "sprawdzian-drugoroczni" = list(
#     czescPomiarowa = list(
#       s1 = list(
#         zmienne = parametryS1$zmienna2[parametryS1$typ == "by" & parametryS1$zmienna1 == "s1"],
#         var1 = FALSE,
#         rasch = FALSE,
#         kryteriaUsuwania = NULL,
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = parametryS1[, c("typ", "zmienna1", "zmienna2", "wartosc")]
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   ),
#   "część pol.-hum." = list(
#     czescPomiarowa = list(
#       gh = list(
#         zmienne = nazwyZmiennych[grep("^gh_[hp](|_)[[:digit:]]", nazwyZmiennych)],
#         var1 = TRUE,
#         rasch = FALSE,
#         kryteriaUsuwania = list(
#           dyskryminacjaPonizej = 0.2,
#           istotnoscPowyzej = 1,
#           nigdyNieUsuwaj = NULL
#         ),
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = NULL
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   ),
#   "test pol." = list(
#     czescPomiarowa = list(
#       gh_p = list(
#         zmienne = list(~gh, gh = "^gh_p"),
#         var1 = TRUE,
#         rasch = FALSE,
#         kryteriaUsuwania = NULL,
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = NULL
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   ),
#   "test WOS-hist." = list(
#     czescPomiarowa = list(
#       gh_h = list(
#         zmienne = list(~gh, gh = "^gh_h"),
#         var1 = TRUE,
#         rasch = FALSE,
#         kryteriaUsuwania = NULL,
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = NULL
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   ),
#   "część mat.-przyr." = list(
#     czescPomiarowa = list(
#       gm = list(
#         zmienne = nazwyZmiennych[grep("^gm_[mp](|_)[[:digit:]]", nazwyZmiennych)],
#         var1 = TRUE,
#         rasch = FALSE,
#         kryteriaUsuwania = list(
#           dyskryminacjaPonizej = 0.2,
#           istotnoscPowyzej = 1,
#           nigdyNieUsuwaj = NULL
#         ),
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = NULL
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   ),
#   "test mat." = list(
#     czescPomiarowa = list(
#       gm_m = list(
#         zmienne = list(~gm, gm = "^gm_m"),
#         var1 = TRUE,
#         rasch = FALSE,
#         kryteriaUsuwania = NULL,
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = NULL
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   ),
#   "test przyr." = list(
#     czescPomiarowa = list(
#       gm_p = list(
#         zmienne = list(~gm, gm = "^gm_p"),
#         var1 = TRUE,
#         rasch = FALSE,
#         kryteriaUsuwania = NULL,
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = NULL
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   )
# ))
# #' @title Procedury skalowania egzaminow.
# #' @description
# #' Procedura skalowania egzaminu gimnazjalnego - w formule do 2011 r.
# #' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
# #' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
# #' @param parametryS0 data.frame z parametrami zadań ze sprawdzianu dla uczniów o standardowej długości toku kształcenia
# #' @param parametryS1 data.frame z parametrami zadań ze sprawdzianu dla uczniów o toku kształcenia wydłużonym o rok
# #' @param processors liczba rdzeni do wykorzystania przy estymacji
# #' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
# #' @seealso \code{\link{skaluj}}
# #' @examples
# #' # chwilowo brak
# #' @export
# procedura_eg_sf = function(nazwyZmiennych, parametryS0, parametryS1, processors=3) return(list(
#   "sprawdzian" = list(
#     czescPomiarowa = list(
#       s0 = list(
#         zmienne = parametryS0$zmienna2[parametryS0$typ == "by" & parametryS0$zmienna1 == "s0"],
#         var1 = FALSE,
#         rasch = FALSE,
#         kryteriaUsuwania = NULL,
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = parametryS0[, c("typ", "zmienna1", "zmienna2", "wartosc")]
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   ),
#   "sprawdzian-drugoroczni" = list(
#     czescPomiarowa = list(
#       s1 = list(
#         zmienne = parametryS1$zmienna2[parametryS1$typ == "by" & parametryS1$zmienna1 == "s1"],
#         var1 = FALSE,
#         rasch = FALSE,
#         kryteriaUsuwania = NULL,
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = parametryS1[, c("typ", "zmienna1", "zmienna2", "wartosc")]
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   ),
#   "część hum." = list(
#     czescPomiarowa = list(
#       gh = list(
#         zmienne = nazwyZmiennych[grep("^gh_[[:digit:]]", nazwyZmiennych)],
#         var1 = TRUE,
#         rasch = FALSE,
#         kryteriaUsuwania = list(
#           dyskryminacjaPonizej = 0.2,
#           istotnoscPowyzej = 1,
#           nigdyNieUsuwaj = NULL
#         ),
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = NULL
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   ),
#   "część mat.-przyr." = list(
#     czescPomiarowa = list(
#       gmp = list(
#         zmienne = nazwyZmiennych[grep("^gm_[[:digit:]]", nazwyZmiennych)],
#         var1 = TRUE,
#         rasch = FALSE,
#         kryteriaUsuwania = list(
#           dyskryminacjaPonizej = 0.2,
#           istotnoscPowyzej = 1,
#           nigdyNieUsuwaj = NULL
#         ),
#         wartosciStartowe = NULL,
#         wartosciZakotwiczone = NULL
#       )
#     ),
#     parametry = list(
#       estimator = "MLR",
#       processors = processors,
#       integration = "STANDARD (15)",
#       fscores = TRUE
#     )
#   )
# ))
