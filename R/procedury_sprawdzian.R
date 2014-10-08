#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania sprawdzianu.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych wektor tekstowy z nazwami zmiennych z data frame'a z danymi, na których ma być prowadzona estymacja (funkcja sama wybierze tylko te, które pasują do wyrażenia regularnego podanego w argumencie \code{maskaZmienne})
#' @param wartosciZakotwiczone opcjonalnie data frame definiujący zakotwiczone wartości parametrów modelu (p. \code{\link{skaluj}})
#' @param maskaZmienne wyrażenie regularne (ciąg znaków), które spełniają nazwy tylko tych zmiennych, które mają być uwzględnione w skalowaniu
#' @param nazwaKonstruktu ciąg znaków - nazwa zmiennej opisującej mierzony konstrukt
#' @param nazwaKroku ciąg znaków - nazwa jedynego kroku tej procedury
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @param integrPt liczba punktów siatki do całkowania numerycznego (po rozkładzie cechy ukrytej)
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_spr = function(nazwyZmiennych, wartosciZakotwiczone=NULL, maskaZmienne="^[kp]_[[:digit:]]+$", nazwaKonstruktu="s", nazwaKroku="sprawdzian", processors=3, integrPt=20) {
  opis = list(
    list(
      czescPomiarowa = list(
        spr = list(
          zmienne = nazwyZmiennych[grep(maskaZmienne, nazwyZmiennych)],
          var1 = ifelse(is.null(wartosciZakotwiczone), TRUE, FALSE),
          rasch = FALSE,
          kryteriaUsuwania = if(is.null(wartosciZakotwiczone)) {
            list(
              dyskryminacjaPonizej = 0.2,
              istotnoscPowyzej = 1,
              nigdyNieUsuwaj = NULL
            )
          } else {NULL},
          wartosciStartowe = NULL,
          wartosciZakotwiczone = wartosciZakotwiczone
        )
      ),
      parametry = list(
        estimator = "MLR",
        processors = processors,
        integration = paste0("STANDARD (", integrPt, ")"),
        fscores = TRUE
      )
    )
  )
  names(opis[[1]]$czescPomiarowa)[1] = nazwaKonstruktu
  return(opis)
}
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania sprawdzianu.
#' @param daneWzorcowe data frame zawierający dane do skalowania wzorcowego
#' @param daneWszyscy data frame zawierający dane wszystkich zdających (do wyliczenia oszacowań umiejętności na podstawe parametrów ze skalowania wzorcowego)
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return
#' lista z elementami:
#' \itemize{
#'   \item{\code{usunieteKryteria} wektor tekstowy z nazwami (pseudo)konstruktów, które
#'         zostały usunięte podczas skalowania wzorcowego.}
#'   \item{\code{parametry} data frame z wyestymowanymi parametrami modelu w jego
#'         ostatecznej postaci (tj. takiej, jak w ostatnim kroku skalowania wzorcowego
#'         i w jedynym kroku skalowania na wszystkich zdających).}
#'   \item{\code{oszacowania} data frame zawierający id_obserwacji i wyliczone
#'         oszacowania umiejętności dla wszystkich zdających.}
#' }
#' @seealso \code{\link{skaluj}}, \code{\link{procedura_spr}}
#' @examples
#' # chwilowo brak
#' @export
skaluj_spr = function(daneWzorcowe, daneWszyscy, processors=2) {
  zmienneKryteria = list(
    wzorcowe = names(daneWzorcowe)[grepl("^[kp]_[[:digit:]]+$", names(daneWzorcowe))],
    wszyscy  = names(daneWszyscy )[grepl("^[kp]_[[:digit:]]+$", names(daneWszyscy ))])
  if (!all(zmienneKryteria$wzorcowe %in% zmienneKryteria$wszyscy) |
        !all(zmienneKryteria$wszyscy %in% zmienneKryteria$wzorcowe)) {
    stop("Niezgodność zestawu zmiennych do skalowania pomiędzy argumentami 'daneWzorcowe' i 'daneWszyscy'.")
  }

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

  zmienneKryteria = zmienneKryteria[[1]]
  daneWzorcowe = daneWzorcowe[, c("id_obserwacji", zmienneKryteria)]
  daneWszyscy  = daneWszyscy[, c("id_obserwacji", zmienneKryteria)]

  opisWzorcowe = procedura_spr(zmienneKryteria, nazwaKroku="skalowanie wzrocowe", processors=processors)
  sprWzorcowe  = skaluj(daneWzorcowe, opisWzorcowe, "id_obserwacji", tytul=tytulWzorcowe, zwrocOszacowania=FALSE)

  wartosciZakotwiczone = sprWzorcowe[[1]][[length(sprWzorcowe[[1]])]]$parametry$surowe
  wartosciZakotwiczone = wartosciZakotwiczone[!(wartosciZakotwiczone$typ %in% c("mean", "variance")), ]
  zmienneKryteriaPoUsuwaniu = wartosciZakotwiczone$zmienna2[wartosciZakotwiczone$typ == "by"]
  opisWszyscy  = procedura_spr(zmienneKryteriaPoUsuwaniu, wartosciZakotwiczone, nazwaKroku="oszacowania umiejętności wszystkich zdających", processors=processors)
  sprWszyscy   = skaluj(daneWszyscy , opisWszyscy , "id_obserwacji", tytul=tytulWszyscy )

  return(list(
    usunieteKryteria = zmienneKryteria[!(zmienneKryteria %in% zmienneKryteriaPoUsuwaniu)],
    parametry = wartosciZakotwiczone,
    oszacowania = sprWszyscy[[1]][[length(sprWszyscy[[1]])]]$zapis
  ))
}
