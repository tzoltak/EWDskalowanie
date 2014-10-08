#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania "jeden krok, jeden wymiar".
#' Funkcja przygotowuje opis bardzo prostej procedury skalowania (do użycia przez funkcję
#' \code{\link{skaluj}}), zawierającej tylko jeden krok, w ramach którego skalowany jest
#' jednowymiarowy konstrukt. Świetnie nadaje się do wykorzystania przy skalowaniu
#' sprawdzianu (p. \code{\link{skaluj_spr}}) i egzaminu gimnazjalnego
#' (p. \code{\link{skaluj_egz_gimn}}), które prowadzone są właśnie w taki prosty sposób.
#'
#' Jeśli nie podano argumentu \code{wartosciZakotwiczone}, to zastosowane zostanie
#' kryterium usuwania zadań \code{dyskryminacjaPonizej = 0.2}. Jeśli został on podany, to
#' nie będą stosowane kryteria usuwania zadań oraz uwolnione zostaną wartość oczekiwana
#' i wariancja konstruktu.
#' @param nazwyZmiennych wektor tekstowy z nazwami zmiennych z data frame'a z danymi, na których ma być prowadzona estymacja (funkcja sama wybierze tylko te, które pasują do wyrażenia regularnego podanego w argumencie \code{maskaZmienne})
#' @param nazwaKonstruktu ciąg znaków - nazwa zmiennej opisującej mierzony konstrukt
#' @param wartosciZakotwiczone opcjonalnie data frame definiujący zakotwiczone wartości parametrów modelu (p. \code{\link{skaluj}})
#' @param wartosciStartowe opcjonalnie data frame definiujący wartości startowe parametrów modelu (p. \code{\link{skaluj}})
#' @param maskaZmienne wyrażenie regularne (ciąg znaków), które spełniają nazwy tylko tych zmiennych, które mają być uwzględnione w skalowaniu
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @param integrPt liczba punktów siatki do całkowania numerycznego (po rozkładzie cechy ukrytej)
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}, \code{\link{skaluj_spr}}, \code{\link{skaluj_egz_gimn}}
#' @examples
#' # chwilowo brak
#' @export
procedura_1k_1w = function(nazwyZmiennych, nazwaKonstruktu=NULL, wartosciZakotwiczone=NULL, wartosciStartowe=NULL, maskaZmienne="^[kp]_[[:digit:]]+$", processors=3, integrPt=20) {
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
          wartosciStartowe = wartosciStartowe,
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
