#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania "jeden krok, jeden wymiar".
#' Funkcja przygotowuje opis bardzo prostej procedury skalowania (do użycia przez funkcję
#' \code{\link{skaluj}}), zawierającej tylko jeden krok, w ramach którego skalowany jest
#' jednowymiarowy konstrukt. Świetnie nadaje się do wykorzystania przy skalowaniu
#' sprawdzianu i egzaminu gimnazjalnego, które prowadzone są właśnie w taki prosty sposób.
#'
#' Jeśli nie podano argumentu \code{wartosciZakotwiczone}, to zastosowane zostanie
#' kryterium usuwania zadań \code{dyskryminacjaPonizej = 0.2}. Jeśli został on podany, to
#' nie będą stosowane kryteria usuwania zadań oraz uwolnione zostaną wartość oczekiwana
#' i wariancja konstruktu.
#' @param nazwyZmiennych wektor tekstowy z nazwami zmiennych z data frame'a z danymi, na
#' których ma być prowadzona estymacja (funkcja sama wybierze tylko te, które pasują do
#' wyrażenia regularnego podanego w argumencie \code{maskaZmienne})
#' @param nazwaKonstruktu ciąg znaków - nazwa zmiennej opisującej mierzony konstrukt
#' @param wartosciZakotwiczone opcjonalnie data frame definiujący zakotwiczone wartości
#' parametrów modelu (p. \code{\link{skaluj}})
#' @param wartosciStartowe opcjonalnie data frame definiujący wartości startowe parametrów
#' modelu (p. \code{\link{skaluj}})
#' @param rasch opcjonalnie wartość logiczna - czy estymować model Rascha?
#' @param wieleGrup opcjonalnie wektor tekstowy z nazwami zmiennych (lub zmiennej)
#' definiujących podział na grupy w modelu wielogrupowym (w ramach grup uwolnione zostaną
#' wartości oczekiwane konstruktu )
#' @param maskaZmienne opcjonalnie wyrażenie regularne (ciąg znaków), które spełniają
#' nazwy tylko tych zmiennych, które mają być uwzględnione w skalowaniu
#' @param nigdyNieUsuwaj opcjonalnie wyrażenie regularne (ciąg znaków), które identyfikuje
#' te zmienne, które nie będą usuwane z modelu mimo spełnienia kryteriów usuwania
#' @param processors opcjonalnie liczba rdzeni do wykorzystania przy estymacji
#' @param integrPt opcjonalnie liczba punktów siatki do całkowania numerycznego (po
#' rozkładzie cechy ukrytej)
#' @param usunWieleNaraz opcjonalnie wartość logiczna - jeśli wiele zadań nie spełnia
#' kryterium \code{dyskryminacjaPonizej = 0.2}, to czy usuwać je wszystkie w jednym
#' kroku?
#' @param usunMimoKotwicy opcjonalnie wartość logiczna - jeśli podano wartości
#' zakotwiczone, to czy mimo to stosować kryteria usuwania zadań (przydaje się przy
#' maturze 2015-2016, gdy jednocześnie skalowana jest "stara" i "nowa" formuła)
#' @param usunDyskrPonizej opcjonalnie wartość liczbowa - próg wartości dyskryminacji
#' zadania, poniżej którego będą one usuwane z modelu skalowania
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji
#' \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}, \code{\link[EWDskale]{skaluj_spr}},
#' \code{\link[EWDskale]{skaluj_egz_gimn}}
#' @examples
#' # chwilowo brak
#' @export
procedura_1k_1w = function(nazwyZmiennych, nazwaKonstruktu="f1", wartosciZakotwiczone=NULL,
                           wartosciStartowe=NULL, rasch=FALSE, wieleGrup=NULL,
                           maskaZmienne="^([kp]_[[:digit:]]+|(s|t[[:digit:]]+)(nf|)_[[:alpha:]_]+)$",
                           nigdyNieUsuwaj=NULL, processors=3, integrPt=20,
                           usunWieleNaraz=FALSE, usunMimoKotwicy=FALSE,
                           usunDyskrPonizej=0.2) {
  opis = list(
    list(
      czescPomiarowa = list(
        list(
          zmienne = nazwyZmiennych[grep(maskaZmienne, nazwyZmiennych)],
          var1 = ifelse(is.null(wartosciZakotwiczone), TRUE, FALSE),
          rasch = ifelse(is.null(wartosciZakotwiczone), rasch, FALSE),
          kryteriaUsuwania = if (is.null(wartosciZakotwiczone) | usunMimoKotwicy) {
            list(
              dyskryminacjaPonizej = usunDyskrPonizej,
              istotnoscPowyzej = 1,
              nigdyNieUsuwaj = nigdyNieUsuwaj,
              usunWieleNaraz = usunWieleNaraz
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
  if (!is.null(wieleGrup)) opis[[1]]$wieleGrup = list(
    zmienneGrupujace = wieleGrup,
    uwolnijWartosciOczekiwane = TRUE,
    uwolnijWariancje = TRUE)
  return(opis)
}
