#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania sprawdzianu.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param nazwaKonstruktu nazwa zmiennej opisującej mierzony konstrukt
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_spr = function(nazwyZmiennych, nazwaKonstruktu="s", processors=3) {
  opis = list(
    "kalibracja na danych CKE" = list(
      czescPomiarowa = list(
        spr = list(
          zmienne = nazwyZmiennych[grep("^s[01]{0,1}_[[:digit:]]", nazwyZmiennych)],
          var1 = TRUE,
          rasch = FALSE,
          kryteriaUsuwania = list(
            dyskryminacjaPonizej = 0.2,
            istotnoscPowyzej = 1,
            nigdyNieUsuwaj = NULL
          ),
          wartosciStartowe = NULL,
          wartosciZakotwiczone = NULL
        )
      ),
      parametry = list(
        estimator = "MLR",
        processors = processors,
        integration = "STANDARD (15)",
        fscores = TRUE
      )
    )
  )
  names(opis[[1]]$czescPomiarowa)[1] = nazwaKonstruktu
  return(opis)
}
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania egzaminu gimnazjalnego - w formule od 2012 r.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param parametryS0 data.frame z parametrami zadań ze sprawdzianu dla uczniów o standardowej długości toku kształcenia
#' @param parametryS1 data.frame z parametrami zadań ze sprawdzianu dla uczniów o toku kształcenia wydłużonym o rok
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_eg = function(nazwyZmiennych, parametryS0, parametryS1, processors=3) return(list(
  "sprawdzian" = list(
    czescPomiarowa = list(
      s0 = list(
        zmienne = parametryS0$zmienna2[parametryS0$typ == "by" & parametryS0$zmienna1 == "s0"],
        var1 = FALSE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = parametryS0[, c("typ", "zmienna1", "zmienna2", "wartosc")]
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  ),
  "sprawdzian-drugoroczni" = list(
    czescPomiarowa = list(
      s1 = list(
        zmienne = parametryS1$zmienna2[parametryS1$typ == "by" & parametryS1$zmienna1 == "s1"],
        var1 = FALSE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = parametryS1[, c("typ", "zmienna1", "zmienna2", "wartosc")]
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  ),
  "część pol.-hum." = list(
    czescPomiarowa = list(
      gh = list(
        zmienne = nazwyZmiennych[grep("^gh_[hp](|_)[[:digit:]]", nazwyZmiennych)],
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = list(
          dyskryminacjaPonizej = 0.2,
          istotnoscPowyzej = 1,
          nigdyNieUsuwaj = NULL
        ),
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  ),
  "test pol." = list(
    czescPomiarowa = list(
      gh_p = list(
        zmienne = list(~gh, gh = "^gh_p"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  ),
  "test WOS-hist." = list(
    czescPomiarowa = list(
      gh_h = list(
        zmienne = list(~gh, gh = "^gh_h"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  ),
  "część mat.-przyr." = list(
    czescPomiarowa = list(
      gm = list(
        zmienne = nazwyZmiennych[grep("^gm_[mp](|_)[[:digit:]]", nazwyZmiennych)],
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = list(
          dyskryminacjaPonizej = 0.2,
          istotnoscPowyzej = 1,
          nigdyNieUsuwaj = NULL
        ),
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  ),
  "test mat." = list(
    czescPomiarowa = list(
      gm_m = list(
        zmienne = list(~gm, gm = "^gm_m"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  ),
  "test przyr." = list(
    czescPomiarowa = list(
      gm_p = list(
        zmienne = list(~gm, gm = "^gm_p"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  )
))
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania egzaminu gimnazjalnego - w formule do 2011 r.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param parametryS0 data.frame z parametrami zadań ze sprawdzianu dla uczniów o standardowej długości toku kształcenia
#' @param parametryS1 data.frame z parametrami zadań ze sprawdzianu dla uczniów o toku kształcenia wydłużonym o rok
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_eg_sf = function(nazwyZmiennych, parametryS0, parametryS1, processors=3) return(list(
  "sprawdzian" = list(
    czescPomiarowa = list(
      s0 = list(
        zmienne = parametryS0$zmienna2[parametryS0$typ == "by" & parametryS0$zmienna1 == "s0"],
        var1 = FALSE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = parametryS0[, c("typ", "zmienna1", "zmienna2", "wartosc")]
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  ),
  "sprawdzian-drugoroczni" = list(
    czescPomiarowa = list(
      s1 = list(
        zmienne = parametryS1$zmienna2[parametryS1$typ == "by" & parametryS1$zmienna1 == "s1"],
        var1 = FALSE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = parametryS1[, c("typ", "zmienna1", "zmienna2", "wartosc")]
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  ),
  "część hum." = list(
    czescPomiarowa = list(
      gh = list(
        zmienne = nazwyZmiennych[grep("^gh_[[:digit:]]", nazwyZmiennych)],
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = list(
          dyskryminacjaPonizej = 0.2,
          istotnoscPowyzej = 1,
          nigdyNieUsuwaj = NULL
        ),
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  ),
  "część mat.-przyr." = list(
    czescPomiarowa = list(
      gmp = list(
        zmienne = nazwyZmiennych[grep("^gm_[[:digit:]]", nazwyZmiennych)],
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = list(
          dyskryminacjaPonizej = 0.2,
          istotnoscPowyzej = 1,
          nigdyNieUsuwaj = NULL
        ),
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (15)",
      fscores = TRUE
    )
  )
))
