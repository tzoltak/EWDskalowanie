#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania egzaminu gimnazjalnego - w formule od 2012 r. - w podejściu wielowymiarowym (eksperymentalnie).
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
procedura_eg_ww = function(nazwyZmiennych, parametryS0, parametryS1, processors=3) return(list(
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
  "usuwanie zadań" = list(
    czescPomiarowa = list(
      eg = list(
        zmienne = nazwyZmiennych[grep("^g[mh]_[hmp][[:digit:]]", nazwyZmiennych)],
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
  "kotwica GH-GMP" = list(
    czescPomiarowa = list(
      gh_a = list(
        zmienne = list(~eg, eg = "^g[h]"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = TRUE,
        wartosciZakotwiczone = NULL
      ),
      gmp_a = list(
        zmienne = list(~eg, eg = "^g[m]"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = TRUE,
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
  "wartości startowe GH" = list(
    czescPomiarowa = list(
      gh_p = list(
        zmienne = list(~eg, eg = "^gh_p"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      ),
      gh_h = list(
        zmienne = list(~eg, eg = "^gh_h"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      ),
      gh = list(
        zmienne = c("gh_p", "gh_h"),
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
  "wartosci startowe GMP" = list(
    czescPomiarowa = list(
      gm_m = list(
        zmienne = list(~eg, eg = "^gm_m"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      ),
      gm_p = list(
        zmienne = list(~eg, eg = "^gm_p"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      ),
      gmp = list(
        zmienne = c("gm_m", "gm_p"),
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
  "ostateczny model GH" = list(
    czescPomiarowa = list(
      gh_p = list(
        zmienne = list(~gh_p, gh_p = "^gh_p"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = TRUE,
        wartosciZakotwiczone = NULL
      ),
      gh_h = list(
        zmienne = list(~gh_h, gh_h = "^gh_h"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = TRUE,
        wartosciZakotwiczone = NULL
      ),
      gh = list(
        zmienne = list(~gh, gh = "^gh_[ph]$"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = TRUE,
        wartosciZakotwiczone = NULL
      ),
      gmp_a = list(
        zmienne = list(~gmp_a, gmp_a = "^gm_"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = TRUE
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (10)",
      fscores = TRUE
    )
  ),
  "ostateczny model GMP" = list(
    czescPomiarowa = list(
      gm_m = list(
        zmienne = list(~gm_m, gm_m = "^gm_m"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = TRUE,
        wartosciZakotwiczone = NULL
      ),
      gm_p = list(
        zmienne = list(~gm_p, gm_p = "^gm_p"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = TRUE,
        wartosciZakotwiczone = NULL
      ),
      gmp = list(
        zmienne = list(~gmp, gmp = "^gm_[mp]$"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = TRUE,
        wartosciZakotwiczone = NULL
      ),
      gh_a = list(
        zmienne = list(~gh_a, gh_a = "^gh_"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = TRUE
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (10)",
      fscores = TRUE
    )
  )
))
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania egzaminu gimnazjalnego - w formule do 2011 r. - w podejściu wielowymiarowym (eksperymentalnie).
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
procedura_eg_sf_ww = function(nazwyZmiennych, parametryS0, parametryS1, processors=3) return(list(
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
  "usuwanie zadań" = list(
    czescPomiarowa = list(
      eg = list(
        zmienne = nazwyZmiennych[grep("^g[mh]_[[:digit:]]", nazwyZmiennych)],
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
  "model dwuwymiarowy" = list(
    czescPomiarowa = list(
      gh = list(
        zmienne = list(~eg, eg = "^gh_"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = TRUE,
        wartosciZakotwiczone = NULL
      ),
      gmp = list(
        zmienne = list(~eg, eg = "^gm_"),
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = TRUE,
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
