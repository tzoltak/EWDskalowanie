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
        zmienne = nazwyZmiennych[grep("^gh_[hp][[:digit:]]", nazwyZmiennych)],
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
        zmienne = nazwyZmiennych[grep("^gm_[mp][[:digit:]]", nazwyZmiennych)],
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
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania egzaminu gimnazjalnego - w formule do 2011 r. na potrzeby danych do wskaźników matrualnych.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param parametryGH data.frame z parametrami zadań z części hum. egzaminu gimnazjalnego
#' @param parametryGMP data.frame z parametrami zadań z części mat.-przyr. egzaminu gimnazjalnego
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_eg_sf_matura = function(nazwyZmiennych, parametryGH=NULL, parametryGMP=NULL, processors=3) {
  procedura = list(
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
  )
  if (!is.null(parametryGH)) {
    procedura$"część hum."$czescPomiarowa$gh = list(
      zmienne = parametryGH$zmienna2[parametryGH$typ == "by" & parametryGH$zmienna1 == "gh"],
      var1 = FALSE,
      rasch = FALSE,
      kryteriaUsuwania = NULL,
      wartosciStartowe = NULL,
      wartosciZakotwiczone = parametryGH[, c("typ", "zmienna1", "zmienna2", "wartosc")]
    )
  }
  if (!is.null(parametryGMP)) {
    procedura$"część mat.-przyr."$czescPomiarowa$gmp = list(
      zmienne = parametryGMP$zmienna2[parametryGMP$typ == "by" & parametryGMP$zmienna1 == "gmp"],
      var1 = FALSE,
      rasch = FALSE,
      kryteriaUsuwania = NULL,
      wartosciStartowe = NULL,
      wartosciZakotwiczone = parametryGMP[, c("typ", "zmienna1", "zmienna2", "wartosc")]
    )
  }
  return(procedura)
}
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania cżęści mat.-przyr. egzaminu gimnazjalnego - w formule do 2011 r. na potrzeby Kalkulatora maturalnego.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param parametry data.frame z parametrami zadań z części mat.-przyr. egzaminu gimnazjalnego
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_gmp_sf_matura_rasch = function(nazwyZmiennych, parametry=NULL, processors=3) {
  procedura = list(
    "część mat.-przyr." = list(
      czescPomiarowa = list(
        gmp_rsch = list(
          zmienne = nazwyZmiennych[grep("^gm_[[:digit:]]", nazwyZmiennych)],
          var1 = TRUE,
          rasch = TRUE,
          kryteriaUsuwania = list(
            dyskryminacjaPonizej = NULL,
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
  if (!is.null(parametry)) {
    procedura$"część mat.-przyr."$czescPomiarowa$gmp_rsch = list(
      zmienne = parametry$zmienna2[parametry$typ == "by" & parametry$zmienna1 == "gmp_rsch"],
      var1 = FALSE,
      rasch = TRUE,
      kryteriaUsuwania = NULL,
      wartosciStartowe = NULL,
      wartosciZakotwiczone = parametry[, c("typ", "zmienna1", "zmienna2", "wartosc")]
    )
  }
  return(procedura)
}
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania matury - przedmioty humanistyczne bez lauratów.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_matura_hm = function(nazwyZmiennych, processors=3) return(list(
  "j. pol" = list(
    czescPomiarowa = list(
      j_pol = list(
        zmienne = nazwyZmiennych[grep("^jpl_[pr]|^s_jpl", nazwyZmiennych)],
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = list(
          dyskryminacjaPonizej = 0.2,
          istotnoscPowyzej = 1,
          nigdyNieUsuwaj = "^s_"
        ),
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (20)",
      fscores = TRUE
    )
  ),
  "przedm. hum." = list(
    czescPomiarowa = list(
      hum = list(
        zmienne = nazwyZmiennych[grep("^(jpl|his|wos)_[pr]|^s_(jpl|his|wos)", nazwyZmiennych)],
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = list(
          dyskryminacjaPonizej = 0.2,
          istotnoscPowyzej = 1,
          nigdyNieUsuwaj = "^s_"
        ),
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (30)",
      fscores = TRUE
    )
  )
))
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania matury - przedmioty humanistyczne z uwzględnieniem laureatów.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param parametryJpol data.frame z parametrami zadań z przedmiotów humanistycznych
#' @param parametryHum data.frame z parametrami zadań z języka polskiego
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_matura_hm_wszyscy = function(nazwyZmiennych, parametryJpol, parametryHum, processors=3) return(list(
  "j. pol" = list(
    czescPomiarowa = list(
      j_pol = list(
        zmienne = parametryJpol$zmienna2[parametryJpol$typ == "by" & parametryJpol$zmienna1 == "j_pol"],
        var1 = FALSE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = parametryJpol[, c("typ", "zmienna1", "zmienna2", "wartosc")]
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (20)",
      fscores = TRUE
    )
  ),
  "przedm. hum." = list(
    czescPomiarowa = list(
      hum = list(
        zmienne = parametryHum$zmienna2[parametryHum$typ == "by" & parametryHum$zmienna1 == "hum"],
        var1 = FALSE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = parametryHum[, c("typ", "zmienna1", "zmienna2", "wartosc")]
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (30)",
      fscores = TRUE
    )
  )
))
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania matury - przedmioty matematyczno-przyrodnicze bez laureatów.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_matura_mp = function(nazwyZmiennych, processors=3) return(list(
  "mat." = list(
    czescPomiarowa = list(
      mat = list(
        zmienne = nazwyZmiennych[grep("^mat_[pr]|^s_mat", nazwyZmiennych)],
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = list(
          dyskryminacjaPonizej = 0.2,
          istotnoscPowyzej = 1,
          nigdyNieUsuwaj = "^s_"
        ),
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (20)",
      fscores = TRUE
    )
  ),
  "przedm. mat.-przyr." = list(
    czescPomiarowa = list(
      mat_prz = list(
        zmienne = nazwyZmiennych[grep("^(bio|che|fiz|geo|inf|mat)_[pr]|^s_(bio|che|fiz|geo|inf|mat)", nazwyZmiennych)],
        var1 = TRUE,
        rasch = FALSE,
        kryteriaUsuwania = list(
          dyskryminacjaPonizej = 0.2,
          istotnoscPowyzej = 1,
          nigdyNieUsuwaj = "^s_"
        ),
        wartosciStartowe = NULL,
        wartosciZakotwiczone = NULL
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (30)",
      fscores = TRUE
    )
  )
))
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania matury - przedmioty matematyczno-przyrodnicze z uwzględnieniem laureatów.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param parametryMat data.frame z parametrami zadań z matematyki
#' @param parametryMatPrzyr data.frame z parametrami zadań z przedmiotów matematyczno-przyrodniczych
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_matura_mp_wszyscy = function(nazwyZmiennych, parametryMat, parametryMatPrzyr, processors=3) return(list(
  "mat." = list(
    czescPomiarowa = list(
      mat = list(
        zmienne = parametryMat$zmienna2[parametryMat$typ == "by" & parametryMat$zmienna1 == "mat"],
        var1 = FALSE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = parametryMat[, c("typ", "zmienna1", "zmienna2", "wartosc")]
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (20)",
      fscores = TRUE
    )
  ),
  "przedm. mat.-przyr." = list(
    czescPomiarowa = list(
      mat_prz = list(
        zmienne = parametryMatPrzyr$zmienna2[parametryMatPrzyr$typ == "by" & parametryMatPrzyr$zmienna1 == "mat_prz"],
        var1 = FALSE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = parametryMatPrzyr[, c("typ", "zmienna1", "zmienna2", "wartosc")]
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (30)",
      fscores = TRUE
    )
  )
))
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania matury - matematyka modelem Rascha na potrzeby Kalkulatora, bez laureatów.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_matura_m_rasch = function(nazwyZmiennych, processors=3) {
  lZmP = sum(grepl("^mat_p", nazwyZmiennych))
  lZmR = sum(grepl("^mat_r", nazwyZmiennych))
  zmienne = c(
    nazwyZmiennych[grep("^mat_p", nazwyZmiennych)],
    nazwyZmiennych[grep("^mat_r", nazwyZmiennych)],
    nazwyZmiennych[grep("^s_mat", nazwyZmiennych)]
  )
  return(list(
    "mat." = list(
      czescPomiarowa = list(
        mat_rsch = list(
          zmienne = zmienne,
          var1 = TRUE,
          rasch = FALSE,
          kryteriaUsuwania = list(
            dyskryminacjaPonizej = NULL,
            istotnoscPowyzej = 1,
            nigdyNieUsuwaj = "^s_"
          ),
          wartosciStartowe = NULL,
          wartosciZakotwiczone = NULL,
          ograniczeniaWartosci = data.frame(typ=rep("by", lZmP + lZmR), zmienna1=rep("mat_rsch", lZmP + lZmR), zmienna2=zmienne[grep("^mat_[pr]", zmienne)], wartosc=c(rep("dyskr_p", lZmP), rep("dyskr_r", lZmR)), stringsAsFactors=FALSE)
        )
      ),
      parametry = list(
        estimator = "MLR",
        processors = processors,
        integration = "STANDARD (20)",
        fscores = TRUE
      )
    )
  ))
}
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania matury - matematyka modelem Rascha na potrzeby Kalkulatora z uwzględnieniem laureatów.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param parametryMat data.frame z parametrami zadań z matematyki
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_matura_m_rasch_wszyscy = function(nazwyZmiennych, parametryMat, processors=3) return(list(
  "mat." = list(
    czescPomiarowa = list(
      mat_rsch = list(
        zmienne = parametryMat$zmienna2[parametryMat$typ == "by" & parametryMat$zmienna1 == "mat_rsch"],
        var1 = FALSE,
        rasch = FALSE,
        kryteriaUsuwania = NULL,
        wartosciStartowe = NULL,
        wartosciZakotwiczone = parametryMat[, c("typ", "zmienna1", "zmienna2", "wartosc")]
      )
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (20)",
      fscores = TRUE
    )
  )
))
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
