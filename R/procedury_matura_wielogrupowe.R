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
procedura_matura_hm_gr = function(nazwyZmiennych, processors=3) return(list(
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
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
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
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
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
procedura_matura_hm_gr_wszyscy = function(nazwyZmiennych, parametryJpol, parametryHum, processors=3) return(list(
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
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
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
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
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
procedura_matura_mp_gr = function(nazwyZmiennych, processors=3) return(list(
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
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
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
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
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
procedura_matura_mp_gr_wszyscy = function(nazwyZmiennych, parametryMat, parametryMatPrzyr, processors=3) return(list(
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
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
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
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
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
#' Procedura skalowania matury - przedmioty humanistyczne bez lauratów.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @seealso \code{\link{skaluj}}
#' @examples
#' # chwilowo brak
#' @export
procedura_matura_hm_gr_rw = function(nazwyZmiennych, processors=3) return(list(
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
        wartosciZakotwiczone = NULL,
        ograniczeniaWartosci = c("^jpl_p", "^jpl_r")
      )
    ),
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
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
        wartosciZakotwiczone = NULL,
        ograniczeniaWartosci = c("^jpl_p", "^jpl_r", "^his_p", "^his_r", "^wos_p", "^wos_r")
      )
    ),
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
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
procedura_matura_mp_gr_rw = function(nazwyZmiennych, processors=3) return(list(
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
        wartosciZakotwiczone = NULL,
        ograniczeniaWartosci = c("^mat_p", "^mat_r")
      )
    ),
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
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
        wartosciZakotwiczone = NULL,
        ograniczeniaWartosci = c("^bio_p", "^bio_r", "^che_p", "^che_r", "^fiz_p", "^fiz_r", "^geo_p", "^geo_r", "^inf_p", "^inf_r", "^mat_p", "^mat_r")
      )
    ),
    wieleGrup = list(
      zmienneGrupujace = "typ_szk",
      uwolnijWartosciOczekiwane = TRUE,
      uwolnijWariancje = TRUE
    ),
    parametry = list(
      estimator = "MLR",
      processors = processors,
      integration = "STANDARD (30)",
      fscores = TRUE
    )
  )
))
