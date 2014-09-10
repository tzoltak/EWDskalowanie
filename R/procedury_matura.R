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
#' Procedura skalowania matury - przedmioty humanistyczne bez lauratów, w modelu wielogrupowym ze względu na typ szkoły.
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
				zmienne = nazwyZmiennych[grep("^jpl_[pr]|^selekcja_jpl", nazwyZmiennych)],
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
			zmienneGrupujace = "typ_szkoly",
			uwolnijWartosciOczekiwane = TRUE,
			uwolnijWariancje = TRUE
		),
		parametry = list(
			estimator = "MLR",
			processors = processors,
			integration = "STANDARD (20)",
			fscores = TRUE
		)
	)#,
# 	"przedm. hum." = list(
# 		czescPomiarowa = list(
# 			hum = list(
# 				zmienne = nazwyZmiennych[grep("^(jpl|his|wos)_[pr]|^s_(jpl|his|wos)", nazwyZmiennych)],
# 				var1 = TRUE,
# 				rasch = FALSE,
# 				kryteriaUsuwania = list(
# 					dyskryminacjaPonizej = 0.2,
# 					istotnoscPowyzej = 1,
# 					nigdyNieUsuwaj = "^s_"
# 				),
# 				wartosciStartowe = NULL,
# 				wartosciZakotwiczone = NULL
# 			)
# 		),
# 		parametry = list(
# 			estimator = "MLR",
# 			processors = processors,
# 			integration = "STANDARD (30)",
# 			fscores = TRUE
# 		)
# 	)
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
