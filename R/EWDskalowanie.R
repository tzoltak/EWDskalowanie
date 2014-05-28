# Funkcje pomocnicze ------------------------------------------------------
#' @title Wczytywanie plikow tekstowych.
#' @description
#' Funkcja w miarę efektywnie wczytuje plik tekstowy zapisany w formacie o stałej szerokości kolumn.
#' @param nazwaPliku nazwa pliku, który ma być wczytany
#' @param szerokosciKolumn wektor liczbowy podający szerokość kolumn
#' @param nazwyKolumn wektor tekstowy podający nazwy kolumn
#' @details
#' Funkcja działa względnie szybko, bo nie próbuje rozpoznawać typów kolumn, tylko wszystko zwraca jako tekst. Jeżeli potrzebujesz skonwertować kolumny np. na liczby, musisz to zrobić już we własnym zakresie.
#' @return data frame, w którym wszystkie kolumny są typu \code{character}
#' @seealso \code{\link[utils]{read.fwf}}
#' @export
wczytaj_fwf = function(nazwaPliku, szerokosciKolumn, nazwyKolumn) {
  plikIstnieje = file.access(nazwaPliku, 4) == 0
  stopifnot(is.character(nazwaPliku), is.character(nazwyKolumn),
            is.numeric(szerokosciKolumn), szerokosciKolumn == as.integer(szerokosciKolumn),
            length(szerokosciKolumn) == length(szerokosciKolumn), plikIstnieje
  )
  # wczytywanie pliku z obsługą błędów
  tryCatch(
	  {plik = file(nazwaPliku, "r")},
	  error = function(e) {stop("Nie daje się otworzyć pliku!")}
  )
  tryCatch(
  	{tresc = readLines(plik)},
  	error = function(e) {stop("Nie daje się odczytać z pliku!")}
  )
  close(plik)
  if (nchar(tresc[1]) != sum(szerokosciKolumn)) stop(paste0("Długość pierwszej linii (", nchar(tresc[1]), ") i podane szerokości kolumn (suma równa ", sum(szerokosciKolumn), ") nie zgadzają się ze sobą!"))
  # przetwarzanie na data.frame
  konce = cumsum(szerokosciKolumn)
  poczatki = c(1, konce[-length(konce)] + 1)
  temp = data.frame(row.names=as.character(1:length(tresc)))
  for (i in 1:length(szerokosciKolumn)){  # idąc kolumnami
    temp = data.frame(
      temp,  # dołączaj do data.frame'a z już wyodrębionymi kolumnami
      do.call(  # i-tą kolumnę
        function(x) {
          x[grepl("^[ ]?[*]$", x)] = NA;
          return(as.numeric(x))
        },
        args = list(x=substr(tresc, poczatki[i], konce[i]))
      )
    )
  }
  names(temp) = nazwyKolumn
  # końcówka
  return(temp)
}
#' @title Funkcja lamie dluzsze ciagi znakow na wiersze.
#' @description
#' Funkcja najpierw łączy wektor tekstowy w jeden ciąg znaków, a następnie w inteligentny sposób łamie go z powrotem na kilkuelementowy wektor tekstowy.
#' Jest to przydatne np. przy przygotowywaniu poleceń dla Mplusa, który nie lubi linii dłuższych niż 90 znaków.
#' @param x wektor tekstowy do pocięcia
#' @param wciecie liczba spacji dostawianych na początku każdego wynikowego elementu wektora
#' @param maxDl maksymalna liczba znaków w linii (uwzględniając wcięcie)
#' @param srednikNaKoncu wartość logiczna - czy na końcu ostatniego elementu wynikowego wektora ma być dopisany średnik (o ile go tam nie ma)? 
#' @param sep spearator używany przy złączaniu elementów \code{x} w jeden ciąg znaków
#' @param lamNaNawiasachOkraglych wartość logiczna - czy dodatkowo łamać wiersze za zamykającymi nawiasami okrągłymi: ')'?
#' @return lista
#' @seealso \code{\link[base]{strwrap}}
#' @examples
#' x=c("y BY", "x1*1.200", "x2*1.000", "x3*0.800", "x4*0.600", "x5*0.400")
#' write(lam_wiersze(x), "")
#' write(lam_wiersze(x, wciecie=4, maxDl=30), "")
#' @export
lam_wiersze = function(x, wciecie=2, maxDl=90, srednikNaKoncu=TRUE, sep=" ", lamNaNawiasachOkraglych=TRUE) {
  stopifnot(is.character(x), is.numeric(wciecie), length(wciecie) == 1,
            is.numeric(maxDl), length(maxDl) == 1, wciecie < (maxDl - 8),
            is.logical(srednikNaKoncu), length(srednikNaKoncu) == 1,
            is.character(sep) & length(sep) == 1,
            is.logical(lamNaNawiasachOkraglych), length(lamNaNawiasachOkraglych) == 1
  )
  wciecie=paste0(rep(" ", wciecie), collapse="")  # przygotowanie wcięcia
  x[1] = paste0(wciecie, x[1])  # i dopisanie na początku pierwszego wiersza
  if (length(x) > 1) {
    if (!lamNaNawiasachOkraglych) {
      x = paste0(x, collapse=sep)  # po prostu scalamy w jeden ciąg znaków
    } else {
      for (i in 2:length(x)) {
        ostatniNieNA = max((1:(i-1))[!is.na(x[1:(i-1)])])  # ostatni element, który nie jest pusty
        if (!grepl(" [(].*[)]$", x[ostatniNieNA])) {  # jeśli ostatnie niepusty nie ma na końcu ')'
          x[ostatniNieNA] = paste0(x[ostatniNieNA], sep, x[i])  # to przyłącz do niego
          x[i] = NA  # a ten ustaw na NA
        }
      }
      x = x[!is.na(x)]
    }
  }
  x = paste0(wciecie, strwrap(x, maxDl - nchar(wciecie)))
  if (srednikNaKoncu & !grepl(";$", x[length(x)])) x[length(x)]=paste0(x[length(x)], ";")
  return(x)
}
#' @title Przygotowywanie polecen Mplusa.
#' @description
#' Funkcja na podstawie elementu listy \code{opisProcedury} przygotowuje blok 'MODEL' do pliku poleceń Mplusa.
#' @param opisModelu lista opisująca konstrukty, typowo element listy \code{opisProcedury}
#' @return lista wektorów tekstowych
#' @details
#' Warto pamiętać, że ustawienia opisywane elementami 'rasch', 'var1' i 'e0' są nadpisywane przez wpisy z elementu wartosciZakotwiczone.
przygotuj_model = function(opisModelu) {
  return(mapply(
    function(x, y) {
      zmienne = y$zmienne
      progiSyntax = wariancjeSyntax = wartosciOczekiwaneSyntax = c()
      e0 = TRUE	# wartość oczekiwana zmiennej ukrytej równa 0
      if (!is.null(y$wartosciZakotwiczone)) {
        dyskryminacje      = y$wartosciZakotwiczone[y$wartosciZakotwiczone$typ == "by", ]
        progi    	       = y$wartosciZakotwiczone[y$wartosciZakotwiczone$typ == "threshold", ]
        wariancje          = y$wartosciZakotwiczone[grep("variance", y$wartosciZakotwiczone$typ), ]  # to może być zarówno "variance" jak i "residual variance"
        wartosciOczekiwane = y$wartosciZakotwiczone[y$wartosciZakotwiczone$typ == "mean", ]
        # zabawy z wybraniem tylko tego, co jest w 'zmienne' i ustawieniem w takiej samej kolejności, jak tam
        dyskryminacje = setNames(
          as.list(dyskryminacje$wartosc[dyskryminacje$zmienna2 %in% zmienne]),
          dyskryminacje$zmienna2[dyskryminacje$zmienna2 %in% zmienne]
        )
        dyskryminacje = unlist(dyskryminacje[y$zmienne[y$zmienne %in% names(dyskryminacje)]])
        y$zmienne[zmienne %in% names(dyskryminacje)] = paste0(y$zmienne[zmienne %in% names(dyskryminacje)], "@", dyskryminacje)
        # z progami bez zabawy w sortowanie (bo je się po prostu dopisuje)
        progi = progi[progi$zmienna1 %in% zmienne, ]
        if (nrow(progi) > 0) {
          progiSyntax = setNames(
            c(progiSyntax, paste0("   [", progi$zmienna1, "$", progi$zmienna2, "@", progi$wartosc, "];")),
            c(names(progiSyntax), paste0(progi$zmienna1, "$", progi$zmienna2))
          )
        }
        # wariancje
        wariancje = wariancje[wariancje$zmienna1 == x, ]
        if (nrow(wariancje) > 0) {
          wariancjeSyntax = setNames(
            c(wariancjeSyntax, paste0("  ", wariancje$zmienna1, "@", wariancje$wartosc, ";")),
            c(names(wariancjeSyntax), wariancje$zmienna1)
          )
        }
        # wartości oczekiwane
        wartosciOczekiwane = wartosciOczekiwane[wartosciOczekiwane$zmienna1 == x, ]
        if (nrow(wartosciOczekiwane) > 0) {
          wartosciOczekiwaneSyntax = setNames(
            c(wartosciOczekiwaneSyntax, paste0(" [", wartosciOczekiwane$zmienna1, "@", wartosciOczekiwane$wartosc, "];")),
            c(names(wartosciOczekiwaneSyntax), wartosciOczekiwane$zmienna1)
          )
        }
        # jeżeli którakolwiek z dyskryminacji jest zakotwiczona, ale nie ma kotwicy na wariancji, to wariancję zmiennej ukrytej należy uwolnić
        if ((length(dyskryminacje) > 0 & any(dyskryminacje != 0)) & !(x %in% names(wariancje))) y$var1 = FALSE
        # jeżeli zakotwiczone dyskryminacje nie są takie same, to nadpisz ustawienie rascha
        if (length(unique(dyskryminacje)) > 1) y$rasch = FALSE
        # jeżeli którykolwiek z progów jest zakotwiczony, ale nie ma kotwicy na wartości oczekiwanej, to wartość oczekiwaną zmiennej ukrytej należy uwolnić
        if (nrow(progi) > 0 & nrow(wartosciOczekiwane) == 0) e0 = FALSE
      }
      if (!is.null(y$wartosciStartowe)) {
        dyskryminacje      = y$wartosciStartowe[y$wartosciStartowe$typ == "by", ]
        progi              = y$wartosciStartowe[y$wartosciStartowe$typ == "threshold", ]
        wariancje          = y$wartosciStartowe[grep("variance", y$wartosciStartowe$typ), ]  # to może być zarówno "variance" jak i "residual variance"
        wartosciOczekiwane = y$wartosciStartowe[y$wartosciStartowe$typ == "mean", ]
        # zabawy z wybraniem tylko tego, co jest w 'zmienne' i ustawieniem w takiej samej kolejności, jak tam
        # dodatkowa zabawa - jeśli coś już przypadkiem ma wartość zakotwiczoną, to nie przypisujemy temu wartości startowej
        dyskryminacje = setNames(
          as.list(dyskryminacje$wartosc[dyskryminacje$zmienna2 %in% zmienne]),
          dyskryminacje$zmienna2[dyskryminacje$zmienna2 %in% zmienne]
        )
        dyskryminacje = unlist(dyskryminacje[y$zmienne[y$zmienne %in% names(dyskryminacje)]])
        y$zmienne[zmienne %in% names(dyskryminacje) & ! grepl("@", y$zmienne)] = paste0(y$zmienne[zmienne %in% names(dyskryminacje) & ! grepl("@", y$zmienne)], "*", dyskryminacje)					
        # z progami bez zabawy w sortowanie (bo je się po prostu dopisuje)
        progi = progi[progi$zmienna1 %in% zmienne, ]
        if (nrow(progi) > 0) progi = progi[!(paste0(progi$zmienna1, "$", progi$zmienna2) %in% names(progiSyntax)), ]  # jeżeli jakiś próg ma już wartość zakotwiczoną, to nie dodawajmy mu startowej
        if (nrow(progi) > 0) {
          progiSyntax = setNames(
            c(progiSyntax, paste0("   [", progi$zmienna1, "$", progi$zmienna2, "*", progi$wartosc, "];")),
            c(names(progiSyntax), paste0(progi$zmienna1, "$", progi$zmienna2))
          )
        }
        # wariancje
        wariancje = wariancje[wariancje$zmienna1 == x, ]
        wariancje = wariancje[!(wariancje$zmienna1 %in% names(wariancjeSyntax)), ]  # jeśli coś ma już przypadkiem wartość zakotwiczoną, to startowa mu niepotrzebna
        if (y$var1) wariancje = wariancje[wariancje$zmienna1 != x, ]
        if (nrow(wariancje) > 0) {
          wariancjeSyntax=setNames(
            c(wariancjeSyntax, paste0("  ", wariancje$zmienna1, "*", wariancje$wartosc, ";")),
            c(names(wariancjeSyntax), wariancje$zmienna1)
          )
        }
        # wartości oczekiwane
        wartosciOczekiwane = wartosciOczekiwane[wartosciOczekiwane$zmienna1 == x, ]
        wartosciOczekiwane = wartosciOczekiwane[!(wartosciOczekiwane$zmienna1 %in% names(wartosciOczekiwaneSyntax)), ]  # jeśli coś ma już przypadkiem wartość zakotwiczoną, to startowa mu niepotrzebna
        if (nrow(wartosciOczekiwane) > 0) {
          wartosciOczekiwaneSyntax = setNames(
            c(wartosciOczekiwaneSyntax, paste0(" [", wartosciOczekiwane$zmienna1, "*", wartosciOczekiwane$wartosc, "];")),
            c(names(wartosciOczekiwaneSyntax), wartosciOczekiwane$zmienna1)
          )
        }
      }
      # jeszcze trochę obsługi elementów 'rasch', 'var1' i 'e0'
      if (y$var1 & !any(grepl(paste0("^[ ]*", x, "@"), wariancjeSyntax))) {  # jeśli wariancja konstruktu ma być ustawiona na 1 i nie ma zakotwiczonych dyskryminacji
        if (!grepl("[@*]", y$zmienne[1])) y$zmienne[1] = paste0(y$zmienne[1], "*")  # uwolnij dyskryminację pierwszej zmiennej związanej z konstruktem (dopisując '*' za jej nazwą)
        wariancjeSyntax = paste0("  ", x, "@1;", wariancjeSyntax)  # i ustal wariancję konstruktu w 1
      }
      if (y$rasch & !y$var1 & !any(grepl("@", y$zmienne))) {  # jeśli to ma być Rasch, dyskryminacje nie są zakotwiczone, a wariancja konstruktu ma być uwolniona
        wynik = c(lam_wiersze(c(x, "BY", paste0(y$zmienne, "@1")), 4))  # zakotwicz wartości wszystkich dyskryminacji w 1
      } else if (y$rasch) {  # jeśli to ma być Rasch, a wariancja konstruktu ma być ustalona w 1 (co obsłużyliśmy kilka linii kodu wcześniej)
        wynik = c(paste0("  ", x, " BY"), paste0("    ", y$zmienne, " (", x, ")"))  # dopisz ograniczenia na równość dyskryminacji
        wynik[length(wynik)] = paste0(wynik[length(wynik)], ";")  # teraz każda zmienną związana z konstruktem jest w oddzielnej linii, ale na końcu ostatniej trzeba dopisać ';'
      }
      else wynik = c(lam_wiersze(c(x, "BY", y$zmienne), 4))  # w innych przypadkach nie trzeba nic więcej cudować
      wynik[1] = substr(wynik[1], 3, nchar(wynik[1])) # wcinamy pierwszy wiersz o 2 znaki mniej, niż pozostałe
      if (length(wariancjeSyntax) == 0) wariancjeSyntax = lam_wiersze(c(x, "*"), 2, sep="")  # choć w zasadzie to niepotrzebne, dostawiamy do syntaxu polecenie opisujące wariancję konstruktu (w takim przypadku uwolnioną)
      if (length(wartosciOczekiwaneSyntax) == 0) {
        if (!e0) wynik = c(wynik, paste0(" [", x, "];"))  # uwalniamy wartość oczekiwaną konstruktu
        else     wynik = c(wynik, paste0(" [", x, "@0];"))  # to niepotrzebne do samej estymacji, ale sprawi, że wartość oczekiwana pojawi się w outpucie (skąd będzie ją można zapisać)
      }
      wynik = c(wynik, progiSyntax, wartosciOczekiwaneSyntax, wariancjeSyntax)
      return(wynik)
    },
    as.list(names(opisModelu$czescPomiarowa)),
    opisModelu$czescPomiarowa,
    SIMPLIFY=FALSE
  ))
}
#' @title Przygotowywanie polecen Mplusa.
#' @description
#' Funkcja przygotowuje plik poleceń .inp Mplusa.
#' @param title tytuł pliku poleceń - tekst
#' @param data polecenia do bloku 'DATA' - lista wektorów tekstowych z elementem 'file' i opcjonalnie 'format'
#' @param variable polecenia do bloku 'VARIABLE' - lista wektorów tekstowych z elmentami 'names', 'usevariables', 'categorical' i 'idvariable' oraz opcjonalnie 'missing' i/lub 'useobservations'
#' @param analysis polecenie do bloku 'ANAlYSIS' - lista wektorów tekstowych z opcjonalnymi elementami 'estimator', 'processors', 'integration'
#' @param model polecenia do bloku 'MODEL' - lista wektorów tekstowych, typowo będących wynikiem działania funkcji \code{\link{przygotuj_model}}
#' @param output polecenia do bloku 'OUTPUT' - lista wektorów tekstowych
#' @param savedata polecenia do bloku 'SAVEDATA' - lista wektorów tekstowych z opcjonalnymi elementami 'file' i 'save'
#' @return wektorów tekstowy
#' @details
#' Elementy poszczególnych list, będących parametrami tej funkcji, są dosyć bezpośrednio wklejane do poleceń Mplusa. Jeśli chcesz zrozumieć, co to się dzieje, pewnie będziesz musiał posiłkować się manualem do Mplusa.
przygotuj_inp = function(title="", data, variable, analysis=list(), model, output=list("STANDARDIZED", "TECH4", "TECH8"), savedata=list()) {
  kod = c(
    paste0("TITLE: ", title),
    "",
    "DATA:",
    paste0('FILE IS "', data$file, '";')
  )
  if ("format" %in% names(data)) kod = c(kod, "FORMAT IS ", lam_wiersze(data$format, sep=", "))
  kod = c(kod,
          "",
          "VARIABLE:"
  )
  if ("missing" %in% names(variable)) kod = c(kod, paste0("MISSING IS ", variable$missing, ";"))
  else if ("format" %in% names(data)) kod = c(kod, "MISSING IS BLANK;")
  kod = c(kod,
          "NAMES ARE", lam_wiersze(variable$names),
          "USEVARIABLES ARE", lam_wiersze(variable$usevariables),
          "CATEGORICAL ARE", lam_wiersze(variable$categorical),
          paste0("IDVARIABLE IS ", variable$idvariable, ";")
  )
  if ("useobservations" %in% names(variable)) kod = c(kod, "USEOBSERVATIONS ARE", lam_wiersze(variable$useobservations))
  kod = c(kod,
          "",
          "ANALYSIS:"
  )
  if ("estimator" %in% names(analysis))   kod = c(kod, paste0("ESTIMATOR IS ", analysis$estimator, ";"))
  if ("processors" %in% names(analysis))  kod = c(kod, paste0("PROCESSORS ARE ", analysis$processors, ";"))
  if ("integration" %in% names(analysis)) kod = c(kod, paste0("INTEGRATION IS ", analysis$integration, ";"))
  kod = c(kod,
          "",
          "MODEL:"
  )
  kod = c(kod,
          unlist(model),
          "",
          "OUTPUT:",
          paste0(unlist(output), ifelse(length(output)>0,";",""))
  )
  kod = c(kod,
          "",
          "SAVEDATA:"
  )
  if ("file" %in% names(savedata)) kod = c(kod, paste0('FILE IS "', savedata$file, '";'))
  if ("save" %in% names(savedata)) kod = c(kod, paste0("SAVE IS ", savedata$save, ";"))
  return(c(kod, ""))
}
#' @title Parsowanie pliku wynikow Mplusa.
#' @description
#' Funkcja parsuje plik poleceń .out Mplusa i wyciąga z niego użyteczne informacje.
#' @param output wektor tekstowy - wczytany plik poleceń Mplusa
#' @return lista
#' @details
#' W elementach zwracanej listy znajdują się:
#' \itemize{
#' \item \code{podsumowanie} - ,
#' \item \code{dopasowanie} - ,
#' \item \code{parametry} - ,
#' \item \code{zapis} - .
#' \item \code{czas} - .
#' }
obrob_out = function(output) {
  # wydzielanie części outputu z informacjami ogólnymi i ustawianiami
  podsumowanie = output[(grep("^SUMMARY OF ANALYSIS$", output)):(grep("^Input data file", output)-1)]
  # wydzielanie i obróbka części outputu ze statystykami dopasowania
  if (any(grepl("^MODEL FIT INFORMATION$", output)) & any(grepl("^MODEL RESULTS$", output))) {
    dopasowanie = output[(grep("^MODEL FIT INFORMATION$", output) + 1):(grep("^MODEL RESULTS$", output) - 1)]
    dopasowanie = sub("[*]$", "", dopasowanie)
    dopasowanie = as.list(dopasowanie[dopasowanie != ""])
    for (i in 1:length(dopasowanie)) {
      pozycjaLiczby = regexpr("[ ](|+-)[[:digit:]]+[[:digit:]+-. ]+$", dopasowanie[[i]])
      if (pozycjaLiczby != -1) {
        dopasowanie[[i]] = unlist(
          list(
            sub("[ ]+$", "", substr(dopasowanie[[i]], 1, pozycjaLiczby-1)),
            as.list(as.numeric(
              strsplit(
                sub("^[ ]+", "", substr(dopasowanie[[i]], pozycjaLiczby, nchar(dopasowanie[[i]]))),
                "[ ]+"
              )[[1]]
            ))
          ),
          recursive=FALSE
        )
      }
    }
    liczbKolumn = max(unlist(lapply(dopasowanie, length)))
    temp = as.data.frame(matrix(NA, nrow=length(dopasowanie), ncol=liczbKolumn, dimnames=list(NULL, rep("_", liczbKolumn))), check.names=FALSE)
    for (i in 1:ncol(temp)) {
      temp[, i] = unlist(lapply(dopasowanie, 
                                function(x, i) {
                                  return(ifelse((1:i) <= length(x), x, rep(NA, i))[i])
                                },
                                i=i
      ))
      if (all(grepl("^[[:digit:]+-.]+$", temp[, i]) | is.na(temp[,i]))) {
        temp[, i]=as.numeric(temp[, i])
      }
    }
    dopasowanie = temp
  }
  else dopasowanie = NULL
  # wydzielanie części outputu z wartościami parametrów
  if (any(grepl("^MODEL RESULTS$", output)) & any(grepl("^(QUALITY OF NUMERICAL RESULTS|MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES)$", output))) {
    temp = output[(grep("^MODEL RESULTS$", output) + 1):(grep("^(QUALITY OF NUMERICAL RESULTS|MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES)$", output)[1] - 1)]
    temp = temp[!grepl("^$|^STANDARDIZED MODEL RESULTS$|[ ]+Two-Tailed$", temp)]
    parametry = list()
    tnij = grep("Standardization|R-SQUARE|IRT PARAMETERIZATION", temp)
    while (length(tnij) > 0) {
      parametry[[length(parametry) + 1]] = temp[1:(tnij[1] - 1)]
      temp = temp[tnij[1]:length(temp)]
      tnij = tnij - tnij[1] + 1
      tnij = tnij[-1]
    }
    parametry[[length(parametry) + 1]] = temp      
    names(parametry) = tolower(unlist(lapply(parametry, 
                                             function(x) {
                                               return(sub("^.*IRT PARAMETERIZATION.*$", "irt",
                                                          sub(" Standardization", "",
                                                              sub("[-]SQUARE", "2",
                                                                  sub("^[ ]+Estimate.+$", "SUROWE", x[1])
                                                              )
                                                          )
                                               ))
                                             }
    )))
    # wstawka do usunięcia nikomu niepotrzebnych wyników w parametryzacji IRT, które Mplus wrzuca tylko wtedy, gdy model jest jednowymiarowy, a zadania są tylko 0-1
    parametry = parametry[names(parametry) != 'irt']
    # i przerabianie go na listę data.frame'ów
    parametry = lapply(parametry,
                       function(x) {
                         if (!grepl("^[ ]+Estimate.*$", x[1])) x=x[-1]
                         # nazwy kolumn
                         kolumny = c("Variable", strsplit(x[1], "[ ]+")[[1]][-1])
                         if (all(kolumny[1:2] == "Variable")) {  # sekcja dotycząca R2 - musi być obrobiona nieco inaczej
                           kolumny = kolumny[-1]
                           temp = list(x[-1])
                           attributes(temp)$typ = "R2"
                           attributes(temp)$zmienna = ""
                         }
                         else {
                           # pocięcie na poszczególne tabelki
                           tnij = c(grep("^ [[:upper:][:digit:]_]{1,8}[ ]+BY$|^ [[:upper:][:digit:]_]{1,8}[ ]+ON$|^ [[:upper:][:digit:]_]{1,8}[ ]+WITH$|[]Intercepts|^[ ]Means$|^[ ]Thresholds$|^[ ]Variances$|^[ ]Residual Variances$", x), length(x) + 1)
                           # stwierdzenie, co opisują kolejne tabelki
                           temp = list()
                           for (i in 1:(length(tnij) - 1)) {
                             temp[[length(temp) + 1]] = x[(tnij[i] + 1):(tnij[i + 1] - 1)]
                           }
                           attributes(temp)$typ = sub("^[ ]+", "", x[tnij[-length(tnij)]])
                           attributes(temp)$typ[grep("(BY|ON|WITH)$", attributes(temp)$typ)] = sub("^.*(BY|ON|WITH)", "\\1", attributes(temp)$typ[grep("(BY|ON|WITH)$", attributes(temp)$typ)])
                           attributes(temp)$zmienna = ifelse(
                             grepl("(BY|ON|WITH)$", attributes(temp)$typ),
                             sub("^([^ ]*)[ ]+(BY|ON|WITH)$", "\\1",
                                 sub("^[ ]+", "", x[tnij[-length(tnij)]])
                             ),
                             rep("", length(attributes(temp)$typ))
                           )
                         }
                         # konwersja tabelek na data.frame'y
                         x = lapply(temp, 
                                    function(x, kolumny) {
                                      x = lapply(strsplit(x, "[ ]+"), function(x) return(x[-1]))
                                      x = as.data.frame(matrix(unlist(x), ncol=length(kolumny), byrow=TRUE, dimnames=list(c(), kolumny)), stringsAsFactors=FALSE)
                                      for (i in 1:ncol(x)) {
                                        x[x[, i] %in% c("999", "999.000"), i] = NA
                                        if (all(grepl("^[[:digit:]+-.]+$", x[, i]) | is.na(x[, i]))) x[, i]=as.numeric(x[, i])
                                      }
                                      return(x)
                                    },
                                    kolumny = kolumny
                         )
                         attributes(x) = attributes(temp)
                         # przygotowanie tabelek do złączenia w jedną wielką tabelkę
                         x = mapply(
                           function(x, typ, zmienna) {
                             x = data.frame(
                               typ=tolower(sub("s$", "", typ)),
                               zmienna1=tolower(zmienna),
                               zmienna2=tolower(x$Variable),
                               wartosc=x$Estimate,
                               x[, !(names(x)%in%c("Variable", "Estimate"))],
                               stringsAsFactors=FALSE, check.names=FALSE
                             )
                             x$zmienna1[grepl("mean|variance|^r2$", x$typ)] = x$zmienna2[grepl("mean|variance|^r2$", x$typ)]
                             x$zmienna2[grepl("mean|variance|^r2$", x$typ)] = ""
                             x$zmienna1[x$typ=="threshold"] = sub("[$][[:digit:]]+$", "", x$zmienna2[x$typ=="threshold"])
                             x$zmienna2[x$typ=="threshold"] = sub("^.+[$]", "", x$zmienna2[x$typ=="threshold"])
                             return(x)
                           },
                           x,
                           as.list(attributes(x)$typ),
                           as.list(attributes(x)$zmienna),
                           SIMPLIFY=FALSE
                         )
                         if (length(x) > 1) temp=rbind(x[[1]], x[[2]])
                         else temp = x[[1]]
                         if (length(x) > 2) {
                           for (i in 3:length(x)) temp = rbind(temp, x[[i]])
                         }
                         return(temp)
                       }
    )
  }
  else parametry = NULL
  # wydzielanie i obróbka części outputu dotyczącej pliku, w którym zostały zapisane wyniki
  if (any(output == "SAVEDATA INFORMATION") & any(grepl("Save file format", output))) {
    zapis = output[(grep("Order and format of variables", output)+2):(grep("Save file format", output)[1]-2)]
    zapis = lapply(strsplit(zapis, "[ ]+"), function(x) return(tolower(x[x != ""])))
    zapis = as.data.frame(matrix(unlist(zapis), ncol=2, byrow=TRUE, dimnames=list(c(), c("zmienna", "szerokosc"))), stringsAsFactors=FALSE)
    zapis$szerokosc = as.numeric(sub("^f([[:digit:]]+).[[:digit:]]+", "\\1", zapis$szerokosc))
  }
  else zapis = NULL
  # wydzielenie z outputu stopki z czasem wykoniania
  czas = output[(grep("(Beginning|Ending|Elapsed) Time", output))]
  # kończenie
  wyniki = list(
    podsumowanie=podsumowanie,
    dopasowanie=dopasowanie,
    parametry=parametry,
    zapis=zapis,
    czas=czas
  )
  if (any(grepl("THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY DUE TO A NON-ZERO", output))) {
    wyniki[[length(wyniki)+1]] = "Model nie zbiegł!"
    names(wyniki)[length(wyniki)] = "brak_zbieznosci"
  }
  return(wyniki)
}
