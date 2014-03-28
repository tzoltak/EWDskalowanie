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
  plik = try(file(nazwaPliku, "r"))
  if ("try-error" %in% class(plik)) stop("Nie daje się otworzyć pliku!")
  tresc = try(readLines(plik))
  if ("try-error" %in% class(plik)) stop("Nie daje się odczytać z pliku!")
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
#' @return lista
#' @seealso \code{\link[base]{strwrap}}
#' @examples
#' x=c("y BY", "x1*1.200", "x2*1.000", "x3*0.800", "x4*0.600", "x5*0.400")
#' write(lam_wiersze(x), "")
#' write(lam_wiersze(x, wciecie=4, maxDl=30), "")
#' @export
lam_wiersze = function(x, wciecie=2, maxDl=90, srednikNaKoncu=TRUE, sep=" ") {
  stopifnot(is.character(x), is.numeric(wciecie), length(wciecie) == 1,
            is.numeric(maxDl), length(maxDl) == 1, wciecie < (maxDl - 8),
            is.logical(srednikNaKoncu), length(srednikNaKoncu) == 1,
            is.character(sep) & length(sep) == 1
  )
  wciecie=paste0(rep(" ", wciecie), collapse="")  # przygotowanie wcięcia
  x[1] = paste0(wciecie, x[1])  # i dopisanie na początku pierwszego wiersza
  if (length(x) > 1) x = paste0(x, collapse=sep)  # scalamy w jeden ciąg znaków
  while (nchar(x[length(x)]) > maxDl) {  # jak długo ostatni element jest dłuższy, niż może być, tnij dalej
    spacje = gregexpr("[ ]", substr(x[length(x)], 1, maxDl))[[1]]  # położenie spacji
    x=c(
      x[(1:length(x)) < length(x)],
      substr(x[length(x)], 1, spacje[length(spacje)]-1),
      paste0(
        wciecie,
        substr(x[length(x)], spacje[length(spacje)] + 1, nchar(x[length(x)]))
      )
    )
  }
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
#' @title Skalowanie modeli IRT.
#' @description
#' Funkcja służąca do przeprowadzania potencjalnie złożonych procedur skalowania (typowo egzaminów) z wykorzystaniem Mplusa (a docelowo również pakietu \code{mirt}).
#' @param dane data.frame z danymi
#' @param opisProcedury p. szczegóły
#' @param idObs ciąg znaków z nazwą zmiennej będącej unikalnym identyfikatorem obserwacji (w \code{dane})
#' @param tytul ciąg znaków, który będzie wstawiany w sekcji TITLE plików poleceń Mplusa, a jego 12 pierwszych znaków zostanie wykorzystane do tworzenia nazw plików z poleceniami Mplus
#' @param zmienneCiagle wektor tekstowy podający nazwy zmiennych, które mają być w analizie traktowane jako ciągłe (domyślnie wszystkie zmienne traktowane są jako porządkowe)
#' @param zmienneSpecjalne lista - na obecnym etapie rozwoju nie wykorzystywana
#' @param zwrocOszacowania wartość logiczna - czy estymować (i zwrócić) również oszacowania natężenia badanych cech dla poszczególnych jednostek obserwacji?
#' @param usunFWF wartość logiczna - czy usuwać pliki o stałej szerokości, w których zapisywane są dane dla Mplusa?
#' @details
#' Poniżej trzeba będzie dodać wyczerpujący opis parametru \code{opisProcedury}
#' \itemize{
#' \item 
#' \item .
#' }
#' @return lista
#' @examples
#' # chwilowo brak
#' @export
skaluj = function(dane, opisProcedury, idObs, tytul="", zmienneCiagle=NULL, zmienneSpecjalne=NULL, zwrocOszacowania=TRUE, usunFWF=FALSE) {
  # podstawowe sprawdzenie argumentów
  cat("Sprawdzanie poprawności argumentów...\n")
  stopifnot(
    is.data.frame(dane),
    is.list(opisProcedury),
    is.character(idObs), idObs %in% names(dane),
    is.character(tytul), length(tytul) == 1,
    is.character(zmienneCiagle) | is.null(zmienneCiagle),
    all(zmienneCiagle %in% names(dane)) | is.null(zmienneCiagle),
    is.list(zmienneSpecjalne) | is.null(zmienneSpecjalne),
    all(zmienneSpecjalne %in% names(dane)) | is.null(zmienneSpecjalne),
    zwrocOszacowania %in% c(TRUE, FALSE),
    usunFWF %in% c(TRUE, FALSE)
  )
  if (max(nchar(names(dane))) > 8) stop("Niestety wszystkie nazwy zmiennych muszą być nie dłuższe niż 8 znaków.")
  if (!all(grepl("^[[:lower:]][[:lower:][:digit:]_]{0,8}$", names(dane)))) stop("Wszystkie nazwy zmiennych muszą składać się wyłącznie z małych liter, cyfr i znaku '_', przy czym pierwszym znakiem musi być litera.")
  # wywalanie zmiennych posiadających same braki danych
  maskaSameNA = unlist(lapply(dane, function(x) return(all(is.na(x)))))
  if (any(maskaSameNA)) {
    warning(paste0("Z danych usunięto zmienne, które przyjmowały wyłącznie wartości 'brak danych':\n - ", paste0(names(dane)[maskaSameNA], collapse="\n - "), "\n"), immediate.=TRUE)
    opisProcesury = lapply(opisProcedury,
                           function(x, zmienneSameNA) {
                             x$czescPomiarowa = lapply(x$czescPomiarowa,
                                                       function(x, zmienneSameNA) {
                                                         x$zmienne = x$zmienne[!(x$zmienne %in% zmienneSameNA)]
                                                         return(x)
                                                       },
                                                       zmienneSameNA=zmienneSameNA
                             )
                             return(x)
                           }, zmienneSameNA=names(dane)[maskaSameNA]
    )
    dane = dane[, !maskaSameNA]
  }
  # wywalanie zmiennych z zerową wariancją
  maskaWariancja0 = unlist(lapply(dane, function(x) return(length(unique(na.omit(x))) == 1)))
  if (any(maskaWariancja0)) {
    warning(paste0("Z danych usunięto zmienne, które miały zerową wariancję (tj. przyjmowały tylko jedną wartość):\n - ", paste0(names(dane)[maskaWariancja0], collapse="\n - "), "\n"), immediate.=TRUE)
    opisProcedury = lapply(opisProcedury,
                           function(x, zmienneWariancja0) {
                             x$czescPomiarowa = lapply(x$czescPomiarowa,
                                                       function(x, zmienneWariancja0) {
                                                         x$zmienne = x$zmienne[!(x$zmienne %in% zmienneWariancja0)]
                                                         return(x)
                                                       }, zmienneWariancja0=zmienneWariancja0
                             )
                             return(x)
                           },
                           zmienneWariancja0=names(dane)[maskaWariancja0]
    )
    dane = dane[, !maskaWariancja0]
  }
  # sprawdzanie poprawności struktury argumentu opisProcedury
  dozwoloneElementy = list(
    krok=c("czescPomiarowa", "parametry"),
    konstrukt=c("zmienne", "var1", "rasch", "kryteriaUsuwania", "wartosciStartowe", "wartosciZakotwiczone"),
    kryteriaUsuwania=c("dyskryminacjaPonizej", "istotnoscPowyzej", "nigdyNieUsuwaj"),
    parametry=c("estimator", "processors", "integration", "fscores"),
    wartosci=c("typ", "zmienna1", "zmienna2", "wartosc")	# akurat te w roli niezbędnych, a nie dozwolonych
  )
  # pętla po krokach procedury
  for (i in 1:length(opisProcedury)) {
    krok = opisProcedury[[i]]  # mały aliasik, żeby mniej pisać
    if (!all(names(krok) %in% dozwoloneElementy$krok)) warning(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ".\nPojawiły się elementy listy, które nie są rozpoznawane przez funkcję:\n - ", paste0(names(krok)[!(names(krok) %in% dozwoloneElementy$krok)], collapse="\n - "), "\nZostaną one przez funkcję pominięte.\n"), immediate.=TRUE)
    # pętla po konstruktach w ramach kroku
    for (j in 1:length(krok$czescPomiarowa)) {
      konstrukt = krok$czescPomiarowa[[j]]
      if (!all(names(konstrukt) %in% dozwoloneElementy$konstrukt)) warning(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nPojawiły się elementy listy, które nie są rozpoznawane przez funkcję:\n - ", paste0(names(konstrukt)[!(names(konstrukt) %in% dozwoloneElementy$konstrukt)], collapse="\n - "), "\nZostaną one przez funkcję pominięte.\n"), immediate.=TRUE)
      
      # sprawdzanie poprawności komponentu 'zmienne'
      if (!"zmienne" %in% names(konstrukt)) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nBrak elementu 'zmienne', podającego zmienne mierzalne związane z konstruktem.\n"))
      if (!(is.character(konstrukt$zmienne) | is.list(konstrukt$zmienne)) | length(konstrukt$zmienne) < 1) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nElement 'zmienne' musi być wektorem tekstowym (typu character), podającym nazwy zmiennych mierzalnych związanych z konstruktem.\n"))
      if (is.character(konstrukt$zmienne)) {
        if (!all(konstrukt$zmienne %in% c(names(dane), names(krok$czescPomiarowa)))) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nPodane zostały nazwy zmiennych, które nie występują w danych:\n - ", paste0(konstrukt$zmienne[!(konstrukt$zmienne %in% names(dane))], collapse="\n - "), "\n"))
      }
      # gdy konstrukt definiowany symbolicznie
      if (is.list(konstrukt$zmienne)) {
        if (!is.language(konstrukt$zmienne[[1]])) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ", w elemencie 'zmienne'.\nNieprawidłowa definicja symboliczna zmiennych związanych z konstruktem.\nPierwszym elementem listy powinna być formuła (formula).\n"))
        if(i > 1) konstruktyZPoprzednichKrokow = unlist(lapply(opisProcedury[1:(i - 1)], function(x) return(names(x$czescPomiarowa))))
        else stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], " w elemencie 'zmienne'.\nW pierwszym kroku konstrukty nie mogą być definiowane symbolicznie.\n"))
        konstruktyWDef = all.vars(konstrukt$zmienne[[1]])
        if (!all(konstruktyWDef %in% konstruktyZPoprzednichKrokow)) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ", w elemencie 'zmienne'.\nPodane zostały nazwy konstruktów, które nie występują w poprzednich krokach procedury:\n - ", paste0(konstruktyWDef[!(konstruktyWDef %in% konstruktyZPoprzednichKrokow)], collapse="\n - "), "\n"))
        if (!all(names(konstrukt$zmienne)[-1] %in% konstruktyWDef)) warning(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ", w elemencie 'zmienne'.\nPojawiły się elementy, które nie są związane z żadnym konstruktem, ze zdefiniowanych w pierwszym elemencie:\n - ", paste0(names(konstrukt$zmienne)[-1][!(names(konstrukt$zmienne)[-1] %in% konstruktyWDef)], collapse="\n - "), "\nZostaną one przez funkcję pominięte.\n"), immediate.=TRUE)
        if (!all(is.character(unlist(konstrukt$zmienne[-1]))) | !all(unlist(lapply(konstrukt$zmienne[-1], length)) == 1))    stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ", w elemencie 'zmienne'.\nNieprawidłowa definicja symboliczna zmiennych związanych z konstruktem.\nWszystkie elementy listy poza pierwszym powinny być ciągami znaków (jednoelementowymi wektorami typu character).\n"))
      }
      
      # sprawdzanie poprawności komponentu 'var1'
      if (!"var1" %in% names(konstrukt)) opisProcedury[[i]]$czescPomiarowa[[j]]$var1=TRUE	# jeśli nie podano, załóż, że wszystkie ładunki mają być uwolnione, a wariancja zmiennej ukrytej zaktowiczona w 1
      if (!all(konstrukt$var1 %in% c(TRUE, FALSE)) | length(konstrukt$var1) != 1) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nElement 'var1' musi być jednoelementowym wektorem logicznym (typu logical), przyjmującym wyłącznie wartość TRUE lub FALSE.\n"))
      
      # sprawdzanie poprawności komponentu 'rasch'
      if (!"rasch" %in% names(konstrukt)) opisProcedury[[i]]$czescPomiarowa[[j]]$rasch=FALSE	# jeśli nie podano, załóż 2PL/SGRM
      if (!all(konstrukt$rasch %in% c(TRUE, FALSE)) | length(konstrukt$rasch) != 1) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nElement 'rasch' musi być jednoelementowym wektorem logicznym (typu logical), przyjmującym wyłącznie wartość TRUE lub FALSE.\n"))
      
      # sprawdzanie poprawności komponentu 'kryteriaUsuwania'
      if (!"kryteriaUsuwania" %in% names(konstrukt)) opisProcedury[[i]]$czescPomiarowa[[j]]$kryteriaUsuwania=NULL
      if (!is.null(konstrukt$kryteriaUsuwania) & !all(names(konstrukt$kryteriaUsuwania) %in% dozwoloneElementy$kryteriaUsuwania)) warning(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nWśród podanych kryteriów usuwania zadań pojawiły się elementy listy, które nie są rozpoznawane przez funkcję:\n - ", paste0(names(konstrukt$kryteriaUsuwania)[!(names(konstrukt$kryteriaUsuwania)%in%dozwoloneElementy$kryteriaUsuwania)], collapse="\n - "), "\nZostaną one przez funkcję pominięte.\n"), immediate.=TRUE)
      if (!is.null(konstrukt$kryteriaUsuwania$dyskryminacjaPonizej)) {
        if (!is.numeric  (konstrukt$kryteriaUsuwania$dyskryminacjaPonizej) | length(konstrukt$kryteriaUsuwania$dyskryminacjaPonizej) != 1 | any(is.na(konstrukt$kryteriaUsuwania$dyskryminacjaPonizej))) {
          stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], "\nkryterium usuwania zadań 'dyskryminacjaPonizej' musi być zdefiniowane jako jednoelementowy wektor liczbowy (typu numeric).\n"))
        }
      }	
      if (!is.null(konstrukt$kryteriaUsuwania$istotnoscPowyzej)) {
        if (!is.numeric  (konstrukt$kryteriaUsuwania$istotnoscPowyzej)     | length(konstrukt$kryteriaUsuwania$istotnoscPowyzej)     != 1 | any(is.na(konstrukt$kryteriaUsuwania$istotnoscPowyzej) | konstrukt$kryteriaUsuwania$istotnoscPowyzej > 1 | konstrukt$kryteriaUsuwania$istotnoscPowyzej <= 0)) {
          stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], "\nkryterium usuwania zadań 'istotnoscPowyzej' musi być zdefiniowane jako jednoelementowy wektor liczbowy (typu numeric), przyjmujący wartości z zakresu (0,1].\n"))
        }
      }
      if (!is.null(konstrukt$kryteriaUsuwania$nigdyNieUsuwaj)) {
        if (!is.character(konstrukt$kryteriaUsuwania$nigdyNieUsuwaj)       | length(konstrukt$kryteriaUsuwania$nigdyNieUsuwaj)       != 1 | any("try_error" %in% try(grepl(konstrukt$kryteriaUsuwania$nigdyNieUsuwaj, "")))) {
          stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], "\nkryterium usuwania zadań 'nigdyNieUsuwaj' musi być zdefiniowane jako jednoelementowy wektor tekstowy, będący poprawnym składniowo wyrażeniem regularnym.\n"))
        }
      }
      # sprawdzanie poprawności komponentu 'wartosciStartowe'
      if (!is.null(konstrukt$wartosciStartowe)) {
        if (is.vector(konstrukt$wartosciStartowe)) {
          if (!all(konstrukt$wartosciStartowe %in% c(TRUE)) | length(konstrukt$wartosciStartowe) > 1 | !is.list(konstrukt$zmienne)) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nElement 'wartosciStartowe' może przyjąć wartość 'TRUE' tylko, jeśli konstrukt jest zdefiniowany symbolicznie.\n"))
        }
        else{
          blad = paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nElement 'wartosciStartowe' musi być data framem z kolumnami 'typ' (character), 'zmienna1' (character), 'zmienna2' (character), 'wartosc' (numeric).\n")
          if (!is.data.frame(konstrukt$wartosciStartowe)) {
            stop(blad)
          } else if (!all(dozwoloneElementy$wartosci %in% names(konstrukt$wartosciStartowe))) {
            stop(blad)
          } else if (!(with(konstrukt$wartosciStartowe, {mode(typ) == "character" & mode(zmienna1) == "character" & mode(zmienna2) == "character" & mode(wartosc) == "numeric"}))) {
            stop(blad)
          }
        }
      }
      # sprawdzanie poprawności komponentu 'wartosciZakotwiczone'
      if (!is.null(konstrukt$wartosciZakotwiczone)) {
        if (is.vector(konstrukt$wartosciZakotwiczone)) {
          if (!all(konstrukt$wartosciZakotwiczone %in% c(TRUE)) | length(konstrukt$wartosciZakotwiczone) > 1 | !is.list(konstrukt$zmienne)) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nElement 'wartosciZakotwiczone' może przyjąć wartość 'TRUE' tylko, jeśli konstrukt jest zdefiniowany symbolicznie.\n"))
        } else {
          blad=paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nElement 'wartosciZakotwiczone' musi być data framem z kolumnami 'typ' (character), 'zmienna1' (character), 'zmienna2' (character), 'wartosc' (numeric).\n")
          if (!is.data.frame(konstrukt$wartosciZakotwiczone)) {
            stop(blad)
          } else if (!all(dozwoloneElementy$wartosci%in%names(konstrukt$wartosciZakotwiczone))) {
            stop(blad)
          } else if (!(with(konstrukt$wartosciZakotwiczone, {mode(typ) == "character" & mode(zmienna1) == "character" & mode(zmienna2) == "character" & mode(wartosc) == "numeric"}))) {
            stop(blad)
          }
        }
        # sprawdzanie interakcji z komponentami 'var1' i 'rasch'
        # (te same przypadki są też na wszelki wypadek obsługiwane w funkcji 'przygotuj_model()', z tym że 'rasch' lub 'var1' ustawiane są tam na FALSE bez komunikowania użytkownikowi)
        if (length(unique(konstrukt$wartosciZakotwiczone$wartosc[konstrukt$wartosciZakotwiczone$typ == "by"])) > 1 & konstrukt$rasch) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\n  Podano wartości zakotwiczone dla dyskryminacji, które dla niektórych zadań różnią się między sobą.\n  W tej sytuacji niedozwolone jest ustawienie wartości elementu 'rasch' na TRUE.\n"))
        if (sum(konstrukt$wartosciZakotwiczone$typ == "by") > 0 & konstrukt$var1) {
          opisProcedury[[i]]$czescPomiarowa[[j]]$var1 = FALSE
          warning(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\n  Podano wartości zakotwiczone dla dyskryminacji i jednocześnie nakazano zakotwiczyć wariancję konstruktu w 1.\n  Jest mało prawdopodobne, że chcesz tego, co zadeklarowałeś.\n  Wartość elementu 'var1' została zmieniona na 'FALSE'.\n  Jeśli jesteś pewien, że chcesz jednocześnie kotwiczyć wartości dyskryminacji i wariancję konstruktu, do zakotwiczenia tej drugiej użyj elementu 'wartosciZakotwiczone'.\n"))
        }
      }
      # sprawdzanie, czy komponenty 'wartosciStartowe' i 'wartosciZakotwiczone' nie są ze sobą sprzeczne
      if (!is.null(konstrukt$wartosciStartowe) & !is.null(konstrukt$wartosciZakotwiczone)) {
        if (is.vector(konstrukt$wartosciStartowe) | is.vector(konstrukt$wartosciZakotwiczone)) {
          stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nKiedy konstrukt definiowany jest w sposób symboliczny można przypisać wartość 'TRUE' albo elementowi 'wartosciStartowe', albo 'wartosciZakotwiczone'.\n"))
        } else {
          konflikty = rbind(konstrukt$wartosciStartowe[, dozwoloneElementy$wartosci[1:3]], konstrukt$wartosciZakotwiczone[, dozwoloneElementy$wartosci[1:3]])
          konflikty = konflikty[duplicated(konflikty), ]
          if (nrow(konflikty) > 0) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nKonflikt elementów 'wartosciStartowe' i 'wartosciZakotwiczone' (nie chce mi się pisać kodu, który powiedział by Ci dokładnie, co się dubluje).\n"))
        }
      }
    }
    # sprawdzanie poprawności parametrów estymacji
    if (!("parametry" %in% names(krok))) opisProcedury[[i]]$parametry = list()
    parametry=opisProcedury[[i]]$parametry
    if (!all(names(parametry) %in% dozwoloneElementy$parametry)) warning(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], "\n, w parametrach sterujących estymacją dla Mplusa pojawiły się elementy listy, które nie są rozpoznawane przez funkcję:\n - ", paste0(names(parametry)[!(names(parametry)%in%dozwoloneElementy$parametry)], collapse="\n - "), "\nZostaną one przez funkcję pominięte.\n"), immediate.=TRUE)
    if ("estimator" %in% names(parametry)) {
      if (!all(parametry$estimator %in% c("ML", "MLM", "MLMV", "MLR", "MLF", "GLS", "WLS", "WML", "WLSM", "WLSMV", "ULSM", "BAYES")) | length(parametry$estimator) != 1) {
        stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", parametr: 'estimator' przyjmuje niedozwoloną wartość (p. dokumentacja Mplusa).\n"))
      }
    }
    if ("processors" %in% names(parametry)) {
      if (!all(parametry$processors %in% (1:64)) | length(parametry$processors) !=1 ) {
        stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", parametr: 'processors' przyjmuje niedozwoloną wartość (powinien być liczbą całkowitą).\n"))
      }
    }
    if ("integration" %in% names(parametry)) {
      if (!all(grepl("^(STANDARD|MONTECARLO) [(][[:digit:]]{1,3}[)]", parametry$integration)) | length(parametry$integration) !=1 ) {
        stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", parametr: 'integration' przyjmuje niedozwoloną wartość (p. dokumentacja Mplusa).\n"))
      }
    }
    if ("fscores" %in% names(parametry)) {
      if (!all(parametry$fscores %in% c(TRUE, FALSE)) | length(parametry$fscores) !=1 ) {
        stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", parametr: 'estimator' przyjmuje niedozwoloną wartość (dozwolone: TRUE, FALSE).\n"))
      }
    }
    else opisProcedury[[i]]$parametry$fscores = FALSE
  }
  # zapis pliku z danymi w formie stałoszerokościowej
  cat("Zapis danych do pliku tekstowego o stałej szerokości kolumn...\n")
  # kowersja factorów na liczby
  maskaFactory = (1:ncol(dane))[unlist(lapply(dane, is.factor))]
  for (i in maskaFactory) dane[, i]=as.numeric(dane[, i])
  # zamiana zmiennych na ciągi znaków o stałej długości
  dane = as.data.frame(
    lapply(dane,
      function(x) {
        y = as.character(x)
        y[is.na(x)] = ""
        pozycjaKropki = regexpr(".", y, fixed=TRUE)
        if (any(pozycjaKropki != -1)) {  # liczby niecałkowite
          maxDoKropki = max(c(max(pozycjaKropki[pozycjaKropki >= 0]), 1 + max(nchar(y[pozycjaKropki == -1]))))
          poKropce = nchar(y[pozycjaKropki >= 0])-pozycjaKropki[pozycjaKropki >= 0]
          maxPoKropce = max(poKropce)
          x = format(x, width=maxDoKropki + maxPoKropce, justify="right", nsmall=maxPoKropce)
        } else {  # liczby całkowite
          maxPoKropce = 0
          maxDoKropki = max(nchar(y))
          x = format(y, width=maxDoKropki, justify="right")
        }
        attributes(x)$format = paste0("F", maxDoKropki, ".", maxPoKropce)
        return(x)
      }
    ),
    stringsAsFactors=FALSE
  )
  szerokosciKolumn = unlist(lapply(dane, function(x) return(attributes(x)$format)))
  # nadawanie szerokosciKolumn bardzie zwartej formy (co by Mplus nie pluł się błędem-nie błędem
  l = 1
  temp = szerokosciKolumn[1]
  for (k in 2:length(szerokosciKolumn)) {
    if (szerokosciKolumn[k] == szerokosciKolumn[k - 1]) {
      l = l + 1
      if (k < length(szerokosciKolumn)) next
      else if (l > 1) temp[length(temp)] = paste0(l, temp[length(temp)])
    } else {
      if (l > 1) temp[length(temp)] = paste0(l, temp[length(temp)])
      temp = c(temp, szerokosciKolumn[k])
      l = 1
    }
  }
  szerokosciKolumn = temp
  # zapis
  temp = try(write.table(apply(dane, 1, paste0, collapse=""), "daneMplusTemp.fwf", row.names=FALSE, col.names=FALSE, quote=FALSE), silent=TRUE)
  if ("try-error" %in% class(temp)) stop("Nie udało się zapisać danych do pliku.")
  # pętla główna
  wyniki = vector(mode="list", length=length(opisProcedury))
  names(wyniki) = names(opisProcedury)
  
  for (i in 1:length(opisProcedury)) {
    cat("\n###################################\n Krok ", i, ". ", names(opisProcedury)[i], "\n###################################\n", sep="")
    # ewaluacja konstruktów zdefiniowanych "symbolicznie"
    for (k in 1:length(opisProcedury[[i]]$czescPomiarowa)) {
      if (!is.language(opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne[[1]])) next
      konstruktyWDef = all.vars(opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne[[1]])
      maski = opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne[-1]
      opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne = NULL
      szkielet = as.data.frame(matrix(NA, nrow=0, ncol=ncol(wyniki[[1]][[1]]$parametry$surowe), dimnames=list(c(), names(wyniki[[1]][[1]]$parametry$surowe))))
      if      (!is.null(opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciZakotwiczone)) opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciZakotwiczone = szkielet
      else if (!is.null(opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe    )) opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe     = szkielet
      
      for (l in konstruktyWDef) {
        indeks = max((1:(i - 1))[unlist(lapply(opisProcedury[1:(i - 1)], function(x, l) return(l %in% names(x$czescPomiarowa)), l=l))])
        if (!(l %in% names(maski))) {
          maski[[length(maski) + 1]] = "^.*$"
          names(maski)[length(maski)] = l
        }
        zmienneTemp = opisProcedury[[indeks]]$czescPomiarowa[names(opisProcedury[[indeks]]$czescPomiarowa) == l][[1]]$zmienne
        opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne = c(opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne, zmienneTemp[grepl(maski[names(maski) == l][[1]], zmienneTemp)])
        
        # tylko dyskryminacje i progi, wariancje zostawiamy w spokoju
        parametryTemp = wyniki[[indeks]][[length(wyniki[[indeks]])]]$parametry$surowe
        parametryTemp = rbind(
          parametryTemp[with(parametryTemp, {typ == "by"        & zmienna1 == l                       & grepl(maski[names(maski) == l][[1]], zmienna2)}), ],
          parametryTemp[with(parametryTemp, {typ == "threshold" & grepl(maski[names(maski) == l][[1]]                                      , zmienna1)}), ]
        )
        if        (!is.null(opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciZakotwiczone)) {
          opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciZakotwiczone = rbind(opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciZakotwiczone, parametryTemp)
        } else if (!is.null(opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe)) {
          opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe     = rbind(opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe,     parametryTemp)
        } #else {
        # jeśli 'wartosciZakotwiczone' i 'wartosciStartowe' oba są NULLami - kotwiczymy
        #    opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciZakotwiczone = rbind(opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciZakotwiczone, parametryTemp)
        #}
      }
    }
    # pętla - kalibracje w ramach kroku
    j = 1
    kalibrujDalej = TRUE
    while (kalibrujDalej) {
      cat("\n   ###################################   \n    Kalibracja ", j, ".\n   ###################################\n", sep="")
      krok = opisProcedury[[i]]	# tu, a nie przed pętlą, bo opisProcedury jest (może być) modyfikowany na podstawie wyników estymacji
      # przygotowanie obiektów dla funkcji tworzącej polecenia Mplusa
      title = paste0(tytul, " Krok ", i, ". ", names(opisProcedury)[i])
      data=list(
        file   = "daneMplusTemp.fwf",
        format = szerokosciKolumn
      )
      zmienneWModelu = unique(unlist(lapply(krok[names(krok) %in% c("czescPomiarowa")], function(x) return(lapply(x, function(x) return(x$zmienne))))))
      zmienneWModelu = zmienneWModelu[zmienneWModelu %in% names(dane) & !(zmienneWModelu %in% zmienneCiagle)]	# trzeba wykluczyć składowe 
      variable = list(
        missing     = "BLANK",
        names       = names(dane),
        usevariables= zmienneWModelu[  zmienneWModelu %in% names(dane)   ],
        categorical = zmienneWModelu[!(zmienneWModelu %in% zmienneCiagle)],
        idvariable  = idObs
      )
      analysis = krok$parametry[names(krok$parametry) %in% c("estimator", "processors", "integration")]
      model = przygotuj_model(krok)
      output = list("STANDARDIZED", "TECH4", "TECH8")
      if (krok$parametry$fscores) {
        savedata = list(
          file = "ocCzynMplusTemp.fwf",
          save = "FSCORES"
        )
      } else {
        savedata = NULL
      }
      # zapis pliku poleceń Mplusa
      nazwaInp = paste0(substr(tytul, 1, min(12, nchar(tytul))), "_krok_", i, "_kalibr_", j, ".inp")
      write.table(
        przygotuj_inp(title, data, variable, analysis, model, output, savedata),
        nazwaInp, row.names=FALSE, col.names=FALSE, quote=FALSE
      )
      # kalibracja w Mplusie
      mplus = system2("mplus", paste0('"', nazwaInp, '"'))
      if (mplus == 1) return(list(wyniki=wyniki, tenKrok=krok))
      wyniki[[i]][[j]] = obrob_out(readLines(sub("[.]inp$", ".out", nazwaInp)))
      if ("brak_zbieznosci" %in% names(wyniki[[i]][[j]])) {
        cat("\n   ###################################\n    Nie osiągnięto zbieżności!\n   ###################################\n")
        return(list(wyniki=wyniki, tenKrok=krok))
      }
      # zapis ocen czynnikowych do bardziej zwartej i łatwiej dostępnej postaci
      if (!is.null(wyniki[[i]][[j]]$zapis)) {
        ocCzyn = wczytaj_fwf(savedata$file, wyniki[[i]][[j]]$zapis$szerokosc, wyniki[[i]][[j]]$zapis$zmienna)
        ocCzyn = ocCzyn[, grepl(
          paste0(
            "^", idObs, "$|^(",
            paste0(unique(wyniki[[i]][[j]]$parametry$surowe$zmienna1[wyniki[[i]][[j]]$parametry$surowe$typ == "by"]), collapse="|"),
            ")(|_se)$"
          ),
          names(ocCzyn)
        )]
        write.csv2(ocCzyn, sub("[.]inp$", ".csv", nazwaInp), row.names=FALSE, na="")
        unlink(savedata$file)
        if (zwrocOszacowania) wyniki[[i]][[j]]$zapis = ocCzyn
        else wyniki[[i]][[j]]$zapis = sub("[.]inp$", ".csv", nazwaInp)
      }							
      # ew. usuwanie zadań nie spełniających kryteriów
      kalibrujDalej = FALSE
      # sprawdzanie części pomiarowej
      for (k in 1:length(opisProcedury[[i]]$czescPomiarowa)) {
        parametry = wyniki[[i]][[j]]$parametry$surowe[wyniki[[i]][[j]]$parametry$surowe$typ == "by" & wyniki[[i]][[j]]$parametry$surowe$zmienna1 == names(opisProcedury[[i]]$czescPomiarowa)[k], ]
        # jeśli zdefiniowano jakieś kryteria usuwania
        if (!is.null(opisProcedury[[i]]$czescPomiarowa[[k]]$kryteriaUsuwania)) {
          cat("\n   ###################################\n    Usuwanie źle dopasowanych zadań: konstrukt ", names(opisProcedury[[i]]$czescPomiarowa)[k], "\n   ###################################\n", sep="")
          kryteria = opisProcedury[[i]]$czescPomiarowa[[k]]$kryteriaUsuwania
          doUsuniecia = c()
          # jeśli zdefiniowano kryterium 'dyskryminacjaPonizej'
          if ("dyskryminacjaPonizej" %in% names(opisProcedury[[i]]$czescPomiarowa[[k]]$kryteriaUsuwania)) {
            pozaZakresem = parametry[parametry$wartosc < kryteria$dyskryminacjaPonizej, ]
            # z listy pozycji do usunięcia wypadają pozycje, których nazwa pasuje do maski nigdyNieUsuwaj
            if (!is.null(opisProcedury[[i]]$czescPomiarowa[[k]]$kryteriaUsuwania$nigdyNieUsuwaj)) {
              pozaZakresem = pozaZakresem[!grepl(opisProcedury[[i]]$czescPomiarowa[[k]]$kryteriaUsuwania$nigdyNieUsuwaj, pozaZakresem$zmienna2), ]
            }
            # wybór zadania do usunięcia i drukowanie informacji
            doUsuniecia = pozaZakresem$zmienna2[which.max(kryteria$dyskryminacjaPonizej-pozaZakresem$wartosc)]
            if (length(doUsuniecia) > 0) {
              cat("    Zadania o dyskryminacji poniżej ", kryteria$dyskryminacjaPonizej, ":\n",  sep="")
              print(
                data.frame(
                  "_"="     ",
                  zadanie=pozaZakresem$zmienna2,
                  dyskryminacja=pozaZakresem$wartosc,
                  "usunięte"=ifelse(pozaZakresem$zmienna2 == doUsuniecia, rep("tak", nrow(pozaZakresem)), rep("",nrow(pozaZakresem))),
                  check.names=FALSE, stringsAsFactors=FALSE
                ),
                row.names=FALSE
              )
            }
          }
          # jeśli nic nie wykluczono na podstawie ww. kryterium, ale zdefiniowano kryterium 'istotnoscPowyzej'
          if (length(doUsuniecia) == 0 & "istotnoscPowyzej" %in% names(opisProcedury[[i]]$czescPomiarowa[[k]]$kryteriaUsuwania)) {
            pozaZakresem = parametry[parametry$"P-Value" > kryteria$istotnoscPowyzej, ]
            doUsuniecia = pozaZakresem$zmienna2[which.max(pozaZakresem$"P-Value" - kryteria$istotnoscPowyzej)]
            # z listy pozycji do usunięcia wypadają pozycje, których nazwa pasuje do maski nigdyNieUsuwaj
            if (!is.null(opisProcedury[[i]]$czescPomiarowa[[k]]$kryteriaUsuwania$nigdyNieUsuwaj)) {
              doUsuniecia = doUsuniecia[!grepl(opisProcedury[[i]]$czescPomiarowa[[k]]$kryteriaUsuwania$nigdyNieUsuwaj, doUsuniecia)]
            }
            # drukowanie informacji o zadaniach do usunięcia
            if (length(doUsuniecia) > 0) {
              cat("    Zadania o istotności powyżej ", kryteria$istotnoscPowyzej, ":\n",  sep="")
              print(
                data.frame(
                  "_"="     ",
                  zadanie=pozaZakresem$zmienna2,
                  istotnosc=pozaZakresem$wartosc,
                  "usunięte"=ifelse(pozaZakresem$zmienna2==doUsuniecia, rep("tak",nrow(pozaZakresem)), rep("",nrow(pozaZakresem))),
                  check.names=FALSE, stringsAsFactors=FALSE
                ),
                row.names=FALSE
              )
            }
          }
          # usuwamy najgorszą pozycję
          if (length(doUsuniecia)>0) {
            kalibrujDalej=TRUE
            opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne=opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne[!(opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne%in%doUsuniecia)]
          }
          else cat("    Brak zadań do usunięcia.\n")
        }
      }
      # jeśli kalibrujemy dalej, to zapiszmy sobie wartości startowe
      if (kalibrujDalej) {
        for (k in 1:(length(opisProcedury[[i]]$czescPomiarowa))){
          opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe = wyniki[[i]][[j]]$parametry$surowe[wyniki[[i]][[j]]$parametry$surowe$zmienna1 %in% c(names(opisProcedury[[i]]$czescPomiarowa)[k], wyniki[[i]][[j]]$parametry$surowe$zmienna2[wyniki[[i]][[j]]$parametry$surowe$typ == "by" & wyniki[[i]][[j]]$parametry$surowe$zmienna1 == names(opisProcedury[[i]]$czescPomiarowa)[k]]), ]
        }
      }
      # inkrementacja na koniec iteracji
      j = j + 1
    }
    names(wyniki[[i]])=paste0("kalibracja", 1:length(wyniki[[i]]))
  }
  # kończenie
  if (usunFWF) unlink("daneMplusTemp.fwf")
  cat("\n")
  return(wyniki)
}
