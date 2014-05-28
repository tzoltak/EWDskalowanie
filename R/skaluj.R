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
#' @param zmienneDolaczaneDoOszacowan wektor tekstowy podajacy nazwy zmiennych, które mają zostać dołączone do zbioru(ów) z oszacowaniami natężania badanych cech
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
skaluj = function(dane, opisProcedury, idObs, tytul="", zmienneCiagle=NULL, zmienneSpecjalne=NULL, zwrocOszacowania=TRUE, zmienneDolaczaneDoOszacowan=NULL, usunFWF=TRUE) {
  # podstawowe sprawdzenie argumentów
  cat("Sprawdzanie poprawności argumentów...\n")
  stopifnot(
    is.data.frame(dane),
    is.list(opisProcedury),
    is.character(idObs), all(idObs %in% names(dane)),
    is.character(tytul), length(tytul) == 1,
    is.character(zmienneCiagle) | is.null(zmienneCiagle),
    all(zmienneCiagle %in% names(dane)) | is.null(zmienneCiagle),
    is.list(zmienneSpecjalne) | is.null(zmienneSpecjalne),
    all(zmienneSpecjalne %in% names(dane)) | is.null(zmienneSpecjalne),
    zwrocOszacowania %in% c(TRUE, FALSE),
    is.character(zmienneDolaczaneDoOszacowan) | is.null(zmienneDolaczaneDoOszacowan),
    usunFWF %in% c(TRUE, FALSE)
  )
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
  # sprawdzanie, czy zmienne występują w danych
  stopifnot(
  	all(idObs %in% names(dane)),
  	all(zmienneCiagle %in% names(dane)),
  	all(zmienneDolaczaneDoOszacowan %in% names(dane))
  )
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
  # obsługa nazw zmiennych
  if (max(nchar(names(dane))) > 8) {
  	stop("Niestety wszystkie nazwy zmiennych muszą być nie dłuższe niż 8 znaków.")
  }
  # rozszerzone id obserwacji
  if (any(idObs == "id_temp")) stop("Parametr idObs nie może mieć przypisanej wartości 'id_temp'.")
  if (length(idObs) > 1) {
  	if ("id_temp" %in% names(dane)) stop("Niestety jeśli argument idObs wskazuje kilka zmiennych, to w danych nie może znajdować się kolumna o nazwie 'id_temp'.")
  	idObsMapowanie = unique(dane[, idObs])
  	if (nrow(idObsMapowanie) < nrow(dane)) stop("Podany identyfikator obserwacji nie jest unikalny.")
  	idObsMapowanie = cbind(id_temp=1:nrow(idObsMapowanie), idObsMapowanie)
  	dane = merge(dane, idObsMapowanie)
  	idObs = "id_temp"
  } else if (length(idObs) == 1) {
  	if (length(unique(dane[, idObs])) < nrow(dane)) stop("Podany identyfikator obserwacji nie jest unikalny.")
  }
  # zapis pliku z danymi w formie stałoszerokościowej
  cat("Zapis danych do pliku tekstowego o stałej szerokości kolumn...\n")
  # wyłączenie do oddzielnego obiektu zmiennych, które mają być potem przyłączane do oszacowań (po skonwertowaniu obiektu 'dane' na teksty nie da się z niego do tego skorzystać)
  if (!is.null(zmienneDolaczaneDoOszacowan)) daneDolaczaneDoOszacowan = dane[, c(idObs, zmienneDolaczaneDoOszacowan)]
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
  tryCatch(
  	write.table(apply(dane, 1, paste0, collapse=""), "daneMplusTemp.fwf", row.names=FALSE, col.names=FALSE, quote=FALSE),
  	error = function(e) {stop("Nie udało się zapisać danych do pliku.")}
  )
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
            "^", tolower(idObs), "$|^(",
            paste0(unique(wyniki[[i]][[j]]$parametry$surowe$zmienna1[wyniki[[i]][[j]]$parametry$surowe$typ == "by"]), collapse="|"),
            ")(|_se)$"
          ),
          names(ocCzyn)
        )]
        # dopisanie do pliku z ocenami czynnikowymi innych zmiennych, o które prosił użytkownik
        if (!is.null(zmienneDolaczaneDoOszacowan)) {
        	ocCzyn = merge(ocCzyn, daneDolaczaneDoOszacowan)
        }
        # dopisanie do pliku z ocenami czynnikowymi kolumn tworzących id obserwacji - jeśli było ich więcej niż jedna
        if (idObs == "id_temp") {
        	ocCzyn = merge(idObsMapowanie, ocCzyn)
        	ocCzyn = ocCzyn[, names(ocCzyn) != "id_temp"]
        }
        # zapis na dysk
        tryCatch(
        	write.csv2(ocCzyn, sub("[.]inp$", ".csv", nazwaInp), row.names=FALSE, na=""),
        	error = function(e) {stop("Nie udało się zapisać pliku z ocenami czynnikowymi!")}
        )
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
