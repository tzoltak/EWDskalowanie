#' @title Skalowanie modeli IRT.
#' @description
#' Funkcja służąca do przeprowadzania potencjalnie złożonych procedur skalowania
#' (typowo egzaminów) z wykorzystaniem Mplusa (a docelowo również pakietu \code{mirt}).
#' @param dane data.frame z danymi
#' @param opisProcedury p. szczegóły
#' @param idObs ciąg znaków z nazwą zmiennej będącej unikalnym identyfikatorem
#' obserwacji (w \code{dane})
#' @param tytul ciąg znaków, który będzie wstawiany w sekcji TITLE plików poleceń
#' Mplusa, a jego 12 pierwszych znaków zostanie wykorzystane do tworzenia nazw plików
#' z poleceniami Mplusa
#' @param zmienneCiagle wektor tekstowy podający nazwy zmiennych, które mają być
#' w analizie traktowane jako ciągłe (domyślnie wszystkie zmienne traktowane są
#' jako porządkowe)
#' @param zmienneSpecjalne lista - na obecnym etapie rozwoju nie wykorzystywana
#' @param zwrocOszacowania wartość logiczna - czy estymować (i zwrócić) również
#' oszacowania natężenia badanych cech dla poszczególnych jednostek obserwacji?
#' @param zmienneDolaczaneDoOszacowan wektor tekstowy podajacy nazwy zmiennych,
#' które mają zostać dołączone do zbioru(ów) z oszacowaniami natężania badanych cech
#' @param usunFWF wartość logiczna - czy usuwać pliki o stałej szerokości, w których
#' zapisywane są dane dla Mplusa?
#' @param bezWartosciStartowychParametrowTypu wyrażenie regularne definiujące typy
#' parametrów, dla których wartości wyestymowane w poprzedniej kalibracji mają \bold{nie}
#' być używane jako wartości startowe w ew. następnych kalibracjach (sposób na obejście
#' problemu z Mplusem, który przy zadaniu wartości startowych dla thresholdów nie
#' utrzymuje inwariancji pomiarowej w modelu wielogrupowym)
#' @details
#' \bold{Struktura argumentu \code{opisProcedury}:}
#'
#' Argument \code{opisProcedury} jest listą, której kolejne elementy opisują kolejne
#' zadania (kroki) w ramach procedury skalowania. Każdy jej element jest listą,
#' składającą się z następujących elementów:
#' \itemize{
#'   \item{\code{czescPomiarowa} lista zawierająca definicje konstruktów. Każdy jej
#'         element musi mieć nazwę, składającą się wyłącznie z liter alfabetu łacińskiego
#'         (ASCII) i cyfr (ale nie może zaczynać się od cyfry) i być listą, składającą
#'         się z następujących elementów:
#'         \itemize{
#'           \item{\code{zmienne} jedno z dwóch:
#'                 \itemize{
#'                   \item{Wektor tekstowy zawierający nazwy zmiennych (obserwowalnych)
#'                         powiązanych z danym konstruktem.}
#'                   \item{Lista \emph{definiująca konstrukt symbolicznie}, składająca
#'                         się z:
#'                         \itemize{
#'                           \item{Prawostronnej formuły zawierającej nazwy konstruktów,
#'                                 występujących we wcześniejszych krokach procedury,
#'                                 z których (przynajmniej) niektóre zmienne mają zostać
#'                                 powiązane z właśnie definiowanym konstruktem.}
#'                           \item{Dla każdego konstruktu występującego w ww. formule
#'                                 element listy o nazwie odpowiadającej nazwie
#'                                 konstruktu, zawierający wyrażenie regularne (ciąg
#'                                 znaków), identyfikujące (w grupie zmiennych
#'                                 powiązanych z danym konstruktem z któregoś
#'                                 z poprzednich kroków procedury) zmienne, które mają
#'                                 być wskaźnikami definiowanego właśnie konstruktu.}
#'                         }}
#'                 }}
#'           \item{\code{var1} opcjonalnie wartość logiczna - czy wariancja konstruktu
#'                 ma być zakotwiczona w jedności (a wszystkie ładunki
#'                 czynnikowe/dyskryminacje uwolnione)?  Jeśli element nie występuje,
#'                 funkcja \code{skaluj()} założy, że ma on wartość \code{TRUE}.}
#'           \item{\code{rasch} opcjonalnie wartość logiczna - czy wszystkie
#'                dyskryminacje w ramach konstruktu mają mieć równe wartości? Jeśli
#'                element nie występuje, funkcja \code{skaluj()} założy, że ma on
#'                wartość \code{FALSE}.}
#'           \item{\code{kryteriaUsuwania} opcjonalnie lista zawierająca definicje
#'                 kryteriów usuwania pytan (zmiennych obserwowalnych). Obecnie
#'                 obsługiwane są:
#'                 \itemize{
#'                   \item{\code{dyskryminacjaPonizej} liczba - zadania o dyskryminacji
#'                         mniejszej niż zadany próg będą usuwane.}
#'                   \item{\code{istotnoscPowyzej} liczba z zakresu (0;1] - zadania
#'                         o wartości istotności parametru dyskryminacji większej niż
#'                         zadany próg będą usuwane.}
#'                   \item{\code{nigdyNieUsuwaj} wyrażenie regularne (ciąg znaków)
#'                         identyfikujące nazwy zmiennych, które nigdy nie są usuwane
#'                         (przydatne np. dla parametrów selekcji).}
#'                 }}
#'           \item{\code{wartosciStartowe} data.frame zawierający ew. wartości startowe
#'                 dla (przynajmniej niektórych) parametrów modelu. Musi składać się
#'                 z kolumn:
#'                 \itemize{
#'                   \item{\code{typ} typu "character", definiująca typ parametru: "by",
#'                         "threshold", "mean" lub "variance".}
#'                   \item{\code{zmienna1} typu "character". W przypadku parametrów "by",
#'                         "mean" i "variance" zawiera nazwę konstruktu. W przypadku
#'                         parametrów "threshold" zawiera nazwę zmiennej obserwowalnej.}
#'                   \item{\code{zmienna2} typu "character". W przypadku parametrów "by"
#'                         zawiera nazwę zmiennej obserwowalnej. W przypadku parametrów
#'                         "threshold" zawiera numer progu (począwszy od 1). W przypadku
#'                         parametrów "mean" i "variance" pusta.}
#'                   \item{\code{wartosc} typu "numeric", zawiera wartość startową
#'                         dla parametru.}
#'                 }
#'                 albo wartość \code{TRUE}. To drugie tylko w sytuacji, gdy konstrukt
#'                 jest definiowany w sposób \emph{symboliczny} - oznacza wtedy, że
#'                 wartości startowe mają zostać przepisane z wyników estymacji
#'                 w poprzednich krokach procedury.}
#'           \item{\code{wartosciZakotwiczone} data frame o strukturze jw., z tym że
#'                 podane wartości parametrów definiują wartości zakotwiczone, albo
#'                 wartość \code{TRUE} (jw., tylko gdy konstrukt jest definiowany
#'                 w sposób \emph{symboliczny})}
#'           \item{\code{ograniczeniaWartosci} jedno z dwojga:
#'                 \itemize{
#'                   \item{Data frame o strukturze jw., poza tym, że kolumna 'wartosc'
#'                         musi być typu "character". Podane w niej wartości parametrów
#'                         definiują etykiety parametrów - jeśli kilka parametrów ma
#'                         przypisaną tą samą etykietę, w estymacji nałożony zostanie na
#'                         nie warunek, że muszą przyjmować taką samą wartość.}
#'                   \item{Wektor wyrażeń regularnych (ciągów tekstu) identyfikujących
#'                         grupy zmiennych obserwowalnych, w ramach których zachowana
#'                         musi być taka sama suma wartości parametrów dyskryminacji.}
#'                 }
#'         }}}
#'   \item{\code{wieleGrup} opcjonalnie lista zawierająca specyfikację modelu
#'         wielogrupowego (p . też sekcja \emph{Modele wielogrupowe} poniżej). Musi
#'         zawierać elementy:
#'         \itemize{
#'           \item{\code{zmienneGrupujace} wektor tekstowy zawierający nazwy zmiennych,
#'                 których kombinacja wartości definiuje podział na grupy.}
#'           \item{\code{uwolnijWartosciOczekiwane} wartość logiczna - czy uwolnić
#'                 wartości oczekiwane konstruktu w ramach grup? Jeśli element nie
#'                 występuje, funkcja \code{skaluj()} założy, że ma on wartość
#'                 \code{TRUE}.}
#'           \item{\code{uwolnijWariancje} wartość logiczna - czy uwolnić wariancje
#'                 konstruktu w ramach grup? Jeśli element nie występuje, funkcja
#'                 \code{skaluj()} założy, że ma on wartość \code{TRUE}.}
#'         }}
#'   \item{\code{parametry} lista zawierająca parametry sterujące estymacją modelu.
#'         Obecnie obsługiwane są:
#'         \itemize{
#'           \item{\code{estimator} ciąg znaków, który zostanie wstawiony w syntax
#'                 Mplusa jako wartość opcji \code{ESTIMATOR} w ramach komendy
#'                 \code{ANALYSIS}. Typowo \code{"MLR"}. \bold{Powinien być zawsze
#'                 podawany.}}
#'           \item{\code{processors} liczba, która zostanie wstawiony w syntax Mplusa
#'                 jako wartość opcji \code{PROCESSORS} w ramach komendy \code{ANALYSIS}.}
#'           \item{\code{integration} ciąg znaków, który zostanie wstawiony w syntax
#'                 Mplusa jako wartość opcji \code{INTEGRATION} w ramach komendy
#'                 \code{ANALYSIS}; typowo \code{"STANDARD (20)"}; \bold{Lepiej, by był
#'                 podany.}}
#'           \item{\code{fscores} wartość logiczna - czy z kalibracji przeprowadzanych
#'                 w ramach danego zadania mają być zapisanywane pliki z ocenami
#'                 czynnikowymi? \bold{Jeśli przyjmuje wartość \code{FALSE}, ma
#'                 priorytet} nad "globalnym" argumentem \code{zwrocOszacowania},
#'                 z którym wywoływana jest funkcja \code{skaluj}.}
#'         }}
#' }
#' \bold{Usuwanie zadań:}
#'
#' O ile zdefiniowane zostały kryteria usuwania, kalibracje w ramach danego kroku
#' procedury ponawiane będą tak długo, aż we wszystkich konstruktach (w których
#' zdefiniowane zostały kryteria usuwania) nie będzie już zadań (zmiennych
#' obserwowalnych) kwalifikujących się do usunięcia.
#'
#' Po każdej kalibracji usuwane jest jedno zadanie - w pierwszej kolejności zadania
#' z najniższą dyskryminacją, a gdy nie ma już zadań podpadających pod kryterium
#' dyskryminacji, to zadania z najwyższą wartością istotności dla parametru
#' dyskryminacji.
#'
#' \bold{\emph{Symboliczne} definiowanie konstruktów:}
#'
#' Mechanizm ten przydaje się przede wszystkim w ramach wieloetapowych procedur
#' skalowania modeli wielowymiarowych, gdzie początkowych krokach (etapach) dokonywane
#' jest usunięcie zadań o szczególnie złych własnościach pomiarowych (realizowane
#' w ramach oddzielnie estymowanych modeli jednowymiarowych, co pozwala zaoszczędzić
#' czas). Jednocześnie wiedzę o tym, które pytania (zmienne obserwowalne) zostały
#' usunięte chcemy wykorzystać podczas dalszych, bardziej złożonych etapów procedury
#' (tak aby tam już nie przeszkadzały).
#'
#' \bold{Modele wielogrupowe:}
#'
#' Ogólnie rzecz biorąc możliwość estymacji modeli wielogrupowych dedykowana jest dla
#' modeli z założeniem \bold{pełnej inwariancji pomiarowej} pomiędzy grupami, tj. przy
#' założeniu, że część pomiarowa jest we wszystkich grupach identyczna, a różnią się one
#' jedynie co do wartości oczekiwanych i/lub wariancji rozkładu cechy ukrytej.
#'
#' Zakotwiczenie wartości parametrów w ramach grup można osiągnąć standardowo przy pomocy
#' elementu \code{wartościZakotwiczone} z tym, że w kolumnie \code{typ} należy podać
#' \code{mean.grNR} lub \code{variance.grNR}, gdzie \code{NR} to numer grupy.
#'
#' Numery grup przypisywane są w ten sposób, że najpierw wybierane są wszystkie
#' \bold{występujące w danych} unikalne kombinacje wartości zmiennych definiujących
#' grupowanie.
#' Następnie układane są one w kolejności rosnącej według wartości pierwszej
#' zmiennej, a w ramach kombinacji o takich samych wartościach pierwszej zmiennej,
#' w kolejności według wartości drugiej zmiennej, itd. W związku z tym konieczne jest
#' zachowanie \bold{daleko idącej ostrożności przy wykorzystywaniu wartości
#' zakotwiczonych i/lub startowych} uzyskanych z wykonania funkcji \code{skaluj} na
#' innych danych. Jeśli zestaw kombinacji zmiennych grupujących w jednych i drugich
#' danych nie jest identyczny, to albo niemożliwa będzie estymacja modelu, albo, co
#' gorsza, uzyska się nieoczekiwane wyniki.
#'
#' Estymacja modeli łamiących założenie o inwariancji pomiarowej w zasadzie jest możliwa,
#' ale nie była testowana i nie ma gwarancji, że wszystko będzie działać dobrze.
#' Uwolnienie parametrów w ramach grup można osiągnąć dodając dotyczące ich wpisy
#' w elemecie \code{wartosciStartowe} danego konstruktu, przy czym w kolumnie \code{typ}
#' należy podać \code{by.grNR} lub \code{threshold.grNR}, gdzie \code{NR} to numer grupy.
#' Odpowiednio powinno działać również kotwiczenie parametrów w ramach grup przy pomocy
#' elementu \code{wartosciZakotwiczone}.
#'
#' \bold{Ograniczenia na równość parametrów:}
#'
#' Funkcja umożliwia zastosowanie jednego z dwóch podejść do nakładania ograniczeń na
#' równość parametrów. Nie mogą być one ze sobą łączone (chyba że podejście oparte na
#' etykietach parametrów dotyczyć będzie parametrów innych niż dyskryminacja).
#'
#' Pierwsze - etykietowanie parametrów, które mają przyjmować takie same wartości -
#' została opisana powyżej, w opisie struktury argumentu
#' \code{opisProcedury$czescPomiarowa[[nr]]$ograniczeniaWartosci}.
#'
#' Drugie rozwiązanie jest bardzo specyficzne i odnosi się do sytuacji, w której chcemy,
#' aby sumy wartości parametrów w ramach pewnych bloków zadań były sobie równe (taki
#' nieco szalony pomysł na skalowanie matury). Można je zastosować podając jako wartość
#' \code{opisProcedury$czescPomiarowa$ograniczeniaWartosci} wektor wyrażeń regularnych,
#' z których każde wskazuje na nazwy zmiennych należących do jednej z ww. grup. Od strony
#' technicznej wykorzystuje ono pierwszy mechaniz, tyle że odpowiednia data frame jest
#' przygototwywana wewnątrz funcji \code{skaluj}. Nie może być stosowane razem
#' z argumentem \code{opisProcedury$czescPomiarowa[[nr]]$rasch=TRUE}. Bardzo mało
#' prawdopodobne, by dało się też sensowenie wykorzystać w modelach wielogrupowych
#' łamiących założenie inwariancji pomiarowej.
#' @return
#' Lista, której każdy element opisuje wyniki skalowania każdego etapu procedury.
#' Każdy jej elemement jest listą opisującą wyniki estymacji kolejnych kalibracji
#' (których może być kilka, jeśli stosowano kryteria usuwania zadań). Elementy opisujące
#' wyniki estymacji kalibracji są listami, składającymi sie z następujących elementów:
#' \itemize{
#'   \item{\code{podsumowanie} wektor tekstowy zawierający linie z pliku outputu Mplusa,
#'         opisujące podsumowanie modelu i procesu estymacji.}
#'   \item{\code{dopasowanie} data frame zawierający statystyki dopasowania modelu
#'         (zwrócone przez Mplusa). UWaga, sposób przechowywania informacji w tym
#'         data framie nie jest całkiem przyjazny (zanim coś z tym zrobisz, sprawdź,
#'         jak to wygląda).}
#'   \item{\code{parametry} lista składająca się z elementów: \code{surowe},
#'         \code{stdyx}, \code{stdy}, \code{std} i \code{r2} zawierająca wyestymowane
#'         parametry modelu, odpowiednio: surowe, standaryzowane ze względu zarówno na
#'         zmienną zależną jak i zmienną niezależną, standaryzowane tylko ze względu na
#'         zmienną zależną, nie pamiętam co to za standaryzacja - sprawdź w dokumentacji
#'         Mplusa i statystyki R-kwadrat. W przypadku modeli wielogrupwych zawiera
#'         dodatkowo element code{grupyMapowanie}, w którym opisane jest mapowanie
#'         kobinacji wartośc zmiennych grupujących na numery grup.}
#'   \item{\code{zapis} data frame zawierający wyliczone oszacowania (co do zasady EAP)
#'         wartości cech ukrytych dla poszczególnych obserwacji i ich błędy standardowe
#'         (z wyjątkiem modeli wielogrupowych, bo Mplus nie zwraca dla nich obecnie
#'         oszacowań błędów standardowych - jest to zapewne jakiś głupi błąd), a także
#'         zmienną/e z id obserwacji, ew. zmienne grupujące (w modelach wielogrupowych)
#'         i zmienne wymienione w argumencie \code{zmienneDolaczaneDoOszacowan}. Ten
#'         element występuje tylko, gdy funkcja została wywołana z argumentem
#'         \code{zwrocOszacowania=TRUE} i w opisie danego kroku procedury nie pojawiła
#'         się definicja \code{parametry$fscores=FALSE}.}
#'   \item{\code{czas} wektor teskstowy zawierający linie z pliku outputu Mplusa,
#'         opisujące czas estymacji modelu.}
#' }
#' @examples
#' # chwilowo brak
#' @export
skaluj = function(dane, opisProcedury, idObs, tytul="", zmienneCiagle=NULL,
                  zmienneSpecjalne=NULL, zmienneDolaczaneDoOszacowan=NULL,
                  zwrocOszacowania=TRUE, usunFWF=TRUE,
                  bezWartosciStartowychParametrowTypu=NULL) {
  # podstawowe sprawdzenie argumentów
  message("Sprawdzanie poprawności argumentów...")
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
    usunFWF %in% c(TRUE, FALSE),
    is.character(bezWartosciStartowychParametrowTypu) | is.null(bezWartosciStartowychParametrowTypu)
  )
  if (!is.null(bezWartosciStartowychParametrowTypu)) stopifnot(length(bezWartosciStartowychParametrowTypu) == 1)
  if (!all(grepl("^[[:lower:]][[:lower:][:digit:]_]*$", names(dane)))) stop("Wszystkie nazwy zmiennych muszą składać się wyłącznie z małych liter, cyfr i znaku '_', przy czym pierwszym znakiem musi być litera.")
  # wywalanie zmiennych posiadających same braki danych
  maskaSameNA = unlist(lapply(dane, function(x) return(all(is.na(x)))))
  if (any(maskaSameNA)) {
    warning(paste0("Z danych usunięto zmienne, które przyjmowały wyłącznie wartości 'brak danych':\n - ", paste0(names(dane)[maskaSameNA], collapse="\n  - "), "\n"), immediate.=TRUE)
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
  maskaWariancja0 = maskaWariancja0 & !(names(dane) %in% zmienneDolaczaneDoOszacowan)
  if (any(maskaWariancja0)) {
    warning(paste0("Z danych usunięto zmienne, które miały zerową wariancję (tj. przyjmowały tylko jedną wartość):\n - ", paste0(names(dane)[maskaWariancja0], collapse="\n  - "), "\n"), immediate.=TRUE)
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
    krok             = c("czescPomiarowa", "wieleGrup", "parametry"),
    konstrukt        = c("zmienne", "var1", "rasch", "kryteriaUsuwania", "wartosciStartowe", "wartosciZakotwiczone", "ograniczeniaWartosci"),
    kryteriaUsuwania = c("dyskryminacjaPonizej", "istotnoscPowyzej", "nigdyNieUsuwaj"),
    parametry        = c("estimator", "processors", "integration", "fscores"),
    wartosci         = c("typ", "zmienna1", "zmienna2", "wartosc"),  # akurat te w roli niezbędnych, a nie dozwolonych
    wieleGrup        = c("zmienneGrupujace", "uwolnijWartosciOczekiwane", "uwolnijWariancje")
  )
  grupyMapowanie = vector(mode="list", length=length(opisProcedury))
  zmGrupujace    = vector(mode="list", length=length(opisProcedury))
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
        } else {
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
      # sprawdzanie poprawności komponentu 'ograniczeniaWartosci'
      if (!is.null(konstrukt$ograniczeniaWartosci)) {
        if (opisProcedury[[i]]$czescPomiarowa[[j]]$rasch == TRUE) stop("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nElement 'ograniczeniaWartosci' nie może być zdefiniowany, jeśli element 'rasch' przyjmuje wartość TRUE.\n")
        if (is.vector(konstrukt$ograniczeniaWartosci)) {
          if (!is.character(konstrukt$ograniczeniaWartosci) | length(konstrukt$ograniczeniaWartosci) < 1) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nElement 'ograniczeniaWartosci' jest niepoprawny.\n"))
        } else {
          blad = paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w konstrukcie: ", names(krok$czescPomiarowa)[j], ".\nElement 'ograniczeniaWartosci' musi być data framem z kolumnami 'typ' (character), 'zmienna1' (character), 'zmienna2' (character), 'wartosc' (character).\n")
          if (!is.data.frame(konstrukt$ograniczeniaWartosci)) {
            stop(blad)
          } else if (!all(dozwoloneElementy$wartosci %in% names(konstrukt$ograniczeniaWartosci))) {
            stop(blad)
          } else if (!(with(konstrukt$ograniczeniaWartosci, {mode(typ) == "character" & mode(zmienna1) == "character" & mode(zmienna2) == "character" & mode(wartosc) == "character"}))) {
            stop(blad)
          }
        }
      }
    }
    # sprawdzanie poprawności elementu 'wieleGrup' (o ile jest)
    if (!("parametry" %in% names(krok))) opisProcedury[[i]]$wieleGrup = NULL
    wieleGrup = opisProcedury[[i]]$wieleGrup
    if (!is.null(wieleGrup)) {
      if (!all(names(wieleGrup) %in% dozwoloneElementy$wieleGrup)) warning(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w elemencie 'wieleGrup' pojawiły się elementy listy, które nie są rozpoznawane przez funkcję:\n - ", paste0(names(wieleGrup)[!(names(wieleGrup) %in% dozwoloneElementy$wieleGrup)], collapse="\n - "), "\nZostaną one przez funkcję pominięte.\n"), immediate.=TRUE)

      # sprawdzanie poprawności komponentu 'zmienneGrupujace'
      if (!"zmienneGrupujace" %in% names(wieleGrup)) {
        opisProcedury[[i]]$wieleGrup = NULL
        warning(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", zdefiniowano element 'wieleGrup', ale nie zdefiniowano w nim elementu 'zmienneGrupujace'.\nModel zostanie wyestymowany jako przy założeniu homogeniczności populacji (bez podziału na grupy)."), immediate.=TRUE)
      }
      if (!is.character(wieleGrup$zmienneGrupujace) | length(wieleGrup$zmienneGrupujace) < 1) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w elemencie 'wieleGrup' element 'zmienneGrupujace' musi być wektorem tekstowym (typu character), podającym nazwy zmiennych mierzalnych związanych z konstruktem.\n"))
      if (!all(wieleGrup$zmienneGrupujace %in% names(dane))) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w elemencie 'zmienneGrupujace' elementu 'wieleGrup' podane zostały nazwy zmiennych, które nie występują w danych:\n - ", paste0(wieleGrup$zmienneGrupujace[!(wieleGrup$zmienneGrupujace %in% names(dane))], collapse="\n - "), "\n"))
      # i dwa pozostałe
      if (!("uwolnijWartosciOczekiwane" %in% names(wieleGrup))) {
        opisProcedury[[i]]$wieleGrup$uwolnijWartosciOczekiwane = TRUE
      } else {
        if (length(wieleGrup$uwolnijWartosciOczekiwane) != 1) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w elemencie 'wieleGrup' element 'uwolnijWartosciOczekiwane' musi być jednoelementowym wektorem logicznym."))
        if (!(wieleGrup$uwolnijWartosciOczekiwane %in% c(TRUE, FALSE))) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w elemencie 'wieleGrup' element 'uwolnijWartosciOczekiwane' musi być jednoelementowym wektorem logicznym."))
      }
      if (!("uwolnijWariancje" %in% names(wieleGrup))) {
        opisProcedury[[i]]$wieleGrup$uwolnijWariancje = TRUE
      } else {
        if (length(wieleGrup$uwolnijWariancje) != 1) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w elemencie 'wieleGrup' element 'uwolnijWariancje' musi być jednoelementowym wektorem logicznym."))
        if (!(wieleGrup$uwolnijWariancje %in% c(TRUE, FALSE))) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w elemencie 'wieleGrup' element 'uwolnijWariancje' musi być jednoelementowym wektorem logicznym."))
      }
      # wyłączanie zmiennych grupujących do oddzielnego obiektu (żeby móc je potem dopisać do ocen czynnikowych niezmaskarowane konwersją poprzedzającą zapis danych do pliku .fwf)
      zmGrupujace[[i]] = dane[, c(idObs, wieleGrup$zmienneGrupujace)]
      # tworzenie zmiennej opisującej podział na grupy w obiekcie 'dane'
      if (paste0("gr_tmp", i) %in% names(dane)) stop(paste0("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", zdefiniowany został model wielogrupowy.\nW takim przypadku w danych nie może występować zmienna o nazwie zaczynającej się od 'gr_tmp'."))
      grupyMapowanie[[i]] = unique(dane[, wieleGrup$zmienneGrupujace, drop=FALSE])
      for (j in ncol(grupyMapowanie[[i]]):1) grupyMapowanie[[i]] = grupyMapowanie[[i]][order(grupyMapowanie[[i]][, j]), , drop=FALSE]  # posortujmy to jeszcze tak, żeby numery grup nie były zbyt przypadkowe (może to mieć znaczenie, jak się chce robić dziwne rzeczy z modelami łamiącymi założenie inwariancji pomiarowej)
      grupyMapowanie[[i]] = cbind(gr_tmp=1:nrow(grupyMapowanie[[i]]), grupyMapowanie[[i]])
      names(grupyMapowanie[[i]]) = sub("gr_tmp", paste0("gr_tmp", i), names(grupyMapowanie[[i]]))
      dane = merge(dane, grupyMapowanie[[i]], all.x=TRUE)
      if (any(is.na(dane[, paste0("gr_tmp", i)]))) stop("W opisie kroku ", i, ".: ", names(opisProcedury)[i], ", w elemencie 'wieleGrup' podane zmienne nie definiują wyczerpującego podziału na grupy.")
    }
    # sprawdzanie poprawności parametrów estymacji
    if (!("parametry" %in% names(krok))) opisProcedury[[i]]$parametry = list()
    parametry = opisProcedury[[i]]$parametry
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
  # wyłączenie do oddzielnego obiektu zmiennych, które mają być potem przyłączane do oszacowań (po skonwertowaniu obiektu 'dane' na teksty nie da się z niego do tego skorzystać)
  if (!is.null(zmienneDolaczaneDoOszacowan)) daneDolaczaneDoOszacowan = dane[, c(idObs, zmienneDolaczaneDoOszacowan)]
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
    if (nrow(unique(dane[, idObs, drop = FALSE])) < nrow(dane)) stop("Podany identyfikator obserwacji nie jest unikalny.")
  }
  # zapis pliku z danymi w formie stałoszerokościowej
  message("Zapis danych do pliku tekstowego o stałej szerokości kolumn...")
  # kowersja factorów na liczby
  maskaFactory = (1:ncol(dane))[unlist(lapply(dane, is.factor))]
  for (i in maskaFactory) dane[, i]=as.numeric(dane[, i])
  # obsługa przydługich nazw zmiennych - przygotowanie
  # trochę upierdliwości związanych jest z tym, że trzeba dodać do mapowania także nazwy konstruktów, oraz ich połęcznie z przyrostkiem "_se" (z tym, że skracać trzeba tylko nazwy konstruktów, a potem do pierwotnych i skróconych dopisać "_se", a nie skracać po dopisaniu przyrostka)
  maxLZn = 8
  nazwyKonstruktow = unlist(lapply(opisProcedury, function(x) {return(names(x$czescPomiarowa))}))
  if (max(nchar(nazwyKonstruktow)) > maxLZn) {
    nazwyKonstruktowSkrocone = skroc_nazwy_zmiennych(nazwyKonstruktow, maxLZn)
  } else {
    nazwyKonstruktowSkrocone = nazwyKonstruktow
  }
  nazwyKonstruktow         = c(nazwyKonstruktow        , paste0(nazwyKonstruktow        , "_se"))
  nazwyKonstruktowSkrocone = c(nazwyKonstruktowSkrocone, paste0(nazwyKonstruktowSkrocone, "_se"))

  nazwyPierwotne = c(
    names(dane),
    unlist(lapply(opisProcedury, function(x) {return(names(x$czescPomiarowa))}))  # nazwy konstruktow
  )
  if (max(nchar(nazwyPierwotne)) > maxLZn) {
    nazwySkrocone = skroc_nazwy_zmiennych(nazwyPierwotne, maxLZn)
  } else {
    nazwySkrocone = nazwyPierwotne
  }

  nazwyPierwotne = as.list(unique(tolower(c(nazwyPierwotne, nazwyKonstruktow))))
  nazwySkrocone  = as.list(unique(tolower(c(nazwySkrocone , nazwyKonstruktowSkrocone))))
  names(nazwySkrocone)  = nazwyPierwotne
  names(nazwyPierwotne) = nazwySkrocone
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
  nazwaPlikuDane = paste0("daneMplus", paste0(LETTERS[sample(1:length(LETTERS), 8, replace=TRUE)], collapse=""), ".fwf")
  # zapis
  tryCatch(
    write.table(apply(dane, 1, paste0, collapse=""), nazwaPlikuDane, row.names=FALSE, col.names=FALSE, quote=FALSE),
    error = function(e) {stop("Nie udało się zapisać danych do pliku.")}
  )
  # pętla główna
  wyniki = vector(mode="list", length=length(opisProcedury))
  names(wyniki) = names(opisProcedury)

  for (i in 1:length(opisProcedury)) {
    message("###################################\n Krok ", i, ". ", names(opisProcedury)[i], "\n###################################")
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
      message("   ###################################   \n    Kalibracja ", j, ".\n   ###################################")
      krok = opisProcedury[[i]]	# tu, a nie przed pętlą, bo opisProcedury jest (może być) modyfikowany na podstawie wyników estymacji
      # obsługa ograniczeń wartości parametrów zdefiniowanych jako wektor wyrażeń regularnych
      modelConstraint = NULL
      for (k in 1:length(krok$czescPomiarowa)) {
        if ("ograniczeniaWartosci" %in% names(krok$czescPomiarowa[[k]])) {
          if (is.vector(krok$czescPomiarowa[[k]]$ograniczeniaWartosci)) {
            temp = krok$czescPomiarowa[[k]]$ograniczeniaWartosci
            temp = lapply(as.list(temp),
                          function(x, zmienne) {
                            return(zmienne[grepl(x, zmienne)])
                          },
                          zmienne = krok$czescPomiarowa[[k]]$zmienne
            )
            modelConstraint = c(
              modelConstraint,
              paste0("  NEW (", names(krok$czescPomiarowa)[k], ");"),
              unlist(lapply(temp,
                            function(x, nazwa) {
                              x = paste0(x, collapse=" + ")
                              x = lam_wiersze(c(nazwa, " = ", x), wciecie = 5 + nchar(nazwa))
                              x[1] = gsub("^ +", "  ", x[1])
                              return(x)
                            },
                            nazwa = names(krok$czescPomiarowa)[k]
              ))
            )
            krok$czescPomiarowa[[k]]$ograniczeniaWartosci = data.frame(
              typ = "by",
              zmienna1 = names(krok$czescPomiarowa)[k],
              zmienna2 = unlist(temp),
              wartosc = unlist(temp),
              stringsAsFactors = FALSE
            )
          }
        }
      }
      # przygotowanie obiektów dla funkcji tworzącej polecenia Mplusa
      title = paste0(tytul, " Krok ", i, ". ", names(opisProcedury)[i])
      data=list(
        file   = nazwaPlikuDane,
        format = szerokosciKolumn
      )
      zmienneWModelu = unique(unlist(lapply(krok[names(krok) %in% c("czescPomiarowa")], function(x) return(lapply(x, function(x) return(x$zmienne))))))
      zmienneWModelu = zmienneWModelu[zmienneWModelu %in% names(dane) & !(zmienneWModelu %in% zmienneCiagle)]	# trzeba wykluczyć składowe
      variable = list(
        missing     = "BLANK",
        names       = unlist(nazwySkrocone[nazwyPierwotne %in% names(dane)]),
        usevariables= unlist(nazwySkrocone[zmienneWModelu[  zmienneWModelu %in% names(dane)   ]]),
        categorical = unlist(nazwySkrocone[zmienneWModelu[!(zmienneWModelu %in% zmienneCiagle)]]),
        idvariable  = nazwySkrocone[[idObs]]
      )
      analysis = krok$parametry[names(krok$parametry) %in% c("estimator", "processors", "integration")]
      if (!is.null(krok$wieleGrup)) {
        wartosciZmGrupujacej = unique(dane[, paste0("gr_tmp", i)])
        variable$classes = krok$wieleGrup$liczbaGrup = length(wartosciZmGrupujacej)
        variable$knownclass = paste0(paste0("gr_tmp", i), "=", wartosciZmGrupujacej)
        analysis$type = "MIXTURE"
        analysis$algorithm = "INTEGRATION"
      }
      model = przygotuj_model(zmien_nazwy_w_kroku_procedury(krok, nazwySkrocone))
      output = list("STANDARDIZED", "TECH4", "TECH8")
      if (krok$parametry$fscores) {
        savedata = list(
          file = paste0("ocCzynMplus", paste0(LETTERS[sample(1:length(LETTERS), 8, replace=TRUE)], collapse=""), ".fwf"),
          save = "FSCORES"
        )
      } else {
        savedata = NULL
      }
      # zapis pliku poleceń Mplusa
      nazwaInp = paste0(substr(tytul, 1, min(16, nchar(tytul))), "_krok_", i, "_kalibr_", j, ".inp")
      write.table(
        przygotuj_inp(title, data, variable, analysis, model, modelConstraint, output, savedata),
        nazwaInp, row.names=FALSE, col.names=FALSE, quote=FALSE
      )
      # kalibracja w Mplusie
      mplus = system2("mplus", paste0('"', nazwaInp, '"'))
      if (mplus == 1) return(list(wyniki=wyniki, tenKrok=krok))
      wyniki[[i]][[j]] = obrob_out(readLines(sub("[.]inp$", ".out", nazwaInp)), nazwyPierwotne)
      if ("brak_zbieznosci" %in% names(wyniki[[i]][[j]])) {
        message("   ###################################\n    Nie osiągnięto zbieżności!\n   ###################################")
        return(list(wyniki=wyniki, tenKrok=krok))
      }
      if (!is.null(krok$wieleGrup)) {
        wyniki[[i]][[j]]$parametry$grupyMapowanie = grupyMapowanie[[i]]
      }
      # zapis ocen czynnikowych do bardziej zwartej i łatwiej dostępnej postaci
      if (!is.null(wyniki[[i]][[j]]$zapis)) {
        ocCzyn = wczytaj_fwf(savedata$file, wyniki[[i]][[j]]$zapis$szerokosc, wyniki[[i]][[j]]$zapis$zmienna)
        ocCzyn = ocCzyn[, grepl(
          paste0(
            "^", tolower(idObs), "$|^gr_tmp|^(",
            paste0(unique(wyniki[[i]][[j]]$parametry$surowe$zmienna1[wyniki[[i]][[j]]$parametry$surowe$typ == "by"]), collapse="|"),
            ")(|_se)$"
          ),
          names(ocCzyn)
        )]
        # dopisanie do pliku z ocenami czynnikowymi kolumn tworzących id obserwacji - jeśli było ich więcej niż jedna
        if (idObs == "id_temp") {
          ocCzyn = merge(idObsMapowanie, ocCzyn)
          ocCzyn = ocCzyn[, names(ocCzyn) != "id_temp"]
        }
        # dopisanie do pliku z ocenami czynnikowymi zmiennych definiujących grupowanie
        if (!is.null(krok$wieleGrup)) {
          ocCzyn = merge(ocCzyn, zmGrupujace[[i]])
        }
        # dopisanie do pliku z ocenami czynnikowymi innych zmiennych, o które prosił użytkownik
        if (!is.null(zmienneDolaczaneDoOszacowan)) {
          ocCzyn = merge(ocCzyn, daneDolaczaneDoOszacowan)
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
          message("   ###################################\n    Usuwanie źle dopasowanych zadań: konstrukt ", names(opisProcedury[[i]]$czescPomiarowa)[k], "\n   ###################################")
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
              message("    Zadania o dyskryminacji poniżej ", kryteria$dyskryminacjaPonizej, ":")
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
              doUsuniecia = doUsuniecia[
                !grepl(opisProcedury[[i]]$czescPomiarowa[[k]]$kryteriaUsuwania$nigdyNieUsuwaj,
                       doUsuniecia)]
            }
            # drukowanie informacji o zadaniach do usunięcia
            if (length(doUsuniecia) > 0) {
              message("    Zadania o istotności powyżej ", kryteria$istotnoscPowyzej, ":")
              print(
                data.frame(
                  "_"="     ",
                  zadanie=pozaZakresem$zmienna2,
                  istotnosc=pozaZakresem$wartosc,
                  "usunięte"=ifelse(pozaZakresem$zmienna2==doUsuniecia,
                                    rep("tak",nrow(pozaZakresem)),
                                    rep("",nrow(pozaZakresem))),
                  check.names=FALSE, stringsAsFactors=FALSE
                ),
                row.names=FALSE
              )
            }
          }
          # usuwamy najgorszą pozycję
          if (length(doUsuniecia)>0) {
            kalibrujDalej=TRUE
            opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne =
              opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne[
                !(opisProcedury[[i]]$czescPomiarowa[[k]]$zmienne%in%doUsuniecia)]
          } else {
            message("    Brak zadań do usunięcia.")
          }
        }
      }
      # jeśli kalibrujemy dalej, to zapiszmy sobie wartości startowe
      if (kalibrujDalej) {
        for (k in 1:(length(opisProcedury[[i]]$czescPomiarowa))){  # nie przepisujemy wartości, które były zakotwiczone (co generalnie nie sprawia dużych problemów, ale odnośnie wartości oczekiwanych konstruktu w grupie odniesienia modelu wielogrupowego już tak)
          opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe =
            wyniki[[i]][[j]]$parametry$surowe[
              wyniki[[i]][[j]]$parametry$surowe$zmienna1 %in%
                c(names(opisProcedury[[i]]$czescPomiarowa)[k],
                  wyniki[[i]][[j]]$parametry$surowe$zmienna2[wyniki[[i]][[j]]$parametry$surowe$typ == "by" &
                                                               wyniki[[i]][[j]]$parametry$surowe$zmienna1 == names(opisProcedury[[i]]$czescPomiarowa)[k]]
                  ), ]
          opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe =
            opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe[
              !is.na(opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe$Est..S.E.), ]
          # nieUzywajWartosciStartowychDlaParametrowTypu
          if (!is.null(bezWartosciStartowychParametrowTypu)) {
            opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe =
              opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe[
                !grepl(bezWartosciStartowychParametrowTypu,
                       opisProcedury[[i]]$czescPomiarowa[[k]]$wartosciStartowe$typ), ]
          }
        }
      }
      # inkrementacja na koniec iteracji
      j = j + 1
    }
    names(wyniki[[i]])=paste0("kalibracja", 1:length(wyniki[[i]]))
  }
  # kończenie
  if (usunFWF) unlink(nazwaPlikuDane)
  message("")
  return(wyniki)
}
