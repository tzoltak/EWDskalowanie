#' @title Oszacowania z modeli Rascha
#' @description
#' Funkcja przygotowuje przeliczenie z sumy punktów na przewidywanie poziomu umiejętności
#' z modelu IRT (żeby miało to większy sens powinna to być jakaś odmiana modelu Rascha).
#' @param sumy data frame zawierający id obserwacji i wyliczone sumy punktów (może to być
#' kilka kolumn z sumami z kilku różnych części egzaminu)
#' @param oszacowania data frame zawierający id obserwacji i oszacowania umiejętności
#' z modelu
#' @param max opcjonalnie wektor liczb zawierających maksymalne możliwe do uzyskania
#' wartości sum - jego elementy muszą nazywać się tak, jak kolumny argumentu \code{sumy}
#' opisujące wartości sum punktów
#' @param span wartość liczbowa - parametr \code{span}, przekazywany do funkcji
#' \code{loess()} przy wygładzaniu i zapełnianiu dziur w przebiegu przekodowania
#' @details
#' W klasycznym modelu Rascha, w którym występują jedynie zadania oceniane binarnie,
#' oszacowanie poziomu umiejętności jest funkcją sumy punktów. Niestety, w przypadku
#' uogólnień modelu Rascha pozwalających uwzględnić zadania o większej liczbie poziomów
#' wykonania, zależność taka już nie zachodzi. Jeśli z jakichś powodów mimo wszystko
#' chcemy uzyskać funkcję przypisującą jednoznacznie oszacowanie sumie punktów, musimy
#' dokonać jakiegoś przybliżenia. Możliwe są dwa podejścia:
#' \itemize{
#'   \item{Wygenerować wszystkie możliwe wektory odpowiedzi, na podstawie wyestymowanych
#'         parametrów modelu wyliczyć odpowiadające im oszacowania oraz ich wiarygodność,
#'         a następnie agregować w ramach grup wyróżnionych ze względu na sumę punktów,
#'         z użyciem średniej ważonej wiarygodnością profilu (a więc wartością
#'         proporcjonalną do teoretycznej częśtości jego występowania).}
#'   \item{Użyć oszacowań wyliczonych na bardzo dużym zbiorze danych (rzeczywistym lub
#'         wygenerowanym metodą Monte-Carlo) i wyliczyć średnie (już nieważone) oszacowań
#'         w ramach grup o tej samej wartości sumy.}
#' }
#' Podejście pierwsze jest teoretycznie bardziej poprawne, ale znajduje zastosowanie tylko
#' dla dosyć krótkich testów, jako że liczba wszystkich możliwych wektorów odpowiedzi
#' rośnie wykładniczo wraz ze wzrostem liczby zadań. Podejście drugie jest aplikowalne
#' zawsze, ale jego sensowność silnie zależy od iczby losowanych wyników (procedura Monte
#' Carlo) lub liczby obserwacji i rozkładu wyników (wykorzystanie danych rzeczywistych).
#'
#' Niniejsza funkcja implementuje drugą procedurą, w oparciu o dane rzeczywiste (czyli
#' w zasadzie najbardziej ryzykowną). Stara się przy tym wykryć i naprawić pewne problemy,
#' które mogą się z nią wiązać:
#' \itemize{
#'   \item{niemonotoniczność przewidywań,}
#'   \item{dziury w rozkładzie sumy.}
#' }
#' Rozwiązanie obu problemów stara się uzyskać wygładzając i interpoulując przewidywanie
#' przy pomocy regresji nieparametrycznej (funkcją \code{loess()}, wywoływana z parametrem
#' \code{span}). Jeśli przewidywanie wynikające z regesji nieparametrycznej jest
#' niemonotoniczne, funkcja będzie zwiększać wartość parametru span o 0.05 tak długo, aż
#' przewidywanie stanie się monotoniczne (wtedy zwórcone zostanie ostrzeżenie) lub aż
#' osiągnieta zostanie wartość parametru \code{span} większa lub równa 1 (jeśli
#' przewidywanie cały czas będzie niemonotonicze, funkcja zwróci błąd).
#'
#' Jeśli minimalna wartość występująca w danych nie jest równa 0, to wartościom niższym
#' od najmniejszej występującej (w ramach danej grupy) przypisane zostanie przewidywanie
#' odpowiadające tej minimalnej wartości sumy, występującej w danych (w ramach danej
#' grupy).
#'
#' Analogicznie, jeśli podana została wartość parametru \code{max}, to w ramach każdej
#' grupy ew. wartościom większym od maksymalnej wartości występującej w danych (w ramach
#' danej grupy) a nie większym od odpowiedniej wartości parametru \code{max} przypisane
#' zostaną przewidywania odpowiadające tej maksymalnej wartości sumy, występującej
#' w danych (w ramach danej grupy).
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{mapowanie} Data frame zawierający mapowanie sum na średnie oszacowania.
#'         Kolumny o nazwach takich, jak kolumny w argumencie \code{sumy} (nie
#'         przechowujące id obserwacji) przyjmują wartość \code{TRUE} jeśli dany wiersz
#'         dotyczy posiadających wynik z odpowiedniej części egzaminu lub wartość
#'         \code{FALSE}, jeśli dany wiersz dotyczy zdających, którzy nie posiadają wyniku
#'         z odpowiedniej części egzaminu. Kolumna \code{suma} opisuje sumę punktów
#'         łącznie z wszystkich części (z których ktoś posiada wyniki), a ostatnia kolumna
#'         zawiera wartość przewidywania.}
#'   \item{\code{przewidywania} Data frame zawierający zastosowanie ww. mapowania do
#'         danych, z którymi wywołana została funkcja.}
#'   \item{\code{odsUtraconejWariancji} Odsetek wariancji oszacowań cechy utracony
#'         w wyniku uśredniania.}
#' }
#' @import plyr
#' @export
przewidywanie_rasch = function(sumy, oszacowania, max=NULL, span=0.2) {
  stopifnot(is.data.frame(sumy), is.data.frame(oszacowania),
            is.numeric(max) | is.null(max),
            is.numeric(span), length(span) == 1)
  stopifnot(all(span > 0))
  stopifnot(length(intersect(names(sumy), names(oszacowania))) > 0)
  stopifnot(ncol(oszacowania) == (length(intersect(names(sumy), names(oszacowania))) + 1))
  stopifnot(!("suma" %in% names(sumy)), !("suma" %in% names(oszacowania)))

  zmId = intersect(names(sumy), names(oszacowania))
  zmSumy = names(sumy)[!names(sumy) %in% names(oszacowania)]
  zmOszacowania = names(oszacowania)[!names(oszacowania) %in% names(sumy)]
  stopifnot(all(names(max) %in% zmSumy))

  # zamiana oddzielnych sum na łączną oraz oznaczenie, które części pisał
  sumy = cbind(sumy[, zmId, drop=FALSE],
               as.data.frame(lapply(sumy[, zmSumy, drop=FALSE], is.na)),
               suma = rowSums(sumy[, zmSumy, drop=FALSE], na.rm=TRUE))
  sumy [, zmSumy] = !sumy [, zmSumy]
  # łączenie z oszacowaniami i uśrednianie
  oszacowania = suppressMessages(join(sumy, oszacowania))
  temp = ddply(oszacowania, c(zmSumy, "suma"),
               function(x, y) {
                 x = c(mean(x[, y]), nrow(x))
                 return(setNames(x, c(y, "n")))
               },
               y = zmOszacowania)
  oszacowania = suppressMessages(join(oszacowania[, names(oszacowania) != zmOszacowania],
                                      temp))
  oszacowania = oszacowania[, c(zmId, zmOszacowania)]
  # oszacujmy, ile na tym uśrednianiu tracimy wariancji
  vO = var(oszacowania[, zmOszacowania])
  vP = cov.wt(as.matrix(temp[, zmOszacowania]), temp$n, method="ML")$cov[1, 1]
  odsUtraconejWariancji = 1 - vP / vO
  message("W wyniku uśredniania utracono ",
          100 * round(odsUtraconejWariancji, 4), "% wariancji.")
  # dopisywanie maksów, żeby można z nich było potem skorzystać
  max = as.list(max)
  temp = ddply(temp, zmSumy,
               function(x, max) {
                 max = sum(unlist( max[ unlist(x[1, names(max)]) ] ))
                 return(cbind(x, max))
              },
               max = max)
  # łatanie i wygładzanie
  temp = ddply(temp, zmSumy,
               function(x, zmOszacowania, span) {
                 if (nrow(x) == 1) {
                    stop("W danych występuje grupa z tylko jednym wynikiem sumarycznym.")
                 } else if (nrow(x) < 5 ) {
                   warning("W danych występuje grupa z mniej niż 5 różnymi wynikami sumarycznymi.",
                           immediate. = TRUE)
                 }
                 monot = FALSE
                 spanTemp = span
                 while (!monot) {
                   if ( (span > 1) & (spanTemp != span)) stop("Nie udało się uzyskać monotonicznego przewidywania.")
                   l = loess(paste0(zmOszacowania, "~ suma"), x, span=spanTemp)
                   suma = seq(min(x$suma), max(x$suma))
                   wynik = data.frame(suma, p=predict(l, suma))
                   monot = with(wynik, all(p[-1] >= p[-length(p)]))
                   if (!monot) spanTemp = spanTemp + 0.05
                 }
                 if (spanTemp != span) warning("Wartość parametru 'span' została zwiększona do ",
                                               spanTemp,
                                               ", aby uzyskać monotoniczne przewidywanie.",
                                               immediate. = TRUE)
                 # wypełnianie braków od 0
                 if (min(wynik$suma) > 0) {
                   wynik = rbind(data.frame(suma = 0:(min(wynik$suma) - 1),
                                            p = wynik$p[1]),
                                 wynik)
                 }
                 # i do maksa
                 if (max(wynik$suma) < x$max[1]) {
                   wynik = rbind(wynik,
                                 data.frame(suma = (max(wynik$suma) + 1):x$max[1],
                                            p = wynik$p[nrow(wynik)]))
                 }
                 names(wynik) = sub("^p$", zmOszacowania, names(wynik))
                 return(wynik)
               },
               zmOszacowania = zmOszacowania, span = span)
  # koniec
  return(list(
    mapowanie = temp,
    przewidywania = oszacowania,
    odsUtraconejWariancji = odsUtraconejWariancji))
}
