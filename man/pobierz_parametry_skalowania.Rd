\name{pobierz_parametry_skalowania}
\alias{pobierz_parametry_skalowania}
\title{Pobieranie parametrów skalowań.}
\usage{
pobierz_parametry_skalowania(nazwa_skali = NULL, id_testu = NULL,
  opis_skalowania = "\%", zrodloDanychODB = "ewd_grzes")
}
\arguments{
  \item{nazwa_skali}{nazwa skali. Kiedy nazwa skali
  przymuje wartość NULL nie jest brana pod uwagę w
  wyszukiwaniu. Wartość domyślna to NULL.}

  \item{id_testu}{id testu.Kiedy id testu przymuje
  wartość NULL nie jest brana pod uwagę w wyszukiwaniu.
  artość domyślna to NULL.}

  \item{opis_skalowania}{wyrażenie regularne określające
  opis skalowania. Domyślna wartość to '\%'.}

  \item{zrodloDanychODB}{string określający źródło
  danych. docelowa wartość domyślna to 'EWD'. Obecnie
  'ewd_grzes'.}
}
\value{
Funkcja zwraca listę taką, że: \itemize{ \item każdy
element listy opisuje parametry innego skalowania; \item
każdy element listy ma też przypisane jako atrybuty
wartości kolumn: 'skalowanie', 'opis', 'estymacja'; \item
zwracana lista ma przypisane jako atrybuty wartości
wszystkich kolumn z tablicy 'skale'; }
}
\description{
Funkcja pobiera parametry skalowań o podanej nazwie skali
i numerze testu oraz których opis skalowań spełnia
podane wyrażenie regularne.
}
\examples{
id_testu = 1128
nazwa_skali = "ktt;1128"
ret = pobierz_parametry_skalowania(nazwa_skali,id_testu)

attributes(ret)
str(ret)

attributes(ret[[1]])
str(ret[[1]])
}

