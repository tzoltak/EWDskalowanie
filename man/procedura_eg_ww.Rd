% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/procedury_eksperymentalne.R
\name{procedura_eg_ww}
\alias{procedura_eg_ww}
\title{Procedury skalowania egzaminow.}
\usage{
procedura_eg_ww(nazwyZmiennych, parametryS0, parametryS1, processors = 3)
}
\arguments{
\item{nazwyZmiennych}{nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja}

\item{parametryS0}{data.frame z parametrami zadań ze sprawdzianu dla uczniów o standardowej długości toku kształcenia}

\item{parametryS1}{data.frame z parametrami zadań ze sprawdzianu dla uczniów o toku kształcenia wydłużonym o rok}

\item{processors}{liczba rdzeni do wykorzystania przy estymacji}
}
\value{
lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
}
\description{
Procedura skalowania egzaminu gimnazjalnego - w formule od 2012 r. - w podejściu wielowymiarowym (eksperymentalnie).
Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
}
\examples{
# chwilowo brak
}
\seealso{
\code{\link{skaluj}}
}
