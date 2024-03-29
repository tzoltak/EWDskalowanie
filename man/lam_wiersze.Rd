% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skaluj_funkcje_pomocnicze.R
\name{lam_wiersze}
\alias{lam_wiersze}
\title{Funkcja lamie dluzsze ciagi znakow na wiersze.}
\usage{
lam_wiersze(
  x,
  wciecie = 2,
  maxDl = 90,
  srednikNaKoncu = TRUE,
  sep = " ",
  lamNaNawiasachOkraglych = TRUE
)
}
\arguments{
\item{x}{wektor tekstowy do pocięcia}

\item{wciecie}{liczba spacji dostawianych na początku każdego wynikowego elementu wektora}

\item{maxDl}{maksymalna liczba znaków w linii (uwzględniając wcięcie)}

\item{srednikNaKoncu}{wartość logiczna - czy na końcu ostatniego elementu wynikowego wektora ma być dopisany średnik (o ile go tam nie ma)?}

\item{sep}{spearator używany przy złączaniu elementów \code{x} w jeden ciąg znaków}

\item{lamNaNawiasachOkraglych}{wartość logiczna - czy dodatkowo łamać wiersze za zamykającymi nawiasami okrągłymi: ')'?}
}
\value{
lista
}
\description{
Funkcja najpierw łączy wektor tekstowy w jeden ciąg znaków, a następnie w inteligentny sposób łamie go z powrotem na kilkuelementowy wektor tekstowy.
Jest to przydatne np. przy przygotowywaniu poleceń dla Mplusa, który nie lubi linii dłuższych niż 90 znaków.
}
\examples{
x=c("y BY", "x1*1.200", "x2*1.000", "x3*0.800", "x4*0.600", "x5*0.400")
write(lam_wiersze(x), "")
write(lam_wiersze(x, wciecie=4, maxDl=30), "")
}
\seealso{
\code{\link[base]{strwrap}}
}
