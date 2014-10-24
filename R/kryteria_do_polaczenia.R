#' @title Wskazywanie kryteriów do połączenia
#' @description
#' Funkcja zwraca listę numerów kryteriów, które powinny być połaczone.
#' @param rodzajEgzaminu rodzaj egzaminu
#' @param czescEgzaminu część egzaminu
#' @param rokEgzaminu rok egzaminu
#' @param zrodloDanychODBC żródło ODBC
#' @param czyPolski zmienna logiczna określająca, czy mamy do czynienia z językiem polskim
#' @param czesciWypracowania parametr liczbowy nie ignorowany, gdy zmienna czyPolski=TRUE.
#' Określa liczbę części wypracowania (domyślnie 6) i ignoruje w wyniku wszystkie kryteria, który opisy powtarzają się tyle razy co wskazuje ten parametr.
#' @return Funkcja zwraca listę numerów kryteriów do połączenia.
#' @examples
#' \dontrun{
#' rodzajEgzaminu = "matura"
#' czescEgzaminu = "informatyka podstawowa"
#' rokEgzaminu = 2014
#' kryteria_do_polaczenia(rodzajEgzaminu, czescEgzaminu, rokEgzaminu)
#' }
#' @export
kryteria_do_polaczenia <- function(rodzajEgzaminu, czescEgzaminu, rokEgzaminu, zrodloDanychODBC = "EWD",
                                   czyPolski = grepl("polski", czescEgzaminu), czesciWypracowania = 6){

  P = odbcConnect(as.character(zrodloDanychODBC))

  zapytanie = "select distinct TK.id_kryterium, PY.opis, TE.id_testu
  from arkusze as AR
  join testy as TE using(arkusz)
  join testy_kryteria as TK using(id_testu)
  join kryteria_oceny as KO using(id_kryterium)
  join pytania as PY using(id_pytania)
  where  rodzaj_egzaminu= ? and czesc_egzaminu = ?  and EXTRACT(YEAR from data_egzaminu) = ?"

  tryCatch({
    # pobranie numerow kryteriow oraz ich opisow
    tablicaDanych = sqlExecute(P, zapytanie, data =data.frame(rodzajEgzaminu, czescEgzaminu, rokEgzaminu)  , fetch = TRUE, stringsAsFactors = FALSE)

    odbcClose(P)
  },
  error=function(e) {
    odbcClose(P)
    stop(e)
  }
  )

  # usuniecie z opisów '_numer'
  tablicaDanych$opis = gsub("_[[:digit:]]+$", "", tablicaDanych$opis)

  # wskazanie duplikarow opisow
  dupl = unique(tablicaDanych$opis[duplicated(tablicaDanych$opis)])

  # jeżeli mamy do czynienia z jezykiem polskim to opisy, które pojawiają się 6 razy nie bedą brane pod uwagę
  if(czyPolski){
    tab = table(tablicaDanych$opis)
    six = names(tab)[ tab==czesciWypracowania]
  } else {
    six = NULL
  }

  # finalnie opisy do połączeń:
  opisyDoPolaczenia = unique(tablicaDanych$opis[tablicaDanych$opis %in% dupl & ! tablicaDanych$opis %in% six])
  
  polaczoneKryteria = NULL
  ret = list()
  for(i in seq_along(opisyDoPolaczenia)){
    ret[[i]] = tablicaDanych$id_kryterium[tablicaDanych$opis == opisyDoPolaczenia[i]]
    names(ret)[i] = opisyDoPolaczenia[i]
    polaczoneKryteria = c(polaczoneKryteria, ret[[i]])
  }
  
  tablicaDanych = tablicaDanych[!tablicaDanych$id_kryterium %in% polaczoneKryteria, ]
  
  for(i in  seq_len(nrow(tablicaDanych))){
    ret[[length(ret)+1]] = tablicaDanych$id_kryterium[i]
    names(ret)[length(ret)] = tablicaDanych$opis[i]
  }
  
  attributes(ret)$id_testu = tablicaDanych$id_testu[1]

  return(ret)
}
#' @title Przygotowanie obiektu do edycji skali
#' @description
#' Funkcja przygotowuje ramkę danych, która może zostać wygodnie użyta do edycji skali przez funkcję \code{\link[ZPD]{edytuj_skale}} z pakietu ZPD.
#' @param kryt lista zwrócona przez funkcję \code{\link{kryteria_do_polaczenia}}
#' @param zrodloDanychODBC żródło danych
#' @return Funkcja zwraca ramkę danych, której wiersze albo zawierają id istniejącego pseudokryterium albo id wszystkich kryteriów, które mają zostać połączone w jedno pseudokryterium.
#' @export
przygotuj_kryteria <- function(kryt, zrodloDanychODBC = "EWD"){
  idTestu = attributes(kryt)$id_testu

  # stworzenie ramki danych z kolumnami opis i id_skrotu
  ret = data.frame(opis = character(0), id_skrotu = numeric(0), 
                   id_pseudokryterium = numeric(0), id_kryterium = numeric(0))

  for(ind in seq_along(kryt)){
    kryteria = kryt[[ind]]
    
    # jeżeli mamy jedno kryterium do połączenia to dołączamy do skali to kryteriu
    if(length(kryteria)==1){
      wiersz = data.frame(id_kryterium = kryteria)
      ret = rbind.fill(ret, wiersz)
      next;
    }

    zapytanie = paste0("select TK.id_kryterium, POK.id_pseudokryterium, P.opis
                       from testy_kryteria as TK
                       join pseudokryteria_oceny_kryteria as POK using(id_kryterium)
                       join pseudokryteria_oceny as P using(id_pseudokryterium)
                       where  id_pseudokryterium in
                       (select distinct POK2.id_pseudokryterium
                       from testy_kryteria
                       join pseudokryteria_oceny_kryteria as POK2 using(id_kryterium)
                       join pseudokryteria_oceny as P using(id_pseudokryterium)
                       where  id_kryterium in (", paste0(rep("?", length(kryteria)), collapse=", "), ") and id_testu = ?) and id_testu = ?")

    P = odbcConnect(as.character(zrodloDanychODBC))

    tryCatch({
      # pobranie danych o pseudokryterium
      psk = sqlExecute(P, zapytanie, data =data.frame(cbind(t(kryteria), idTestu, idTestu))  , fetch = TRUE, stringsAsFactors = FALSE)
      odbcClose(P)
    },
    error=function(e) {
      odbcClose(P)
      stop(e)
    }
    )

    # Jeżeli informacje z bazy o kryteriach pokrywają się z danymi do połączenia to zmienna wiersz zawiera tylko pseudokryterium.
    # W przeciwnym wypadku zmienna wiersz zawiera wszystkie id kryteriów.
    # w powyższym zapytaniu sql zakładam, że kryterium nie może należeć do kilku pseudokryteriów w ramech tego samego egzaminu
    if ( nrow(psk)!=0 &  all(sort(psk$id_kryterium)==sort(kryteria))){
      wiersz = data.frame(id_pseudokryterium  = psk$id_pseudokryterium[1])
    } else {
      wiersz = data.frame(t(c(kryteria, names(kryt)[ind])))
      names(wiersz) = c(paste0("id_kryterium_", seq_along(kryteria)), "opis")
    }

    ret = rbind.fill(ret, wiersz)
  }

  return(ret)
}
