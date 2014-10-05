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
kryteria_do_polaczenia <- function(rodzajEgzaminu, czescEgzaminu, rokEgzaminu, zrodloDanychODBC = "EWD_grzes", 
                                   czyPolski = grepl("polski", czescEgzaminu), czesciWypracowania = 6){
  
  P = odbcConnect(as.character(zrodloDanychODBC))
  
  zapytanie = "select distinct TK.id_kryterium, PY.opis
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
  
  ret = list()
  for(i in seq_along(opisyDoPolaczenia)){
    ret[[i]] = tablicaDanych$id_kryterium[tablicaDanych$opis == opisyDoPolaczenia[i]]
  }
  
  return(ret)
}