#' @title Generowanie poziomów dla (pseudo)kryterium
#' @description
#' Funkcja na podstawie bazy danych oraz samych danych uczniów określa liczbę potencjalnych poziomów dla podanego (pseudo)kryterium.
#' @param vec wektor z odpowiedziami na (pseudo)kryterium
#' @param nazwa_pytania nazwa pytania
#' @param zrodloDanychODBC źródło danych ODBC
#' @return wektor poziomów
#' @import RODBCext 
generuj_poziom_pytania <- function(vec, nazwa_pytania, zrodloDanychODBC = "EWD"){
  
  numerPytania = as.numeric(gsub("([[:alnum:]]+_)", "", nazwa_pytania))
  
  if(grepl("k_", nazwa_pytania)){
    czyPseudokryterium = FALSE
  } else if (grepl("p_", nazwa_pytania)){
    czyPseudokryterium = TRUE
  } else {
    stop("Nie można zidentyfikować rodzaju pytania o nazwie: ", nazwa_pytania)
  }
  
  if(czyPseudokryterium){
    zapytanie = "select sum(l_punktow) from pseudokryteria_oceny
                join pseudokryteria_oceny_kryteria as POK using(id_pseudokryterium)
                join kryteria_oceny as KO on POK.id_kryterium = KO.id_kryterium
                where id_pseudokryterium = ?
                group by id_pseudokryterium"
    
  } else {
    zapytanie = "select l_punktow from kryteria_oceny 
                where id_kryterium  = ?"
  }
  
  tryCatch({
    P = odbcConnect(zrodloDanychODBC)
    maksymalnePunkty = sqlExecute(P, zapytanie, data = data.frame(numerPytania), fetch = TRUE)[[1]]
    odbcClose(P)
  },
  error=function(e) {
    odbcClose(P)
    stop(e)
  })
  
  if( all(vec%%1==0) ){
    # w przypadku zadań punktowanych 0-2 to poziom 1 nie występuje, jednak w skróceniu będzie notka, że poziom 1 jest zamieniany na któryś z sąsiednich.
    # ta sytuacja i tak nie będzie w praktyce występować.
    poziomy = 0:maksymalnePunkty
  } else if( all(vec%%0.5==0) ){
    # dla połówek
    poziomy = seq(0, maksymalnePunkty, by=0.5)
  } else{
    stop("Punktacja zawiera liczby, które nie są całkowite oraz połówkowe")
  }
  
  return(poziomy)
}

#' @title Łączenie dwóch poziomów dla wektora klasy factor
#' @description
#' Pomocnicza funkcja, która łączy dwa poziomy sprawdzając wcześniej, czy spełniają one dopuszczalne warunki (patrz opis funkcji \code{\link{czy_poziomy_sa_okej}}).
#' @param vecFactor wektor factorów
#' @param maxLPozWyk maksymalna liczba poziomów pytania
#' @param minLiczebnPozWyk minimalna liczebność obserwacji w każdym z poziomów
#' @param minOdsPozWyk minimalny odsetek obserwacji, które ma zawierać każdy poziom.
#' @return Funkcja zwraca wektor klasy factor, który dodatkowo posiada atrybuty. Pierwszy 'czy_poziomy_sa_okej' określa, czy poziomy spełniają zadane warunki.
#' Drugi atrybut 'polaczenie' zawiera tablicę z połączeniami. Jeżeli wektor wejściowy zawierał ten argument to funkcja dopisuje do niego wiersz z wykonanym połączniem
#' oraz aktualizuje poprzenie wiersze tego atrybutu.
#' @export
polacz_poziom <- function(vecFactor, maxLPozWyk=5, minLiczebnPozWyk=100, minOdsPozWyk=0.05){
  
  tab = table(vecFactor)
  len = length(tab)
  
  pol = attributes(vecFactor)$polaczenie 
  
  if(!is.null(pol)){
    tab = tab[!names(tab) %in% pol$zmieniany]
  }
  
  ret = vecFactor
  attributes(ret)$czy_poziomy_sa_okej = czy_poziomy_sa_okej(tab, maxLPozWyk, minLiczebnPozWyk, minOdsPozWyk)
  
  if(attributes(ret)$czy_poziomy_sa_okej == TRUE){
    return(ret)
  }
  
  toPol1 = which.min(tab)[1]
  
  toPol2 = NULL
  if( toPol1 - 1 > 0 ){
    toPol2 = toPol1 - 1
  }
  
  if( toPol1 + 1 <= length(tab)  & (is.null(toPol2) || tab[toPol1 + 1] < tab[toPol2])){
    toPol2 = toPol1 + 1
  } 
  
  ret[!is.na(ret) & ret==names(tab)[toPol1]] = as.numeric(names(tab)[toPol2])
  
  attributes(ret)$polaczenie = data.frame(rbind(attributes(vecFactor)$polaczenie, c(names(tab)[toPol1], names(tab)[toPol2])), stringsAsFactors= FALSE)
  colnames(attributes(ret)$polaczenie) <- c("zmieniany", "zastepujacy")
  
  # w kolumnie zastepujacy też należy zmienić poziom na nowy
  attributes(ret)$polaczenie$zastepujacy[attributes(ret)$polaczenie$zastepujacy == names(tab)[toPol1]] = names(tab)[toPol2]
  
  attributes(ret)$czy_poziomy_sa_okej = czy_poziomy_sa_okej(tab, maxLPozWyk, minLiczebnPozWyk, minOdsPozWyk)
  return(ret)
}

#' @title Funkcja określająca skrótcenie
#' @description
#' Pomocnicza funkcja, która łączy dwa poziomy sprawdzając wcześniej, czy spełniają one dopuszczalne warunki (patrz pis funkcji \code{\link{czy_poziomy_sa_okej}}).
#' @param vec wektor z wynikami danego pytania
#' @param poziomy wektor z dopuszczalnymi poziomami dla danego pytania
#' @param maxLPozWyk maksymalna liczba poziomów pytania
#' @param minLiczebnPozWyk minimalna liczebność obserwacji w każdym z poziomów
#' @param minOdsPozWyk minimalny odsetek obserwacji, które ma zawierać każdy poziom.
#' @return Funkcja zwraca skrócony wektor - factor, którego atrybut 'polaczenie' zawiera tabelę z zastępowanymi i zastępującym poziomami.
#' @export
okresl_skrocenie <- function(vec, poziomy, maxLPozWyk=5, minLiczebnPozWyk=100, minOdsPozWyk=0.05){
  
  vecFaktor = factor(vec, levels = poziomy)
  czy_poziomy_okej = FALSE
  
  zamiany = NULL
  while( !czy_poziomy_okej ){
    vecFaktor = polacz_poziom(vecFaktor, maxLPozWyk, minLiczebnPozWyk, minOdsPozWyk)
    czy_poziomy_okej = attributes(vecFaktor)$czy_poziomy_sa_okej
  }
  return(vecFaktor)
}

#' @title Sprawdzanie czy poziomy spełniają określone kryteria
#' @description
#' Funkcja na podstawie tablicy poziomów zwróconych przez funkcję \code{\link[base]{table}} określa,
#' czy spełnione są określone warunki.
#' @param tablicaPoziomow tablica poziomów
#' @param maxLPozWyk maksymalna liczba poziomów pytania
#' @param minLiczebnPozWyk minimalna liczebność obserwacji w każdym z poziomów
#' @param minOdsPozWyk minimalny odsetek obserwacji, które ma zawierać każdy poziom.
#' @return wartość logiczna
#' @export
czy_poziomy_sa_okej <- function(tablicaPoziomow, maxLPozWyk=5, minLiczebnPozWyk=100, minOdsPozWyk=0.05){
  
  if(length(tablicaPoziomow) > maxLPozWyk){
    return(FALSE)
  }
  
  if(min(tablicaPoziomow) < minLiczebnPozWyk){
    return(FALSE)
  }
  
  if(min(tablicaPoziomow/sum(tablicaPoziomow)) < minOdsPozWyk){
    return(FALSE)
  }
  
  return(TRUE)
}

#' @title Zapisywanie skrócenia do bazy
#' @description
#' Funkcja zapisuje skrócenie do bazy.
#' @param polaczenie tablica poziomów (patrz opis wartości zwracanej przez funkcję \code{\link{okresl_skrocenie}} )
#' @param numerPytania numer kryterium lub pseudokryterium.
#' @param idSkali numer skali
#' @param czyPseudokryterium zmienna logiczna określająca czy mamy do czynienia z kryterium (FALSE) czy z pseudokryterium (TRUE)
#' @param zrodloDanychODBC źródło danych ODBC
#' @return funkcja nic nie zwraca
#' @import RODBCext
#' @export
zapisz_skrot_do_bazy <- function(polaczenie, numerPytania, idSkali, czyPseudokryterium, zrodloDanychODBC = "EWD"){
  
  opisSkrotu = paste(paste(polaczenie$zmieniany, polaczenie$zastepujacy, sep=","), collapse=";")
  
  tryCatch(
{
  P = odbcConnect(zrodloDanychODBC)
  
  odbcSetAutoCommit(P, FALSE) # rozpocznij transakcję
  
  zapytanie = "insert into skroty_skal values (nextval('skroty_skal_id_skrotu_seq'), ?) returning id_skrotu"
  idSkrotu = sqlExecute(P, zapytanie, data = data.frame(opisSkrotu), fetch = TRUE)
  
  zapytanie2 = "insert into skroty_skal_mapowania values (?, ?, ?)"
  for(ir in 1:nrow(polaczenie)){
    sqlExecute(P, zapytanie2, data = data.frame(idSkrotu, polaczenie$zmieniany[ir], polaczenie$zastepujacy[ir]), fetch = TRUE)
  }
  
  zapytanie3 = paste0("update skale_elementy set id_skrotu = ?
                      where ", ifelse(czyPseudokryterium, "id_pseudokryterium", "id_kryterium"),  "= ? and id_skali = ?")
  
  
  sqlExecute(P, zapytanie3, data = data.frame(idSkrotu, numerPytania, idSkali), fetch = TRUE)
  
  odbcEndTran(P, TRUE) # zatwierdzamy transakcję
  
},
error = stop,
finally = odbcClose(P)
  )
return(invisible(NULL))
}

#' @title Zapisywanie skróceń dla części egzaminu
#' @description
#' Funkcja zapisuje skrócenie do bazy.
#' @param dane tablica z danymi z części egzaminu
#' @param idSkali numer skali
#' @param maxLPozWyk maksymalna liczba poziomów pytania
#' @param minLiczebnPozWyk minimalna liczebność obserwacji w każdym z poziomów
#' @param minOdsPozWyk minimalny odsetek obserwacji, które ma zawierać każdy poziom.
#' @param zrodloDanychODBC żródło danych
#' @return funkcja nic nie zwraca
#' @import RODBCext
#' @export
zapisz_skrocenia_do_bazy <- function(dane, idSkali, maxLPozWyk=5, minLiczebnPozWyk=100, minOdsPozWyk=0.05, zrodloDanychODBC = "EWD"){
  
  indPytan = which(grepl("^([[:alnum:]]+_)[[:digit:]]+", names(dane)))
  
  # poziomy = generuj_poziomy_pytan(dane, idSkali, zrodloDanychODBC)
  
  for(ind in indPytan){
    
    if(grepl("k_",names(dane)[ind])){
      czyPseudokryterium = FALSE
    } else if (grepl("p_",names(dane)[ind])){
      czyPseudokryterium = TRUE
    } else {
      stop("Nie można zidentyfikować rodzaju pytania o nazwie: ", names(dane)[ind])
    }
    
    vec = dane[, ind]
    poziomy = generuj_poziom_pytania(vec, names(dane)[ind], zrodloDanychODBC)
    skrocenie = okresl_skrocenie(vec, poziomy, maxLPozWyk, minLiczebnPozWyk, minOdsPozWyk)
    polaczenie = attributes(skrocenie)$polaczenie
    numerPytania = as.numeric(gsub("([[:alnum:]]+_)", "", names(dane)[ind]))
    if(!is.null(polaczenie)){
      zapisz_skrot_do_bazy(polaczenie, numerPytania, idSkali, czyPseudokryterium, zrodloDanychODBC)
    }
  }
  
  return(invisible(NULL))
}









