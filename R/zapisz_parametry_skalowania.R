#' @title Zapisywanie parametrow skalowan.
#' @description
#' Funkcja zapisuje parametry skalowania do bazy danych
#' @param nazwa_skali nazwa skali. Kiedy nazwa skali przymuje wartość NULL nie jest brana pod 
#' uwagę w wyszukiwaniu. Wartość domyślna to NULL.
#' @param id_testu id testu. Kiedy id testu przymuje wartość NULL nie jest brana pod 
#' uwagę w wyszukiwaniu. Wartość domyślna to NULL.
#' @param parametry ramka danych o strukturze zgodnej z tą, w jakiej zwraca oszacowania parametrów funkcja skaluj().
#' @param opis opis estymacji
#' @param estymacja parametr opisujący metodę estymacji - musi być jedną z wartości występujących w tablicy sl_estymacje_parametrow.
#' @param zrodloDanychODB string określający źródło danych. Wartość domyślna to 'EWD'.
#' @return Funkcja nie zwraca żadnej wartości.
#' @export
zapisz_parametry_skalowania <- function(nazwa_skali=NULL, id_testu=NULL, parametry, opis, estymacja, zrodloDanychODBC='ewd_grzes') {
  if( is.null(nazwa_skali) & is.null(id_testu) ){
    stop("Nazwa skali oraz id testu nie mogą mieć jednocześnie wartości null.")
  }
  
  P = odbcConnect(zrodloDanychODBC)
  tryCatch({
            # zapytanie
            if(!is.null(nazwa_skali)){
              zapytanie = paste0("SELECT id_skali FROM skale where nazwa = '", nazwa_skali, "'")
              if(!is.null(id_testu)){
                zapytanie = paste0(zapytanie, " AND id_testu = ", id_testu, "")
              }
            } else{
              zapytanie = paste0("SELECT id_skali FROM skale WHERE id_testu = '", id_testu, "'")
            }
            skaleZap = bezpieczne_sqlQuery(P, zapytanie)
            
            if( length(na.omit(skaleZap))==0 ){
              odbcClose(P)
              stop("Nie ma skali w bazie danych.")
            } else if(length(skaleZap)!=1){
              odbcClose(P)
              stop("Skala określona niejednoznacznie.")
            }
            idSkali = skaleZap$id_skali[1]
            
            zapytanie = paste0("SELECT count(*) FROM sl_estymacje_parametrow WHERE estymacja = '", estymacja, "'")
            estymacjeNumZap = bezpieczne_sqlQuery(P, zapytanie)
            
            zapytanie = "SELECT max(skalowanie) FROM skalowania"
            maxSkalowanieZap = as.numeric(bezpieczne_sqlQuery(P, zapytanie))
            
            zapytanie = "SELECT parametr FROM sl_parametry"
            nazwyParametrow = bezpieczne_sqlQuery(P, zapytanie)$parametr
            
            zapytanie = paste0("SELECT greatest(max(skalowanie), count(*)) + 1 AS skalowanie
                               FROM skalowania WHERE id_skali = ", idSkali)
            numerSkalowania = bezpieczne_sqlQuery(P, zapytanie)$skalowanie
            
            odbcClose(P)
          },
          error=function(e){
            odbcClose(P)
            stop(e)
          }
        )
  if( estymacjeNumZap == 0 ){
    stop("Podanej metody estymacji nie ma w bazie danych.")
  }
  
  P = odbcConnect(zrodloDanychODBC)
  tryCatch({
          parBy = parametry[parametry$typ=="by", ]
          parTreshold= parametry[parametry$typ=="threshold", ]
          
          by = wydziel_kryteria_pseudokryteria(parBy$zmienna2)
          kryteriaBy = by$kryteria
          pseudokryteriaBy = by$pseudokryteria
           
          tres = wydziel_kryteria_pseudokryteria(parTreshold$zmienna1)
          kryteriaTres = tres$kryteria
          pseudokryteriaTres = tres$pseudokryteria
          
          liczbaParam = table(kryteriaTres)
          
          zapytanie = paste0("SELECT kolejnosc, id_kryterium, id_pseudokryterium FROM skale_elementy 
                             WHERE id_skali =", idSkali)
          skaleElementy = bezpieczne_sqlQuery(P, zapytanie)
          kryteriaBaza = na.omit(skaleElementy$id_kryterium)
          pseudokryteriaBaza = na.omit(skaleElementy$id_pseudokryterium)
          
          sprawdz_zgodnosc_kryteriow(kryteriaTres, kryteriaBy  , "kryteria 'treshold'" , "kryteria 'by'")
          sprawdz_zgodnosc_kryteriow(kryteriaBy  , kryteriaTres, "kryteria 'by'"       , "kryteria 'treshold'")
          sprawdz_zgodnosc_kryteriow(kryteriaTres, kryteriaBaza, "kryteria z parametru", "kryteria z bazy")
          sprawdz_zgodnosc_kryteriow(kryteriaBaza, kryteriaTres, "kryteria z bazy"     , "kryteria z parametru", error=FALSE)
          
          sprawdz_zgodnosc_kryteriow(pseudokryteriaTres, pseudokryteriaBy  , "pseudokryteria 'treshold'"         , "pseudokryteria 'by'")
          sprawdz_zgodnosc_kryteriow(pseudokryteriaBy  , pseudokryteriaTres, "pseudokryteria 'by'"               , "pseudokryteria 'treshold'")
          sprawdz_zgodnosc_kryteriow(pseudokryteriaTres, pseudokryteriaBaza, "pseudokryteria z parametru funkcji", "pseudokryteria z bazy")
          sprawdz_zgodnosc_kryteriow(pseudokryteriaBaza, pseudokryteriaTres, "pseudokryteria z bazy"             , "pseudokryteria z parametru funkcji", error=FALSE)
          
          bezpieczne_sqlQuery(P, "BEGIN;")
          
          insert = paste0("INSERT INTO skalowania (skalowanie, opis , estymacja, id_skali)  VALUES
                          ( ", numerSkalowania,", '", opis, "', '", estymacja, "',", idSkali, ")" )
          bezpieczne_sqlQuery(P, insert)
          
          for(k in 1:length(liczbaParam)){
            krytNum = as.numeric(names(liczbaParam)[k])
            
            kolejnoscTemp = NULL
            if(krytNum %in% kryteriaBaza){
              kolejnoscTemp = skaleElementy$kolejnosc[ (skaleElementy$id_kryterium == krytNum) %in% TRUE ]
            } else if(krytNum %in% pseudokryteriaBaza){
              kolejnoscTemp = skaleElementy$kolejnosc[ (skaleElementy$id_pseudokryterium == krytNum) %in% TRUE ]
            }
            
            # model 2PL
            if( liczbaParam[k]== 1 ){
              
              wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp,numerSkalowania,
                                           "dyskryminacja","2PL",parBy$wartosc[kryteriaBy==krytNum],parBy$'S.E.'[kryteriaBy==krytNum])
              
              wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp,numerSkalowania,
                                           "trudność","2PL",
                                           parTreshold$wartosc[kryteriaTres==krytNum]/parBy$wartosc[kryteriaBy==krytNum],
                                           parTreshold$'S.E.'[kryteriaTres==krytNum]/parBy$wartosc[kryteriaBy==krytNum] )
              
              
            } else if ( liczbaParam[k] > 1 ){ # model GRM
              dyskryminacja = parBy[kryteriaBy==krytNum,"wartosc"]
              
              wstaw_do_skalowania_elementy (P, idSkali, kolejnoscTemp,
                                            numerSkalowania,"dyskryminacja",
                                            "GRM",dyskryminacja,parBy$'S.E.'[kryteriaBy==krytNum])
              
              indTres = which(kryteriaTres==krytNum)
              srednia = mean(parTreshold[indTres, "wartosc"]) / parBy$wartosc[kryteriaBy==krytNum]
              
              wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp,numerSkalowania,
                                           "trudność","GRM",srednia,NULL)
                      
              for(m in indTres ){
                nazwaPar = paste0("k", which(m==indTres))
                
                if( ! nazwaPar %in% nazwyParametrow ){
                  bezpieczne_sqlQuery(P, paste0("INSERT INTO sl_parametry(parametr,opis) values ('", nazwaPar, "','kn(GRM) - patrz opis parametru k1')")  )
                }
                
                wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp,numerSkalowania,nazwaPar,"GRM",
                                             parTreshold$wartosc[m]/parBy$wartosc[kryteriaBy==krytNum] - srednia,
                                             parTreshold$'S.E.'[m]/parBy$wartosc[kryteriaBy==krytNum])
              }
            }
          } 
          bezpieczne_sqlQuery (P, "COMMIT;")
          odbcClose(P)
          },
          error=function(e){
            bezpieczne_sqlQuery(P, "ROLLBACK;")
            odbcClose(P)
            stop(e)
          }
        )
  invisible(NULL)
}
#' @title Wstawianie danych do tablicy 'skalowania_elementy'
#' @description
#' Funkcja wstawia jeden wiersz danych do tablicy 'skalowania_elementy'.
#' @param P źródło danych ODBC.
#' @param idSkali numer opisujący id skali
#' @param kolejnosc liczba
#' @param numerSkalowania liczba 
#' @param nazwaParametru ciąg znaków określający nazwę parametru
#' @param model ciąg znaków
#' @param wartosc liczba
#' @param odchylenie liczba opisująca odchylenie standardowe parametru.
#' @return Funkcja nie zwraca żadnej wartości.
wstaw_do_skalowania_elementy <- function(zrodloODBC, idSkali, kolejnosc,numerSkalowania,
                                         nazwaParametru,model,wartosc,odchylenie){
  insSkalowania = paste0("INSERT INTO skalowania_elementy  (id_elementu,id_skali,kolejnosc,
                         skalowanie,parametr,model,wartosc,uwagi",ifelse(is.null(odchylenie),"",",bl_std"),")
                         VALUES (nextval('skalowania_elementy_id_elementu_seq'),", idSkali,
                         ",", kolejnosc, ",",
                         numerSkalowania, " , '", nazwaParametru, "' ,'", model, "', ",
                         wartosc,",''",ifelse(is.null(odchylenie),"",paste0(",",odchylenie)), ")"
                         )
  bezpieczne_sqlQuery(zrodloODBC, insSkalowania)
  invisible(NULL)
}
#' @title Bezpieczna funkcja do wykonywania polecen SQL.
#' @description
#' Funkcja wykonuje polecenie SQL. W przypadku niepowodzenia wyświetla błąd zwracany przez bazę danych.
#' @param channel połączenie z bazą danych zwracane przez funkcję \code{\link{odbcConnect}}.
#' @param query polecenie SQL.
#' @return Funkcja zwraca wynik polecenia SQL.
bezpieczne_sqlQuery <- function(channel, query){
  ret = sqlQuery(channel, query)
  
  if(grepl("Error while executing the query", ret[1])){
    stop("Wykonanie polecenia SQL nie powiodło się: \n", paste0(ret, collapse="\n"))
  }
  return (ret)
}
#' @title Sprawdzanie zgodnosci kryteriow.
#' @description
#' Funkcja sprawdza, czy wektor kryteriów kryt1 jest zawarty w zbiorze kryteriów kryt2.
#' @param kryt1 wektor liczb opisujący kryteria
#' @param kryt2 wektor liczb opisujący kryteria
#' @param nazwa1 nazwa opisująca wektor kryt1
#' @param nazwa2 nazwa opisująca wektor kryt2
#' @param error jeżeli TRUE to funkcje wyświetla błąd w przypadku, gdy któryś z elementów kryt1
#' nie należy do kryt2.
#' @return Funkcja nie zwraca żadnej wartości.
sprawdz_zgodnosc_kryteriow <- function(kryt1, kryt2, nazwa1, nazwa2, error=TRUE){
  if(  length(fInds <- which( ! unique(kryt1) %in% unique(kryt2))) !=0  ){
    if(error){
      stop(paste0(nazwa1, " i ", nazwa2," nie pokrywają się.",
                  " Brakujące ", nazwa1, ":\n"),
           paste(unique(kryt1)[fInds], collapse="\n"))
    } else {
      warning(paste0(nazwa1," i ", nazwa2," nie pokrywają się.",
                     " Brakujące ", nazwa1,":\n"),
              paste(unique(kryt1)[fInds], collapse="\n"))
    }
  } 
  invisible(NULL)
}
#' @title Wydzielanie kryteriow i pseudokryteriow
#' @description
#' Funkcja wydziala kryteria i pseudokryteria z wektora ciągów znakowych oraz sprawdza poprawność ich nazw.
#' @param nazwy wektor ciągów znakowych
#' @return Funkcja zwraca dwuelementową listę. Pierwszy element o nazwie kryteria zawiera wektor liczb,
#' który zawiera numery kryteriow. Drugi element to wektor numerów pseudokryteriów.
wydziel_kryteria_pseudokryteria <- function(nazwy){
  poprawneKrytBy = grepl("^k_[0-9]", nazwy)
  poprawnePseudokrytBy = grepl("^p_[0-9]", nazwy)
  poprawneIinneKryteriaBy =  grepl("^[[:alnum:]]+_", nazwy)
  
  if(sum(! poprawneIinneKryteriaBy) > 0){
    stop("Nazwy nie pasujące do kryteriów i pseudokryteriów:\n",
         paste(krytNazwyBy[! poprawneIinneKryteriaBy], collapse="\n" ))
  }
  if(sum(poprawneIinneKryteriaBy & ! poprawneKrytBy & ! poprawnePseudokrytBy) > 0){
    warning("Niepoprawne nazwy kryteriów:\n",
            paste(nazwy[poprawneIinneKryteriaBy & ! poprawneKrytBy & ! poprawnePseudokrytBy], collapse="\n" ))
  }
  
  numery = as.numeric( sub("^[[:alnum:]]+_", "", nazwy) )
  
  kryteria = numery[poprawneIinneKryteriaBy & ! poprawnePseudokrytBy]
  pseudokryteria = numery[poprawnePseudokrytBy]
  
  return(list(kryteria=kryteria, pseudokryteria = pseudokryteria))
}
