#' @title Pobieranie parametrów skalowań.
#' @description
#' Funkcja pobiera parametry skalowań o podanej nazwie skali i numerze testu oraz których
#' opis skalowań spełnia podane wyrażenie regularne.

#' @param nazwa_skali nazwa skali. Kiedy nazwa skali przymuje wartość NULL nie jest brana pod 
#' uwagę w wyszukiwaniu. Wartość domyślna to NULL.
#' @param id_testu id testu.Kiedy id testu przymuje wartość NULL nie jest brana pod 
#' uwagę w wyszukiwaniu. artość domyślna to NULL.
#' @param opis_skalowania wyrażenie regularne określające opis skalowania. Domyślna wartość to '\%'. 
#' @param zrodloDanychODB string określający źródło danych. docelowa wartość domyślna to 'EWD'. Obecnie 'ewd_grzes'.
#' @return Funkcja zwraca listę taką, że:
#' \itemize{
#'    \item każdy element listy opisuje parametry innego skalowania;
#'    \item każdy element listy ma też przypisane jako atrybuty wartości kolumn: 'skalowanie', 'opis', 'estymacja';
#'    \item zwracana lista ma przypisane jako atrybuty wartości wszystkich kolumn z tablicy 'skale';
#' }
#' @examples
#' id_testu = 1128
#' nazwa_skali = "ktt;1128"
#' ret = pobierz_parametry_skalowania(nazwa_skali,id_testu)
#' 
#' attributes(ret)
#' str(ret)
#' 
#' attributes(ret[[1]])
#' str(ret[[1]])
#' @export
pobierz_parametry_skalowania <- function(nazwa_skali=NULL, id_testu=NULL, 
                                         opis_skalowania='.*', zrodloDanychODB = 'ewd_grzes'){
  
  if(is.null(nazwa_skali) & is.null(id_testu)  ){
    stop("Nazwa skali oraz id testu nie mogą mieć jednocześnie wartości null.")
  }
  
  where = "where"
  if( !is.null(nazwa_skali) ){
    where = paste0(where," nazwa = '", nazwa_skali, "'" )
    
    if(!is.null(id_testu)){
      where = paste0(where, " and id_testu = ", id_testu, "")
    }
  } else {
    where = paste0(where, " id_testu = ", id_testu, "")
  }
  where = paste0(where, " and SA.opis ~* '", opis_skalowania, "'")
  
  joiny =       "
                from skale AS S
                JOIN skale_elementy AS SE USING(id_skali)
                JOIN skalowania_elementy AS SAE USING(id_skali,kolejnosc)
                JOIN skalowania AS SA USING(id_skali,skalowanie)
                "
  
                # SA.id_skali, kolejnosc, 
  zapytanie1 = paste0("
                select  
                id_kryterium, id_pseudokryterium,
                SAE.model,SAE.parametr,SAE.wartosc, SA.skalowanie
                      ", joiny , where, '\n ORDER BY skalowanie, kolejnosc, parametr')
  
  zapytanie2 = paste0("
                select distinct 
                SA.skalowanie,SA.opis,SA.estymacja
                      ", joiny , where)
  
  zapytanie3 = paste0("
                select distinct 
                S.*
                      ", joiny , where)
  
  
  require(RODBC)
  P = odbcConnect(zrodloDanychODB)
  tryCatch({
    
    tablicaDanych = sqlQuery(P, gsub(" ", " ", zapytanie1))
    opisSkalowan =  sqlQuery(P, gsub(" ", " ", zapytanie2))
    skale =  sqlQuery(P, gsub(" ", " ", zapytanie3))
    odbcClose(P)
    
  },
  error=function(e){
    odbcClose(P)
    stop(e)
  })
  
  if(nrow(tablicaDanych)==0){
    warning("Nie znalezniono danych spełniających kryteria wyszukiwania")
    return(NULL)
  }
  
  ret = list()
  
  for(ind in 1:nrow(opisSkalowan)){
    ret[[ind]] = tablicaDanych[
                            tablicaDanych[, "skalowanie"] == opisSkalowan[,ind], 
                            colnames(tablicaDanych) != "skalowanie"
                            ]
    
    for(k in 2:ncol(opisSkalowan)){
      attr(ret[[ind]], colnames(opisSkalowan)[k]) = as.character(opisSkalowan[ind, k])
    }
  }
  
  if( nrow(skale) > 1 ){
    cat("Skale spełniające kryteria wyszukiwania: \n")
    print(skale)
    
    stop("Więcej niż jedna skala przpisana do wyników.")
  }
  
  for( k in 1:ncol(skale) ){
    attr(ret, colnames(skale)[k]) = as.character(skale[1, k])
  }
  
  return(ret)
}





















