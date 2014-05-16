#' @title Pobieranie parametrow skalowan.
#' @description
#' Funkcja pobiera parametry skalowań o podanej nazwie skali i numerze testu oraz których
#' opis skalowań spełnia podane wyrażenie regularne.
#' @param nazwa_skali nazwa skali. Kiedy nazwa skali przymuje wartość NULL nie jest brana pod 
#' uwagę w wyszukiwaniu. Wartość domyślna to NULL.
#' @param id_testu id testu.Kiedy id testu przymuje wartość NULL nie jest brana pod 
#' uwagę w wyszukiwaniu. Wartość domyślna to NULL.
#' @param opis_skalowania wyrażenie regularne określające opis skalowania. Domyślna wartość to '\%'. 
#' @param zrodloDanychODB string określający źródło danych. docelowa wartość domyślna to 'EWD'. Obecnie 'ewd_grzes'.
#' @param parametryzacja parametr określający format zwracanego wyniku. Domyślna wartość to 'baza'.
#' Inna możliwa wartość to 'mplus'.
#' @return W przyopadku użycia parametryzacji 'baza', funkcja zwraca listę taką, że:
#' \itemize{
#'    \item każdy element listy opisuje parametry innego skalowania;
#'    \item każdy element listy ma też przypisane jako atrybuty wartości kolumn: 'skalowanie', 'opis', 'estymacja';
#'    \item zwracana lista ma przypisane jako atrybuty wartości wszystkich kolumn z tablicy 'skale';
#' }
#' W przyopadku użycia parametryzacji 'mplus' funkcja zwraca parametry skalowania w formie ramki danych, która jest
#' w postaci zwracanej przez funkcję skaluj().
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
                                         opis_skalowania='.*', zrodloDanychODB = 'ewd_grzes', parametryzacja = "baza"){
  
  if(parametryzacja=="mplus"){
    return (pobierz_parametry_mplus(nazwa_skali, id_testu,opis_skalowania, zrodloDanychODB) )
  } else if(parametryzacja != "baza"){
    stop("Niepoprawna wartość parametru 'parametryzacja': ",parametryzacja)
  }
  
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
  
  joiny =         "
                  from skale AS S
                  JOIN skale_elementy AS SE USING(id_skali)
                  JOIN skalowania_elementy AS SAE USING(id_skali,kolejnosc)
                  JOIN skalowania AS SA USING(id_skali,skalowanie)
                  "
  
  zapytanie1 = paste0("
                      select  
                      id_kryterium, id_pseudokryterium,
                      SAE.model, SAE.parametr, SAE.wartosc, SA.skalowanie
                      ", joiny , where, '\n ORDER BY skalowanie, kolejnosc, parametr')
  
  zapytanie2 = paste0("
                      select distinct 
                      SA.skalowanie, SA.opis, SA.estymacja
                      ", joiny , where)
  
  zapytanie3 = paste0("
                      select distinct 
                      S.*
                      ", joiny , where)
  
  
  require(RODBC)
  P = odbcConnect(zrodloDanychODB)
  tryCatch({
            tablicaDanych = sqlQuery(P, gsub(" ", " ", zapytanie1))
            opisSkalowan  = sqlQuery(P, gsub(" ", " ", zapytanie2))
            skale         = sqlQuery(P, gsub(" ", " ", zapytanie3))
            odbcClose(P)
          },
          error=function(e) {
            odbcClose(P)
            stop(e)
          }
          )
  
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
    
    stop("Więcej niż jedna skala przypisana do wyników.")
  }
  
  for( k in 1:ncol(skale) ){
    attr(ret, colnames(skale)[k]) = as.character(skale[1, k])
  }
  
  return(ret)
}
#' @title Pobieranie parametrow skalowan zapisanych do bazy w formacie funkcji skaluj()
#' @description
#' Funkcja pobiera parametry skalowań o podanej nazwie skali i numerze testu oraz których
#' opis skalowań spełnia podane wyrażenie regularne. Funkcja zakłada, że parametry zostały zapisane przez 
#' funkcje zapisz_parametry_skalowania().
#' @param nazwa_skali nazwa skali. Kiedy nazwa skali przymuje wartość NULL nie jest brana pod 
#' uwagę w wyszukiwaniu. Wartość domyślna to NULL.
#' @param id_testu id testu.Kiedy id testu przymuje wartość NULL nie jest brana pod 
#' uwagę w wyszukiwaniu. artość domyślna to NULL.
#' @param opis_skalowania wyrażenie regularne określające opis skalowania. Domyślna wartość to '\%'. 
#' @param zrodloDanychODB string określający źródło danych. docelowa wartość domyślna to 'EWD'. Obecnie 'ewd_grzes'.
#' @return
#' Funkcja zwraca ramkę danych, która jest zgodna z postacią ramek zwracanych przez funkcję skaluj().
pobierz_parametry_mplus <- function(nazwa_skali=NULL, id_testu=NULL, 
                                    opis_skalowania='.*', zrodloDanychODB = 'ewd_grzes'){
  
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
  zapytanie1 = paste0("
                      select  
                      parametr, model, wartosc, bl_std, id_kryterium, id_pseudokryterium
                      ", joiny , where, '\n ORDER BY skalowanie, kolejnosc, parametr')
  
  require(RODBC)
  P = odbcConnect(zrodloDanychODB)
  tryCatch({
            tablicaDanych = sqlQuery(P, zapytanie1)
            odbcClose(P)
          },
          error=function(e) {
            odbcClose(P)
            stop(e)
          }
          )
  
  # 2PL
  dwaPL = tablicaDanych[tablicaDanych[,"model"]=="2PL",]
  
  if( length ( errInds <- which( ! dwaPL[, "parametr"] %in% c("dyskryminacja","trudność")  ) ) != 0 ){
    stop("Niepoprawne rodzaje parametrów dla modelu 2PL: \n", paste(errInds,collapse="\n"))
  }
  
  # Jak rozumiem baza gwarantuje, że każdy element skale_elementy posiada kryterium albo pseudokryterium.
  kryt = dwaPL[,"id_kryterium"]
  kryt[is.na(kryt)] = dwaPL[,"id_pseudokryterium"]
  
  ret2PL = data.frame()
  for(krytNum in unique(kryt)){
    czyKryterium = krytNum %in% na.omit(dwaPL[,"id_kryterium"])
    
    by = dwaPL$wartosc[dwaPL$id_kryterium == krytNum & dwaPL$parametr=="dyskryminacja" ]
    byStd = dwaPL$bl_std[dwaPL$id_kryterium == krytNum & dwaPL$parametr=="dyskryminacja" ]
    
    zmienna1 = ifelse(czyKryterium,"k_id","p_id")
    zmienna2 = paste0(zmienna1,"_",krytNum)
    
    ret2PL = rbind(ret2PL, data.frame(typ="by", zmienna1, zmienna2, wartosc = by, S.E.= byStd, EstSE = by/byStd ))
    
    tres = dwaPL$wartosc[dwaPL$id_kryterium == krytNum & dwaPL$parametr=="trudność" ]
    tresStd = dwaPL$bl_std[dwaPL$id_kryterium == krytNum & dwaPL$parametr=="trudność" ]
    
    ret2PL = rbind(ret2PL, data.frame(typ="treshold", zmienna1=zmienna2,zmienna2=zmienna1,
                                      wartosc = tres*by, S.E.=tresStd*by, EstSE = tres / tresStd  ))
  }
  
  #GRM
  grm = tablicaDanych[tablicaDanych[, "model"]=="GRM", ]
  
  kryt = grm[,"id_kryterium"]
  kryt[is.na(kryt)] = grm[, "id_pseudokryterium"]
  
  
  retGRM = data.frame()
  for(krytNum in unique(kryt)){
    
    czyKryterium = krytNum %in% tablicaDanych$id_kryterium
    
    by  = grm[ grm$parametr=="dyskryminacja" & kryt == krytNum, c("wartosc","bl_std")]
    srednia = grm$wartosc[ grm$parametr=="trudność" & kryt == krytNum] * by$wartosc
    kPar = grm[ grepl("^k[[:digit:]+]$", grm$parametr) & kryt == krytNum , c("wartosc","bl_std")]  
    
    zmienna1 = ifelse(czyKryterium,"k_id","p_id")
    zmienna2 = paste0(zmienna1,"_",krytNum)
    retGRM = rbind(retGRM, 
                   data.frame(typ ='by', zmienna1=zmienna1, zmienna2=zmienna2, wartosc = by$wartosc, 
                              S.E.=by$bl_std, EstSE=by$wartosc /by$bl_std  ) )
    
    retGRM = rbind(retGRM, 
                   data.frame(typ ='treshold', zmienna1=zmienna2, zmienna2=zmienna1, wartosc = kPar$wartosc*by$wartosc + srednia, S.E.=kPar$bl_std*by$wartosc,
                              EstSE=(kPar$wartosc*by$wartosc + srednia)/(kPar$bl_std*by$wartosc)  ) )
  }
  
  ret = rbind(ret2PL,retGRM)
  colnames(ret)[ colnames(ret) == "EstSE" ] = "Est./S.E."
  
  ret = ret[order(ret$typ,ret$zmienna1,ret$zmienna2),] 
  
  return (ret)
}