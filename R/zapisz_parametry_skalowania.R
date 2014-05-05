# TO DO:
# warning dla nazw zmiennych
# opis funkcji

zapisz_parametry_skalowania <- function(nazwa_skali=NULL, id_testu=NULL, parametry, opis, estymacja, zrodloDanychODBC='ewd_grzes')
{
  if(is.null(nazwa_skali) & is.null(id_testu)  ){
    stop("Nazwa skali oraz id testu nie mogą mieć jednocześnie wartości null.")
  }
  
  P = odbcConnect(zrodloDanychODBC)
  tryCatch({
    
    # zapytanie
    if(!is.null(nazwa_skali)){
      zapytanie = paste0("select id_skali from skale where nazwa = '",nazwa_skali,"'")
      if(!is.null(id_testu)){
        zapytanie = paste0(zapytanie, " and id_testu = ",id_testu,"")
      }
    } else{
      zapytanie = paste0("select id_skali from skale where id_testu = '",id_testu,"'")
    }
    skaleZap = sqlQuery(P, zapytanie)
    
    zapytanie = paste0("SELECT count(*) from sl_estymacje_parametrow where estymacja = '",estymacja,"'")
    estymacjeNumZap = sqlQuery(P, zapytanie)
    
    zapytanie = "SELECT MAX(skalowanie) from skalowania"
    maxSkalowanieZap = as.numeric(sqlQuery(P, zapytanie))
    
    zapytanie = "select parametr from sl_parametry"
    nazwyParametrow = sqlQuery(P, zapytanie)[,1]
    
    odbcClose(P)
  },
  error=function(e){
    odbcClose(P)
    stop(e)
  })
  
  if( length(skaleZap)==0 ){
    stop("Nie ma w bazie skali.")
  } else if(length(skaleZap)!=1){
    stop("Skala określona niejednoznacznie.")
  }
  idSkali = skaleZap[1,1]
  
  if( estymacjeNumZap == 0 ){
    stop("Podanej estymacji nie ma w bazie danych.")
  }
  
  P = odbcConnect(zrodloDanychODBC)
  
  tryCatch({
    sqlQuery(P, "BEGIN;")
    
    insert = paste0("INSERT INTO skalowania (skalowanie, opis , estymacja,id_skali) 
                    VALUES (",maxSkalowanieZap+1,",\'",opis,"\', \'",estymacja,"\',",idSkali,") 
                    ;")
    sqlQuery(P, insert)
    
    parBy = parametry[parametry[,1]=="by",]
    parTreshold= parametry[parametry[,1]=="threshold",]
    
    kryteriaBy = as.numeric (sapply(strsplit(parBy[,"zmienna2"], "_"), function(x) x[length(x)]  ))
    kryteriaTres = as.numeric (sapply(strsplit(parTreshold[,"zmienna1"], "_"), function(x) x[length(x)]  ))
    
    liczbaParam = table(kryteriaTres)
    
    iter = 1 
    for(k in 1:length(liczbaParam)){
      
      kryt = as.numeric(names(liczbaParam)[k])
      
      # model 2PL
      if( liczbaParam[k]== 1 ){
        
        # insert do tabeli skale_elementy
        insSkale =  paste0("INSERT INTO skale_elementy (kolejnosc,id_skali,id_kryterium)
                           VALUES(",iter,",",idSkali,",",names(liczbaParam)[k],")"
        )
        sqlQuery(P, insSkale)
        
        insSkalowania =  paste0("INSERT INTO skalowania_elementy  (id_skali,kolejnosc,skalowanie,parametr,model,wartosc,uwagi)
                                VALUES (",idSkali,
                                ",",iter,",",
                                maxSkalowanieZap+1,
                                " , 'dyskryminacja' ,","'2PL'",", ",
                                parBy[kryteriaBy==kryt,"wartosc"],",'')"
                                )
        sqlQuery(P, insSkalowania)
        
        insSkalowania = paste0("INSERT INTO skalowania_elementy  (id_skali,kolejnosc,skalowanie,parametr,model,wartosc,uwagi) 
                               VALUES (",idSkali,
                               ",",iter,",",
                               maxSkalowanieZap+1,
                               " , 'trudność' ,","'2PL'",", ",
                               parTreshold[kryteriaTres==kryt,"wartosc"]/parBy[kryteriaTres==kryt,"wartosc"],
                               ",'')"
                               )
      } else if ( liczbaParam[k] > 1 ){ # model GRM
        
        insSkale =  paste0("INSERT INTO skale_elementy (kolejnosc,id_skali,id_kryterium)
                           VALUES(",iter,",",idSkali,",",names(liczbaParam)[k],")"
        )
        sqlQuery(P, insSkale)
        
        dyskryminacja = parBy[kryteriaBy==kryt,"wartosc"]
        insSkalowania =  paste0("INSERT INTO skalowania_elementy  (id_skali,kolejnosc,skalowanie,parametr,model,wartosc,uwagi)
                                VALUES (",idSkali,
                                ",",iter,",",
                                maxSkalowanieZap+1,
                                " , 'dyskryminacja' ,","'GRM'",", ",
                                dyskryminacja,",'')"
                                )
        sqlQuery(P, insSkalowania)
        
        indTres = which(kryteriaTres==kryt)
        srednia = mean(parTreshold[indTres,"wartosc"])
        
        insSkalowania = paste0("INSERT INTO skalowania_elementy  (id_skali,kolejnosc,skalowanie,parametr,model,wartosc,uwagi) 
                               VALUES (",idSkali,
                               ",",iter,",",
                               maxSkalowanieZap+1,
                               " , 'trudność' ,","'2PL'",", ",
                               srednia,
                               ",'')"
                               )
        
        sqlQuery(P, insSkalowania)
        
        for(m in indTres ){
          
          nazwaPar = paste0("k",which(m==indTres))
          
          if( ! nazwaPar %in% nazwyParametrow ){
            sqlQuery(P, paste0("INSERT INTO sl_parametry(parametr,opis) values ('",nazwaPar,"','kn(GRM) - patrz opis parametru k1')")  )
          }
          
          insSkalowania = paste0("INSERT INTO skalowania_elementy  (id_skali,kolejnosc,skalowanie,parametr,model,wartosc,uwagi)
                                 VALUES (",idSkali,
                                 ",",iter,",",
                                 maxSkalowanieZap+1,
                                 " , nazwaPar ,","'GRM'",", ",
                                 parTreshold[m,"wartosc"]/parBy[kryteriaBy==kryt,"wartosc"] - srednia,
                                 ",'')"
                                 )
          sqlQuery(P, insSkalowania)
        }
      }
      
      iter = iter + 1 
    } 
    
    sqlQuery(P, "COMMIT;")
    
    odbcClose(P)
  },
  error=function(e){
    sqlQuery(P, "ROLLBACK;")
    odbcClose(P)
    stop(e)
  })
  
}