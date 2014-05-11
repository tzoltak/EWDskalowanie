# TO DO:
# warning dla nazw zmiennych
# opis funkcji

zapisz_parametry_skalowania <- function(nazwa_skali=NULL, id_testu=NULL, parametry, opis, estymacja, zrodloDanychODBC='ewd_grzes') {
  if(is.null(nazwa_skali) & is.null(id_testu)  ){
    stop("Nazwa skali oraz id testu nie mogą mieć jednocześnie wartości null.")
  }
  
  P = odbcConnect(zrodloDanychODBC)
  tryCatch({
	    # zapytanie
	    if(!is.null(nazwa_skali)){
	      zapytanie = paste0("SELECT id_skali FROM skale WHERE nazwa = '", nazwa_skali, "'")
	      if(!is.null(id_testu)){
	        zapytanie = paste0(zapytanie, " and id_testu = ", id_testu, "")
	      }
	    } else {
	      zapytanie = paste0("SELECT id_skali FROM skale WHERE id_testu = '", id_testu, "'")
	    }
	    skaleZap = sqlQuery(P, zapytanie)
	    
	    zapytanie = paste0("SELECT count(*) FROM sl_estymacje_parametrow WHERE estymacja = '", estymacja, "'")
	    estymacjeNumZap = sqlQuery(P, zapytanie)
	    
	    zapytanie = "SELECT MAX(skalowanie) FROM skalowania"
	    maxSkalowanieZap = as.numeric(sqlQuery(P, zapytanie))
	    
	    zapytanie = "SELECT parametr FROM sl_parametry"
	    nazwyParametrow = sqlQuery(P, zapytanie)[, 1]
	    
	    odbcClose(P)
	  },
	  error=function(e){
	    odbcClose(P)
	    stop(e)
	  }
 	)
  
  if( length(skaleZap)==0 ){
    stop("Nie ma w bazie skali.")
  } else if(length(skaleZap) != 1) {
    stop("Skala określona niejednoznacznie.")
  }
  idSkali = skaleZap$id_skali[1]
  
  if( estymacjeNumZap == 0 ){
    stop("Podanej estymacji nie ma w bazie danych.")
  }
  
  P = odbcConnect(zrodloDanychODBC)
  tryCatch({
	    sqlQuery(P, "BEGIN;")
	    
	    insert = paste0("INSERT INTO skalowania (skalowanie, opis, estymacja, id_skali) 
	                    VALUES (", maxSkalowanieZap+1, ",\'", opis, "\', \'", estymacja, "\',", idSkali, ") 
	                    ;")
	    sqlQuery(P, insert)
	    
	    parBy        = parametry[parametry$typ == "by"       , ]
	    parThreshold = parametry[parametry$typ == "threshold", ]
	    
	    kryteriaBy    = as.numeric(sapply(strsplit(       parBy$zmienna2, "_"), function(x) {return(x[length(x)])} ))
	    kryteriaThres = as.numeric(sapply(strsplit(parThreshold$zmienna1, "_"), function(x) {return(x[length(x)])} ))
	    
	    liczbaThresh = table(kryteriaThres)
	    iter = 1 
	    for(k in 1:length(liczbaThresh)){
	      kryt = as.numeric(names(liczbaThresh)[k])
	      
	      # model 2PL
	      if( liczbaThresh[k]== 1 ){
	        # insert do tabeli skale_elementy
	        insSkale =  paste0("INSERT INTO skale_elementy (kolejnosc, id_skali, id_kryterium)
	                           VALUES(", iter, ",", idSkali, ",", names(liczbaThresh)[k],")"
	        )
	        sqlQuery(P, insSkale)
	        
	        insSkalowania = paste0("INSERT INTO skalowania_elementy (id_skali, kolejnosc, skalowanie, parametr, model, wartosc, uwagi)
	                                VALUES (", idSkali,
	                                ",", iter, ",",
	                                maxSkalowanieZap+1,
	                                " , 'dyskryminacja' ,", "'2PL'", ", ",
	                                parBy[ kryteriaBy==kryt, "wartosc"], ",'')"
	                                )
	        sqlQuery(P, insSkalowania)
	        
	        insSkalowania = paste0("INSERT INTO skalowania_elementy (id_skali, kolejnosc, skalowanie, parametr, model, wartosc, uwagi) 
	                               VALUES (", idSkali,
	                               ",", iter, ",",
	                               maxSkalowanieZap+1,
	                               " , 'trudność' ,", "'2PL'", ", ",
	                               parThreshold$wartosc[kryteriaThres==kryt]/parBy$wartosc[kryteriaThres==kryt],
	                               ",'')"
	                               )
	      } else if ( liczbaThresh[k] > 1 ){ # model GRM
	        insSkale =  paste0("INSERT INTO skale_elementy (kolejnosc, id_skali, id_kryterium)
	                           VALUES(", iter, ",", idSkali, ",", names(liczbaThresh)[k], ")"
	        )
	        sqlQuery(P, insSkale)
	        
	        dyskryminacja = parBy$wartosc[kryteriaBy==kryt]
	        insSkalowania = paste0("INSERT INTO skalowania_elementy  (id_skali, kolejnosc, skalowanie, parametr, model, wartosc, uwagi)
	                                VALUES (", idSkali,
	                                ",", iter, ",",
	                                maxSkalowanieZap+1,
	                                " , 'dyskryminacja' ,", "'GRM'", ", ",
	                                dyskryminacja, ",'')"
	                                )
	        sqlQuery(P, insSkalowania)
	        
	        indThres = which(kryteriaThres==kryt)
	        srednia = mean(parThreshold$wartosc[indThres] / parBy$wartosc[kryteriaBy==kryt])
	        
	        insSkalowania = paste0("INSERT INTO skalowania_elementy  (id_skali, kolejnosc, skalowanie, parametr, model, wartosc, uwagi) 
	                               VALUES (", idSkali,
	                               ",", iter, ",",
	                               maxSkalowanieZap+1,
	                               " , 'trudność' ,", "'2PL'", ", ",
	                               srednia,
	                               ",'')"
	                               )
	        sqlQuery(P, insSkalowania)
	        
	        for(m in indThres ){
	          nazwaPar = paste0("k", parThreshold$zmienna2[m])
	          
	          if( !(nazwaPar %in% nazwyParametrow) ){
	            sqlQuery(P, paste0("INSERT INTO sl_parametry(parametr, opis) values ('", nazwaPar, "','kn(GRM) - patrz opis parametru k1')")  )
	          }
	          
	          insSkalowania = paste0("INSERT INTO skalowania_elementy  (id_skali, kolejnosc, skalowanie, parametr, model, wartosc, uwagi)
	                                 VALUES (", idSkali,
	                                 ",", iter, ",",
	                                 maxSkalowanieZap + 1,
	                                 " , nazwaPar ,", "'GRM'", ", ",
	                                 parThreshold$wartosc[m] / parBy$wartosc[kryteriaBy==kryt] - srednia,
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
	  }
  )
  invisible(NULL)
}