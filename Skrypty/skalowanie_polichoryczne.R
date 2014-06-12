options(encoding = "UTF-8")
library(ZPD)
library(foreign)
library(polycor)
library(RODBCext)
library(EWDskalowanie)
sink("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/skalowanie_polichoryczne.log", split=TRUE)

# ścieżka do danych
dane = read.csv2("/home/g.golonka/gimnazjum_dane/EG2008.csv")

names(dane)[names(dane)=="id_obserwacji"] = "id_obs"
daneKor = dane[,grepl("^(gh_)[[:digit:]]+", names(dane))]
kryteria = as.numeric(gsub("^[[:alnum:]]+_", "", names(daneKor)))

wiazki_pyt_kryt = pobierz_wiazki_pytania_kryteria(kryteria)
kor_wiazki = policz_korelacje_wiazki(daneKor, wiazki_pyt_kryt)
graniczna_miara = round(1 - 0.80^2, 1)
kor_wiazki_ok = kor_wiazki[ kor_wiazki$miara < graniczna_miara, ]

proceduraEG = procedura_eg_hum(names(dane), parametryGH = NULL, processors = 6)
skWynik2008 = skaluj_polichorycznie(dane, proceduraEG, kor_wiazki, "EG_2008_hum", ileKrokow = 7)
skWynik2008 = kolejny_krok_polich(skWynik2008)
save(skWynik2008, file="~//R/Wyniki/skWynik2008")
#

names(kolejny_krok_polich(kolejny_krok_polich(initializuj_skalowanie_polich(dane, proceduraEG, kor_wiazki, "EG_2008_hum")))$dane)

names(skaluj_polichorycznie(dane, proceduraEG, kor_wiazki, "EG_2008_hum", ileKrokow = 5)$dane)

names(skWynik2008$dane)

cat("Finito!!! \n")
