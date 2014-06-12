load(file="/home//g.golonka/R/Wyniki/skWynik2008")

numerSkalowania = 3 
wynikiSkalowania = skWynik2008$skalowania[[numerSkalowania]]$kalibracja1

pp = wynikiSkalowania$parametry$surowe
qqnorm(pp$wartosc[pp$typ=="threshold"])
qqnorm(pp$wartosc[pp$typ=="by"])

dd = wynikiSkalowania$zapis$gh
hist(dd,breaks= seq(min(dd)-0.01, max(dd)+0.01,length.out=1000))
plot(density(dd))










