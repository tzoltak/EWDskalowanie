#' @title Obsluga ew. wystapienia duplikatow po skroceniu nazw zmiennych.
#' @description
#' Funkcja sprawdza, czy w pierwszym wektorze nie występują duplikaty. Jeśli występują, to podmienia je (zarówno pierwsze wystąpienie jak i powtórzenia) odpowiednimi wartościami z drugiego wektora.
#' @param x wektor
#' @param y wektor
#' @return wektor
czy_sie_nie_powtarza = function(x, y) {
	stopifnot(length(x) == length(y))
	if (any(duplicated(x)))	{
		maska = x %in% x[duplicated(x)]
		x[maska] = y[maska]
	}
	return(x)
}
#' @title Skracanie nazw zmiennych.
#' @description
#' Funkcja stara się względnie inteligentnie skrócić podane nazwy do zadanej długości, zachowując ich unikalność.
#' @param nazwy wektor tekstowy z nazwami do skrócenia
#' @param dl liczba całkowita maksymalna dopuszczalna długość nazwy
#' @details
#' Funkcja ma zaimplementowanych na sztywno kilka tłumaczeń-mapowań, następnie stara się usunąć "_" i "-", następnie usunąć samogłoski, ostatecznie nadaje nazwy "kolejne" bez związku z wcześniejszą nazwą.
#' @return wektor typu \code{character}
#' @export
skroc_nazwy_zmiennych = function(nazwy, dl) {
	stopifnot(is.character(nazwy), is.numeric(dl), length(dl) == 1)
	stopifnot(as.integer(dl) == dl, !any(duplicated(nazwy)))
	if (dl < 4) stop("Bez przesady z tym skracaniem!")

	nazwy = tolower(nazwy)
	# podejście pierwsze - sztywno zdefiniowane mapowania
	nazwy[nchar(nazwy) > dl] = sapply(nazwy[nchar(nazwy) > dl],
																		function(x) {
																			x = czy_sie_nie_powtarza(sub("obserwacji", "obs", x), x)
																			x = czy_sie_nie_powtarza(sub("uczni(a|ow|owski)", "uczn", x), x)
																			x = czy_sie_nie_powtarza(sub("szkol(y|a|ny|na)", "szk", x), x)
																			x = czy_sie_nie_powtarza(sub("spr(|awdzian)", "s", x), x)
																			x = czy_sie_nie_powtarza(sub("gimn(azjum|azjalny|azalna|azjalno)", "g", x), x)
																			x = czy_sie_nie_powtarza(sub("matur(a|y|alny|alna)", "m", x), x)
																			x = czy_sie_nie_powtarza(sub("egzamin", "e", x), x)
																			x = czy_sie_nie_powtarza(sub("dysl(eksja|eksji)", "ds", x), x)
																			x = czy_sie_nie_powtarza(sub("laur(eat|eatka)", "lr", x), x)
																			x = czy_sie_nie_powtarza(sub("arkusz", "ark", x), x)
																			x = czy_sie_nie_powtarza(sub("test(u)", "ts", x), x)
																			x = czy_sie_nie_powtarza(sub("kryt(erium|eria)", "k", x), x)
																			x = czy_sie_nie_powtarza(sub("pseudo(|kryt(erium|eria))", "p", x), x)
																			x = czy_sie_nie_powtarza(sub("pseudo(|kryt(erium|eria))", "p", x), x)
																			x = czy_sie_nie_powtarza(sub("sel(|ekcja)", "s", x), x)
																			x = czy_sie_nie_powtarza(sub("temat", "t", x), x)
																			x = czy_sie_nie_powtarza(sub("publiczn(a|y|e)", "pbl", x), x)
																			x = czy_sie_nie_powtarza(sub("dla(|_)doroslych", "ddr", x), x)
																			x = czy_sie_nie_powtarza(sub("specjaln(a|e)", "spc", x), x)
																			x = czy_sie_nie_powtarza(sub("przyszpitaln(a|e)", "pszp", x), x)
																			return(x)
																		}
	)
	# podejście drugie - usuwamy '_', '-' i '.'
	nazwy[nchar(nazwy) > dl] = czy_sie_nie_powtarza(
		gsub("[-._]", "", nazwy[nchar(nazwy) > dl]),
		nazwy[nchar(nazwy) > dl]
	)
	# podejście trzecie - usuwamy samogłoski
	nazwy[nchar(nazwy) > dl] = czy_sie_nie_powtarza(
		gsub("[eyuioa]", "", nazwy[nchar(nazwy) > dl]),
		nazwy[nchar(nazwy) > dl]
	)
	# podejście czwarte - zamieniamy na nic niemówiące nazwy
	for (i in letters[length(letters):1]) {
		if (!any(nchar(nazwy) > dl)) break
		nazwy[nchar(nazwy) > dl] = czy_sie_nie_powtarza(
			paste0(i, 1:sum(nchar(nazwy) > dl)),
			nazwy[nchar(nazwy) > dl]
		)
	}
	if (any(nchar(nazwy) > dl)) stop("Nie udało się skrócić nazw.")
	# zwracamy wynik
	return(nazwy)
}
#' @title Skracanie nazw zmiennych.
#' @description
#' Funkcja przechodzi przez elementy \code{zmienne}, \code{wartosciStartowe}, \code{wartosciZakotwoczone}, \code{ograniczeniWartosci} kroku procedury i zamienia w nich nazwy zmiennych (typowo na skrócone).
#' @param krok krok opisu procedury skalowania
#' @param nazwyDoZmiany lista, której elementami są zmienione (skrócone) nazwy zmiennych, a nazwami elementów listy są obecne nazwy zmiennych
#' @return wektor
zmien_nazwy_w_kroku_procedury = function(krok, nazwyDoZmiany) {
	stopifnot(is.list(krok), is.list(nazwyDoZmiany))

	for (i in 1:length(krok$czescPomiarowa)) {
		krok$czescPomiarowa[[i]]$zmienne = unlist(nazwyDoZmiany[krok$czescPomiarowa[[i]]$zmienne])
		krok$wieleGrup$zmienneGrupujace = unlist(nazwyDoZmiany[krok$wieleGrup$zmienneGrupujace])
		elementy = c("wartosciStartowe", "wartosciZakotwiczone", "ograniczeniaWartosci")
		for (j in elementy) {
      kolumny = c("zmienna1", "zmienna2")
      if (is.character(krok$czescPomiarowa[[i]][[j]]$wartosc)) kolumny = c(kolumny, "wartosc")
			for (k in kolumny) {
				maska = krok$czescPomiarowa[[i]][[j]][, k] %in% names(nazwyDoZmiany)
				krok$czescPomiarowa[[i]][[j]][maska, k] = unlist(nazwyDoZmiany[krok$czescPomiarowa[[i]][[j]][maska, k]])
			}
		}
	}
	return(krok)
}
