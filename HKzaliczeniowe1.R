library(stringr)
library(HMM)

Sys.setlocale("LC_CTYPE", "polish") #konieczne ¿eby mi dzia³a³y polskie znaki

wczytaj_i_przygotuj <- function(nazwa_pliku='pan-tadeusz.txt', polskie = TRUE){
  plik <- readLines(nazwa_pliku) #wczytuje plik
  Encoding(plik) <- 'UTF-8' #zmieniam kodowanie na odpowiednie
  plik <- tryCatch(plik[1:which(plik == '-----')-1],error = function(e) plik, finally= {}) #usuwam wszystko po ----- (jeœli jest w pliku)
  plik <- gsub("[[:punct:]…—[:digit:]]", "", plik) #usuwam wszystkie znaki przestankowe
  plik <- tolower(plik[plik!=""]) #usuwam  puste
  if(polskie){
    cat('POLSKIE LITERY! ¹ê!')
  } else {
    plik <- iconv(plik,from="UTF-8",to="ASCII//TRANSLIT") #bez polskich
  }
  return(plik)
  #co z sytuacjami kiedy mamy "taby" - kilka spacji? usuwac czy zostawic?
}

daj_symbole <- function(obserwacja){
  cat('dlugosc obserwacji: ')
  cat(obserwacja)
  #ta funkcja ekstrachuje wszystkie unikalne elementy - czyli dla nas symbole
  symbole <- unique(obserwacja)
  return(symbole)
}

symuluj <- function(obserwacja,stany){
  #ta funkcja robi hmm i baumaWelcha (uczy siê)
  cat(length(obserwacja))
  symbole <- daj_symbole(obserwacja)
  macierz_emisji <- numeric(0)
  for(i in 1:length(stany)){
    x <- rep(1/length(symbole),length(symbole))
    x[i] <- x[i] - 1/100000000 #dajemy ma³e zaburzenie
    x[i+1] <- x[i+1] + 1/100000000 #tu te¿, ¿eby siê nadal sumowa³y w wierszu do 1
    macierz_emisji <- c(macierz_emisji,x)
  }
  hmm <- initHMM(stany,symbole,NULL,NULL,matrix(macierz_emisji,nrow = length(stany),ncol = length(symbole), byrow = TRUE))
  wynik <- baumWelch(hmm,obserwacja)
  post_prwdp <- posterior(wynik$hmm, obserwacja)
  plot(post_prwdp["pierwszy",], type = 'l')
  return(wynik) #co zwracaæ? oto jest pytanie
}

litery <- function(przygotowany_plik=wczytaj_i_przygotuj()){
  obserwacja <- unlist(strsplit(przygotowany_plik, ''))
  stany <- c('pierwszy','drugi')
  wynik <- symuluj(obserwacja,stany)
  return(wynik)
}

slowa <- function(przygotowany_plik=wczytaj_i_przygotuj()){
  #s³owa wymagaj¹ trochê wiêcej przygotowañ
  plik <- unlist(strsplit(przygotowany_plik," "))
  obserwacja <- str_sub(plik[plik!=""],start=-3)
  stany <- c('pierwszy','drugi','trzeci','czwarty','pi¹ty')
  wynik <- symuluj(obserwacja,stany)
  return(wynik)
}

testuj <- function(litery_nie_slowa = TRUE, nazwa_pliku = 'pan-tadeusz.txt', polskie = TRUE){
  if(litery_nie_slowa){
  wynik <- litery(wczytaj_i_przygotuj(nazwa_pliku, polskie))}
  else {
  wynik <- slowa(wczytaj_i_przygotuj(nazwa_pliku, polskie))}
  # Error in if (d < delta) { : missing value where TRUE/FALSE needed daje slowa?
  return(wynik)
}

#litery
#zrobic observation - literami, na ich podstawie wygenerowac wystepujace litery.
#sprawdzic co z polskimi znakami
#przygotowac 2 stany
#zrobic hmm
#dac bauma-welsha
#obejrzec
#pomyslec jak zwizualizowac

#s³owa
#jakims str_sub ze stringr albo ze stringi wyciagnac sufiksy 3 literowe (sprawdzic jak w przypadku krotszych slow!)
#juz mamy przygotowane observation w tym momencie
#przygotowac 5 stanow
#hmm
#baum-welsh
#obejrzec
#wizualizacja

#jak wytrenowanym zrobic nowy zbior - poszukac
#wybrac wiersze
#zasymulowac tym wyuczonym

#raport np w markdownie i do pdf/html

#litery
wyniki <- testuj() # z polskimi
grupy <- wyniki$hmm$emissionProbs["pierwszy",]-wyniki$hmm$emissionProbs["drugi",]
#jak ujemne to bardziej drugi
print('To jest grupa pierwsza:')
cat(names(grupy[grupy>=0]))
print('To jest grupa druga:')
cat(names(grupy[grupy<0]))
#--------------------
wyniki <- testuj(polskie = FALSE) #bez polskich
grupy <- wyniki$hmm$emissionProbs["pierwszy",]-wyniki$hmm$emissionProbs["drugi",]
#jak ujemne to bardziej drugi
print('To jest grupa pierwsza:')
cat(names(grupy[grupy>=0]))
print('To jest grupa druga:')
cat(names(grupy[grupy<0]))
#mozna jeszcze plotowac posteriori odjete i patrzec gdzie wiecej samoglosek
#podobnie z sufixami
#slowa
#wyniki <- testuj(litery_nie_slowa = FALSE,polskie=FALSE)
#head(wyniki$hmm$emissionProbs)
#pytania
#te dziwne znaki usuniete w notepadzie (pokazac screeny)
#jak z tym lekkim zaburzeniem
#czy jak nie dzieli w zadnym przypadku na samogloski/spolgloski to sie martwic czy to byla podpucha ze tak wyjdzie
#co z tym bledem ze slow