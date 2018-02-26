;---------------------------------------------------------------------------------------------------------------------------------------
;|															Czesc deklaracyjna														   |
;---------------------------------------------------------------------------------------------------------------------------------------

data1 segment
	ArgTab		db	128 dup(24h)	;	Wypełnienie tablicy argumentow pobranych z lini polecen samymi znakami "$" -  w skutek czego  
									;	nie trzeba juz dodawać na koncu ciagu tego znaku. Maksymalnie przyjmuje 127 znakow + dolar 
									;	terminujacy - tyle przyjmuje maksymalnie od adresu PSP:81h do PSP:FFh
	ArgStrLen 	db	0				;	Dlugosc ciagu znakow przekazanego do wiersza polecenia jako argument
	ArgNumber	db 	0				;	Ilosc argumentow
	ArgsLen		db	64 dup(0)		;	Tablica (maksymalnie 64 argumenty, bo gdyby przekazac arg1 arg2 ... to liczba argumentow 
									;	maksymalnie wyniesie 64 - kazdy argument po 1 znak (1B ASCII) + odstep, np spacja 20h 
									;	ASCII - 1B) - okreslenie przesuniecia wzgledem poczatku tablicy
	ArgOffsets	dw 	64 dup(offset ArgTab)	;	Tablica offsetow argumentów - dla bezpieczenstwa - ustawiam offsety na istniejacy poczatek 1. argumentu
	ExtArgTab:
		xmin		dt	-1.5
		xmax		dt	1.5
		ymin		dt	-1.5
		ymax		dt	1.5
		cr			dt	-0.16
		ci			dt	0.667
	endl		db	10,13,"$"
	errorMsgOff	dw 	offset statusOK
	statusOK	db 	"Status: [OK!]$"
	WrNumArgs	db 	"Status: [ERROR] Podano nieodpowiednia ilosc argumentow argumentow!",10,13,"Format polecenia JULIA.EXE xmin xmax ymin ymax cr ci$"
	ErrConv		db	"Status: [ERROR] Podano argument w formacie nieodpowiednim do konwersji na format extended!$"
	nonExiArgEx db 	"Status: [ERROR] Odwolanie do nieistniejacego argumentu!$"
	badFPForm	db	"Status: [ERROR] Zly format liczby zmiennopozycyjnej!$"
	DEX			dq 	10.0 ; podstawa systemu 10
	NegOne		dq	-1.0
	Four 		dt 	4.0
	FPUnitWordBuffer dw ?
	NegativeNumber	db	0
	XRes		dw 	320
	YRes		dw 	200

data1 ends

;---------------------------------------------------------------------------------------------------------------------------------------
;|													   Czesc deklaracyjna - KONIEC													   |
;---------------------------------------------------------------------------------------------------------------------------------------


;---------------------------------------------------------------------------------------------------------------------------------------
;|																SEGMENT KODU														   |
;---------------------------------------------------------------------------------------------------------------------------------------

code1 segment
	assume CS:code1, DS:data1, SS:stack1 		;	INFORMUJE kompilator do ktorego rej. ma sie odwolywac gdy napotka podana etykiete
	.286										;	Dla ułatwienia - pusha/popa dzieki czemu jedno polecenie odklada/zdejmuje wszystkie 
												;	rejestry na/ze stos/-u
	
	start:

			;	Inicjalizacja stosu
			mov AX, seg stack1
			mov SS, AX
			mov SP, offset top
			;	Koniec inicjalizacji

			; Inicjalizacja DS, aby wskazywal na segment data1
			mov AX, seg data1
			mov DS, AX
			

			;--------------------------------------------------------------------------------
			;	Punkt wejscia
			;--------------------------------------------------------------------------------
			
			main:

			finit
			call ParseArguments ; Funkcja dokonuje parsowania argumentow
			call ArgsIntoExtPrecision ; Funkcja wykonuje zamiane ciagu znakow podanego przez uzytkownika na liczby Extended Precision z wykryciem bledow konwersji
			
			call DrawSet ; Funkcja rysuje wnetrze zbioru Julii

			Exit:
				call Terminate
		
			;--------------------------------------------------------------------------------
			;	KONIEC WYKONANIA
			;--------------------------------------------------------------------------------



	;----------------------------------------------------------------------------------------------------------------
	;
	;	Procedura ParseArguments
	;	Wykonuje parsowanie argumentow
	; 	Sparsowane argumenty umieszcza w tablicy ArgTab, ArgsLen, ArgOffsets
	;	IN: 	none
	;	OUT: 	none
	;	DESC:	Procedura wlasciwa sparsowania argumentow. Najpierw zabezpieczane sa wartosci rejestrow. Nastepnie 
	;			wywolywana jest funkcja 51h przerwania 21h, ktora powoduje zaladowanie do BX segmentu PSP. wartosc
	;			jest kopiowana do rejestru segmentowego ES. Nastepnie pobierana jest do AL dlugosc przekazanych
	;			(wraz z poczatkowa spacja) z adresu PSP:[80h]. Jezeli nie podano zadnych argumentow - Wywolanie
	;			procedury Terminate. Nastepnie ustawiana jest wlasciwa dlugosc parametrow w pamieci (ArgStrLen)
	;			i ustawiony zostaje licznik petli na ta dlugosc. Rejestr indeksowy SI bedzie odpowiadal za wskazanie
	;			kolejnego znaku PSP:SI od wartosci SI 82h - pominiecie spacji. DI - wskaze miejsce do kopiowania
	;			DS:SI, gdzie DS to tak naprawde segment ArgTab. Przy okazji DS:BX bedzie wskazywac kolejne komorki
	;			tablicy ArgsLen - gdzie beda dlugosci poszczegolnych argumentow. W petli reading przechodzimy po
	;			wzsystkich znakach - wywolujac na kazdym procedure HandleCharacter. Po zakonzeniu petli musimy
	; 			wstawic znak terminujacy $ na koncu ostatniego argumentu i okreslic prawidlowo jego dlugosc.
	; 
	;----------------------------------------------------------------------------------------------------------------


	ParseArguments proc
		
		pusha
		mov AH, 51h
		int 21h						;	Wywolanie przerwania celem ustalenia PSP w BX
		mov AX, BX
		mov ES, AX 					;	Unikam przekazania nie z AX do rejestru segmentowego	
		mov AL, byte ptr ES:[80h]	;	DO AL ilosc znakow przekazanych w argumentach 
		cmp AL, 1 					;	Sprawdzam czy ze spacja jest <= 1 znak - jesli tak to nie ma argumentow
		jbe NoArgs					;	Brak argumentow
		; else 
		dec AL 						;	Pomijanie spacji ES:[81h] -> 1B 
		mov ArgStrLen, AL 			;	Do zmiennej dlugosc argumentow	 

		xor CX, CX
		mov CL, AL					;	Ustawienie licznika petli
		
		mov SI, 82h					;	Poczatek ciagu argumentow - pomijam spacje ES:[81h] -> 1B 
		mov DI, offset ArgTab		;	DI bedzie adresowac przesuniecia offsetu tavlicy
		xor AX, AX 					;	Przyjmuje nastepujaca konwencje AL -> aktualny znak
									;	AH -> ilosc znakow argumentu (zaakceptowanych)
		mov BX, offset ArgsLen
		mov DX, offset ArgOffsets	
		reading:					;	Petla wczytujaca kolejne znaki
			mov AL, ES:[SI]
			call HandleCharacter
			inc SI
		loop reading
		
		mov [DI], byte ptr "$" 				; terminacja argumentu
		inc AH 						; 	Zwiekszenie dlugosci
		mov [BX], AH 				;	Przypisanie dlugosci do zakanczanego argumentu

		finSt:
			popa
			ret

		NoArgs:
			mov ArgNumber, 0d
			jmp finSt

	ParseArguments endp

	;-------------------------------------------------------------------------------------------------------------
	;	Koniec procedury ParseArguments
	;-------------------------------------------------------------------------------------------------------------

	
	;-------------------------------------------------------------------------------------------------------------
	;
	;	Procedura Terminate
	;	Konczy program
	; 	IN: none
	;	OUT: none
	;
	;-------------------------------------------------------------------------------------------------------------


	Terminate proc

		mov AX, seg errorMsgOff ; Wypisanie statusu wykonania
		mov DS, AX
		mov DX, errorMsgOff
		mov AH, 9
		int 21h

		retToOS:
			mov AH, 4CH
			int 21h
		
	Terminate endp

	;-------------------------------------------------------------------------------------------------------------
	;	Koniec procedury Terminate
	;-------------------------------------------------------------------------------------------------------------


	;-------------------------------------------------------------------------------------------------------------
	;
	;	Procedura HandleCharacter(PRIVATE)
	;	Konczy program
	; 	BIDIRECT: 	AL -> znak, AH -> ilosc znakow argumentu (ktore juz posiada)
	;				DI -> offset do zapisania kolejnego znaku
	;				SI -> Czytany offset
	;				BX -> przesuniecie wzgledem tablicy ArgsLen
	;				DX -> przesuniecie wzgledem tablicy ArgOffsets
	;	OUT: 		modyfikacja podanych wyzej rejestow
	;	DESC:		Procedura sprawdza najpierw czy AL (znak) ma kod ASCII znaku CR, lub LF. Wowczas przechodzi 
	;				do etykiety CRLEEncounter.
	;				CRLEEncounter:
	;					Jesli ilosc znakow w przetwarzanym argumencie jest rozna niz zero - nalezy zakonczyc 
	;					ten argument - skok do CloseArgument
	;					W przwciwnym razie skok do HandleCharacterFinalizer - wyjscie z procedury
	;				Jezeli nie bylo skoku to sprawdzane jest czy znak to inny znak niedrukowalny 
	;				(kod ASCII <= 20h). Jezei tak to skok do OtherWhiteCharEncounter
	;				OtherWhiteCharEncounter:
	;					Jesli ilosc znakow w przetwarzanym argumencie jest rozna niz zero - wystepuje bialy znak
	;					po argumencie i tzrreba skoczyc do CloseArgument, w przeciwnym razie wychodzimy - nic nie
	;					robiac (w glownej petli oczywiscie zmienia sie SI - ktore wskazuje na znak do badania)
	;				Jeżeli to jednak nie byl bialy znak sprawdzane jest czy jest to pierwszy czy kolejny znak
	;				argumentu. Pierwszy -> skok do OpenNewArg
	;				OpenNewArg:
	;					Zwiekszenie liczby argumentow i skok do AddCharToArg
	;				AddCharToArg:
	;					Obsluga dodawania znaku do listy argumentow. Do DS:[DI] - kolejny bajt w tablicy ArgTab
	;					dopisywany jest dany znak. DI jest przestawiany na kolejny bajt, zwiekszana jest liczba
	;					znakow argumentu. Potem nastepuje wyjscie.
	;				CloseArgument:
	;					Zwiekszenie AH ($ na koncu tez zajmuje miejsce) i wpisanie liczby do DS:[BX] - czyli 
	;					kolejnej komorki ArgsLen. BX jest przestawiany o bajt do przpodu, AH jest zerowane,
	;					do DS:[DI] wpisany jest znak terminujacy argument ($), przestawiany jest [DI] na kolejny
	;					bajt i wychodzimy.
	;				HandleCharacterFinalizer:
	;					Powrot do caller'a.   
	;
	;-------------------------------------------------------------------------------------------------------------


	HandleCharacter proc
		
		; Nie modyfikuje rejestrow nieswiadmie - nie odkladam nic na stos

		
		cmp AL, 0Dh
		je CRLEEncounter 						; Napotkano CR
		cmp AL, 0Ah
		je CRLEEncounter						; Napotkano LF
		cmp AL, 20h
		jbe OtherWhiteCharEncounter 			;	AL <= 20h - napotkano bialy znak (inny niz CR LF)
		;	czyli jeednak napotkano znak drukowany
		
		cmp AH, 0d 								;	Znaleziono znak otwierajacy nowy argument
		je OpenNewArg
		jmp AddCharToArg						; 	else - Dodanie znaku do istniejaceg argumentu

		CRLEEncounter:
			cmp AH, 0d
			jne CloseArgument					;	AH != 0 -> bialy znak PO argumencie
			jmp HandleCharacterFinalizer
		
		OtherWhiteCharEncounter:
			cmp AH, 0d
			jne CloseArgument					;	AH != 0 -> bialy znak PO argumencie
			jmp HandleCharacterFinalizer

		OpenNewArg:
			inc ArgNumber
			push SI
			mov SI, DX
			mov [SI], word ptr DI
			pop SI
			jmp AddCharToArg

		AddCharToArg:
			mov [DI], AL
			inc DI
			inc AH
			jmp HandleCharacterFinalizer

		CloseArgument:
			inc AH
			mov [BX], AH 						;	Przypisanie dlugosci do zakanczanego argumentu
			inc BX
			xor AH, AH 							; 	Zerowanie ilosci znakow argumentu
			mov [DI], byte ptr "$"				;	Wpisanie $ na koncu argumentu
			inc DI	
			inc DX								; Zwiększenie offsetu tablicy ArgOffsets o 2 - bo przechowuje word'y
			inc DX
			jmp HandleCharacterFinalizer

		HandleCharacterFinalizer:
			ret
		
	HandleCharacter endp

	;-------------------------------------------------------------------------------------------------------------
	;	Koniec procedury HandleCharacter
	;-------------------------------------------------------------------------------------------------------------


	;-------------------------------------------------------------------------------------------------------------
	;	
	;	Procedura GetArg
	;	Wypisuje argumenty
	; 	IN: 	REGISTERS: 	AL -> numer argumentu do pobrania
	;			MEMORY:		ArgNumber, ArgTab, ArgsLen
	;	OUT: 	AX -> offset argumentu
	;	DESC:	Procedura zwraca w AX offset odpowiedniego argumentu. Najpierw zabezpieczana jest wartosc 
	;			rejestrow, nastepnie sprawdzamy, czy nie odwolujemy sie do argumentu poza zakresem - jesli tak
	;			-> wywolanie obslugi bledu. nastepnie CL jako licznik jest ustawiany na o jeden mniej niz numer
	;			argumentu. Do bazwoego offsetu dodajemy dlugosci wszystkich poprzedzajacych go argumentow w petli 
	;			while (etykieta testif)
	;			
	;-------------------------------------------------------------------------------------------------------------


	GetArg proc
		
		push CX						;	Odlozenie na stos rejestrow ktore beda wykorzystane
		push BX
		push DX
		push SI

		XOR CX, CX					;	Wyzerowanie CX

		cmp AL, ArgNumber			; 	Jesli proba odwolania sie do nieistniej. argumentu - wyjatek
		ja ErrHandling 				;	AL > ArgNumber
		; else
		dec AL
		sal AL, 1 	; Mnozenie razy 2 - najstarszy bit zostawiany jak jest
		
		mov SI, offset ArgOffsets ; W tym momencie wykonanie procedury to O(1)
		xor AH, AH
		add SI, AX
		mov AX, [SI]

		pop SI
		pop DX 						;	Przywrocenie wartosci rejestrow ze stosu
		pop BX
		pop CX 						
		
		ret
		
		;	Obsluga bledu
		ErrHandling:
			pop DX 						;	Przywrocenie wartosci rejestrow ze stosu
			pop BX
			pop CX 	
			push AX
			mov AX, offset nonExiArgEx
			mov errorMsgOff, AX
			pop AX
			call Terminate		

	GetArg endp

	;-------------------------------------------------------------------------------------------------------------
	;	Koniec procedury GetArg
	;-------------------------------------------------------------------------------------------------------------


	;-------------------------------------------------------------------------------------------------------------
	;	
	;	Procedura ArgsIntoExtPrecision
	;			
	;-------------------------------------------------------------------------------------------------------------

	ArgsIntoExtPrecision proc
		pushf
		pusha
		; Prawdzanie ilosci argumentow
		cmp ArgNumber, 6d
		je Conversion
		mov AX, offset WrNumArgs
		mov errorMsgOff, AX
		popa
		popf
		call Terminate

		Conversion:
		xor CX, CX ; przygotowanie licznika
		ConvertLoop:
			cmp CL, ArgNumber ; while(CL < ArgNumber)
			je FinProc
			mov AL, CL ; konwencja wywolania GetArg -> w AL numer argumentu (numeracja od 1)
			inc AL ; skoro numeracja zaczyna sie od 1!
			call GetArg ; teraz w AX jest offset do argumentu, a konwencja wlania StringToExt wymaga aby ten offset byl w SI
			mov SI, AX ; teraz offset jest w SI
			call StringToExt ; procedura wpisze do tablicy ExtArgTab pod odpowiedni index liczbe skonwertowana na format extended -> jesli blad zwroci AL = 0
			cmp AL, 0d
			je ErrorConversion ; wykryto blad konwersji
			inc CL ; zwiekszenie licznika
			;FCLEX
		jmp ConvertLoop
		
		ErrorConversion:
			mov AX, offset ErrConv
			mov errorMsgOff, AX
			popa
			popf
			call Terminate

		FinProc:
			popa
			popf
			ret

	ArgsIntoExtPrecision endp

	;-------------------------------------------------------------------------------------------------------------
	;	Koniec procedury ArgsIntoExtPrecision
	;-------------------------------------------------------------------------------------------------------------

	;-------------------------------------------------------------------------------------------------------------
	;	
	;	Procedura StringToExt
	;	IN:		SI -> offset argumentu
	;			CL -> numer argumentu (zaczyna sie od 0)
	;			ArgsLen -> dlugosc argumentow -> ilosc zawsze zwiekszona o 1 ze wzgledu na znak terminacji $
	;	OUT:	AL -> jako flaga bledu: 1 OK, 0 BLAD
	;			
	;-------------------------------------------------------------------------------------------------------------

	StringToExt proc
		

		; 1. krok -> sprawdzenie -/+/brak znaku (domyslnie plus)
		; 2. krok -> zmiana czesci calkowitej na BIN -> wykrycie nieodpowiedniego znaku : blad
		; 3. krok -> zmiana czesci ulamkowej na BIN -> wykrycie nieodpowiednigo znaku : blad
		; 4. krok -> zapisanie do pamieci liczby extended
		push CX ; zabezpieczenie CX
		push DI ; zabezpieczenie DI
		mov DI, offset ArgsLen
		add DI, CX
		xor CX, CX
		mov CL, [DI] ; teraz w CL bedzie dlugosc argumentu (kontrola zeby nie wyjsc z konwersja poza jeden argument, jesli nie ma kropki) UWAGA: numeracja od 1
		pop DI ; powrot DI na swoje miejsce

		;	USTAWIENIE POCZATKOWYCH WARTOSCI KOPROCESORA
		fld DEX ; st(0) = 10.0
		; 10.0 |
		fldz ; ustawia stala 0 w st, a wiec teraz st(0) -> 0.0, st(1) -> 10.0
		; 0.0 | 10.0 |

		xor AX, AX
		mov AL, DS:[SI] ; DO AL laduje znak do przetworzenia
		cmp AL, "+"
		je OmmitSign
		cmp AL, "-"
		jne ReadIntegerPartLoop
		mov NegativeNumber, 1d ; flaga okreslajaca ze liczba bedzie ujemna
		OmmitSign:
			inc SI ; celem ominiecia znaku +/- przesuniecie o 1
			dec CL ; jeden znak do przetworzenia mniej

		ReadIntegerPartLoop:
			
			cmp CL, 1d ; porownanie z jedynka bo dlugosc zawsze powiekszona o 1 ze wzgledu na znak terminujacy string $
			je DecimalSeparatorDetected ; np liczba +10286 bedzie poprawna, wiec nie ide od razu do bledu (dziesietne rozwiniecie bedzie z automatu 0)
			xor AX, AX
			mov AL, DS:[SI] ; do AL (w sumie jest to AH: 00000000 AL: kod znaku do 0xff -> czyly mamy kod znaku skonwertowany na 16b tak jak koprocesor potrzebuje)
			cmp AL, "." ; sprwadzam czy nie napotkalem kropki
			je DecimalSeparatorDetected

			; CZESC SPRAWDZAJACA POPRAWNOSC LICZBY
			cmp AL, "0"
			jb error
			cmp AL, "9"
			ja error ; jesli w przebiegu programu wpadniemy w ktorys z warunkow oznacza to, ze podana liczba nie zgadza sie z formatem +11.2982
			 
			sub AX, word ptr "0" ; wowczas w DS:[SI] wartosc cyfry 0-9, teraz jestesmy gotowi do dzialania na koprocesorze
			fmul st(0), st(1) ; pomnoży st(0), gdzie jest wynik poprzedniego sumowania z st(1) gdzie zawsze bedzie 10.0 w tej petli
			; ost_wynik * 10.0 | 10.0 |
			mov FPUnitWordBuffer, AX
			fiadd word ptr FPUnitWordBuffer ; st(0) = st(0) + zawartosc AX, w sumie st(0) = stare st(0) * 10.0 + AX (gdzie byla wartosc cyfry)
			; ost_wynik * 10.0 + AX | 10.0 |
			inc SI ; przejscie na kolejny znak do przetwarzania
			dec CL

		jmp ReadIntegerPartLoop

		DecimalSeparatorDetected: 	; 	w tym momencie CL wyznacza ile jeszcze zostalo cyfr do przetworzenia 
									;	st(0) -> czesc dziesietna st(1) -> 10.0
			fldz		; 	Teraz st(0) -> 0.0 (tu bedzie wartosc rozwiniecia dziesietnego), st(1) -> wartosc dziesietna, st(2) -> 10.0
			; 0.0 | integer_part | 10.0 |

			cmp AL, "." ; sprawdzenie czy to napotkanie kropki sprawilo ze wyszlismy z petli ReadIntegerPartLoop
			jne finalize
			inc SI ; pominiecie . jesli wystapila
			dec CL
			add SI, CX ; ustawiam SI na zaostatni ciagu znakow
			dec SI ; ustawiam SI na ostatnia cyfre do przetworzenia -> teraz wskazuje na $
			dec SI ; teraz SI pokazuje na cyfre do wczytania
		
		ReadDecimalPartLoop:

			cmp CL, 1d
			je finalize
			
			xor AX, AX
			mov AL, DS:[SI]
			; sprawdzanie poprawnosci liczby -> t\na tym etapie poprawna liczba do konca powinna miec juz cyfry 0-9
			cmp AL, "0"
			jb error
			cmp AL, "9"
			ja error ; jesli w przebiegu programu wpadniemy w ktorys z warunkow oznacza to, ze podana liczba nie zgadza sie z formatem +11.2982

			sub AX, word ptr "0" ; wowczas w DS:[SI] wartosc cyfry 0-9, teraz jestesmy gotowi do dzialania na koprocesorze
			mov FPUnitWordBuffer, AX
			fiadd word ptr FPUnitWordBuffer
			; stare_rozw_dziesietne + AX | integer_part | 10.0 |
			fdiv st(0), st(2)
			; (stare_rozw_dziesietne + AX) / 10 | integer_part | 10.0 |
			dec SI
			dec CL

		jmp ReadDecimalPartLoop

		; decimal_part | integer_part | 10.0 |

		finalize: ; w tym momencie st(0) -> decimal part, st(1) -> integer part, st(2) -> 10.0
			pop CX
			faddp st(1), st(0) ; zdejmie stary wierzcholek a w st(0) bedzie integral+decimal, st(1) -> 10.0
			; integer_part + decimal_part | 10.0 |
			cmp NegativeNumber, 1d
			jne cleanFPU
			;wymnoz razy -1!!
			;fld NegOne ; st(0) -> -1, st(1) wartosc bezwzgledna liczby, st(2) -> 10.0
			;fmulp st(1), st(0) ; pomnozy wynik razy -1 i zdejmie -1 ze stosu: st(0) -> liczba, st(1) -> 10.0
			fchs ; mnozy st(0) * (-1)
			; -1 * (integer_part + decimal_part) | 10.0 |


			cleanFPU:
				; zapis do pamieci liczby:
				.486
				push DI
				mov DI, offset ExtArgTab
				mov AX, CX
				shl AX, 3
				shl CX, 1
				add AX, CX
				shr CX, 1
				add DI, AX
				fstp tbyte ptr DS:[DI] ; wynik odkladany w odpowiednie miejsce st(0) -> 10.0
				pop DI
				; 10.0 |
				; Niestety sie nie da :( xor [ExtArgTab+ CX], 80000000000000000000h
				ffree st(0) ; zwilnienie 10.0 -> zeby nie zostaly smieci
				; FREE |
				fincstp ; przesowa wszystko o 1 do gory na stosie i ustawia odpowiednio wskaznik na top
				mov NegativeNumber, 0d ; zerwowanie flagi negatywnego wyniku
				mov AL, 1d
				ret
		error:
			pop CX
			mov AL, 0d
			ret

	StringToExt endp

	;-------------------------------------------------------------------------------------------------------------
	;	Koniec procedury StringToExt
	;-------------------------------------------------------------------------------------------------------------



	;-------------------------------------------------------------------------------------------------------------
	;	
	;	Procedura DrawSet
	;			
	;-------------------------------------------------------------------------------------------------------------

	DrawSet proc
		
		pusha
		
		mov AH, 0d
		mov AL, 13h ; tryb 13h 320px*200px*256col
		int 10h

		; Konwencja wyswietlania grafiki -> zapis do odpowiednieg miejsca pamieci: segment 0A000h (umieszczam segment w ES)
		mov AX, 0A000h ; unikam bezposredniego niedozwolonego podstawienia
		mov ES, AX
		; Przygotowanie do szybkiego wstawiania koloru bialy 0xff i czarny 0x00 odpowiednio w BH i BL
		xor BX, BX
		mov BH, 00Fh
		; Przygotowanie petli: AX bedzie licznikiem wierszy (y), CX - licznikiem kolumn (x)
		xor AX, AX
		fld tbyte ptr Four ; st(0) -> 4.0
		; 4.0 |
		fld tbyte ptr cr
		; CR | 4.0 |
		fld tbyte ptr ci
		; CI | CR | 4.0 |

		outerLoop:
			cmp AX, YRes
			je AfterOuterLoop
			xor CX, CX

			innerLoop:
				; do innerLoop wchodze st(0) -> CI, st(1) -> CR, st(2) -> 4.0
				; CI | CR | 4.0 |
				cmp CX, XRes
				je AfterInnerLoop

				push CX
				mov FPUnitWordBuffer, CX ; zapamietuje sobie CX aka N w buforze
				
				; inicjalizacja zr, zi, w tym momencie koprocesor powinien byc wyczyszczony ze wszystkich smieci, a w st(0) -> CI, st(1) -> CR, st(2) -> 4.0
				;fld tbyte ptr ExtArgTab ; st(0) -> XMIN, st(1) -> CI, st(2) -> CR, st(3) -> 4.0
				fld tbyte ptr xmin
				; XMIN | CI | CR | 4.0 |
				fld tbyte ptr xmax
				; XMAX | XMIN | CI | CR | 4.0 |
				fsub st(0), st(1) ; st(0) -> XMAX - XMIN, st(1) -> XMIN, st(2) -> CI, st(3) -> CR, st(4) -> 4.0
				; XMAX - XMIN | XMIN | CI | CR | 4.0 |
				fild word ptr XRes ; st(0) -> XRes, st(1) -> XMAX - XMIN, st(2) -> XMIN, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
				; XRes | XMAX - XMIN | XMIN | CI | CR | 4.0 |
				fdivp st(1), st(0) ; st(0) -> (XMAX- XMIN) / XRes, st(1) -> XMIN, st(2) -> CI, st(3) -> CR, st(4) -> 4.0
				; XRes | (XMAX - XMIN) / Xres | XMIN | CI | CR | 4.0 |
				; (XMAX - XMIN) / Xres | XMIN | CI | CR | 4.0 |
				fild word ptr FPUnitWordBuffer ; st(0) -> N, st(1) -> (XMAX- XMIN) / XRes, st(2) -> XMIN, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
				; N | (XMAX - XMIN) / Xres | XMIN | CI | CR | 4.0 |
				fmulp st(1), st(0) ; st(0) -> N * (XMAX- XMIN) / XRes, st(1) -> XMIN, st(2) -> CI, st(3) -> CR, st(4) -> 4.0
				; N | N * (XMAX - XMIN) / Xres | XMIN | CI | CR | 4.0 |
				; N * (XMAX - XMIN) / Xres | XMIN | CI | CR | 4.0 |
				faddp st(1), st(0) ; st(0) -> XMIN + N * (XMAX- XMIN) / XRes, st(1) -> CI, st(2) -> CR, st(3) -> 4.0
				; N * (XMAX - XMIN) / Xres | XMIN + N * (XMAX - XMIN) / Xres | CI | CR | 4.0 |
				; zr = XMIN + N * (XMAX - XMIN) / Xres | CI | CR | 4.0 |
				mov FPUnitWordBuffer, AX ; zapamietuje AX aka M w buforze

				fld tbyte ptr ymin
				; ymin | zr | CI | CR | 4.0 |
				fld tbyte ptr ymax
				; ymax | ymin | zr | CI | CR | 4.0 |
				fsub st(0), st(1) ;  st(0) -> YMAX - YMIN, st(1) -> YMIN, st(2) -> XMIN + N * (XMAX- XMIN) / XRes, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
				; ymax - ymin | ymin | zr | CI | CR | 4.0 |
				fild word ptr YRes ; st(0) -> YRes, st(1) -> YMAX - YMIN, st(2) -> YMIN, st(3) -> XMIN + N * (XMAX- XMIN) / XRes, st(4) -> CI, st(5) -> CR, st(6) -> 4.0
				; YRes | ymax - ymin | ymin | zr | CI | CR | 4.0 |
				fdivp st(1), st(0) ; st(0) -> (YMAX - YMIN) / YRes, st(1) -> YMIN, st(2) -> XMIN + N * (XMAX- XMIN) / XRes, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
				; YRes | (ymax - ymin) / YRes | ymin | zr | CI | CR | 4.0 |
				; (ymax - ymin) / YRes | ymin | zr | CI | CR | 4.0 |
				fild word ptr FPUnitWordBuffer ; st(0) -> M, st(1) -> (YMAX - YMIN) / YRes, st(2) -> YMIN, st(3) -> XMIN + N * (XMAX- XMIN) / XRes, st(4) -> CI, st(5) -> CR, st(6) -> 4.0
				; M | (ymax - ymin) / YRes | ymin | zr | CI | CR | 4.0 |
				fmulp st(1), st(0) ; st(0) -> M * (YMAX - YMIN) / YRes, st(1) -> YMIN, st(2) -> XMIN + N * (XMAX- XMIN) / XRes, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
				; M | M * (ymax - ymin) / YRes | ymin | zr | CI | CR | 4.0 |	
				; M * (ymax - ymin) / YRes | ymin | zr | CI | CR | 4.0 |			
				faddp st(1), st(0) ; st(0) -> YMIN +  M * (YMAX - YMIN) / YRes, st(1) -> XMIN + N * (XMAX- XMIN) / XRes, st(2) -> CI, st(3) -> CR, st(4) -> 4.0
				; M * (ymax - ymin) / YRes | ymin + M * (ymax - ymin) / YRes | zr | CI | CR | 4.0 |
				; ymin + M * (ymax - ymin) / YRes = zi| zr | CI | CR | 4.0 |
				; teraz st(0) -> Y (zi), st(1) -> X (zr), st(2) -> CI, st(3) -> CR, st(4) -> 4.0
				

				xor CX, CX
				DrawingLoop: ; X | Y | CI | CR | 4.0 |
					cmp CX, 1000d
					je AfterDrawingLoop
					; inicjalizacja tmp
					fld st(1) ; st(0) -> zr aka X, st(1) -> zi , a potem Y, st(2) -> zr, a potem X, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
					; X | Y | X | CI | CR | 4.0
					fmul st(0), st(2) ; st(0) -> zr * zr = X^2, st(1) -> zi = Y, st(2) -> zr = X, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
					; X^2 | Y | X | CI | CR | 4.0
					fld st(1) ; st(0) -> zi aka Y, st(1) -> zr * zr = X * X, st(2) -> zi = Y, st(3) -> zr = X, st(4) -> CI, st(5) -> CR, st(6) -> 4.0 
					; Y | X^2 | Y | X | CI | CR | 4.0
					fmul st(0), st(2) ; st(0) -> zi * zi = Y^2, st(1) -> zr * zr = X * X, st(2) -> zi = Y, st(3) -> zr = X, st(4) -> CI, st(5) -> CR, st(6) -> 4.0 
					; Y^2 | X^2 | Y | X | CI | CR | 4.0
					fsubp st(1), st(0) ; st(0) -> zr * zr - zi * zi = X * X - Y * Y, st(1) -> zi, st(2) -> zr, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
					; Y^2 | X^2 - Y^2 | Y | X | CI | CR | 4.0
					; X^2 - Y^2 | Y | X | CI | CR | 4.0
					fadd st(0), st(4) ; st(0) -> zr * zr - zi * zi + CR (= TMP), st(1) -> zi, st(2) -> zr, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
					; X^2 - Y^2 + CR | Y | X | CI | CR | 4.0
					fxch st(1) ; st(0) -> zi, st(1) -> zr * zr - zi * zi + CR (= TMP), st(2) -> zr, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
					; Y | X^2 - Y^2 + CR | X | CI | CR | 4.0
					fmul st(0), st(2) ; st(0) -> zi * zr, st(1) -> zr * zr - zi * zi + CR (= TMP), st(2) -> zr, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
					; Y*X | X^2 - Y^2 + CR | X | CI | CR | 4.0
					fadd st(0), st(0) ; st(0) -> 2 * zi * zr, st(1) -> zr * zr - zi * zi + CR (= TMP), st(2) -> zr, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
					; 2*Y*X | X^2 - Y^2 + CR | X | CI | CR | 4.0
					fadd st(0), st(3) ; st(0) -> 2 * zi * zr + CI, st(1) -> zr * zr - zi * zi + CR (= TMP), st(2) -> zr, st(3) -> CI, st(4) -> CR, st(5) -> 4.0
					; 2*Y*X + CI | X^2 - Y^2 + CR | X | CI | CR | 4.0
					fxch st(2) ; st(0) -> zr (= stary X), st(1) -> zr * zr - zi * zi + CR (= TMP), st(2) -> 2 * zi * zr + CI (= Y), st(3) -> CI, st(4) -> CR, st(5) -> 4.0
					; X | X^2 - Y^2 + CR | 2*Y*X + CI | CI | CR | 4.0
					; kasuje ze stosu starego x i jako nowe x traktuje tmp
					ffree st(0)
					; EMPTY | X^2 - Y^2 + CR | 2*Y*X + CI | CI | CR | 4.0
					fincstp ; wszystkie rejestry o jeden do gory st(0) -> zr * zr - zi * zi + CR (= X), st(1) -> 2 * zi * zr + CI (= Y), st(2) -> CI, st(3) -> CR, st(4) -> 4.0
					; X^2 - Y^2 + CR | 2*Y*X + CI | CI | CR | 4.0
					
					;; W tej czesci kodu sprawdzamy czy X * X + Y * Y  > 4.0

					fld st(0) ; st(0) -> X, st(1) -> zr * zr - zi * zi + CR (= X), st(1) -> 2 * zi * zr + CI (= Y), st(2) -> CI, st(3) -> CR, st(4) -> 4.0
					; X | X^2 - Y^2 + CR = X | 2*Y*X + CI = Y| CI | CR | 4.0
					fmul st(0), st(1) ; st(0) -> X^2, st(1) -> zr * zr - zi * zi + CR (= X), st(2) -> 2 * zi * zr + CI (= Y), st(3) -> CI, st(4) -> CR, st(5) -> 4.0
					; X^2 | X | Y | CI | CR | 4.0
					fld st(2); st(0) -> Y, st(1) -> X * X, st(2) -> zr * zr - zi * zi + CR (= X), st(3) -> 2 * zi * zr + CI (= Y), st(4) -> CI, st(5) -> CR, st(6) -> 4.0
					; Y | X^2 | X | Y | CI | CR | 4.0
					fmul st(0), st(3) ; st(0) -> Y, st(1) -> X * X, st(2) -> zr * zr - zi * zi + CR (= X), st(3) -> 2 * zi * zr + CI (= Y), st(4) -> CI, st(5) -> CR, st(6) -> 4.0
					; Y^2 | X^2 | X | Y | CI | CR | 4.0
					faddp st(1), st(0) ; st(0) -> X * X + Y * Y, st(1) -> zr * zr - zi * zi + CR (= X), st(2) -> 2 * zi * zr + CI (= Y), st(3) -> CI, st(4) -> CR, st(5) -> 4.0
					; Y^2 | X^2 + Y^2 | X | Y | CI | CR | 4.0
					; X^2 + Y^2 | X | Y | CI | CR | 4.0
					fcomp st(5) ; porownanie st(0) z st(5) i zdjecia st, czyli X*X + Y*Y z 4.0, teraz st(0) -> zr * zr - zi * zi + CR (= X), st(1) -> 2 * zi * zr + CI (= Y), st(2) -> CI, st(3) -> CR, st(4) -> 4.0
					; X | Y | CI | CR | 4.0
					push AX ; zabezpieczam AX
					fstsw AX ; przeniesienie flag C0, ..., C3 do AX zf = C3, pf = C2, cf = C0
					sahf ; przenosi AH do rejestru znacznikow dzieki czamu porowanie na koprocesorze wywola zmiany flag i mozna uzywac instrukcji skoku warunkowegi	
					pop AX

					; uprzatniecie koprocesora przed kolejna ewentualna iteracja
					; zeby sie zgadzalo zamiana st(0) z st(1)
					fxch st(1) ; st(0) -> 2 * zi * zr + CI (= Y), st(1) -> zr * zr - zi * zi + CR (= X), st(2) -> CI, st(3) -> CR, st(4) -> 4.0
					; Y | X | CI | CR | 4.0
					; po tym sprzataniu gotowy do wejscia do petli DrawingLoop ponownie
					ja AfterDrawingLoop ; st(0) > 4.0 : warunek przerwania iteracji

					inc CX
				jmp DrawingLoop

				AfterDrawingLoop:
					.486 ; w celu zwiekszenia szybkosci wykonywania mnozenia *320, gdyz .486 zapobiega zamianie SHR AX, 4 na 4 instrukcje SHR AX, 1
					; Trick mnozenia razy 320: 320 = 256 + 64 = 2^8 + 2^6
					; jesli chcemy pomnozyc y * 320, to tak jakbysmy sumowali y * 256 + y * 64
					; mnozenie * 256 otrzymamy przez SHL REJESTR_A, 8
					; mnozenie * 64 otrzymamy przez SHL REJESTR_B, 6
					; wynik add REJESTR_A, REJESTR_B
					; przy czym offset odpowieniej komorki w pamieci obrazu dla komorki (X, Y) otrzymamy w wyniku: 320 * y + x. Przy zalozeniu
					; ze lewy gorny rog to (0, 0), a prawy (319, 199) -> operujemy na adresacj: 0A000:0000 - 0A000:18FF, a wiec maksymalny wyniik naszego
					; mnozenia wyniesie 64.000(DEX), co nie przekroczy zakresu rejestru 16b (maks. 65535)
					cmp CX, 1000d
					pop CX ; przywracam CX na wartosc licznika petli innerLoop
					jne fin

					; ZMIANA KOLORU ODPOWIEDNIEGO PIKSELA NA BIALY
					; Szybkie mnozenie przy pomocy rejestru DI oraz DX
					mov DX, AX ; w AX mamy wartosc y aka M
					mov DI, DX ; w DI duplikujemy DX
					shl DI, 8d ; w DI byla wartosc <0;199> wiec nic nie przepadnie
					shl DX, 6d ; w DX byla wartosc <0;199> wiec nic nie przepadnie
					add DI, DX
					add DI, CX ; w CX mamy wartosc kolumny (x aka N)
					mov byte ptr ES:[DI], BH


				fin:	
					; tutaj wchodze z nastepujacym stanem stosu: st(0) -> 2 * zi * zr + CI (= Y), st(1) -> zr * zr - zi * zi + CR (= X), st(2) -> CI, st(3) -> CR, st(4) -> 4.0
					ffree st(0) 

					fincstp ; st(0) -> zr * zr - zi * zi + CR (= X), st(1) -> CI, st(2) -> CR, st(3) -> 4.0
					ffree st(0)
					fincstp ; st(0) -> CI, st(1) -> CR, st(2) -> 4.0 -> gotowy do nastepnego wejscia do innerLoop
					
					inc CX
			jmp innerLoop

			AfterInnerLoop:
			

			inc AX
		jmp outerLoop

		AfterOuterLoop:


		;	OCZEKIWANIE NA KLAWISZ PO ZAKONCZENIU RYSOWANIA		

		xor AX, AX
		int 16h

		;	POWROT DO TRYBU GRAICZNEGO
		mov AX, 3
		int 10h
		popa
		ret

	DrawSet endp

	;-------------------------------------------------------------------------------------------------------------
	;	Koniec procedury DrawSet
	;-------------------------------------------------------------------------------------------------------------

code1 ends

;---------------------------------------------------------------------------------------------------------------------------------------
;|															SEGMENT KODU - KONIEC													   |
;---------------------------------------------------------------------------------------------------------------------------------------



;---------------------------------------------------------------------------------------------------------------------------------------
;|																SEGMENT STOSU														   |
;---------------------------------------------------------------------------------------------------------------------------------------

;	Deklaruje wielkosc stosu na 256 slow (512 B)
stack1 segment stack
		dw 	511 dup(?)
	top	dw 	?
stack1 ends

;---------------------------------------------------------------------------------------------------------------------------------------
;|															SEGMENT KODU - KONIEC													   |
;---------------------------------------------------------------------------------------------------------------------------------------

end start