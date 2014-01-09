\documentclass[10pt,a4paper]{article}
\usepackage{a4wide}
\usepackage[slovak]{babel}
\usepackage[T1]{fontenc}
\usepackage[cp1250]{inputenc}

\usepackage{xcolor}

\usepackage{listings}
\lstnewenvironment{code}{
	\lstset{language=Haskell,
			basicstyle=\small\ttfamily,%
			frame=single,%
			backgroundcolor=\color[gray]{0.9}%
			}}{}

\long\def\ignore#1{}

\newenvironment{komentar}{\begin{trivlist}\itshape\item}{\end{trivlist}}


\begin{document}
\section{Rie�en� �lohy}
%TBD
\section{�lohy pre z�kladn� programovanie}

\subsection{Jednoduch� cykly}

Vo v�etk�ch nasleduj�cich �loh�ch budeme predpoklada�, �e %pam�ov� miest-
% maj� dostato�n� ve�kos� na v�po�et uveden�ch hodn�t
m�me dostatok pam�te na v�po�et uveden�ch hodn�t.

\ignore{
\begin{code}
import Data.List (nub)
\end{code}
}

% ---------------------------------------------------
\subsubsection*{2.1.1}
Je dan� prirodzen� ��slo $n$. Je potrebn� vypo��ta�:

\begin{enumerate}
\item[a)] $2^n$

\begin{code}
f_2_1_1_a n = 2^n
\end{code}

\item[b)] $n!$

\begin{code}
f_2_1_1_b n = product [1..n]
\end{code}


\item[c)] $(1 + 1^{-2})\cdot (1 + 2^{-2}) \cdot \cdots \cdot (1 + n^{-2})$

\begin{code}
f_2_1_1_c n = product [ 1+i**(-2) | i <- [1..n] ]
\end{code}

\item[d)] $\sin^{-1} 1 + (\sin 1 + \sin 2)^{-1} + \cdots + (\sin 1 + \sin n)^{-1}$

\begin{code}
f_2_1_1_d n = sum [(sin 1 + sin i)**(-1) | i <- [1..n]]
\end{code}

\item[e)] $\sqrt{2 + \sqrt{2 + \dots + \sqrt{2}}}$ ($n$ druh�ch odmocn�n)

\begin{code}
f_2_1_1_e n = last $ take n $ iterate sqrt 2
\end{code}

\item[f)] $\cos(1) \cdot \sin^{-1} 1 \cdot (\cos 1 + \cos2) \cdot (\sin 1 + \sin 2)^{-1} \cdot \dots \cdot (\cos 1 + \cdots + \cos n) \cdot (\sin 1 + \sin n)^{-1}$


\item[g)] $\sqrt{3+ \sqrt{6 + \dots + \sqrt{3(n-1)+\sqrt{3n}}}}$
\end{enumerate}

\emph{Vstup:} prirodzen� ��slo $n$;

\emph{V�stup:} vypo��tan� hodnota pod�a uveden�ho vz�ahu.

% ---------------------------------------------------

\subsubsection*{2.1.3}
Je dan� re�lne ��slo $a$. Je potrebn� n�js�:

\begin{enumerate}
\item medzi ��slami $1, 1 + 2^{-1}, 1 + 2^{-1} + 3^{-1}, \dots$ prv� ��slo tak�, ktor� je v��ie ne� $a$;

\begin{komentar}
Sta�� si uvedomi�, �e ide o z�pis ��siel $1, 1 + {1 \over 2}, 1 + {1 \over 2} + {1 \over 3},\dots$. Ka�d� prvok tejto postupnosti je $\sum_i^n {1 \over i}$.
\end{komentar}

\begin{code}
f_2_1_3_sum n = sum [1 / i | i <- [1..n]]
f_2_1_3_a a = head $ dropWhile (<=a) [ f_2_1_3_sum n | n <- [1..]]
\end{code}

\item najmen�ie prirodzen� ��slo $n$ tak�, �e $1 + 2^{-1} + 3^{-1} + \dots + n^{-1} > a$

\begin{komentar}
Generujeme postupnosti s��tov dovtedy, k�m je prvok postupnosti (teda jedna suma), men�ia ne� $a$.
\end{komentar}

\begin{code}
f_2_1_3_b a = length $ takeWhile (<a) [ f_2_1_3_sum n | n <- [1..]]

\end{code}
\end{enumerate}

\emph{Vstup:} re�lne ��slo $a$;

\emph{V�stup:} ��slo vyhovuj�ce podmienke

% ---------------------------------------------------

\subsubsection*{2.1.5}
Je dan� prirodzen� ��slo $n$. 

\begin{enumerate}
\item[a)] Ko�ko je cifier v ��sle $n$?
\begin{code}
pocetCifier n 
	| n < 0 = error "Zaporne cislo"
	| n < 10 = 1
	| otherwise = 1 + pocetCifier (n `div` 10)

f_2_1_5_a = pocetCifier 
\end{code}

\item[b)] Ko�ko r�znych cifier je v ��sle $n$?

\begin{komentar}
Pou�ijeme pomocn� funkciu, ktor� pre dan� ��slo vr�ti zoznam jeho cifier. Popri tom vyu�ijeme pomocn� funkciu \verb|Data.List.nub|, ktor� vr�ti unik�tne prvky zo zoznamu.
\end{komentar}

\begin{code}
cifier n
	| n < 10 = [n]
	| otherwise = cifier (n `div` 10) ++ [cifra]
	where cifra = n `mod` 10
cisloNaCifry = cifier

pocetRoznychCifier n = length $ nub $ cifier n 
f_2_1_5_b = pocetRoznychCifier
\end{code}

\item[c)] �omu sa rovn� s��et v�etk�ch jeho cifier?
\begin{code}
sucetCifier n = sum $ cifier n
f_2_1_5_c n = sucetCifier
\end{code}

\item[d)] �omu sa rovn� s��et v�etk�ch jeho cifier na p�rnych poz�ci�ch?


\begin{code}
sucetCifierNaParnychPoziciach n = 
	sum [cifier n !! (index - 1) | index <- [2,4..pocetCifier n] ]
f_2_1_5_d = sucetCifierNaParnychPoziciach
\end{code}

\end{enumerate}

\emph{Vstup:} prirodzen� ��slo $n$;

\emph{V�stup:} v�sledn� hodnota ako odpove� na dan� ot�zku

% ---------------------------------------------------

\subsubsection*{2.1.6}
Je dan� prirodzen� ��slo $n$. Je potrebn�

\begin{enumerate}
\item[a)] ur�i� po�et v�skytov ��slice 3 v z�pise ��sla $n^2$;

\begin{code}
-- pocetVyskytovCifry cifra n = length 
--		$ filter (\c -> c == cifra) (cisloNaCifry n)
pocetVyskytovCifry cifra n = length 
		$ filter (== cifra) (cisloNaCifry n)
pocetVyskytovTrojkyVNNaDruhu n = pocetVyskytovCifry 3 (n^2)
f_2_1_6_a = pocetVyskytovTrojkyVNNaDruhu 

\end{code}

\item[b)] zameni� poradie ��slic v ��sle $n$ na opa�n�;

\begin{code}
cifryNaCislo cifry = foldl (\acc cifra -> acc * 10 + cifra) 0 cifry
zamenitPoradieCifierNaOpacne n = cifryNaCislo $ reverse $ cifier n
f_2_1_6_b = zamenitPoradieCifierNaOpacne 
\end{code}

\item[c)] vymeni� prv� a posledn� ��slicu v ��sle $n$;

\begin{code}
vymenitPrvuAPoslednuCifru n 
	| n < 0 = error "Zaporne cislo"
	| n < 10 = n
	| otherwise = cifryNaCislo $ [posledna] ++ stred ++ [prva]
	where prva = head cifry
	      posledna = last cifry
	      stred = init ( tail cifry )
	      cifry = cifier n
f_2_1_6_c = vymenitPrvuAPoslednuCifru

\end{code}


\item[d)] prip�sa� dvojku na za�iatok a koniec ��sla.

\begin{code}
pripisatDvojkuNaZaciatokAKoniec n = cifryNaCislo $ 
	[2] ++ cisloNaCifry n ++ [2]
f_2_1_6_d = pripisatDvojkuNaZaciatokAKoniec 

\end{code}
\end{enumerate}

\emph{Vstup:} prirodzen� ��slo $n$;

\emph{V�stup:} hodnoty vyhovuj�ce podmienkam.
 

\end{document}