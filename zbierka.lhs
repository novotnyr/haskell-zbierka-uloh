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
\section{Riešené úlohy}
%TBD
\section{Úlohy pre základné programovanie}

\subsection{Jednoduché cykly}

Vo všetkıch nasledujúcich úlohách budeme predpoklada, e %pamäové miest-
% majú dostatoènú ve¾kos na vıpoèet uvedenıch hodnôt
máme dostatok pamäte na vıpoèet uvedenıch hodnôt.

\ignore{
\begin{code}
import Data.List (nub)
\end{code}
}

% ---------------------------------------------------
\subsubsection*{2.1.1}
Je dané prirodzené èíslo $n$. Je potrebné vypoèíta:

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

\item[e)] $\sqrt{2 + \sqrt{2 + \dots + \sqrt{2}}}$ ($n$ druhıch odmocnín)

\begin{code}
f_2_1_1_e n = last $ take n $ iterate sqrt 2
\end{code}

\item[f)] $\cos(1) \cdot \sin^{-1} 1 \cdot (\cos 1 + \cos2) \cdot (\sin 1 + \sin 2)^{-1} \cdot \dots \cdot (\cos 1 + \cdots + \cos n) \cdot (\sin 1 + \sin n)^{-1}$


\item[g)] $\sqrt{3+ \sqrt{6 + \dots + \sqrt{3(n-1)+\sqrt{3n}}}}$
\end{enumerate}

\emph{Vstup:} prirodzené èíslo $n$;

\emph{Vıstup:} vypoèítaná hodnota pod¾a uvedeného vzahu.

% ---------------------------------------------------

\subsubsection*{2.1.3}
Je dané reálne èíslo $a$. Je potrebné nájs:

\begin{enumerate}
\item medzi èíslami $1, 1 + 2^{-1}, 1 + 2^{-1} + 3^{-1}, \dots$ prvé èíslo také, ktoré je väèšie ne $a$;

\begin{komentar}
Staèí si uvedomi, e ide o zápis èísiel $1, 1 + {1 \over 2}, 1 + {1 \over 2} + {1 \over 3},\dots$. Kadı prvok tejto postupnosti je $\sum_i^n {1 \over i}$.
\end{komentar}

\begin{code}
f_2_1_3_sum n = sum [1 / i | i <- [1..n]]
f_2_1_3_a a = head $ dropWhile (<=a) [ f_2_1_3_sum n | n <- [1..]]
\end{code}

\item najmenšie prirodzené èíslo $n$ také, e $1 + 2^{-1} + 3^{-1} + \dots + n^{-1} > a$

\begin{komentar}
Generujeme postupnosti súètov dovtedy, kım je prvok postupnosti (teda jedna suma), menšia ne $a$.
\end{komentar}

\begin{code}
f_2_1_3_b a = length $ takeWhile (<a) [ f_2_1_3_sum n | n <- [1..]]

\end{code}
\end{enumerate}

\emph{Vstup:} reálne èíslo $a$;

\emph{Vıstup:} èíslo vyhovujúce podmienke

% ---------------------------------------------------

\subsubsection*{2.1.5}
Je dané prirodzené èíslo $n$. 

\begin{enumerate}
\item[a)] Ko¾ko je cifier v èísle $n$?
\begin{code}
pocetCifier n 
	| n < 0 = error "Zaporne cislo"
	| n < 10 = 1
	| otherwise = 1 + pocetCifier (n `div` 10)

f_2_1_5_a = pocetCifier 
\end{code}

\item[b)] Ko¾ko rôznych cifier je v èísle $n$?

\begin{komentar}
Pouijeme pomocnú funkciu, ktorá pre dané èíslo vráti zoznam jeho cifier. Popri tom vyuijeme pomocnú funkciu \verb|Data.List.nub|, ktorá vráti unikátne prvky zo zoznamu.
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

\item[c)] Èomu sa rovná súèet všetkıch jeho cifier?
\begin{code}
sucetCifier n = sum $ cifier n
f_2_1_5_c n = sucetCifier
\end{code}

\item[d)] Èomu sa rovná súèet všetkıch jeho cifier na párnych pozíciách?


\begin{code}
sucetCifierNaParnychPoziciach n = 
	sum [cifier n !! (index - 1) | index <- [2,4..pocetCifier n] ]
f_2_1_5_d = sucetCifierNaParnychPoziciach
\end{code}

\end{enumerate}

\emph{Vstup:} prirodzené èíslo $n$;

\emph{Vıstup:} vısledná hodnota ako odpoveï na danú otázku

% ---------------------------------------------------

\subsubsection*{2.1.6}
Je dané prirodzené èíslo $n$. Je potrebné

\begin{enumerate}
\item[a)] urèi poèet vıskytov èíslice 3 v zápise èísla $n^2$;

\begin{code}
-- pocetVyskytovCifry cifra n = length 
--		$ filter (\c -> c == cifra) (cisloNaCifry n)
pocetVyskytovCifry cifra n = length 
		$ filter (== cifra) (cisloNaCifry n)
pocetVyskytovTrojkyVNNaDruhu n = pocetVyskytovCifry 3 (n^2)
f_2_1_6_a = pocetVyskytovTrojkyVNNaDruhu 

\end{code}

\item[b)] zameni poradie èíslic v èísle $n$ na opaèné;

\begin{code}
cifryNaCislo cifry = foldl (\acc cifra -> acc * 10 + cifra) 0 cifry
zamenitPoradieCifierNaOpacne n = cifryNaCislo $ reverse $ cifier n
f_2_1_6_b = zamenitPoradieCifierNaOpacne 
\end{code}

\item[c)] vymeni prvú a poslednú èíslicu v èísle $n$;

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


\item[d)] pripísa dvojku na zaèiatok a koniec èísla.

\begin{code}
pripisatDvojkuNaZaciatokAKoniec n = cifryNaCislo $ 
	[2] ++ cisloNaCifry n ++ [2]
f_2_1_6_d = pripisatDvojkuNaZaciatokAKoniec 

\end{code}
\end{enumerate}

\emph{Vstup:} prirodzené èíslo $n$;

\emph{Vıstup:} hodnoty vyhovujúce podmienkam.
 

\end{document}