10 rem -- Advent of Code 2021 - Day 1 task A & B
20 rem -- Solutions in (old school) BASIC
30 rem -- Also solving in other languages like Haskell, Lisp, Rust and Swift
40 rem -- (Ter leering ende vermaeck...)
50 rem --
60 rem -- The count of increased measurements is: 1692
70 rem -- The count of increased triplets is: 1724
80 rem --
90 rem -- (cl) by Arno Jacobs, 01-12-2021
100 dim sl(2000)
110 c1 = 0 : c2 = 0 
120 p1 = 999999999 
130 i = 0
140 f$ = "./data/inputDay01_2021.txt"
150 open f$ for input as #1
190 rem File read Loop
200 if eof(#1) then goto 310
210 input #1,l$
220 if l$ = "" then goto 200
230 gosub 400
240 goto 200
300 rem End of loop - reading input lines 
310 close #1
315 gosub 500
320 print "Advent of Code 2021 - day 1 - both parts in BASIC" 
330 print "The count of increased measurements is: ";c1
340 print "The count of increased triplets is: ";c2
350 print "0K." : print
360 rem End and exit the BASIC
370 exit 
390 rem Count increasing measurements while reading data
400 v = val(l$)
410 i = i + 1 : sl(i) = v
420 if v > p1 then c1 = c1 + 1  
430 p1 = v
440 return
500 rem Count increasing triplets
510 for j = 1 to i-3
520 t1 = sl(j  ) + sl(j+1) + sl(j+2) 
530 t2 = sl(j+1) + sl(j+2) + sl(j+3) 
540 if t1 < t2 then c2 = c2 + 1
550 next j 
560 return
990 rem End of Code
