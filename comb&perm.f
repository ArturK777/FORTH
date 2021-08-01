\ ********  Combinatorics & Permutations  ********
\ Combinatorics R from N; N = scope, R = # : C = N!/R!(N-R)! ( 3 from 5 C=10 )
: CRINIT  ( N R -- adr )   \ init array of R elements, ADR: R N --#R-- , R N start from 1 <= R < N < 256
   DUP DUP 2+ ALLOCATE THROW DUP >R C! SWAP R@ 1+ C! R> SWAP 0 DO I OVER I + 2+ C! LOOP ;
: CRN+  ( adr -- ? )   \ next combination R from N (in adr+2), TRUE if last, indexes start from 0
   \ DUP 1+ C@ OVER C@ - OVER 2+ C@ = IF DROP TRUE EXIT THEN  \ is already last combination?
   DUP C@
   BEGIN 1- 2DUP + 2+ C@ 2 PICK DUP 1+ C@ SWAP C@ - 2 PICK + <> UNTIL
   OVER C@ SWAP -1 -ROT
   DO IF DUP I + 2+ 1 SWAP +! 0
      ELSE DUP I + DUP 1+ C@ 1+ SWAP 2+ C! 0 THEN
   LOOP DROP  DUP 1+ C@ OVER C@ - SWAP 2+ C@ = ;
\ ---------   test   --------------   
: PRNCRN  ( adr n -- )   5 .R 4 SPACES DUP C@ SWAP 2+ SWAP 0 DO DUP C@ . 1+ LOOP CR DROP ;
: TESTCRN+  ( adr -- )   
   CR ." Non-repeat Combinatorics " DUP C@ . ." from " DUP 1+ C@ . CR 
   0 BEGIN 1+
      2DUP PRNCRN  OVER CRN+
   UNTIL  1+ PRNCRN ;   
 8 4 CRINIT TESTCRN+
\ ===============================================================================================
\ Permutations of R = R!, combinations store in adr, init in ascending order, R start from 1
CREATE PERM 0 C, 1 C, 2 C, 3 C, 4 C, 5 C, 6 C, 7 C, 8 C, 9 C,
: SWP@  ( i j adr -- )   >R 2DUP R@ + C@ SWAP R@ + C@ ROT R@ + C! SWAP R> + C! ;
: PERMLEX+  ( adr r -- ? )   \ next lexicographic permutation in byte array adr[r], FALSE if last
   SWAP >R DUP 2-
   BEGIN DUP DUP R@ + C@  SWAP 1+ R@ + C@  >
   WHILE 1- DUP 0< IF 2DROP R> DROP FALSE EXIT THEN
   REPEAT OVER 1-
   BEGIN DUP R@ + C@  2 PICK R@ + C@ SWAP  >
   WHILE 1-
   REPEAT 2DUP R@ SWP@
   DROP 1+ SWAP 1-
   BEGIN 2DUP  <
   WHILE 2DUP R@ SWP@  1- SWAP 1+ SWAP
   REPEAT 2DROP R> DROP TRUE ;
\ ---------   test   --------------   
: PRN  ( adr n -- )   0 DO DUP I + C@ . LOOP DROP CR ;
: TESTPERM  ( adr n -- )   
   CR ." Lexicographic permutations of " DUP . ." elements " CR
   1 DUP . 5 SPACES -ROT 2DUP PRN
   BEGIN 2DUP PERMLEX+
   WHILE ROT 1+ DUP . -ROT 5 SPACES 2DUP PRN
   REPEAT 2DROP DROP ;
PERM 4 TESTPERM   
\ ===============================================================================================
\EOF
 4 VALUE N  2 VALUE R  PAD R + 1+ VALUE PADR  \ copy of PAD for permutations

: !!  ( u<13 -- u! )   DUP IF 1 SWAP 1+ 1 ?DO I * LOOP  ELSE DROP 1 THEN ;
: COMB  ( n r -- u )   2DUP - !!  SWAP !! *  SWAP !! SWAP /  ; \  # combinations r from n
: INITCOMB  ( -- )   R 1+ 0 DO I PAD I + C! LOOP ;
: CRL  ( i -- end )   N R - + ;   \ last index
: CRA  ( i -- adr )  PAD + ;   \ adr of i's element
: CR@  ( i -- r )   CRA C@ ;   \ i's element

: CRN@  ( adr -- )   1+ C@ ;  \ N
: NCR+N  ( R -- ? )    \  next combination R from N (in PAD), true if last
   DUP N R - + OVER CR@   10 ^
      = \ is max value in this (R) position ?
   IF 1-   30 ^
      RECURSE 
   ELSE -1 SWAP R 1+ SWAP  40 ^
      DO IF I CRA 1 SWAP +! 0  41 ^
         ELSE I DUP 1- CR@ 1+ SWAP CRA ! 0  42 ^
            THEN
      LOOP DROP
      0 CR@ N R - =  \ finish?
   THEN ; 

: NCR+  ( R -- ? )   \  next combination R from N (in PAD), true if last
   DUP CRL  OVER CR@    1 ^
      = \ cell on top ?
   IF 1- RECURSE
   ELSE -1 SWAP R 1+ SWAP
      DO IF I CRA 1 SWAP +! 0
         ELSE I DUP 1- CR@ 1+ SWAP CRA ! 0  THEN
      LOOP DROP
      0 CR@ 0 CRL =  \ finish?
   THEN ; 

\ test routines   
: TYPECOMB   ( i -- )   3 .R 4 SPACES  R 1+ 0 DO I PAD + C@ . LOOP CR ;
: TYPECOMB+   ( -- )   7 SPACES  R 1+ 0 DO I PADR + C@ . LOOP CR ;
: TEST  ( -- )   0
   BEGIN 1+
      DUP TYPECOMB  PAD R 1+ 2DUP + SWAP CMOVE
      \ BEGIN NEXTPERM
      \ WHILE TYPECOMB+
      \ REPEAT CR
      R NCR+N
   UNTIL 
   1+ DUP TYPECOMB    PAD R 1+ 2DUP + SWAP CMOVE
   BEGIN \ NEXTPERM
   WHILE TYPECOMB+
   REPEAT CR DROP ;
