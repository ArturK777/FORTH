: PRIME?  ( u -- flag )   \ is u a prime number - full ver
   DUP 2 = IF EXIT ELSE DUP 2 < IF DROP 0 EXIT THEN THEN
   DUP 2 ?DO DUP I MOD IF ELSE DROP 0 LEAVE THEN  LOOP ;

: PRIME2?  ( u -- flag )   \ is u a prime number - short ver
   DUP 3 DO DUP I MOD IF ELSE DROP 0 LEAVE THEN 2 +LOOP ;

: PRIME@  ( i -- i's_prime )    \ get i's prime
   DUP 1 = IF DROP 2 LEAVE THEN
   2 1
   BEGIN  2+  DUP PRIME2?
      IF SWAP 1+ SWAP THEN
      >R 2DUP = R> SWAP
   UNTIL NIP NIP ;  

: PRIME#  ( # -- adr last-prime )   2 \ fill list of # primes
   OVER CELLS ALLOCATE THROW
   DUP 2 SWAP ! DUP 1 CELLS + 3 SWAP ! 3
   BEGIN  2+  DUP PRIME2?
      IF DUP 2 PICK 4 PICK CELLS + !
         ROT 1+ -ROT
      THEN
      2SWAP 2DUP = >R 2SWAP R>
   UNTIL
   2SWAP 2DROP ;

: PRIMELIST  ( adr i -- adr )   0 ?DO DUP I CELLS + @ . LOOP ;

313 DUP PRIME# DROP SWAP PRIMELIST

\EOF
Usefull primes words:
PRIME?   - check number is prime
PRIME2?  - same but starts from 5 and check only odd numbers
PRIME@   - get Nth prime
PRIME#   - fill list of N primes
PRIMELIST - display this list