$offeolcom
$eolcom #
option limrow = 1, limcol = 1, solPrint = on;
binary variables
    M(A, A) "1 if A1 and A2 are matched 0 otherwise",
    delta(A) "Capacity is not reached for A",
    pref(A, A, A) "A1 prefers A2 to A3",
    twoOut3in(A, A, A) "A2 not in match of A1 but A3 is in match of A1",
    matchPref(A, A) "A1 prefers A2 to at least one of his matches in M",
    semiBlock(A, A) "Either capacity is not reach for A1 or A1 prefers A2 to at least one of his matches in M",
    isBlock(A, A) "A1, A2 is a blocking pair";

free variables
    obj;

equations
    matchEq(A, A),
    maxCapacityEq(A),
    capacityEq(A) "Define delta",
    twoOut3inEq(A, A, A),
    prefEq(A, A, A),
    matchPrefEq(A, A),
    semiBlockEq(A, A),
    isBlockEq(A, A),
    objEq;

matchEq(A1, A2)..
    M(A1, A2) =E= M(A2, A1);
M.fx(A1, A1) = 0;

maxCapacityEq(A1)..
    sum(A, M(A1, A)) =L= c(A1);

capacityEq(A1)..
    sum(A2, M(A1, A2)) - (-1 - c(A1)) * delta(A1) =G= c(A1);

twoOut3inEq(A1, A2, A3) $ [ord(A1) <> ord(A2) and ord(A2) <> ord(A3) and ord(A1) <> ord(A3)].. 
    M(A1, A3) + (1 - M(A1, A2)) - 2.1 * twoOut3in(A1, A2, A3) =L= 1.9;

prefEq(A1, A2, A3) $ [ord(A1) <> ord(A2) and ord(A2) <> ord(A3) and ord(A1) <> ord(A3)]..
    twoOut3in(A1, A2, A3) * (rank(A1, A2) - rank(A1, A3)) - (-10) * pref(A1, A2, A3) =G= 0;
    
matchPrefEq(A1, A2) $ [ord(A1) <> ord(A2)]..
    sum(A, pref(A1, A2, A)) - card(A) * matchPref(A1, A2) =L= 0;

semiBlockEq(A1, A2) $ [ord(A1) <> ord(A2)]..
    delta(A1) + matchPref(A1, A2) - 2 * semiBlock(A1, A2) =L= 0;
    
isBlockEq(A1, A2) $ [ord(A1) <> ord(A2)]..
    semiBlock(A1, A2) + semiBlock(A2, A1) - 2.1 * isBlock(A1, A2) =L= 1.9;

objEq..
    sum((A1, A2), isBlock(A1, A2)) =E= obj;

model sr / all /;
solve sr using mip min obj;

display isBlock.l, M.l, obj.l;
display pref.l;
