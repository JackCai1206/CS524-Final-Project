$offeolcom
$eolcom #
integer variable
    group(A);
group.lo(A) = 1;
group.up(A) = card(A) / g;

binary variable
    groupBigger(A1, A2),
    groupSmaller(A1, A2);

equations
    groupMaxEq(A),
    groupMinEq(A),
    sameGroupEq1(A, A),
    sameGroupEq2(A, A),
    sameGroupImpMatchEq(A, A),
    matchImpSameGroupEq1(A, A),
    matchImpSameGroupEq2(A, A),
    isBlockEq2(A, A);

groupMaxEq(A1)..
    sum(A, M(A1, A)) =L= g-1;

groupMinEq(A1)..
    sum(A, M(A1, A)) =G= 1;
    
sameGroupEq1(A1, A2) $ [ord(A1) <> ord(A2)]..
    group(A1) - group(A2) - (-g) * groupBigger(A1, A2) =G= 0.1;

sameGroupEq2(A1, A2) $ [ord(A1) <> ord(A2)]..
    group(A1) - group(A2) - g * groupSmaller(A1, A2) =L= -0.1;

sameGroupImpMatchEq(A1, A2) $ [ord(A1) <> ord(A2)]..
    groupBigger(A1, A2) + groupSmaller(A1, A2) =L= 1 + M(A1, A2);

matchImpSameGroupEq1(A1, A2) $ [ord(A1) <> ord(A2)]..
    M(A1, A2) =L= groupBigger(A1, A2);

matchImpSameGroupEq2(A1, A2) $ [ord(A1) <> ord(A2)]..
    M(A1, A2) =L= groupSmaller(A1, A2);

isBlockEq2(A1, A2) $ [ord(A1) <> ord(A2)]..
    matchPref(A1, A2) + matchPref(A2, A1) - 2.1 * isBlock(A1, A2) =L= 1.9;

model sr3 /
    groupMaxEq,
    groupMinEq,
    sameGroupImpMatchEq,
    matchImpSameGroupEq1,
    matchImpSameGroupEq2,
    matchEq,
    twoOut3inEq,
    prefEq,
    matchPrefEq,
    isBlockEq2,
    objEq
/;
solve sr3 using mip min obj;

display M.l;
