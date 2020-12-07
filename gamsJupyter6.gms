$offeolcom
$eolcom #
free variable
    combineRank;

equations
    assignEq(A),
    combineRankEq;

assignEq(A1)..
    sum(A, M(A1, A)) =E= c(A1);

combineRankEq..
    sum((A1, A2), M(A1, A2) * (rank(A1, A2) + rank(A2, A1))) =E= combineRank;
    
model sr2 / matchEq, assignEq, combineRankEq /;
solve sr2 using rmip min combineRank;

display M.l;
