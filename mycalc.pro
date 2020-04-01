% 18B09784 Tangjitruamboon Tananan 2020/1/27

%Program code
%  :
mycalc(Xs,Rv):-exprA(Xs,[],Rv).
exprA(Xs,Zs,Rv):-exprB(Xs,Ys,Rv1),exprAa(Ys,Zs,Rv1,Rv).
exprAa(Xs,Xs,Acc,Acc).
exprAa([+|Xs],Zs,Acc,Rv):-exprB(Xs,Ys,Rv1),Rv2 is Acc + Rv1,exprAa(Ys,Zs,Rv2,Rv).
exprAa([-|Xs],Zs,Acc,Rv):-exprB(Xs,Ys,Rv1),Rv2 is Acc - Rv1,exprAa(Ys,Zs,Rv2,Rv).
exprB([X|Xs],Ys,Rv):-digit(X),exprBsub(Xs,X,Rv,Ys).
exprBSub([],A,A,[]).
exprBsub([X|Xs],A,A,[X|Xs]):-not(digit(X)).
exprBsub(B,A,A,B):-B=[].
exprBsub([X|Xs],A,B,Ys):-digit(X),AA is A*10 + X,exprBsub(Xs,AA,B,Ys).
digit(0). digit(1). digit(2). digit(3). digit(4).
digit(5). digit(6). digit(7). digit(8). digit(9).
