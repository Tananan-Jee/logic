sentence(Xs,Ks):-postpositionPhr(Xs,Ys,K1s),verbPhr(Ys,Zs,K2s),myAppend(K1s,K2s,Ks),Zs=[].
postpositionPhr(Xs,Zs,Ks):-noun(Xs,Ys,K1s),postposition(Ys,Zs,K2s),myAppend(K1s,K2s,Ks).
verbPhr(Xs,Zs,Ks):-postpositionPhr(Xs,Ys,K1s),verbPhr(Ys,Zs,K2s),myAppend(K1s,K2s,Ks).
verbPhr(Xs,Zs,K):-verb(Xs,Zs,K).
noun(Xs,Rs,[W]):-word(n,W,Ss),matchhead(Xs,Ss,Rs).
postposition(Xs,Rs,[W]):-word(p,W,Ss),matchhead(Xs,Ss,Rs).
verb(Xs,Rs,[W]):-word(v,W,Ss),matchhead(Xs,Ss,Rs).


matchhead(As,[],As).
matchhead([X|As],[Y|Bs],Cs):-X=Y,matchhead(As,Bs,Cs).

myAppend([],X,X).
myAppend([X|L],Y,[X|Z]):-myAppend(L,Y,Z).

word(n,'スモモ',[s,u,m,o,m,o]).
word(n,'桃',[m,o,m,o]).
word(n,'酢',[s,u]).
word(n,'巣',[s,u]).
word(n,'藻',[m,o]).
word(p,'も',[m,o]).
word(v,'食べる',[t,a,b,e,r,u]).
