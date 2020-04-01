% 18B09784 Tangjitruamboon Tananan 2020/1/23

%Program code
%  :
delta(0,keiko,1).
delta(1,is,4).
delta(4,a,5).
delta(5,tall,6).
delta(6,girl,9).
delta(0,ichiro,2).
delta(2,likes,4).
delta(2,is,7).
delta(3,is,7).
delta(0,jun,3).
delta(7,a,8).
delta(8,boy,9).
goal(9).

path(X,[S],Y):-delta(X,S,Y).
path(X,[S|Ss],Y):-delta(X,S,Z),path(Z,Ss,Y).
accept(Ss):-goal(G),path(0,Ss,G).
