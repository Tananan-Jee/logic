% 18B09784 Tangjitruamboon Tananan 2020/2/7

%Program code
%  :
% TechChan Question Answering System
% (Template code for an exercise of Logic and Reasoning)
% Takahiro Shinozaki, 2018/2/1
% Ver1.1
%
% Ver. 1.1 fixed the position of cut(!) at techchan(Query, Answer)
% clause 2018/2/2
% Ver. 1.0 Initial version 2018/2/1
%
% Examples
% ?- techchan([anata, ha, dare, desu, ka], Answer).
% ?- techchan([can,i,watch,rugby,on,1,august],Answer).
% ?- techchan([engineering,kara,daigakuin,ni,human_centered_science_and_biomedical_engineering,wo,susumitai],Answer).
% ?- techchan([kamata,kara,futakotamagawa,made,ichiban,hayai],Answer).

% TechChan is a mascot for Kodai-sai.
% https://koudaisai.jp/mascot/
% When you extend this program, follow the rules of TechChan.
% https://koudaisai.jp/mascot/agreement/

techchan(Query, Answer):-recognize(Query, Slots, Flags),!,think(Slots, Flags, Answer),writeln(Answer).
techchan(_, Answer):-Answer=[konnichiwa, boku, tokodai, no, techchan, desu], writeln(Answer).

% Basic information for Tokyotech
school(science).
school(engineering).
school(materials_and_chemical_technology).
school(computing).
school(life_science_and_technology).
school(environment_and_society).
graduate(energy_science_and_engineering).
graduate(engineering_sciences_and_design).
graduate(human_centered_science_and_biomedical_engineering).
graduate(nuclear_engineering).
graduate(artificial_intelligence).
graduate(urban_design_and_build_environment).
graduate(mathematics).
graduate(physics).
graduate(chemistry).
graduate(earth_and_planetary_sciences).
graduate(mechanical_engineering).
graduate(systems_and_control_engineering).
graduate(electrical_and_electronic_engineering).
graduate(information_and_communications_engineering).
graduate(industrial_engineering_and_economics).
graduate(materials_science_and_engineering).
graduate(chemical_science_and_engineering).
graduate(mathematical_and_computing_science).
graduate(computer_science).
graduate(life_science_and_technology).
graduate(architecture_and_building_engineering).
graduate(civil_engineering).
graduate(global_engineering_for_development_environment_and_society).


% Basic information for Olympic
sport(ceremony).
sport(aquatics).
sport(archery).
sport(athletics).
sport(badminton).
sport(baseball).
sport(softball).
sport(basketball).
sport(boxing).
sport(canoe).
sport(cycling).
sport(equestrian).
sport(fencing).
sport(football).
sport(golf).
sport(gymnastics).
sport(handball).
sport(hockey).
sport(judo).
sport(karate).
sport(modern_pentathlon).
sport(rowing).
sport(rugby).
sport(sailing).
sport(shooting).
sport(skateboarding).
sport(sport_climbing).
sport(surfing).
sport(table_tennis).
sport(taekwondo).
sport(tennis).
sport(triathlon).
sport(volleyball).
sport(weightlifting).
sport(wrestling).
month(january).
month(february).
month(march).
month(april).
month(may).
month(june).
month(july).
month(august).
month(september).
month(october).
month(november).
month(december).

% Basic information for Trains
station(chuorinkan).
station(suzukakedai).
station(nagatsuta).
station(futakotamagawa).
station(kikuna).
station(jiyugaoka).
station(oookayama).
station(shibuya).
station(ooimachi).
station(yokohama).
station(tamagawa).
station(hatanodai).
station(kamata).
train(megurosen).
train(ikegamisen).
train(tokyutamagawasen).
train(denentoshisen).
train(yokohamasen).
train(toyokosen).
train(ooimachisen).


% knowledge about language to understand the query(automaton)

%% delta(fromState, arcSymbol, toState, slot, flag)  for Tokyotech
delta(0, W, 1, [studyplan, W,_],[1,1,0,1]):-school(W).
delta(1, kara, 2, _, _).
delta(2, daigakuin, 3, _, _).
delta(3, ni, 4, _, _).
delta(4, W, 5, [studyplan, _,W], [1,0,1,1]):-graduate(W).
delta(5, wo, 6, _, _).
delta(6, susumitai, 7, _, _).

delta(0, daigakuin, 1, [studyplan, 0, _], [1,1,0,1]).
delta(1, ni, 2, _, _).
delta(2, W, 3, [studyplan, _,W], [1,0,1,1]):-graduate(W).
delta(3, wo, 4, _, _).
delta(4, susumitai, 7, _, _).

delta(0, W, 1, [studyplan, W,_],[1,1,0,1]):-school(W).
delta(1, kara, 2, _, _).
delta(2, donna, 3, _, _).
delta(3, daigakuin, 4, _, _).
delta(4, ni, 5, _, _).
delta(4, susumeruka, 7, [studyplan, _,0], [1,0,1,1]).
delta(5, susumeruka, 7, [studyplan, _,0], [1,0,1,1]).
goal(7).


%% delta(fromState, arcSymbol, toState, slot, flag) for Olympic
delta2(0, can, 1, _, _).
delta2(1, i, 2, _, _).
delta2(2, watch, 3, _, _).
delta2(3, W, 4, [olympic, W, _, _], [1,1,0,0]):-sport(W).
delta2(4, on, 5, _, _).
delta2(5, W, 6, [olympic, _, W, _], [1,0,1,0]).
delta2(6, W, 7, [olympic, _, _, W], [1,0,0,1]):-month(W).


%% delta(fromState, arcSymbol, toState, slot, flag) for Train
delta3(0, W, 1, [transport, W, _, _],[1,1,0,0]):-station(W).
delta3(1, kara, 2, _, _).
delta3(2, W, 3, [transport, _, W, _], [1,0,1,0]):-station(W).
delta3(3, made, 4, _, _).
delta3(3, made, 7, [transport, _, _, normal], [0,0,0,1]).
delta3(4, ikitai, 7, [transport, _, _, normal], [0,0,0,1]).
delta3(4, nanpun, 7, [transport, _, _, normal], [0,0,0,1]).
delta3(4, ichiban, 5, _, _).
delta3(5, hayai, 7, [transport, _, _, fast], [1,0,0,1]).


% knowledge about transportation to answer the question (directed graph)

%% link(bachelor, fromSchool, toMaster)
link(chemistry, science, energy_science_and_engineering).
link(mechanical_engineering, engineering, energy_science_and_engineering).
link(mechanical_engineering, engineering, engineering_sciences_and_design).
link(mechanical_engineering, engineering, human_centered_science_and_biomedical_engineering).
link(mechanical_engineering, engineering, nuclear_engineering).
link(systems_and_control_engineering, engineering, engineering_sciences_and_design).
link(electrical_and_electronic_engineering, engineering, energy_science_and_engineering).
link(electrical_and_electronic_engineering, engineering, human_centered_science_and_biomedical_engineering).
link(electrical_and_electronic_engineering, engineering, nuclear_engineering).
link(information_and_communications_engineering, engineering, human_centered_science_and_biomedical_engineering).
link(industrial_engineering_and_economics, engineering, engineering_sciences_and_design).
link(materials_science_and_engineering, materials_and_chemical_technology, energy_science_and_engineering).
link(materials_science_and_engineering, materials_and_chemical_technology, human_centered_science_and_biomedical_engineering).
link(materials_science_and_engineering, materials_and_chemical_technology, nuclear_engineering).
link(chemical_science_and_engineering, materials_and_chemical_technology, energy_science_and_engineering).
link(chemical_science_and_engineering, materials_and_chemical_technology, human_centered_science_and_biomedical_engineering).
link(chemical_science_and_engineering, materials_and_chemical_technology, nuclear_engineering).
link(mathematical_and_computing_science, computing, artificial_intelligence).
link(computer_science, computing, artificial_intelligence).
link(life_science_and_technology, life_science_and_technology, human_centered_science_and_biomedical_engineering).
link(architecture_and_building_engineering, environment_and_society, engineering_sciences_and_design).
link(architecture_and_building_engineering, environment_and_society, urban_design_and_build_environment).
link(civil_and_environmental_engineering, environment_and_society, engineering_sciences_and_design).
link(civil_and_environmental_engineering, environment_and_society, urban_design_and_build_environment).
link(transdisciplinary_science_and_engineering, environment_and_society, energy_science_and_engineering).
link(transdisciplinary_science_and_engineering, environment_and_society, engineering_sciences_and_design).
link(transdisciplinary_science_and_engineering, environment_and_society, nuclear_engineering).
link(mathematics, science, mathematics).
link(physics, science, physics).
link(chemistry, science, chemistry).
link(earth_and_planetary_sciences, science, earth_and_planetary_sciences).
link(mechanical_engineering, engineering, mechanical_engineering).
link(systems_and_control_engineering, engineering, systems_and_control_engineering).
link(electrical_and_electronic_engineering, engineering, electrical_and_electronic_engineering).
link(information_and_communications_engineering, engineering, information_and_communications_engineering).
link(industrial_engineering_and_economics, engineering, industrial_engineering_and_economics).
link(materials_science_and_engineering, materials_and_chemical_technology, materials_science_and_engineering).
link(chemical_science_and_engineering, materials_and_chemical_technology, chemical_science_and_engineering).
link(mathematical_and_computing_science, computing, mathematical_and_computing_science).
link(computer_science, computing, computer_science).
link(life_science_and_technology, life_science_and_technology, life_science_and_technology).
link(architecture_and_building_engineering, environment_and_society, architecture_and_building_engineering).
link(civil_and_environmental_engineering, environment_and_society, civil_engineering).
link(transdisciplinary_science_and_engineering, environment_and_society, global_engineering_for_development_environment_and_society).

%% link(sport, july, august)
link(ceremony,[24],[9]).
link(aquatics,[25,26,27,28,29,30,31],[1,2,3,4,5,6,7,8,9]).
link(archery,[24,25,26,27,28,29,30,31],[1]).
link(athletics,[31],[1,2,3,4,5,6,7,8,9]).
link(badminton,[25,26,27,28,29,30,31],[1,2,3]).
link(baseball,[29,30,31],[1,2,3,4,5,6,8]).
link(softball,[22,23,25,26,27,28],[]).
link(basketball,[25,26,27,28,29,30,31],[1,2,3,4,5,6,7,8,9]).
link(boxing,[25,26,27,28,29,30,31],[1,2,4,5,6,7,8,9]).
link(canoe,[26,27,28,29,30,31],[3,4,5,6,7,8]).
link(cycling,[25,26,27,28,29,30,31],[1,2,3,4,5,6,7,8,9]).
link(equestrian,[25,26,28,29,31],[1,2,3,4,5,7,8]).
link(fencing,[25,26,27,28,29,30,31],[1,2]).
link(football,[22,23,25,26,28,29,31],[1,3,4,6,7,8]).
link(golf,[30,31],[1,2,5,6,7,8]).
link(gymnastics,[25,26,27,28,29,30,31],[1,2,3,4,7,8,9]).
link(handball,[25,26,27,28,29,30,31],[1,2,3,4,5,6,7,8,9]).
link(hockey,[25,26,27,28,29,30,31],[1,2,3,4,5,6,7]).
link(judo,[25,26,27,28,29,30,31],[1]).
link(karate,[],[6,7,8]).
link(modern_pentathlon,[],[6,7,8]).
link(rowing,[24,25,26,27,28,29,30,31],[]).
link(rugby,[27,28,29,30,31],[1]).
link(sailing,[26,27,28,29,30,31],[1,2,3,4,5]).
link(shooting,[25,26,27,28,29,30,31],[1,2,3]).
link(skateboarding,[26,27],[5,6]).
link(sport_climbing,[],[4,5,6,7]).
link(surfing,[26,27,28,29,30,31],[1,2]).
link(table_tennis,[25,26,27,28,29,30,31],[2,3,4,5,6,7]).
link(taekwondo,[25,26,27,28],[]).
link(tennis,[25,26,27,28,29,30,31],[1,2]).
link(triathlon,[27,28],[1]).
link(volleyball,[25,26,27,28,29,30,31],[1,2,3,4,5,6,7,8,9]).
link(weightlifting,[25,26,27,28,29],[1,2,3,4,5]).
link(wrestling,[],[2,3,4,5,6,7,8]).

%% link(transportation, fromStation, toStation, time)
link(denentoshisen, chuorinkan, suzukakedai, 7).
link(denentoshisen, suzukakedai, nagatsuta, 4).
link(denentoshisen, nagatsuta, futakotamagawa, 27).
link(yokohamasen, nagatsuta, kikuna, 18).
link(ooimachisen, futakotamagawa, jiyugaoka, 8).
link(ooimachisen, jiyugaoka, oookayama, 3).
link(denentoshisen, shibuya, futakotamagawa, 12).
link(toyokosen, shibuya, jiyugaoka, 9).
link(toyokosen, kikuna, yokohama, 6).
link(yokohamasen, kikuna, yokohama, 10).
link(toyokosen, jiyugaoka, tamagawa, 3).
link(toyokosen, kikuna, tamagawa, 14).
link(megurosen, oookayama, tamagawa, 4).
link(tokyutamagawasen, tamagawa, kamata, 18).
link(ikegamisen, hatanodai, kamata, 21).
link(ooimachisen, oookayama, hatanodai, 3).
link(ooimachisen, ooimachi, hatanodai, 4).

% Assume the links are bi-directional (make it undirected)
bilink(X, Y, Z, M):-link(X, Y, Z, M).
bilink(X, Y, Z, M):-link(X, Z, Y, M).

% recognize (Slot filling based on automaton)
recognize(Ss, Slots, Flags):-goal(G),path(0, Ss, G, Slots, [0,0,0,0], Flags).
%Tokyotech
path(X,[S],Y, Slots, Flags, NewFlags):-delta(X, S, Y, Slots, Flags1), listOr(Flags, Flags1, NewFlags).
path(X,[S|Ss],Y, Slots, Flags, NewFlags)
:-delta(X, S, Z, Slots, Flags1), listOr(Flags, Flags1, NewFlags1), path(Z, Ss, Y, Slots, NewFlags1, NewFlags).
%Olympic
path(X,[S],Y, Slots, Flags, NewFlags):-delta2(X, S, Y, Slots, Flags1), listOr(Flags, Flags1, NewFlags).
path(X,[S|Ss],Y, Slots, Flags, NewFlags)
:-delta2(X, S, Z, Slots, Flags1), listOr(Flags, Flags1, NewFlags1), path(Z, Ss, Y, Slots, NewFlags1, NewFlags).
%Train
path(X,[S],Y, Slots, Flags, NewFlags):-delta3(X, S, Y, Slots, Flags1), listOr(Flags, Flags1, NewFlags).
path(X,[S|Ss],Y, Slots, Flags, NewFlags)
:-delta3(X, S, Z, Slots, Flags1), listOr(Flags, Flags1, NewFlags1), path(Z, Ss, Y, Slots, NewFlags1, NewFlags).


% think to answer for Tokyotech
think([studyplan, School, Master], [1,1,1,1], Answer):-planBy(School, Master, Plan),not(myMember(School,[0])),plans2utterance(School, Master, Plan, Answer).
think([studyplan, 0, Master], [1,1,1,1], Answer):-planBymaster(Schoolsearch, Master, Plan),plans2utterance(Schoolsearch, Master, Plan, Answer).
think([studyplan, School, 0], [1,1,1,1], Answer):-planByschool(School, Mastersearch, Plan),plans2utterance(School, Mastersearch, Plan, Answer).

% think to answer for Olympic
think([olympic, Sport, Day, Month], [1,1,1,1], Answer):-check(Sport, Day, Month, Answer).


% think engine about for Tokyotech
% search how to move
planBy(School, Master, Plan):-link(Plan, School, Master).
planBymaster(Schoolsearch, Master, Plan):-link(Plan, Schoolsearch, Master).
planByschool(School, Mastersearch, Plan):-link(Plan, School, Mastersearch).

% think to answer for Train
think([transport, From, To, normal], [1,1,1,1], Answer):-reachBy(From, To, _, Transports, Time),Hour is div(Time, 60), Min is mod(Time, 60), timetext(Hour, Min, Text), transports2utterance(Transports, Answertrain), myAppend(Answertrain, Text, Answer).
think([transport, From, To, fast], [1,1,1,1], Answer):-fastpath(From, To, Time, Transports),Hour is div(Time, 60), Min is mod(Time, 60), timetext(Hour, Min, Text), transports2utterance(Transports, Answertrain), myAppend(Answertrain, Text, Answer).
%find reachBy after know the shortest path
fastpath(From, To, Tmin, Transports):-findfastpath(From, To, Tmin),reachBy(From, To, _, Transports, Tmin).
%fastpath(From, To, Time_min, Path)
findfastpath(From, To, Tmin) :-path(From, To, Tmin, Path),
    \+ (path(From, To, Lower, OtherPath),
        Lower =< Tmin,
        OtherPath \= Path).

path(X, Y, N, PathWithTrans) :- pathsub(X, Y, N, [], _, [], PathWithTrans).
%direct route
pathsub(X, Y, N, Seen, [X], _, [[X,Line]]) :-not(myMember(X, Seen)), bilink(Line, X, Y, N).
%norikae
pathsub(X, Z, N, Seen, [X|T], Seen2, [[X,Line]|Tt]) :-not(myMember(X, Seen)),bilink(Line, X, Y, N0),pathsub(Y, Z, N1, [X|Seen], T, [[X,Line]|Seen2], Tt),not(myMember(X, T)),N is N0 + N1.


% think engine about olympic
check(Sport, Day, Month, Answer):-checkMonth(Month, Num),checkDate(Sport, Num, Day, Answer).
checkMonth(Month, 1):-Month=july.
checkMonth(Month, 2):-Month=august.
checkMonth(Month, 3):-not(myMember(Month, [july, august])).
checkDate(Sport, 1, Day, Answer):-link(Sport,A,_), myMember(Day, A),Answer=[yes].
checkDate(Sport, 1, Day, Answer):-link(Sport,A,B), not(myMember(Day, A)),not(empty(A)),not(empty(B)),Answer=[no,but,you,can,watch,Sport,on,A,july,and,B,august].
checkDate(Sport, 1, Day, Answer):-link(Sport,A,B), not(myMember(Day, A)),not(empty(A)),empty(B),Answer=[no,but,you,can,watch,Sport,on,A,july].
checkDate(Sport, 1, Day, Answer):-link(Sport,A,B), not(myMember(Day, A)),empty(A),not(empty(B)),Answer=[no,but,you,can,watch,Sport,on,B,august].
checkDate(Sport, 2, Day, Answer):-link(Sport,_,B), myMember(Day, B),Answer=[yes].
checkDate(Sport, 2, Day, Answer):-link(Sport,A,B), not(myMember(Day, B)),not(empty(A)),not(empty(B)),Answer=[no,but,you,can,watch,Sport,on,A,july,and,B,august].
checkDate(Sport, 2, Day, Answer):-link(Sport,A,B), not(myMember(Day, B)),not(empty(A)),empty(B),Answer=[no,but,you,can,watch,Sport,on,A,july].
checkDate(Sport, 2, Day, Answer):-link(Sport,A,B), not(myMember(Day, B)),empty(A),not(empty(B)),Answer=[no,but,you,can,watch,Sport,on,B,august].
checkDate(_, 3, _, Answer):-Answer=[no, olympic, is, from, 22, july, to, 9, august].

% think engine about transportation (graph search)
% search how to move
reachBy(From, To, Stations, Transports, Time):-reachBySub(From, To, [], Stations, [], Transports, Time).
reachBySub(P, P, Ss, Stations, Y, Transports, 0)
:-not(myMember(P, Ss)),myReverse([P|Ss], Stations), myReverse([[P, you_arrived]|Y], Transports).
reachBySub(From, To, [], Stations, [], Transports, Timesum)
:-bilink(Tr, From, Y, Timemid),reachBySub(Y, To, [From], Stations, [[From,Tr]], Transports, Time), Timesum is Timemid + Time.
reachBySub(From, To, Ss, Stations, [[Fr,Tr]|FrTrs], Transports, Timesum)
:-not(myMember(From, Ss)),bilink(Tr, From, Y, Timemid),reachBySub(Y, To, [From|Ss], Stations, [[Fr,Tr]|FrTrs], Transports, Time), Timesum is Timemid + Time.
reachBySub(From, To, Ss, Stations, [[Fr,Tr]|FrTrs], Transports, Timesum)
:-not(myMember(From, Ss)),bilink(Tr1, From, Y, Timemid),not(Tr==Tr1),reachBySub(Y, To, [From|Ss], Stations, [[From,Tr1],[Fr,Tr]|FrTrs], Transports, Time),Timesum is Timemid + Time.

% form answer utterance
plans2utterance(School, Master, Plan, [School, kara, Plan, wo, benkyoushite, Master, ni, susumeruyo]).

% form answer utterance
transports2utterance([[X, you_arrived]], [X, ni]).
transports2utterance([[From, By]|X], [From, kara, By, ninotte|Utterance]):- transports2utterance(X, Utterance).

% time
timetext(Hour, Min, [Min, bun, de, tsukeruyo]):-myMember(Hour,[0]).
timetext(Hour, Min, [Hour, jikan, Min, bun, de, tsukeruyo]):-not(myMember(Hour,[0])).

% some list tools
empty(List):- not(member(_,List)).

myAppend([], X, X).
myAppend([X | L], Y, [X | Z]) :- myAppend(L, Y, Z).

myReverse([], []).
myReverse([X | L], Y):-myReverse(L, Z), myAppend(Z, [X], Y).

myMember(X, [X | _]).
myMember(X, [_ | L]):-myMember(X, L).

listOr([],[],[]).
listOr([0|X], [0|Y], [0|Z]):-!,listOr(X, Y, Z).
listOr([_|X], [_|Y], [1|Z]):-listOr(X, Y, Z).
