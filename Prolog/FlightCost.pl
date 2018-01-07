% knowledge base

flight(edremit,edirne,5).
flight(edremit,erzincan,7).
flight(istanbul,izmir,3).
flight(izmir,antalya,1).
flight(antalya,diyarbakir,5).
flight(diyarbakir,konya,1).
flight(ankara,konya,8).
flight(izmir,ankara,6).
flight(istanbul,ankara,5).
flight(istanbul,trabzon,3).
flight(trabzon,ankara,2).
flight(kars,konya,5).
flight(kars,gaziantep,3).


% rules

connected(X,Y,C) :- flight(X,Y,C) ; flight(Y,X,C).

route(X,Y,C) :- route6(X,Y,[X],Path,0,C).

croute(X,Y,C) :- findall(Cost,route(X,Y,Cost),Costs), minimum(Costs,C).


route6(X,Y,Visited,[Y|Visited],C,R) :- 
       connected(X,Y,C1),
       duplicate_control([Y|Visited]),
       R is C+C1.

route6(X,Y,Visited,Path,C,R) :-
       connected(X,Z,C1),           
       not_equal(Y,Z),
       not_member(Z,Visited),
       route6(Z,Y,[Z|Visited],Path,C,C2),
       R is C+C1+C2.

minimum(List,Min) :- 
	sort(List,Sorted),
	Sorted = [Min|Rest].

duplicate_control(List) :-
    sort(List, RemovedDublicateds),
    length(RemovedDublicateds, N),
    length(List, N).

not_equal(Element1,Element2) :-
	Element1 \== Element2.

not_member(Element,List) :-
	\+member(Element,List).