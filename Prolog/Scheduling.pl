% knowledge base

when(a,10).
when(g,13).
when(f,9).
when(c,11).
when(d,16).
when(e,17).
when(b,12).

where(a,101).
where(g,101).
where(f,102).
where(c,102).
where(d,103).
where(e,103).
where(b,104).

enrollment(1,a).
enrollment(1,b).
enrollment(2,c).
enrollment(3,d).
enrollment(4,e). 
enrollment(5,f). 
enrollment(6,g).     
enrollment(6,h). 
enrollment(7,a).
enrollment(7,b).
enrollment(8,b).
enrollment(8,c).
enrollment(9,c).
enrollment(9,d).
enrollment(10,d).
enrollment(10,e).
enrollment(10,f).
enrollment(11,e).
enrollment(11,f).

% rules


schedule(S,P,T) :- 
	enrollment(S,C),when(C,T),where(C,P).

usage(P,T) :- 
	where(C,P),when(C,T).

conflict(C1,C2) :- 
	where(C1,P1),where(C2,P2),
	P1==P2,
	when(C1,T1),when(C2,T2),
	abs(T1-T2)<2.

meet(S1,S2) :-
	schedule(S1,P1,T1),schedule(S2,P2,T2),
	P1==P2,
	abs(T1-T2)<2.	