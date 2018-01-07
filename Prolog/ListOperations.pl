% knowledge base



% rules


intersect(L1,L2,I) :-
	findall(R,intersect4(L1,L2,[],R),Q),
	takeFirst(Q,R), 
	reverse(R,I).

intersect4([Head|Tail],L2,SubResult,Result) :-
	length(Tail, N), N >= 0,
	member(Head,L2),
	intersect4(Tail,L2,[Head|SubResult],Result).

intersect4([Head|Tail],L2,SubResult,Result) :-
	length(Tail,N), N >= 0,
	not_member(Head,L2),
	intersect4(Tail,L2,SubResult,Result).

intersect4(Element,L2,SubResult,SubResult) :-
	not_member(Element,L2).

intersect4(Element,L2,SubResult,[Element|SubResult]) :-
	member(Element,L2).



union(L1,L2,U) :-
	findall(R,union4(L1,L2,L1,R),Q),
	takeFirst(Q,Un),
	takeRest(Un,L),
	reverse(L,U).

union4(L1,[Head|Tail],SubResult,Result) :-
	length(Tail, N), N >= 0,
	member(Head,L1),
	write(Head),write(SubResult),nl,
	union4(L1,Tail,SubResult,Result).

union4(L1,[Head|Tail],SubResult,Result) :-
	length(Tail, N), N >= 0,
	not_member(Head,L1),
	write(Head),write(SubResult),nl,
	union4(L1,Tail,[Head|SubResult],Result).

union4(L1,Element,SubResult,[Element|SubResult]) :-
	not_member(Element,L1).

union4(L1,Element,SubResult,SubResult) :-
	member(Element,L1).



flatten(L1,F) :-
	findall(R,flatten3(L1,[],R),Q),
	takeFirst(Q,L).

flatten3([Head|Tail],SubResult,Result) :-
	length(Tail,N), N >= 0,
	\+is_list(Head),
	flatten3(Tail,[Head|SubResult],Result).

flatten3([Head|Tail],SubResult,Result) :-
	length(Tail,N), N >= 0,
	is_list(Head),
	flatten3(Head,SubResult,NestedResult),
	flatten3(Tail,NestedResult,Result).


flatten3(Element,SubResult,Result) :-
	reverse(SubResult,Result).


not_equal(Element1,Element2) :-
	Element1 \== Element2.

not_member(Element,List) :-
	\+member(Element,List).


takeFirst([Head|Tail],Head).

takeRest([Head|Tail],Tail).