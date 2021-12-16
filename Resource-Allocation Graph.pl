

processes([p1,p2,p3,p4]).

available_resources([[r1, 0], [r2,0]]).

allocated(p1, [r2]).
allocated(p2, [r1]).
allocated(p3, [r1]).
allocated(p4, [r2]).

requested(p1, [r1]).
requested(p3, [r2]).
%requested(p4, [r2]).
%requested(p2, [r2]).


count([], 0).
count([_| T], N) :-
  count(T, N1),
  N is N1 + 1,
  !.

count_processes(N) :- 
	processes(X),
	count(X,N),!.

get_allo_req(X , AR):-
	allocated(X,_),requested(X,_) -> AR = [1,1];
	allocated(X,_) -> AR = [1,0];
	requested(X,_) -> AR = [0,1]. 


get_allo(X , Nallo ):-
	(allocated(X,[r1]),allocated(X,[r2])) -> Nallo = [1,1] ;
	(allocated(X,[r1])) -> Nallo = [1,0];
	(allocated(X,[r2])) -> Nallo = [0,1];
	not(allocated(X,_)) -> Nallo = [0,0].


get_req(X , Nreq):-
	(requested(X,[r1]),requested(X,[r2])) -> Nreq = [1,1] ;
	(requested(X,[r1])) -> Nreq = [1,0];
	(requested(X,[r2])) -> Nreq = [0,1];
	not(requested(X,_)) -> Nreq = [0,0].

	
get_available(A):-
	available_resources([[_,V1|_],[_,V2|_]|_]),
	A = [V1,V2].


get_processes(P):-
	processes(P).


add_tail([],X,[X]).
add_tail([H|T],X,[H|L]):-
	add_tail(T,X,L).


safe_state(X):-
	count_processes(Size),
	get_available(Available),
	get_processes(P),
	add_to_sequence(P,0,Size,Available,_,X).


add_to_sequence([],_,_,_,TR,TR):-!.
add_to_sequence(_,S,S,_,_,false):-!.
add_to_sequence([H|T],C,S,[Fa,Sa],Seq,RESULT):-
	not(C is S),
	get_req(H,[Fr,Sr]),
	( ((Fr =< Fa ) , ( Sr =< Sa )) -> 
		(	
			get_allo(H,[Fall,Sall]),
			Nfa = Fa + Fall,
			Nsa = Sa + Sall,
			add_tail(Seq,H,Seq1),
			add_to_sequence(T,C,S,[Nfa,Nsa],Seq1,RESULT)
		);
		(	C2 is C+1,
			add_tail(T,H,T2),
			add_to_sequence(T2,C2,S,[Fa,Sa],Seq,RESULT)
		)
	),!.
