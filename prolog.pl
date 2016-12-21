%prolog.pl

%winter 2012

sorted([]).
sorted([_]).
sorted([A,B|T]) :- (B >= A), sorted([B|T]).

sortedme(L1, L2) :- permutation(L1, L2), sorted(L2).

split([], [], []).
split([X], [X], []).
split([X | T], [X|T2] , L3  ) :- split(T, L3, T2).

merge([],L,L).
merge(L,[],L).
merge( [H1|T1] , [H2|T2] , [H1|R] ) :-  (H1 < H2), merge(T1, [H2|T2], R) .
merge( [H1|T1], [H2|T2] , [H2|R] )  :-  (H1 >= H2), merge( [H1|T1], T2,R  ).

%merge([],L,L).
%merge(L,[],L).
%merge( [H1|T1], [H2|T2], [H1|R] ) :-  H1<H2, merge(T1, [H2|T2], R).
%merge( [H1|T1], [H2|T2], [H2|R] ) :-  H1>=H2, merge( [H1|T1], T2, R).

merge_sort( [], [] ).
merge_sort( [A], [A]).
merge_sort(L,S) :- split(L, [H1|T1], [H2|T2]),  (H1 < H2), merge(T1,T2,R), merge_sort(R,S,A).



%winter 2013

remove_all(X,[],[]).
remove_all(X,[X|T], Y) :- remove_all(X,T, Y).
remove_all(X,[H|T], [H|T2]) :- X \= H, remove_all(X,T, T2 ).

remove_first(X,[],[]).
remove_first(X,[X|T], T).
remove_first(X,[H|T], [H|T2]) :- X \= H , remove_first(X,T, T2 ).


prefix( [] , _ ).
prefix( [P|F] ,[P|T] ) :-  prefix(F, T).

segment( A , B ) :- prefix(A, B).
segment( A , [H|T] ) :- segment(A, T).






%fall 2013

link(san_diego, seattle).
link(seattle, dallas).
link(dallas, new_york).
link(new_york, chicago).
link(new_york, seattle).
link(chicago, boston).
link(boston, san_diego).

path_2(A,B) :- link(A,C),link(C,B). 

path_3(A,B) :- link(A,C),link(C,D),link(D,B).

% case for N = 1
path_N(A,B,N) :- N = 1, link(A,B).
% case for N > 1
path_N(A,B,N) :- N > 1, link(A,X), R is N -1, path_N(X,B,R).

path(A, B) :- path_helper(A, B, [A]).
% In path_helper below, Seen is the cities we have see so far, so we
% can avoid cycles.
path_helper(A, B, Seen) :- link(A,B), not(member(B, Seen)).
path_helper(A, B, Seen) :- link(A,C),not(member(C, Seen)) , path_helper( C , B , [C|Seen] ).





















%spring 2013
%PROBLEM 3
zip([], [], []).
zip([X|Xs], [Y|Ys], [[X,Y]|Zs]) :- zip(Xs,Ys,Zs).
%PROBLEM 4
%prolog- recursion is always 
part([],_ ,[],[]).
part( [L|Ls] , P , R1 , R2 ) :- L =< P , R1 = [L|A], part(Ls, P, A, R2).
part( [L|Ls] , P , R1 , R2 ) :- L > P , R2 = [L|B] , part(Ls, P, R1, B).
%quicksort
qsort([],[]).
qsort([L|Ls],R) :-  part(Ls,L, R1, R2), qsort(R1,L1), qsort(R2,L2), append(L1,[L|L2], R).

%qsort([], []).
%qsort([H|T], R) :- part(T,H,L1,L2), qsort(L1,R1), qsort(L2,R2), append(R1, [H|R2], R). 
