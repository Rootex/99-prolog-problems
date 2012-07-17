% 1.01 Find the last element of a list
my_last(X,[X]) :- !.
my_last(X,[_|T]) :- my_last(X,T).

% 1.02 Find the last but one element of a list.
lst_but_one(X, [X,_]) :- !.
lst_but_one(X, [_|T]) :- lst_but_one(X,T).

% 1.03 Find the K'th element of a list.
element_at(X,[X|_], 1) :- !.
element_at(X,[_|T], N) :-
        M is N - 1,
        element_at(X, T, M).

% 1.04 Find the number of elements of a list
my_length(1, [_]) :- !.
my_length(X, [_|T]) :-
        my_length(Y,T),
        X is Y+1.

% 1.05 Reverse a list.
my_reverse(L, R) :- my_reverse(L,[],R).
my_reverse([H],L,[H|L]) :- !.
my_reverse([X|T],L,R) :- my_reverse(T,[X|L],R).

% 1.06 Find out wether a list is a palindrome.
palindrome(L) :- reverse(L,L).

% 1.07 Flatten a nested list structure.
my_flatten([H], X) :-
        is_list(H),
        my_flatten(H,X).
my_flatten([H], [H]).
my_flatten([H|T], X) :-
        is_list(H),
        my_flatten(H, Y),
        my_flatten(T, Z),
        append(Y,Z,X).
my_flatten([H|T], [H|X]) :- my_flatten(T,X).

% 1.08 Elimiate consecutive duplicates of list elements.
compress([H], [H]).
compress([H,H|T], X) :- compress([H|T],X).
compress([H|T], [H|X]) :- compress(T, X).

% 1.09 Pack consecutive duplicates of list elements into sublists.
pack([], []).
pack([H,H|T], X) :- pack([[H,H]|T],X).
pack([[H|Hs]|[H|T]], X) :- pack([[H,H|Hs]|T], X).
pack([H|T], [[H]|X]) :-
        not(is_list(H)),
        pack(T,X).
pack([H|T], [H|X]) :- pack(T,X).

% 1.10 Run-length encoding of a list.
encode([], []) :- !.
encode(L, [[N,X]|Y]) :-
        pack(L,[[X|Xs]|T]),
        length([X|Xs],N),
        !,encode(T, Y).

% 1.11 Modified run-length encoding.
encode_modified([], []).
encode_modified(L,X) :-
        encode(L, Y),
        enc_mod(Y,X).

enc_mod([], []).
enc_mod([[1,H]|T], [H|X]) :- enc_mod(T, X), !.
enc_mod([H|T], [H|X]) :- enc_mod(T,X).

% 1.12 Decode a run-length encoded list.
decode([], []).
decode([[1,H]|T], [H|X]) :- decode(T,X).
decode([[N,H]|T], [H|X]) :-
        M is N-1,
        decode([[M,H]|T], X), !.
decode([H|T], [H|X]) :- decode(T, X).

% 1.13 Run-length encoding of a list (direct solution).
encode_direct([], []).
encode_direct([H,H|T], X) :- encode_direct([[2,H]|T], X), !.
encode_direct([[N,H],H|T], X) :-
        M is N + 1,
        encode_direct([[M,H]|T], X).
encode_direct([H|T], [H|X]) :- encode_direct(T, X).

% 1.14 Diplicate the elements of a list.
duplicate([H], [H,H]).
duplicate([H|L], [H,H|X]) :- duplicate(L, X).

% 1.15 Duplicate the elements of a list a given number of times.
duplicate([H], 1, [H]) :- !.
duplicate([H], N, [H|X]) :-
        M is N - 1,
        M > 0,
        duplicate([H], M, X).
duplicate([H|T], N, X) :-
      duplicate([H], N, Y),
      duplicate(T, N, Z),
      !, append(Y, Z, X).

% 1.16 Drop every N'th element from a list.
drop(L, N, L) :-
        length(L, Length),
        Length < N.
drop(List, N, NthDropped) :-
      length(List,Length),
      Length >= N,
      length(WithNth, N),
      append(WithNth, Tail, List),
      drop(Tail, N, Rest),
      length(NthElem, 1),
      append(WithoutNth, NthElem, WithNth),
      append(WithoutNth, Rest, NthDropped).

% 1.17 Split a list into two parts; the length of the first part is given.
split(L, 0, [], L).
split([H|T], N, [H|X], L2) :-
        N > 0,
        M is N - 1,
        split(T, M, X, L2).

% 1.18 Extract a slice from a list.
slice([H|_], 1, 1, [H]).
slice([H|T], 1, To, [H|X]) :-
        N is To - 1,
        slice(T, 1, N, X).
slice([_|T], From, To, L) :-
        N is From - 1,
        M is To - 1,
        slice(T, N, M, L).

% 1.19 Rotate a list N places to the left.
rotate([H|T], 1, X) :-
        append(T, [H], X), !.
rotate([H|T],N,X) :-
        append(T, [H], Y),
        N1 is N - 1,
        rotate(Y, N1, X).

% 1.20 remove the K'th element from a list.
remove_at(H, [H|T], 1, T).
remove_at(X, [H|T], N, [H|R]) :-
        N1 is N - 1,
        remove_at(X, T, N1, R).

% 1.21 Insert an element at a given position into a list.
insert_at(A, L, 1, [A|L]).
insert_at(A, [H|T], N, [H|R]) :-
        N1 is N - 1,
        insert_at(A, T, N1, R).

% 1.22 Create list containing all intergers within a given range.
range(N, N, [N]) :- !.
range(N, M, [N|T]) :-
        N1 is N + 1,
        range(N1, M, T).

% 1.23 Extract a given number of randomly selected elements from a list.
rnd_select(L, 1, [H]) :-
        length(L, Length),
        R is random(Length) + 1,
        remove_at(H, L, R, _).
rnd_select(L, N, [H|T]) :-
       length(L, Length),
       R is random(Length) + 1,
       remove_at(H, L, R, X),
       N1 is N - 1,
       rnd_select(X, N1, T).

% 1.24 Lotto: Draw N different random numbers from the set 1..M.
lotto(N, M, X) :-
        range(1, M, L),
        rnd_select(L, N, X).

% 1.25 Generate a random permutation of the elements of a list.
rnd_permutation(L, X) :-
        length(L, S),
        rnd_select(L,S,X).

% 1.26 Generate the combinations of K distinct objects chosen from the N elements
%      of a list.
combination(1, [H|_], [H]).
combination(N, [H|T], [H|C]) :-
        N1 is N - 1,
        N1 > 0,
        combination(N1, T, C).
combination(N, [_|T], C) :- combination(N, T, C).

% 1.27 Group the elements of a set into disjoint subset.
% a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2,
%    3 and 4 persons?
group3(L, G1, G2, G3) :-
        combination(2, L, G1),
        combination(3, L, G2),
        combination(4, L, G3),
        forall(member(M1, G1),
               forall(member(M2, G2),
                      forall(member(M3, G3),
                             (M1 \== M2,
                              M2 \== M3,
                              M1 \== M3)))).

% b) Generalize the above predicate in a way that we can specify a list of group
%    sizes and the predicate will return a list of groups.
group(_, [], []).
group(List, [H|T], [G|Groups]) :-
        combination(H, List, G),
        group(List, T, Groups),
        forall(member(Mg, Groups),
               forall(member(M1, Mg),
                      forall(member(M2, G),
                             M2 \== M1))).

% 1.28 Sorting a list of lists according to length of sublists
% a) The objective is to sort the elements of InList according to their length.
lsort([], []).
lsort([H|T],[H|R]) :-
        length(H, Lh),
        forall(member(M, T),
               (length(M, Lm),
                Lh =< Lm)),
        lsort(T, R), !.
lsort([F,S|T], R) :-
        append(T,[F],X),
        lsort([S|X], R).
        
% b) Again, we suppose that a list (InList) contains elements that are lists
%    themselves. But this time the objective is to sort the elements of InList
%    according to their length frequency.
lfsort([], []).
lfsort([H|T], [H|R]) :-
        freq(H,T,Fh),
        forall(member(M, T),
               (freq(M, T, Fm),
                Fh =< Fm)),
        lfsort(T,R), !.
lfsort([F,S|T], R) :-
        append(T, [F], X),
        lfsort([S|X], R).

freq(_, [], 0).
freq(F, [H|T], N) :-
        F == H,
        freq(F,T, M), !,
        N is 1 + M.
freq(F, [_|T], N) :-
        freq(F,T,N).