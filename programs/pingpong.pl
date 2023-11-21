:- table d/1,e/1.

top :- abolish_all_tables, d(_), fail.
top.

% Two mutually recursive predicates:
d(X) :- e(Y), Y < 20000, X is Y + 1.
d(0).

e(X) :- d(Y), Y < 20000, X is Y + 1.
e(0).
