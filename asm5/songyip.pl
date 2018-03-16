% Jeremy Fischer 932-447-681
% Yipeng Song    932-470-819
% Peter Dorich   932-441-378

% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).


% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- parent(X,_), female(X).
isFather(X) :- parent(X,_), male(X).


% 3. Define a predicate `grandparent/2`.
grandparent(X,Z) :- parent(X,Y), parent(Y,Z).  


% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(Z,X), parent(Z,Y), X \= Y.


% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- sibling(X,Y), male(X).
sister(X,Y) :- sibling(X,Y), female(X).


% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- married(X,Z), sibling(Z,Y), X \= Y.
siblingInLaw(X,Y) :- sibling(X,Z), married(Z,Y), X \= Y.


% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- sister(X,Z), parent(Z,Y).
aunt(X,Y) :- siblingInLaw(X,Z), parent(Z,Y), female(X).
uncle(X,Y) :- brother(X,Z), parent(Z,Y).
uncle(X,Y) :- siblingInLaw(X,Z), parent(Z,Y), male(X).


% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- child(X,Z), sibling(Z,W), parent(W,Y).


% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), parent(Z,Y). 


% Extra credit: Define the predicate `related/2`.
temp(X,Y) :- parent(Y,X).
temp(X,Y) :- parent(A,X), temp(A,Y).

related(X,Y) :- child(X,Y).
related(X,Y) :- parent(X,Y).
related(X,Y) :- sibling(X,Y).
related(X,Y) :- ancestor(X,Y).
related(X,Y) :- temp(X,Y).
related(X,Y) :- temp(X,A), sibling(A,Y), X \= Y.
related(X,Y) :- ancestor(X,A), sibling(A,Y), X \= Y.
related(X,Y) :- ancestor(X,B), related(B,Y), X \= Y.



%%
% Part 2. Language implementation
%%

bool(t).
bool(f).

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.

%Below are test Commands:

%Test1 cmd("hello",[4],S). 
%	result should be S = ["hello", 4].

%Test2 cmd(4,S,[4,"goodbye"]). 
%	result should be S = ["goodbye"].

%Test3 cmd(add,[2,3,4],S). 
%	result should be S = [5, 4].

%Test4 cmd(lte,[2,3,4],S). 
%	result should be S = [t, 4].

%Test5 cmd(lte,[5,3,t],S). 
%	result should be S = [f, t]

%Test6: prog([if([add],[3])], [t,5,2], S). 
%	result should be S = [7]
%Test7: prog([if([add],[3])], [f,5,2], S). 
%	result should be S = [3,5,2]


cmd(C,S1,S2)					:-	number(C), S2 = [C|S1].
cmd(C,S1,S2)					:-	string(C), S2 = [C|S1].
cmd(C,S1,S2)					:-	bool(C), S2 = [C|S1].
cmd(add,[NumA,NumB|S1], S2)		:-	S2 = [Addition|S1], Addition is NumA + NumB.
cmd(lte,[BoolA,BoolB|S1],S2)	:-	S2 = [t|S1], BoolA =< BoolB.
cmd(lte,[BoolA,BoolB|S1],S2)	:-	S2 = [f|S1], BoolA > BoolB.
cmd(if(P1,_),[t|S1],S2)			:- prog(P1,S1,S2).
cmd(if(_,P2),[f|S1],S2)			:- prog(P2,S1,S2).


% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.

%Below are test Commands:

%Test prog([3,4,add],[],S).
%	result should be S = [7] .

%Test prog([3,4,add,6,lte,9],[],S).
%	result should be S = [9, t] .

%Test prog([if(["foo"],[3]),4],[t,5],S).
%	result should be S = [4, "foo", 5] .

%Test prog([2,lte,if(["foo"],[3]),4],[1],S).
%	result should be S = [4, 3] .

prog([CurCmd], S1, S2) 			:- cmd(CurCmd,S1,S2).
prog([CurCmd|TailCmds], S1, S3) :- cmd(CurCmd,S1,S2), prog(TailCmds,S2,S3).
