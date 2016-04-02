% -*- mode: prolog -*-

%% CPSA tools in Prolog

%% Translates the output of the cpsasas program into the syntax of
%% Razor.

%% Known to work in SWI-Prolog, but not with GNU Prolog.

%% To handle the subsort relations of the message algebra, add the
%% following axioms.

%%    akey(t) => mesg(t);
%%    skey(t) => mesg(t);
%%    name(t) => mesg(t);
%%    text(t) => mesg(t);
%%    data(t) => mesg(t);

%% The precedes axioms are:

%%    sprec(x, y) => prec(x, y);
%%    prec(x, y) & prec(y, z) => prec(x, y);

%% Copyright (c) 2011 The MITRE Corporation
%%
%% This program is free software: you can redistribute it and/or
%% modify it under the terms of the BSD License as published by the
%% University of California.

:- module(razor, [razor/2]).

:- use_module(pp).
:- use_module(sexpr).

%% razor(+In, +Out) Translates cpsasas program output in file In,
%% into the syntax of Razor and places it in file Out.
razor(In, Out) :-
	sexpr:read_sexpr_list(In, Forms),
	open(Out, write, Stream),
	top_forms_pp(Stream, Forms),
	close(Stream).

%% Ignore all forms except the ones that start with defgoal.
top_forms_pp(_, []).
top_forms_pp(Out, [[defgoal, _, [forall|Rest]]|Forms]) :-
	!,
	form_to_pretty([forall|Rest], Pretty),
	pp:pr(Out, 72, Pretty),
	put_char(Out, ';'),
	nl(Out),
	nl(Out),
	top_forms_pp(Out, Forms).
top_forms_pp(Out, [_|Forms]) :-
	top_forms_pp(Out, Forms).

%% Formula classification

form_to_pretty([=|Terms], Pretty) :-
	!,
	equal(Terms, Pretty).
form_to_pretty([implies|Forms], Pretty) :-
	!,
	implication(Forms, Pretty).
form_to_pretty([and|Forms], Pretty) :-
	!,
	junction(and, Forms, Pretty).
form_to_pretty([or| Forms], Pretty) :-
	!,
	junction(or, Forms, Pretty).
form_to_pretty([forall|Rest], Pretty) :-
	!,
	quantifier(forall, Rest, Pretty).
form_to_pretty([exists|Rest], Pretty) :-
	!,
	quantifier(exists, Rest, Pretty).
form_to_pretty(Forms, Pretty) :-
	atomic_form(Forms, Pretty).

%% In general, breaks are before binary operators.

%% Equality

equal([L, R], Pretty) :-
	term_to_pretty(L, Left),
	pp:brk(1, Brk),
	pp:atm('= ', Op),
	term_to_pretty(R, Right),
	pp:blo(0, [Left, Brk, Op, Right], Pretty).

%% Implication

implication([H, C], Pretty) :-
	form_to_pretty(H, Hypoth),
	pp:brk(1, Brk),
	pp:atm('=> ', Op),
	form_to_pretty(C, Concl),
	pp:blo(0, [Hypoth, Brk, Op, Concl], Pretty).

%% Conjunction and disjunction

junction(and, [], Pretty) :-
	pp:atm('Truth', Pretty).
junction(or, [], Pretty) :-
	pp:atm('Falsehood', Pretty).
junction(_, [Form], Pretty) :-
	!,
	form_to_pretty(Form, Pretty).
junction(Kind, [Form|Forms], Pretty) :-
	form_to_pretty(Form, First),
	junction_op(Kind, Op),
	junction_rest(Op, Forms, Pretties),
	pp:blo(0, [First|Pretties], Pretty).

junction_op(and, Pretty) :-
	pp:atm('&', Pretty).
junction_op(or, Pretty) :-
	pp:atm('|', Pretty).

junction_rest(_, [], []).
junction_rest(Op, [Form|Forms], [Brk, Op, Sp, Next|Pretties]) :-
	pp:brk(1, Brk),
	pp:atm(' ', Sp),
	form_to_pretty(Form, Next),
	junction_rest(Op, Forms, Pretties).

%% Quantifiers forall and exists

quantifier(_, [[], Body], Pretty) :-
	!,
	form_to_pretty(Body, Pretty).
quantifier(Kind, [Decls, Body], Pretty) :-
	quantifier_op(Kind, Op),
	decls(Decls, SVars),
	quantifier_preds(SVars, Vars, Preds),
	quantifier_body(Kind, Preds, Body, SBody),
	quantifier_rest(Kind, Op, Vars, SBody, Pretties),
	pp:blo(2, Pretties, Pretty).

quantifier_op(forall, Pretty) :-
	pp:atm(all, Pretty).
quantifier_op(exists, Pretty) :-
	pp:atm(exists, Pretty).

%% Collect vars in declarations.

decls(Decls, SortedVars) :-
	decls(Decls, SortedVars, []).

decls([], SVars, SVars).
decls([Decl|Decls], SVars, End) :-
	decl(Decl, _, SVars, Middle),
	decls(Decls, Middle, End).

decl([Sort], Sort, SVars, SVars) :-
	atom(Sort).
decl([Var|Decl], Sort, [(Var,Sort)|SVars], End) :-
	atom(Var),
	decl(Decl, Sort, SVars, End).

quantifier_preds([], [], []).
quantifier_preds([(Var,Sort)|SVars], [Var|Vars], [[Sort, Var]|Preds]) :-
        quantifier_preds(SVars, Vars, Preds).

quantifier_body(_, [], Body, Body) :-
        !.
quantifier_body(exists, Preds, [and|Forms], [and|Conj]) :-
        !,
        append(Preds, Forms, Conj).
quantifier_body(exists, Preds, Body, [and|Conj]) :-
        append(Preds, [Body], Conj).
quantifier_body(forall, Preds, [implies, [and|Forms], Body],
		[implies, [and|Conj], Body]) :-
        !,
        append(Preds, Forms, Conj).
quantifier_body(forall, Preds, [implies, Form, Body],
		[implies, [and|Conj], Body]) :-
        !,
        append(Preds, [Form], Conj).
quantifier_body(forall, Preds, Body, [implies, [and|Preds], Body]).

quantifier_rest(_, _, [], Body, [Pretty]) :-
        !,
        form_to_pretty(Body, Pretty).
quantifier_rest(forall, _, _, Body, [Pretty]) :-
        form_to_pretty(Body, Pretty).
quantifier_rest(exists, Op, [Var|Vars], Body, [Op|Pretties]) :-
        quantifier_rest_exists(Var, Vars, Body, Pretties).

quantifier_rest_exists(Var, [], Body, [Sp, V, Period, Brk, Pretty]) :-
	pp:atm(' ', Sp),
	term_to_pretty(Var, V),
        pp:atm('.', Period),
	pp:brk(1, Brk),
        form_to_pretty(Body, Pretty).
quantifier_rest_exists(Var, [Vn|Vars], Body, [Brk, V| Pretties]) :-
	pp:brk(1, Brk),
	term_to_pretty(Var, V),
        quantifier_rest_exists(Vn, Vars, Body, Pretties).

% Atomic formulas

atomic_form([Pred], Pretty) :-
	atom(Pred),
	pp:atm(Pred, Pretty).
% Role position predicates
atomic_form([p, Role, Pos, Term], Pretty) :-
        string(Role),
	integer(Pos), !,
	role_pos_symbol(Role, Pos, Symbol),
	pp:atm(Symbol, P),
	pp:atm('(', Left),
	term_to_pretty(Term, T),
	terms_to_pretty([], Ts),
	pp:blo(2, [P, Left, T|Ts], Pretty).
% Role parameter predicates
atomic_form([p, Role, Var, Term1, Term2], Pretty) :-
        string(Role),
	string(Var), !,
	role_param_symbol(Role, Var, Symbol),
	pp:atm(Symbol, P),
	pp:atm('(', Left),
	term_to_pretty(Term1, T),
	terms_to_pretty([Term2], Ts),
	pp:blo(2, [P, Left, T|Ts], Pretty).
% All other predicates with nonzero arity
atomic_form([Pred, Term|Terms], Pretty) :-
	atom(Pred),
	symbol(Pred, Symbol),
	pp:atm(Symbol, P),
	pp:atm('(', Left),
	term_to_pretty(Term, T),
	terms_to_pretty(Terms, Ts),
	pp:blo(2, [P, Left, T|Ts], Pretty).

role_pos_symbol(Role, Pos, Symbol) :-
        atom_chars(p_, L1),
	string_to_atom(Role, Atom),
	atom_chars(Atom, L2),
	number_chars(Pos, L3),
	append(L2, ['_'|L3], L4),
	append(L1, L4, L5),
	hyphen(L5, L6),
        atom_chars(Symbol, L6).

role_param_symbol(Role, Var, Symbol) :-
        atom_chars(p_, L1),
	string_to_atom(Role, Atom1),
	atom_chars(Atom1, L2),
	string_to_atom(Var, Atom2),
	atom_chars(Atom2, L3),
	append(L2, ['_'|L3], L4),
	append(L1, L4, L5),
	hyphen(L5, L6),
        atom_chars(Symbol, L6).

terms_to_pretty([], [Right]) :-
	pp:atm(')', Right).
terms_to_pretty([Term|Terms], [Comma, Brk, T|Pretties]) :-
	pp:atm(',', Comma),
	pp:brk(1, Brk),
	term_to_pretty(Term, T),
	terms_to_pretty(Terms, Pretties).

term_to_pretty(Term, Pretty) :-
	load_term(Term, Internal),
	term_pp(Internal, Pretty).

%% Load a term using CPSA's parsing rules for terms.  Also, convert
%% variables into uppercase atoms, and constants into lowercase atoms.

load_term(Term, Internal) :-
	atom(Term),
	symbol(Term, Internal).
load_term(Term, '\'lsn') :-
	string(Term),
	string_length(Term, 0),
	!.
load_term(Term, Internal) :-
	string(Term),
	string_to_atom(Term, Atom),
	symbol(Atom, Symbol),
        atom_chars(Symbol, List),
        atom_chars(Internal, ['\''|List]).
load_term(Term, Internal) :-
	integer(Term),
	Term >= 0,
        number_chars(Term, List),
	atom_chars(Internal, ['\'','_'|List]).
load_term([privk|Terms], Internals) :-
	!,
	load_term([invk, [pubk|Terms]], Internals).
load_term([cat,Term], Internals) :-
	!,
	load_term(Term, Internals).
load_term([cat,Term|Terms], [cat, X, Y]) :-
	!,
	load_term(Term, X),
	load_term([cat|Terms], Y).
load_term([enc|Terms], Internal) :-
	!,
	load_enc(Terms, Internal).
load_term([Term|Terms], [Term|Internals]) :-
	atom(Term),
	load_terms(Terms, Internals).

load_enc(Terms, [enc, X, Y]) :-
	split(Terms, As, B),
	load_term([cat|As], X),
	load_term(B, Y).

symbol(Atom, Symbol) :-
	atom_chars(Atom, Chars),
	hyphen(Chars, Parts),
	atom_chars(Symbol, Parts).

hyphen([], []).
hyphen(['-'|Rest], ['_'|Tail]) :-
	!,
	hyphen(Rest, Tail).
hyphen([First|Rest], [First|Tail]) :-
	hyphen(Rest, Tail).

split([X], [], X).
split([X, Y|Z], [X|A], B) :-
	split([Y|Z], A, B).

load_terms([], []).
load_terms([T|Ts], [I|Is]) :-
	load_term(T, I),
	load_terms(Ts, Is).

%% Pretty print a term.

term_pp(Term, Pretty) :-
	atom(Term),
	pp:atm(Term, Pretty).
term_pp([Term], Pretty) :-
	atom(Term),
	pp:atm(Term, Pretty).
term_pp([Pred, Term|Terms], Pretty) :-
	atom(Pred),
	pp:atm(Pred, P),
	pp:atm('(', Left),
	term_pp(Term, Arg),
	args_pp(Terms, Pretties),
	blo(2, [P, Left, Arg|Pretties], Pretty).

args_pp([], [Pretty]) :-
	pp:atm(')', Pretty).
args_pp([Term|Terms], [Comma, Brk, Pretty|Pretties]) :-
	pp:atm(',', Comma),
	pp:brk(1, Brk),
	term_pp(Term, Pretty),
	args_pp(Terms, Pretties).
