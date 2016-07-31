/* -*-Prolog-*- source code */
/* $Id: program.pl,v 2.0 2012/02/29 17:03:04 reiaaoyama Exp $ */

/* Main Database definition:  */
/*   case(ID, PR, JunosVersion, Series, Severity). */
/*   rcpr(PR, Class, Category, Type, RCPR). */
/*   duppr(PRkey, [DupPRs]). */


/* Utilities */

/*
not(P) :- call(P),!,fail.
not(P).
*/

sp :- !,write(' ').

bar :- !,write('|').

prntable :- write('{table-plus}'),nl.

prnheader1 :- !,
	prntable,
	write('|| PR || PSN Value '),
	write('|| S1 || S2 || S3 || Total ||').

prnheader :- prnheader1,nl.

prnheaderrc :- prnheader1,write(' Root Cause || Category ||'),nl.

/*-----------------------------------------------*/
/* 

    Oh, I see.  fa() is for mapping database to ances()
    This way we don't have to write separate function to
    find ancestor of each database.
    It looks like we currently use "rcpr", and "duppr" 
    database in calculating ancestor()

*/

fa(rc,X,Y) :- rcpr(Y,_,_,_,X).


fa(duppr,X,Y) :- duppr(X,Z),member(Y,Z).
fa(duppr,X,X) :- !,duppr(X,[]).

/* finding ancestries.  4 generations is probably enough */
ances(DB,X,Y) :- fa(DB,X,Y).
ances(DB,X,Y) :- fa(DB,X,Z),fa(DB,Z,Y).
ances(DB,X,Y) :- fa(DB,X,A),fa(DB,A,B),fa(DB,B,Y).
ances(_,X,X) :- !.


setDupPR(KeyPR,Result) :-
	findall(Child,ances(duppr,KeyPR,Child),List),
	append([KeyPR],List,Result).

child(PRans,List) :- 
	findall(PR,ances(PRans,PR),List).

uniq([],[]).
uniq([Hi|Ti],Ro) :- member(Hi,Ti),!,uniq(Ti,Ro).
uniq([Hi|Ti],[Hi|Ro]) :- !,uniq(Ti,Ro).


dupcase(Case,PR,DupOf) :-  case(Case,PR,_,_,_),
	fa(duppr,DupOf,PR).

doone :- dupcase(C,P,D),
	write(C),write(' '),write(P),write(' '),write(D),nl.

doall :- findall(_,doone,_).

muprl(UPRL) :- findall(PR,case(_,PR,_,_,_),PrL),uniq(PrL,UPRL).
 
/* just a sum of a list of number */
/* it would be nice to make sure that the member is number */

sumlist([],0).
sumlist([X|Y],FR) :- sumlist(Y,R), FR is X + R.

/* constant for severity value */
sevalue(1,8).
sevalue(2,1).
sevalue(3,0.31).
sevalue(_,0).

/* count occurences of X in a List */

lcount(_,[],0) :- !.
lcount(X,[X|Y],Count) :- !,lcount(X,Y,SubCount),
	Count is SubCount + 1.
lcount(X,[_|Y1],Count) :- lcount(X,Y1,Count),!.

/* This procedure will find PSN value for a give PR */
/* we only limit out number to PSN > 12 */

prpsn :- muprl(List),
	sort(List,SList),
	member(PR,SList),
	findall(X,(case(_,PR,_,_,Sev),sevalue(Sev,X)),XCList),
	/* length(XCList,Xlen), */ 
	sumlist(XCList,PSN),
	PSN > 12,
/*	uniq(XCList,UCList), */
	findall([Member,Count],(member(Member,[8,1,0.31,0]),
	   lcount(Member,XCList,Count)),Histo),
	bar,
	write(PR),sp,bar,sp,
	format('~3f',[PSN]),sp,
	bar,sp, write(Histo),bar,
	nl.

psn :- prpsn,fail.


/* The following section has to do with finding a key PR */
/* that other PRs will be considered a duplication of  */
/* It is actually pretty convoluted...... */

/* and it does not seems to work */

caseddup(C,Sev,PR,KeyPR) :-  case(C,PR,_,_,Sev),
	findall(X,ances(duppr,X,PR),List),
	uniq(List,Ulist),
	sort(Ulist,[KeyPR|_]).



/* Print out a table of Case/Severity/PR/KeyPR */
/*   It has fail at the end which looks kind of strange. */
/*   But what it does it to print out all known fact about */
/*   Case and what we think the dup PRs is. */

/* prncasedup() is for printing a human readable table */
prncasedup :-  caseddup(C,Sev,PR,KeyPR),
	write(C),sp,write(Sev),sp,write(PR),sp,write(KeyPR),nl,fail.

/* addCaseDup - to assert this fact into database      */
/*    It should help speed up the calculation process. */
/*    As the result of "addCaseDup.", we should have a */
/*    number of new facts added to our dynamic database*/
/*    It is currently not saved.  It would be a good   */
/*    exercise to try to do so. */
/*    BTW, the "fail" at the end is always strange.    */
/*    It is just a way to force backtracking.          */

addCaseDup :- retractall(cddup(_,_,_,_)),fail.
addCaseDup :- caseddup(C,Sev,PR,KeyPR),
	assertz(cddup(C,Sev,PR,KeyPR)),fail.

/* calulate a PSN value of a PR after ddup */
/* First, we find out a list of PR in our dup list */

dupprpsn :- 
	prnheaderrc,
	findall(KeyPR,cddup(_,_,_,KeyPR),LofKPRs),
	uniq(LofKPRs,ULoKPRs),
	sort(ULoKPRs,SULoKPRs),
	member(PsnPR,SULoKPRs),
	findall(Value,
	   (cddup(_,Sev,_,PsnPR),sevalue(Sev,Value)),
	   LValue),
	sumlist(LValue,PSN),
	PSN > 12,
	findall([Member,Count], 
	    ( 
	      member(Member,[8,1,0.31,0]),
	      lcount(Member,LValue,Count)
	    ),
	    Histogram),
	rcpr(PsnPR,_,RcCat,RcType,RcID),
	bar,sp,
	write(PsnPR),sp,bar,sp,
	format('~3f',[PSN]),sp,bar,sp,write(Histogram),
	bar,
	write(RcType),write(':'),write(RcID),bar,write(RcCat),bar,
	nl.
	
/* Trying to find RC for a PR */
/* If it is a dup, there should be a PR without it */

wrcpr(PR,Class,Cat,RCType,RCPR) :- 
	rcpr(PR,Class,Cat,RCType,RCPR).

wrcpr(PR,Class,Cat,RCType,RCPR) :-
	duppr(DPR,LdPR),member(PR,LdPR),!,
	wrcpr(DPR,Class,Cat,RCType,RCPR).

wrcpr(X,unknown,unknown,unknown,unknown) :- nonvar(X),!.

/********************************************/


/* 
Loading Databases.  This one will fail sothat it can force
    backtrack
*/

run :- consult(case),
	consult(duppr),
	consult(rcpr),
	addCaseDup.

run :- nl,nl,write('h2. PSN Table'),nl,
	prnheader, fail.

run :- psn,fail.

run :- prntable, fail.


run :- nl,nl,write('h2. PSN Table with duplicate PRs'),nl,fail.

run :- dupprpsn,fail.

run :- prntable, fail.

run :- !.
