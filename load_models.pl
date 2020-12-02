/* ================================================== */ 
/*                                                    */
/* Predicates for reading NWN ascii .mdl files        */
/* Part of the CleanModels 3 suite by OldMansBeard    */
/*                                                    */
/* This version dated 2014-04-25                    */
/*                                                    */
/* ================================================== */ 

:- dynamic mdl_file/1.
:- dynamic load_failed/1.
:- dynamic gotdata/3.
:- dynamic gotdata/4.
:- dynamic gotdata/5.
:- dynamic gotdata/6.
:- dynamic edgetile/1.
:- dynamic user_option/1.
:- dynamic external_node/3.
:- dynamic external_node_parent/4.

/* Removed in V3.5.1 : Indexing is deprecated in Prolog 6.2.5
:- index(gotdata(0,0,1,1,1)).
:- index(gotdata(0,0,1,1,1,1)).
*/

/* ============= */
/* load_models/2 */
/* ============= */

load_models(_,_) :-
  retractall(gotdata(_,_,_)),
  retractall(gotdata(_,_,_,_)),
  retractall(gotdata(_,_,_,_,_)),
  retractall(gotdata(_,_,_,_,_,_)),
  fail.

load_models(InDir,Pattern) :-
  get_files(InDir,Pattern),
  fail.

load_models(InDir,_) :-
  mdl_file(File),
  cm3_load_file(InDir,File),
  fail.

load_models(_,_) :- !,
  (predicate_property(mdl_file(_),number_of_clauses(NFiles)) -> true; NFiles=0),
  (predicate_property(load_failed(_),number_of_clauses(NFailed)) -> true; NFailed=0),
  NSuccessful is NFiles - NFailed,
  nl,
  write(NSuccessful), write(' Models loaded, '), write(NFailed), write(' failed'), nl, nl.
  
/* ============ */
/* get_files/2  */
/* get_files1/2 */
/* ============ */

get_files(InDir,Pattern) :-
  working_directory(WkDir,InDir),
  retractall(mdl_file(_)),
  name(Pattern,P),
  expand_file_name(P,Files), !,
  get_files1(Files,WkDir).

get_files1([],WkDir) :-
  !, working_directory(_,WkDir).

get_files1([F|F0],WkDir) :-
  downcase_atom(F,F1), assertz(mdl_file(F1)), !,
  get_files1(F0,WkDir).

/* =============== */
/* cm3_load_file/2 */
/* =============== */

cm3_load_file(InDir,File) :-
  working_directory(WkDir,InDir),
  retractall(load_failed(File)),
  (exists_file(File) ->
    open(File,read,Stream0,[type(binary)]),
    get_byte(Stream0,Test0),
    get_word(Stream0,3,Test),
    close(Stream0),
    !,
    cm3_load_file(Test0,Test,File)
   ;
    write(File), write(' does not exist.'), nl,
    assertz(load_failed(File))
  ),
  (load_failed(File) ->
     write('** Load failed for file '), write(File), write(' **'), nl
     ;
     write(File), write(' loaded.'), nl
  ),
  working_directory(_,WkDir).

cm3_load_file(0,0,File) :-
  !, import_binary(File).
 
cm3_load_file(0,_,File) :-
  !,
  write('Input file '), write(File), write(' contains binary data'), nl,
  assertz(load_failed(File)).

cm3_load_file(_,_,File) :-
  open(File,read,Stream),
  read_line_to_codes(Stream,Line),
  (member(Char,Line), Char>127 ->
     write('Input file '), write(File), write(' contains binary data'), nl,
     assertz(load_failed(File))
   ;
     load_model(File,Stream,Line,0)
  ),
  close(Stream).
  

/* ================================== */
/* load_model(File,Stream,Line,State) */
/* ================================== */

load_model(_,_,end_of_file,_) :- !.

load_model(File,Stream,Line,State) :-
  interpret_line(File,Line,State,NextState),
  read_line_to_codes(Stream,NextLine), !,
  load_model(File,Stream,NextLine,NextState).

/* ======================================== */
/* interpet_line(File,Line,State,NextState) */
/* ======================================== */

interpret_line(_,[],State,State) :- !.
interpret_line(_,[35|_],State,State) :- !.
interpret_line(File,[32|L],State,NextState) :- !, interpret_line(File,L,State,NextState).
interpret_line(File,[9|L],State,NextState) :- !, interpret_line(File,L,State,NextState).
interpret_line(File,Line,State,NextState) :- tokenlist(Line,Tokens), interpret_tokens(File,State,NextState,Tokens).

tokenlist([],[]) :- !.
tokenlist([35|_],[]) :- !.
tokenlist([32|L],T)  :- !, tokenlist(L,T).
tokenlist([9|L],T)   :- !, tokenlist(L,T).
tokenlist(L,[T|T0])  :- token1(L,T,L0), !, tokenlist(L0,T0).

token1([],[],[]).
token1([32|L],[],L) :- !.
token1([9|L],[],L)  :- !.
/* token1([46|X0],[48,46|T],L) :- !, token1(X0,T,L). */
token1([X|X0],[X|T],L) :- token1(X0,T,L).

/* ============================================= */
/* interpret_tokens(File,State,NextState,Tokens) */
/* ============================================= */

interpret_tokens(File,0,0,[T0|_]) :-
  name(N0,T0), near_match(N0,filedependency), !,
  retractall(gotdata(File,[],filedependency(_))),
  assertz(gotdata(File,[],filedependency('Unknown'))).

/* 0 <-> model(ModelName) -> donemodel(ModelName) -> 2 */

interpret_tokens(File,0,model(ModelName),[T0,T|_]) :-
  name(N0,T0), near_match(N0,newmodel),
  name(N,T), downcase_atom(N,ModelName), !,
  assertz(gotdata(File,ModelName,newmodel(ModelName))).

interpret_tokens(_,model(ModelName),donemodel(ModelName),[T0,T|_]) :-
  name(N0,T0), near_match(N0,donemodel),
  name(N,T), downcase_atom(N,ModelName), !.

interpret_tokens(File,donemodel(ModelName),2,_) :-
  !, write(File), write(': data after donemodel '), write(ModelName), write(' ignored'), nl.

interpret_tokens(_,2,2,_).

/* Within model(ModelName) */

interpret_tokens(File,model(ModelName),model(ModelName),[T0,T1,T2|_]) :-
  name(N0,T0), near_match(N0,setsupermodel),
  name(N1,T1), downcase_atom(N1,ModelName), name(N2,T2), downcase_atom(N2,X),
  (X==null -> Supermodel='NULL' ; Supermodel=X), !,
  retractall(gotdata(File,ModelName,setsupermodel(ModelName,_))),
  assertz(gotdata(File,ModelName,setsupermodel(ModelName,Supermodel))).

interpret_tokens(File,model(ModelName),model(ModelName),[T0,T|_]) :-
  name(N0,T0), near_match(N0,setanimationscale),
  my_number_chars(Scale,T), !,
  retractall(gotdata(File,ModelName,setanimationscale(_))),
  assertz(gotdata(File,ModelName,setanimationscale(Scale))).

interpret_tokens(File,model(ModelName),model(ModelName),[T0,T|_])  :-
  name(N0,T0), near_match(N0,classification),
  name(N,T), upcase_atom(N,Class), !,
  retractall(gotdata(File,ModelName,classification(_))),
  assertz(gotdata(File,ModelName,classification(Class))).

interpret_tokens(File,model(ModelName),model(ModelName),[T0,T1,T2,T3|_])  :-
  name(N0,T0), near_match(N0,bmin),
  my_number_chars(X,T1), my_number_chars(Y,T2), my_number_chars(Z,T3), !,
  retractall(gotdata(File,ModelName,bmin(_,_,_))),
  assertz(gotdata(File,ModelName,bmin(X,Y,Z))).

interpret_tokens(File,model(ModelName),model(ModelName),[T0,T1,T2,T3|_])  :-
  name(N0,T0), near_match(N0,bmax),
  my_number_chars(X,T1), my_number_chars(Y,T2), my_number_chars(Z,T3), !,
  retractall(gotdata(File,ModelName,bmax(_,_,_))),
  assertz(gotdata(File,ModelName,bmax(X,Y,Z))).

interpret_tokens(File,model(ModelName),model(ModelName),[T0,T|_]) :-
  name(N0,T0), near_match(N0,radius),
  my_number_chars(Radius,T), !,
  retractall(gotdata(File,ModelName,radius(_))),
  assertz(gotdata(File,ModelName,radius(Radius))).

interpret_tokens(File,model(ModelName),model(ModelName),[T0,T|_]) :-
  name(N0,T0), near_match(N0,ignorefog),
  my_number_chars(IgnoreFog,T), !,
  retractall(gotdata(File,ModelName,ignorefog(_))),
  assertz(gotdata(File,ModelName,ignorefog(IgnoreFog))).

/* model(ModelName) <-> modelgeom(ModelName) */

interpret_tokens(File,model(ModelName),modelgeom(ModelName),[T0,T|_]) :-
  name(N0,T0), near_match(N0,beginmodelgeom),
  name(N,T), downcase_atom(N,ModelName), !,
  assertz(gotdata(File,ModelName,beginmodelgeom(ModelName))).

interpret_tokens(_,modelgeom(ModelName),model(ModelName),[T0,T|_]) :-
  name(N0,T0), near_match(N0,endmodelgeom),
  name(N,T), downcase_atom(N,ModelName), !.

/* model(ModelName) <-> anim(AnimName,ModelName) */

/* Changed in V3.6.2s to tolerate wrong model names in animations */
/*
interpret_tokens(File,model(ModelName),anim(AnimName,ModelName),[T0,T1,T2|_]) :-
  name(N0,T0), near_match(N0,newanim),
  name(N1,T1), downcase_atom(N1,AnimName),
  name(N2,T2), downcase_atom(N2,ModelName), !,
  assertz(gotdata(File,ModelName,newanim(AnimName,ModelName))).

interpret_tokens(_,anim(AnimName,ModelName),model(ModelName),[T0,T1,T2|_]) :-
  name(N0,T0), near_match(N0,doneanim),
  name(N1,T1), downcase_atom(N1,AnimName),
  name(N2,T2), downcase_atom(N2,ModelName), !.
*/

interpret_tokens(File,model(ModelName),anim(AnimName,ModelName),[T0,T1|_]) :-
  name(N0,T0), near_match(N0,newanim),
  name(N1,T1), downcase_atom(N1,AnimName), !,
  assertz(gotdata(File,ModelName,newanim(AnimName,ModelName))).

interpret_tokens(_,anim(AnimName,ModelName),model(ModelName),[T0,T1|_]) :-
  name(N0,T0), near_match(N0,doneanim),
  name(N1,T1), downcase_atom(N1,AnimName), !.

/* modelgeom(ModelName) <-> node(ModelName,NodeName,NodeType,NodeRef) */

interpret_tokens(File,modelgeom(ModelName),node(ModelName,NodeName,NodeType,NodeRef),[T0,T1|T2]) :-
  name(N0,T0), near_match(N0,node),
  name(N1,T1), downcase_atom(N1,NodeType0),
  flatten(T2,TT2), name(N2,TT2), downcase_atom(N2,NodeName0), !,
  (
    NodeType0=dummy,
    atom_concat(ModelName,'sl',_aPrefix),
    atom_concat(_aPrefix,_aSuffix0,NodeName0),
    name(_aSuffix0,_sSuffix0),
    name(_nSuffix0,_sSuffix0),
    integer(_nSuffix0),
    between(1,2,_nSuffix0)
    ->
    NodeType=light,
    atom_concat(_aPrefix,_nSuffix0,NodeName)
    ;
    NodeType0=dummy,
    atom_concat(ModelName,'_d0',_aPrefix0),
    atom_concat(_aPrefix0,_aSuffix0,NodeName0),
    name(_aSuffix0,_sSuffix0),
    name(_nSuffix0,_sSuffix0),
    integer(_nSuffix0),
    between(0,9,_nSuffix0)
    ->
    NodeType=dummy,
    atom_concat(ModelName,'_D0',_aPrefix1),
    atom_concat(_aPrefix1,_nSuffix0,NodeName)
    ;
    NodeType0=dummy,
    atom_concat(ModelName,'_u0',_aPrefix0),
    atom_concat(_aPrefix0,_aSuffix0,NodeName0),
    name(_aSuffix0,_sSuffix0),
    name(_nSuffix0,_sSuffix0),
    integer(_nSuffix0),
    between(0,9,_nSuffix0)
    ->
    NodeType=dummy,
    atom_concat(ModelName,'_U0',_aPrefix1),
    atom_concat(_aPrefix1,_nSuffix0,NodeName)
    ;
    NodeType=NodeType0,
    NodeName=NodeName0
  ),
  asserta(gotdata(File,ModelName,node(NodeType,NodeName)),NodeRef).

interpret_tokens(_,node(ModelName,_,_,_),modelgeom(ModelName),[T0|_]) :-
  name(N0,T0), near_match(N0,endnode), !.

/* Within node(ModelName,NodeName,NodeType,NodeRef) */

interpret_tokens(File,node(ModelName,NodeName,NodeType,NodeRef),node(ModelName,NodeName,NodeType,NodeRef),[T0,T1|_]) :-
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  name(TN0,T0), downcase_atom(TN0,Q0),
  (paramtype(NodeType,mn1,Q0) -> Q1=Q0; paramtype(NodeType,mn1,Q1), near_match(Q0,Q1)),
  my_number_chars(X,T1), !,
  Qx=..[Q1,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,Qx)),
  Q=..[Q1,X], assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)).

interpret_tokens(File,node(ModelName,NodeName,NodeType,NodeRef),node(ModelName,NodeName,NodeType,NodeRef),[T0,T1,T2,T3|_]) :-
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  name(TN0,T0), downcase_atom(TN0,Q0), 
  (paramtype(NodeType,mn3,Q0) -> Q1=Q0; paramtype(NodeType,mn3,Q1), near_match(Q0,Q1)),
  my_number_chars(X,T1), my_number_chars(Y,T2), my_number_chars(Z,T3), !,
  Qx=..[Q1,_,_,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,Qx)),
  Q=..[Q1,X,Y,Z], assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)).

interpret_tokens(File,node(ModelName,NodeName,NodeType,NodeRef),node(ModelName,NodeName,NodeType,NodeRef),[T0,T1,T2,T3,T4|_]) :-
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  name(TN0,T0), downcase_atom(TN0,Q0),
  (paramtype(NodeType,mn4,Q0) -> Q1=Q0; paramtype(NodeType,mn4,Q1), near_match(Q0,Q1)),
  my_number_chars(X,T1), my_number_chars(Y,T2), my_number_chars(Z,T3), my_number_chars(A,T4), !,
  (Q1=orientation -> snap_pi(A,A1); A1=A),
  Qx=..[Q1,_,_,_,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,Qx)),
  Q=..[Q1,X,Y,Z,A1], assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)).

interpret_tokens(File,node(ModelName,NodeName,NodeType,NodeRef),node(ModelName,NodeName,NodeType,NodeRef),[T0,T|_]) :-
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  name(TN0,T0), downcase_atom(TN0,Q0),
  (paramtype(NodeType,man,Q0) -> Q1=Q0; paramtype(NodeType,man,Q1), near_match(Q0,Q1)),
  name(N,T), downcase_atom(N,X), (X==null -> Value='NULL'; Value=X), !,
  Qx=..[Q1,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,Qx)),
  Q=..[Q1,Value], assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)).

interpret_tokens(File,node(ModelName,NodeName,NodeType,NodeRef),node(ModelName,NodeName,NodeType,NodeRef),[T0|T]) :-
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  name(TN0,T0), downcase_atom(TN0,Q0),
  (paramtype(NodeType,manf,Q0) -> Q1=Q0; paramtype(NodeType,manf,Q1), near_match(Q0,Q1)),
  flatten(T,TT), name(N,TT), downcase_atom(N,X), (X==null -> Value='NULL'; Value=X), !,
  Qx=..[Q1,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,Qx)),
  (Q1\=='parent' -> Q=..[Q1,Value] ;
   Value=='NULL' -> Q=parent('NULL') ;
   clause(gotdata(File,ModelName,node(_,Value)),true,PRef), PRef\=NodeRef -> Q=parent(Value/PRef) ;
   Q=parent(Value/ -1)
  ),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)).

interpret_tokens(File,node(ModelName,NodeName,NodeType,NodeRef),node(ModelName,NodeName,NodeType,NodeRef),[T0|_]) :-
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  name(TN0,T0), downcase_atom(TN0,Q0), /* ignore parameters for the wrong node type */
  \+ paramtype(NodeType,_,Q0), paramtype(_,Type,Q0), Type\=mx(_), !.
  
/* node(ModelName,NodeName,NodeType,NodeRef) <-> list(ModelName,NodeName,NodeType,NodeRef,ListType,N,Count,W) */

interpret_tokens(File,node(ModelName,NodeName,aabb,NodeRef),R,[[97,97,98,98],T1,T2,T3,T4,T5,T6,T7|_]) :-
  clause(gotdata(File,ModelName,node(aabb,NodeName)),true,NodeRef),
  gotdata(File,ModelName,NodeName,NodeRef,faces(Nfaces)), Count is 2*Nfaces-1,
  my_number_chars(F1,T1), my_number_chars(F2,T2), my_number_chars(F3,T3), my_number_chars(F4,T4),
  my_number_chars(F5,T5), my_number_chars(F6,T6), my_number_chars(F7,T7), !,
  (Count>1 -> R=list(ModelName,NodeName,aabb,NodeRef,aabb,1,Count,7); R=node(ModelName,NodeName,aabb,NodeRef)),
  retractall(gotdata(File,ModelName,NodeName,NodeRef,aabb(_))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,aabb(Count))),
  retractall(gotdata(File,ModelName,NodeName,NodeRef,aabb(0,_,_,_,_,_,_,_))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,aabb(0,F1,F2,F3,F4,F5,F6,F7))).

interpret_tokens(File,node(ModelName,NodeName,NodeType,NodeRef),R,[T0,T1|_]) :-
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  name(TN0,T0), downcase_atom(TN0,Q0),
  (paramtype(NodeType,mx(W),Q0) -> Q1=Q0; paramtype(NodeType,mx(W),Q1), near_match(Q0,Q1)),
  my_number_chars(Count,T1), !,
  (Count>0 -> R=list(ModelName,NodeName,NodeType,NodeRef,Q1,0,Count,W); R=node(ModelName,NodeName,NodeType,NodeRef)),
  Qx=..[Q1,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,Qx)),
  Q=..[Q1,Count], assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)).

interpret_tokens(File,node(ModelName,NodeName,NodeType,NodeRef),R,[T0,T1|_]) :-
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  name(TN0,T0), downcase_atom(TN0,Q0),
  paramtype(_,mx(W),Q0), /* This parameter belongs to a different node type */
  my_number_chars(Count,T1), !,
  (Count>0 -> R=list(ModelName,NodeName,NodeType,NodeRef,[],0,Count,W); R=node(ModelName,NodeName,NodeType,NodeRef)),
  write(File), write(': ignoring '), write(Q0), write(' data for '), write(NodeType), write(' node '), write(NodeName), nl.

interpret_tokens(_,list(ModelName,NodeName,NodeType,NodeRef,[],N,Count,_),R,_) :-
  N1 is N+1, (N1<Count -> R=list(ModelName,NodeName,NodeType,NodeRef,[],N1,Count,a); R=node(ModelName,NodeName,NodeType,NodeRef)), !. 

interpret_tokens(File,list(ModelName,NodeName,NodeType,NodeRef,Q0,N,Count,a),R,[T|_]) :-
  name(M,T), !,
  N1 is N+1, (N1<Count -> R=list(ModelName,NodeName,NodeType,NodeRef,Q0,N1,Count,a); R=node(ModelName,NodeName,NodeType,NodeRef)),
  Qx=..[Q0,N,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,Qx)),
  Q=..[Q0,N,M], assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)). 

interpret_tokens(File,list(ModelName,NodeName,NodeType,NodeRef,Q0,N,Count,an),R,Tokens) :-
  maplist(name,Args0,Tokens), maplist(my_downcase,Args0,Args), !,
  N1 is N+1, (N1<Count -> R=list(ModelName,NodeName,NodeType,NodeRef,Q0,N1,Count,an); R=node(ModelName,NodeName,NodeType,NodeRef)),
  Qx=..[Q0,N,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,Qx)),
  Q=..[Q0,N,Args], assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)). 

interpret_tokens(File,list(ModelName,NodeName,NodeType,NodeRef,Q0,N,Count,W),R,Tokens) :-
  Q0==colors, W=3,
  length(Tokens,W), maplist(my_number_chars,[R1,G1,B1],Tokens), !,
  R255 is round(255*R1), G255 is round(255*G1), B255 is round(255*B1), Args=[R255,G255,B255],
  N1 is N+1, (N1<Count -> R=list(ModelName,NodeName,NodeType,NodeRef,Q0,N1,Count,W); R=node(ModelName,NodeName,NodeType,NodeRef)),
  length(Dummy,W), Qx=..[Q0,N|Dummy], retractall(gotdata(File,ModelName,NodeName,NodeRef,Qx)),
  Q=..[Q0,N|Args], assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)). 

interpret_tokens(File,list(ModelName,NodeName,NodeType,NodeRef,Q0,N,Count,W),R,Tokens) :-
  Q0\==colors,
  length(Tokens,W), maplist(my_number_chars,Args,Tokens), !,
  N1 is N+1, (N1<Count -> R=list(ModelName,NodeName,NodeType,NodeRef,Q0,N1,Count,W); R=node(ModelName,NodeName,NodeType,NodeRef)),
  length(Dummy,W), Qx=..[Q0,N|Dummy], retractall(gotdata(File,ModelName,NodeName,NodeRef,Qx)),
  Q=..[Q0,N|Args], assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)). 

/* Within anim(AnimName,ModelName) */

interpret_tokens(File,anim(AnimName,ModelName),anim(AnimName,ModelName),[T0,T1|_]) :-
  name(TN0,T0), downcase_atom(TN0,Q0),
  (paramtype(anim,mn1,Q0) -> Q1=Q0; paramtype(anim,mn1,Q1), near_match(Q0,Q1)),
  my_number_chars(N,T1), !,
  Qx=..[Q1,_], retractall(gotdata(File,ModelName,anim(AnimName),Qx)),
  Q=..[Q1,N], assertz(gotdata(File,ModelName,anim(AnimName),Q)). 

interpret_tokens(File,anim(AnimName,ModelName),anim(AnimName,ModelName),[T0,T1|_]) :-
  name(N0,T0), downcase_atom(N0,Q0),
  (paramtype(anim,man,Q0) -> Q1=Q0; paramtype(anim,man,Q1), near_match(Q0,Q1)),
  name(TN1,T1), downcase_atom(TN1,N1), !,
  Qx=..[Q1,_], retractall(gotdata(File,ModelName,anim(AnimName),Qx)),
  Q=..[Q1,N1], assertz(gotdata(File,ModelName,anim(AnimName),Q)). 

interpret_tokens(File,anim(AnimName,ModelName),anim(AnimName,ModelName),[T0,T1,T2|_]) :-
  name(N0,T0), downcase_atom(N0,Q0),
  (paramtype(anim,mna,Q0) -> Q1=Q0; paramtype(anim,mna,Q1), near_match(Q0,Q1)),
  my_number_chars(N1,T1),  name(TN2,T2), downcase_atom(TN2,N2), !,
  Qx=..[Q1,N1,_], retractall(gotdata(File,ModelName,anim(AnimName),Qx)),
  Q=..[Q1,N1,N2], assertz(gotdata(File,ModelName,anim(AnimName),Q)). 

interpret_tokens(File,anim(AnimName,ModelName),anim(AnimName,ModelName),[T0|T1]) :-
  name(N0,T0), downcase_atom(N0,Q0),
  (paramtype(anim,manf,Q0) -> Q1=Q0; paramtype(anim,manf,Q1), near_match(Q0,Q1)),
  flatten(T1,TT1), name(TN1,TT1), downcase_atom(TN1,N1), !,
  Qx=..[Q1,_], retractall(gotdata(File,ModelName,anim(AnimName),Qx)),
  Q=..[Q1,N1], assertz(gotdata(File,ModelName,anim(AnimName),Q)). 

/* anim(AnimName,ModelName) <-> animnode(ModelName,NodeName,NodeRef,AnimName) */
/* anim(AnimName,ModelName) <-> externalnode(ModelName,NodeName,AnimName) */

interpret_tokens(File,anim(AnimName,ModelName),animnode(ModelName,NodeName,NodeRef,AnimName),[T0,_|T1]) :-
  name(N0,T0), near_match(N0,node),
  flatten(T1,TT1), name(N1,TT1), downcase_atom(N1,NodeName),
  clause(gotdata(File,ModelName,node(_,NodeName)),true,NodeRef), !. 

interpret_tokens(File,anim(AnimName,ModelName),animnode(ModelName,[],-1,AnimName),[T0,_|T1]) :-
  name(N0,T0), near_match(N0,node),
  flatten(T1,TT1), name(N1,TT1), downcase_atom(N1,LowerName),
  gotdata(File,ModelName,node(_,NodeName)), downcase_atom(NodeName,LowerName), !. 

interpret_tokens(File,anim(AnimName,ModelName),externalnode(ModelName,NodeName,AnimName),[T0,_|T1]) :-
  name(N0,T0), near_match(N0,node),
  flatten(T1,TT1), name(N1,TT1), downcase_atom(N1,NodeName),
  external_node(File,ModelName,NodeName), !. 

interpret_tokens(File,anim(AnimName,ModelName),externalnode(ModelName,NodeName,AnimName),[T0,_|T1]) :-
  name(N0,T0), near_match(N0,node),
  flatten(T1,TT1), name(N1,TT1), downcase_atom(N1,NodeName),
  assertz(external_node(File,ModelName,NodeName)), !. 

interpret_tokens(File,anim(AnimName,ModelName),animnode(ModelName,[],-1,AnimName),[T0,_|T1]) :-
  name(N0,T0), near_match(N0,node),
  flatten(T1,TT1), name(NodeName,TT1),
  write(File), write(': ignoring data for node '), write(NodeName), write(' in animation '), write(AnimName), nl. 

interpret_tokens(_,animnode(ModelName,_,_,AnimName),anim(AnimName,ModelName),[T0|_]) :-
  name(N0,T0), near_match(N0,endnode), !.

interpret_tokens(_,externalnode(ModelName,_,AnimName),anim(AnimName,ModelName),[T0|_]) :-
  name(N0,T0), near_match(N0,endnode), !.

/* Within externalnode(ModelName,NodeName,AnimName) */

interpret_tokens(File,externalnode(ModelName,NodeName,AnimName),externalnode(ModelName,NodeName,AnimName),[T0|T1]) :-
  name(N0,T0), near_match(N0,parent),
  flatten(T1,TT1), name(N1,TT1), downcase_atom(N1,ParentName),
  external_node_parent(File,ModelName,NodeName,ParentName), !.

interpret_tokens(File,externalnode(ModelName,NodeName,AnimName),externalnode(ModelName,NodeName,AnimName),[T0|T1]) :-
  name(N0,T0), near_match(N0,parent),
  flatten(T1,TT1), name(N1,TT1), downcase_atom(N1,ParentName),
  \+ external_node_parent(File,ModelName,NodeName,ParentName),
  once(( external_node(File,ModelName,ParentName) ; gotdata(File,ModelName,node(_,ParentName)) )),
  assertz(external_node_parent(File,ModelName,NodeName,ParentName)), !.

interpret_tokens(_,externalnode(ModelName,NodeName,AnimName),externalnode(ModelName,NodeName,AnimName),_).

/* Within animnode(ModelName,NodeName,NodeRef,AnimName) */

interpret_tokens(_,animnode(ModelName,[],-1,AnimName),animnode(ModelName,[],-1,AnimName),_) :- !.

interpret_tokens(_,animnode(ModelName,NodeName,NodeRef,AnimName),animnode(ModelName,NodeName,NodeRef,AnimName),[T0|_]) :-
  name(N0,T0), near_match(N0,parent), !.

interpret_tokens(_,animnode(ModelName,NodeName,NodeRef,AnimName),animnode(ModelName,NodeName,NodeRef,AnimName),[T0|_]) :-
  name(N0,T0), near_match(N0,endlist), !.

interpret_tokens(File,animnode(ModelName,NodeName,NodeRef,AnimName),animnode(ModelName,NodeName,NodeRef,AnimName),[T0|_]) :-
  name(TN0,T0), downcase_atom(TN0,Q0), near_match(Q0,'bitmap'),
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef), !,
  write(File), write(': ignoring bitmap parameter for '), write(NodeType), write(' node '), write(NodeName), write(' in animation '), write(AnimName), nl.

	/* V3.4.2e: Don't apply the V3.4.1k changes that follow, to animmesh parameters */

interpret_tokens(File,animnode(ModelName,NodeName,NodeRef,AnimName),animnode(ModelName,NodeName,NodeRef,AnimName),[T0,T1|_]) :-
  name(TN0,T0), downcase_atom(TN0,Q0),
  clause(gotdata(File,ModelName,node(animmesh,NodeName)),true,NodeRef),
  (clause(paramtype(animmesh,mn1,Q0),true) -> Q1=Q0; clause(paramtype(animmesh,mn1,Q1),true), near_match(Q0,Q1)),
  my_number_chars(N,T1), !,
  Qx=..[Q1,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qx)),
  Q=..[Q1,N], assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Q)).

interpret_tokens(File,animnode(ModelName,NodeName,NodeRef,AnimName),alist(ModelName,NodeName,NodeRef,AnimName,Q0,W,0),[T0]) :-
  name(TN0,T0), downcase_atom(TN0,Q0),
  clause(gotdata(File,ModelName,node(animmesh,NodeName)),true,NodeRef),
  member(Q0,[verts,tverts,faces,animverts,animtverts]),
  paramtype(anim,mx(W),Q0),
  !.

interpret_tokens(File,animnode(Model,NodeName,NodeRef,AnimName),nlist(Model,NodeName,NodeRef,AnimName,Q0,W,Max,0),[T0,T1|_]) :-
  name(TN0,T0), downcase_atom(TN0,Q0),
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  member(Q0,[verts,tverts,faces,animverts,animtverts]),
  paramtype(anim,mx(W),Q0),
  my_number_chars(Max,T1), Max>0, !,
  Qx=..[Q0,_], retractall(gotdata(File,Model,NodeName,NodeRef,AnimName,Qx)),
  Q=..[Q0,Max], assertz(gotdata(File,Model,NodeName,NodeRef,AnimName,Q)).

	/* V3.4.1k */
	/* Change the next clauses to assert single animation keys rather than static parameters */
	/* and check that it is a genuine controller for this node type, otherwise ignore it.    */

interpret_tokens(File,animnode(ModelName,NodeName,NodeRef,AnimName),animnode(ModelName,NodeName,NodeRef,AnimName),[T0,T1|_]) :-
  name(TN0,T0), downcase_atom(TN0,Q0),
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  (paramtype(NodeType,mn1,Q0) -> Q1=Q0; paramtype(NodeType,mn1,Q1), near_match(Q0,Q1)),
  atom_concat(Q1,'key',K), clause(paramtype(anim,mx(2),K),true),
  my_number_chars(N,T1), !,
  Qx1=..[Q1,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qx1)),
  Qx2=..[K,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qx2)),
  Qx3=..[K,_,_,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qx3)),
  Qy1=..[K,1], assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qy1)),
  Qy2=..[K,0,0,N], assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qy2)).

interpret_tokens(File,animnode(ModelName,NodeName,NodeRef,AnimName),animnode(ModelName,NodeName,NodeRef,AnimName),[T0,T1,T2,T3|_]) :-
  name(TN0,T0), downcase_atom(TN0,Q0),
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  (paramtype(NodeType,mn3,Q0) -> Q1=Q0; paramtype(NodeType,mn3,Q1), near_match(Q0,Q1)),
  atom_concat(Q1,'key',K), clause(paramtype(anim,mx(4),K),true),
  my_number_chars(N1,T1), my_number_chars(N2,T2), my_number_chars(N3,T3), !,
  Qx1=..[Q1,_,_,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qx1)),
  Qx2=..[K,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qx2)),
  Qx3=..[K,_,_,_,_,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qx3)),
  Qy1=..[K,1], assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qy1)),
  Qy2=..[K,0,0,N1,N2,N3], assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qy2)).

interpret_tokens(File,animnode(ModelName,NodeName,NodeRef,AnimName),animnode(ModelName,NodeName,NodeRef,AnimName),[T0,T1,T2,T3,T4|_]) :-
  name(TN0,T0), downcase_atom(TN0,Q0),
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  (paramtype(NodeType,mn4,Q0) -> Q1=Q0; paramtype(NodeType,mn4,Q1), near_match(Q0,Q1)),
  atom_concat(Q1,'key',K), clause(paramtype(anim,mx(5),K),true),
  my_number_chars(N1,T1), my_number_chars(N2,T2), my_number_chars(N3,T3), my_number_chars(N4,T4), !,
  Qx1=..[Q1,_,_,_,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qx1)),
  Qx2=..[K,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qx2)),
  Qx3=..[K,_,_,_,_,_,_], retractall(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qx3)),
  Qy1=..[K,1], assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qy1)),
  Qy2=..[K,0,0,N1,N2,N3,N4], assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Qy2)).

interpret_tokens(File,animnode(ModelName,NodeName,NodeRef,AnimName),animnode(ModelName,NodeName,NodeRef,AnimName),[T0|_]) :-
  /* ignore parameters that are for the right node type but are not controllers */
  name(TN0,T0), downcase_atom(TN0,Q0),
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  (paramtype(NodeType,_,Q0) -> Q1=Q0; paramtype(NodeType,_,Q1), near_match(Q0,Q1)),
  atom_concat(Q1,'key',K), \+ clause(paramtype(anim,_,K),true),
  write(File), write(': ignoring non-keyable parameter '), write(Q0), write(' for '), write(NodeType), write(' node '), write(NodeName), write(' in animation '), write(AnimName), nl.

interpret_tokens(File,animnode(ModelName,NodeName,NodeRef,AnimName),animnode(ModelName,NodeName,NodeRef,AnimName),[T0|_]) :-
  /* ignore parameters for the wrong node type */
  clause(gotdata(File,ModelName,node(NodeType,NodeName)),true,NodeRef),
  name(TN0,T0), downcase_atom(TN0,Q0),
  \+ paramtype(NodeType,_,Q0), paramtype(_,Type,Q0), Type\=mx(_), !,
  write(File), write(': ignoring '), write(Q0), write(' parameter for '), write(NodeType), write(' node '), write(NodeName), write(' in animation '), write(AnimName), nl.

	/* End of changes for V3.4.1k */

interpret_tokens(_,animnode(ModelName,NodeName,NodeRef,AnimName),alist(ModelName,NodeName,NodeRef,AnimName,Q1,W,0),[T0]) :-
  name(TN0,T0), downcase_atom(TN0,Q0),
  (paramtype(anim,mx(W),Q0) -> Q1=Q0; paramtype(anim,mx(W),Q1), near_match(Q0,Q1)), !.

interpret_tokens(Model,alist(ModelName,NodeName,NodeRef,AnimName,Q0,_,N),animnode(ModelName,NodeName,NodeRef,AnimName),[T0|_]) :-
  name(N0,T0), near_match(N0,endlist),
  (N>0 ->
    Qx=..[Q0,_], retractall(gotdata(Model,ModelName,NodeName,NodeRef,AnimName,Qx)),
    Q=..[Q0,N], assertz(gotdata(Model,ModelName,NodeName,NodeRef,AnimName,Q))
    ;
    true), !.

interpret_tokens(Model,alist(ModelName,NodeName,NodeRef,AnimName,Q0,W,N),alist(ModelName,NodeName,NodeRef,AnimName,Q0,W,N1),Tokens) :-
  length(Tokens,W), maplist(my_number_chars,Args,Tokens), !,
  N1 is N+1,
  length(Dummy,W), Qx=..[Q0,N|Dummy], retractall(gotdata(Model,ModelName,NodeName,NodeRef,AnimName,Qx)),
  Q=..[Q0,N|Args], assertz(gotdata(Model,ModelName,NodeName,NodeRef,AnimName,Q)). 

interpret_tokens(Model,animnode(ModelName,NodeName,NodeRef,AnimName),nlist(ModelName,NodeName,NodeRef,AnimName,Q1,W,Max,0),[T0,T1|_]) :-
  name(TN0,T0), downcase_atom(TN0,Q0),
  (paramtype(anim,mx(W),Q0) -> Q1=Q0; paramtype(anim,mx(W),Q1), near_match(Q0,Q1)),
  my_number_chars(Max,T1), Max>0, !,
  Qx=..[Q1,_], retractall(gotdata(Model,ModelName,NodeName,NodeRef,AnimName,Qx)),
  Q=..[Q1,Max], assertz(gotdata(Model,ModelName,NodeName,NodeRef,AnimName,Q)).

interpret_tokens(Model,nlist(ModelName,NodeName,NodeRef,AnimName,Q0,W,Max,N),R,Tokens) :-
  length(Tokens,W), maplist(my_number_chars,Args,Tokens), !,
  N1 is N+1, (N1<Max -> R=nlist(ModelName,NodeName,NodeRef,AnimName,Q0,W,Max,N1); R=animnode(ModelName,NodeName,NodeRef,AnimName)),
  length(Dummy,W), Qx=..[Q0,N|Dummy], retractall(gotdata(Model,ModelName,NodeName,NodeRef,AnimName,Qx)),
  Q=..[Q0,N|Args], assertz(gotdata(Model,ModelName,NodeName,NodeRef,AnimName,Q)).

/* Default rule - write out any lines that are not understood and flag the file */
/* Softened - discard single tokens (e.g. repeated bitmaps) but don't flag the file */

interpret_tokens(_,P,P,T) :-
  /* -1.#QNAN */
  member([45, 49, 46, 35, 81, 78, 65, 78|_],T),
  write('Parse state '), write(P), nl,
  untoken(T,U), write('Discarding "'), printlist(U), write('"'), nl, !.

interpret_tokens(File,P,P,T) :-
  write('Parse state '), write(P), nl,
  untoken(T,U), write('Do not understand "'), printlist(U), write('"'), nl, !,
  (T\=[_] -> (\+ load_failed(File) -> assertz(load_failed(File)); true); true).

untoken([],[]).
untoken([T|T0],[U|U0]) :- name(U,T), !, untoken(T0,U0).

/* =========== */
/* paramtype/3 */
/* =========== */

paramtype(dummy,manf,parent).
paramtype(dummy,mn3,position).          /* controller */
paramtype(dummy,mn4,orientation).       /* controller */
paramtype(dummy,mn1,scale).             /* controller */
paramtype(dummy,mn1,inheritcolor).
paramtype(dummy,mn3,wirecolor).

paramtype(trimesh,A,B) :- paramtype(dummy,A,B).
paramtype(trimesh,mn3,selfillumcolor).  /* controller */
paramtype(trimesh,mn1,alpha).           /* controller */
paramtype(trimesh,mn3,diffuse).
paramtype(trimesh,mn3,ambient).
paramtype(trimesh,mn3,specular).
paramtype(trimesh,mn1,shininess).
paramtype(trimesh,mn1,shadow).
paramtype(trimesh,mn1,beaming).
paramtype(trimesh,mn1,render).
paramtype(trimesh,mn1,transparencyhint).
paramtype(trimesh,man,renderhint).
paramtype(trimesh,man,bitmap).
paramtype(trimesh,man,texture1).
paramtype(trimesh,man,texture2).
paramtype(trimesh,man,materialname).
paramtype(trimesh,mn1,tilefade).
paramtype(trimesh,mn1,lightmapped).
paramtype(trimesh,mn1,rotatetexture).
paramtype(trimesh,mn3,center).
paramtype(trimesh,man,center).
paramtype(trimesh,mx(a),multimaterial).
paramtype(trimesh,mx(3),verts).
paramtype(trimesh,mx(3),colors).
paramtype(trimesh,mx(3),tverts).
paramtype(trimesh,mx(3),tverts1).
paramtype(trimesh,mx(3),tverts2).
paramtype(trimesh,mx(3),tverts3).
paramtype(trimesh,mx(3),texindices0).
paramtype(trimesh,mx(3),texindices1).
paramtype(trimesh,mx(3),texindices2).
paramtype(trimesh,mx(3),texindices3).
paramtype(trimesh,mx(8),faces).

paramtype(skin,A,B) :- paramtype(trimesh,A,B).
paramtype(skin,mx(an),weights).

paramtype(animmesh,A,B) :- paramtype(trimesh,A,B).
paramtype(animmesh,mn1,sampleperiod).
paramtype(animmesh,mn1,clipu).
paramtype(animmesh,mn1,clipv).
paramtype(animmesh,mn1,clipw).
paramtype(animmesh,mn1,cliph).

paramtype(danglymesh,A,B) :- paramtype(trimesh,A,B).
paramtype(danglymesh,mn1,danglymesh).
paramtype(danglymesh,mn1,tightness).
paramtype(danglymesh,mn1,period).
paramtype(danglymesh,mn1,displacement).
paramtype(danglymesh,man,showdispl).
paramtype(danglymesh,mn1,displtype).
paramtype(danglymesh,man,gizmo).
paramtype(danglymesh,mx(1),constraints).

paramtype(aabb,A,B) :- paramtype(trimesh,A,B).
paramtype(aabb,mx(7),aabb).

paramtype(light,A,B) :- paramtype(dummy,A,B).
paramtype(light,mn1,flareradius).
paramtype(light,mn1,lensflares).           /* What does this really do ? */
paramtype(light,mx(1),flaresizes).
paramtype(light,mx(1),flarepositions).
paramtype(light,mx(3),flarecolorshifts).
paramtype(light,mx(a),texturenames).
paramtype(light,mn1,lightpriority).
paramtype(light,mn1,ambientonly).
paramtype(light,mn1,isdynamic).
paramtype(light,mn1,ndynamictype).
paramtype(light,mn1,affectdynamic).
paramtype(light,mn1,shadow).
paramtype(light,mn1,generateflare).
paramtype(light,mn1,fadinglight).
paramtype(light,mn1,negativelight).        /* NWMax only - not compiled */ 
paramtype(light,mn3,color).                /* controller */
paramtype(light,mn1,radius).               /* controller */
paramtype(light,mn1,multiplier).           /* controller */
paramtype(light,mn1,shadowradius).         /* controller - not NWMax */
paramtype(light,mn1,verticaldisplacement). /* controller - not NWMax */

paramtype(emitter,A,B) :- paramtype(dummy,A,B).
paramtype(emitter,mn1,deadspace).
paramtype(emitter,mn1,blastradius).
paramtype(emitter,mn1,blastlength).
paramtype(emitter,mn1,xgrid).
paramtype(emitter,mn1,ygrid).
paramtype(emitter,mn1,spawntype).
paramtype(emitter,man,update).
paramtype(emitter,man,render).
paramtype(emitter,man,blend).
paramtype(emitter,man,texture).
paramtype(emitter,man,chunkname).
paramtype(emitter,mn1,twosidedtex).
paramtype(emitter,mn1,loop).
paramtype(emitter,mn1,renderorder).
paramtype(emitter,mn1,p2p).            /* Bit Flag */
paramtype(emitter,mn1,p2p_sel).        /* Bit Flag */
paramtype(emitter,mn1,affectedbywind). /* Bit Flag */
paramtype(emitter,mn1,m_istinted).     /* Bit Flag */
paramtype(emitter,mn1,bounce).         /* Bit Flag */
paramtype(emitter,mn1,random).         /* Bit Flag */
paramtype(emitter,mn1,inherit).        /* Bit Flag */
paramtype(emitter,mn1,inheritvel).     /* Bit Flag */
paramtype(emitter,mn1,inherit_local).  /* Bit Flag */
paramtype(emitter,mn1,splat).          /* Bit Flag */
paramtype(emitter,mn1,inherit_part).   /* Bit Flag */

paramtype(emitter,man,p2p_type).       /* Not exported by NWMax */
paramtype(emitter,mn1,render_sel).     /* Not exported by NWMax */
paramtype(emitter,mn1,blend_sel).      /* Not exported by NWMax */
paramtype(emitter,mn1,update_sel).     /* Not exported by NWMax */
paramtype(emitter,mn1,spawntype_sel).  /* Not exported by NWMax */
paramtype(emitter,mn1,opacity).        /* Not exported by NWMax */
paramtype(emitter,mn1,iconsize).       /* Not exported by NWMax */
paramtype(emitter,mn1,lockaxes).       /* Not exported by NWMax */
paramtype(emitter,mn1,chunky).         /* Not exported by NWMax */

paramtype(emitter,mn1,xsize).           /* controller */
paramtype(emitter,mn1,ysize).           /* controller */
paramtype(emitter,mn1,threshold).       /* controller - not recognised by NWMax */
paramtype(emitter,mn1,combinetime).     /* controller */
paramtype(emitter,mn3,colorstart).      /* controller */
paramtype(emitter,mn3,colorend).        /* controller */
paramtype(emitter,mn1,alphastart).      /* controller */
paramtype(emitter,mn1,alphaend).        /* controller */
paramtype(emitter,mn1,sizestart).       /* controller */
paramtype(emitter,mn1,sizeend).         /* controller */
paramtype(emitter,mn1,sizestart_y).     /* controller - not recognised by NWMax */
paramtype(emitter,mn1,sizeend_y).       /* controller - not recognised by NWMax */
paramtype(emitter,mn1,birthrate).       /* controller */
paramtype(emitter,mn1,lifeexp).         /* controller */
paramtype(emitter,mn1,mass).            /* controller */
paramtype(emitter,mn1,spread).          /* controller */
paramtype(emitter,mn1,particlerot).     /* controller */
paramtype(emitter,mn1,velocity).        /* controller */
paramtype(emitter,mn1,randvel).         /* controller */
paramtype(emitter,mn1,bounce_co).       /* controller */
paramtype(emitter,mn1,blurlength).      /* controller - not recognised by NWMax */
paramtype(emitter,mn1,fps).             /* controller */
paramtype(emitter,mn1,framestart).      /* controller */
paramtype(emitter,mn1,frameend).        /* controller */
paramtype(emitter,mn1,lightningdelay).  /* controller */
paramtype(emitter,mn1,lightningradius). /* controller */
paramtype(emitter,mn1,lightningsubdiv). /* controller - not nwnmdlcomp */
paramtype(emitter,mn1,lightningscale).  /* controller */
paramtype(emitter,mn1,p2p_bezier2).     /* controller */
paramtype(emitter,mn1,p2p_bezier3).     /* controller */
paramtype(emitter,mn1,grav).            /* controller */
paramtype(emitter,mn1,drag).            /* controller */
/* 'detonate' with no parameters can be used as an animation controller */
paramtype(emitter,mn1,alphamid).	/* controller - not NWMax       */
paramtype(emitter,mn3,colormid).	/* controller - not NWMax       */
paramtype(emitter,mn1,percentstart).	/* controller - not NWMax       */
paramtype(emitter,mn1,percentmid).	/* controller - not NWMax       */
paramtype(emitter,mn1,percentend).	/* controller - not NWMax       */
paramtype(emitter,mn1,sizemid).		/* controller - not NWMax       */
paramtype(emitter,mn1,sizemid_y).	/* controller - not NWMax       */

paramtype(anim,mn1,length).
paramtype(anim,mn1,transtime).
paramtype(anim,manf,animroot).
paramtype(anim,mna,event).
paramtype(anim,mx(2),gizmokey). /* This is a kludge */
paramtype(anim,mx(4),positionkey).
paramtype(anim,mx(4),centerkey).
paramtype(anim,mx(5),orientationkey).
paramtype(anim,mx(2),scalekey).
paramtype(anim,mx(4),selfillumcolorkey).
paramtype(anim,mx(2),alphakey).
paramtype(anim,mx(4),colorkey).
paramtype(anim,mx(2),radiuskey).
paramtype(anim,mx(2),multiplierkey).
paramtype(anim,mx(2),shadowradiuskey).
paramtype(anim,mx(2),verticaldisplacementkey).

/* These are the ones NWMax accepts as input for emitters */
paramtype(anim,mx(2),alphastartkey).
paramtype(anim,mx(2),alphaendkey).
paramtype(anim,mx(2),birthratekey).
paramtype(anim,mx(2),bounce_cokey).
paramtype(anim,mx(2),combinetimekey).
paramtype(anim,mx(2),dragkey).
paramtype(anim,mx(2),fpskey).
paramtype(anim,mx(2),framestartkey).
paramtype(anim,mx(2),frameendkey).
paramtype(anim,mx(2),gravkey).
paramtype(anim,mx(2),lifeexpkey).
paramtype(anim,mx(2),lightningdelaykey).
paramtype(anim,mx(2),lightningradiuskey).
paramtype(anim,mx(2),lightningscalekey).
paramtype(anim,mx(2),lightningsubdivkey).
paramtype(anim,mx(2),masskey).
paramtype(anim,mx(2),p2p_bezier2key).
paramtype(anim,mx(2),p2p_bezier3key).
paramtype(anim,mx(2),particlerotkey).
paramtype(anim,mx(2),randvelkey).
paramtype(anim,mx(2),sizestartkey).
paramtype(anim,mx(2),sizeendkey).
paramtype(anim,mx(2),spreadkey).
paramtype(anim,mx(2),velocitykey).
paramtype(anim,mx(2),xsizekey).
paramtype(anim,mx(2),ysizekey).
paramtype(anim,mx(4),colorstartkey).
paramtype(anim,mx(4),colorendkey).

/* These are valid ones that NWMax does not recognise as input for emitters */
paramtype(anim,mx(2),thresholdkey).	/* controller - not recognised by NWMax */
paramtype(anim,mx(2),sizestart_ykey).	/* controller - not recognised by NWMax */
paramtype(anim,mx(2),sizeend_ykey).	/* controller - not recognised by NWMax */
paramtype(anim,mx(2),blurlengthkey).	/* controller - not recognised by NWMax */
paramtype(anim,mx(2),alphamidkey).	/* controller - not NWMax       */
paramtype(anim,mx(4),colormidkey).	/* controller - not NWMax       */
paramtype(anim,mx(2),percentstartkey).	/* controller - not NWMax       */
paramtype(anim,mx(2),percentmidkey).	/* controller - not NWMax       */
paramtype(anim,mx(2),percentendkey).	/* controller - not NWMax       */
paramtype(anim,mx(2),sizemidkey).	/* controller - not NWMax       */
paramtype(anim,mx(2),sizemid_ykey).	/* controller - not NWMax       */

/* These are ones that should also be tolerated on import */
paramtype(anim,mx(2),ParamKey) :- paramtype(emitter,mn1,Param), atom_concat(Param,'key',ParamKey).
paramtype(anim,mx(4),ParamKey) :- paramtype(emitter,mn3,Param), atom_concat(Param,'key',ParamKey).
paramtype(anim,mx(2),ParamKey) :- paramtype(emitter,man,Param), atom_concat(Param,'key',ParamKey).

/* These are for animmesh nodes */
paramtype(anim,mx(3),verts).
paramtype(anim,mx(3),tverts).
paramtype(anim,mx(8),faces).
paramtype(anim,mx(3),animverts).
paramtype(anim,mx(3),animtverts).

paramtype(reference,A,B) :- paramtype(dummy,A,B).
paramtype(reference,man,refmodel).
paramtype(reference,mn1,reattachable).

/* These are to be discarded for all types of node */

paramtype(none,none,dummy).
paramtype(none,none,omni_light).
paramtype(none,none,cone_angle_manipulator).

/* ============ */
/* near_match/2 */
/* ============ */

near_match(A,B) :- dwim_match(A,B).
near_match(n_dynamic_type,ndynamictype).

/* =========== */
/* snap/2      */
/* snap/3      */
/* snap_up/2   */
/* snap_up/3   */
/* snap_down/2 */
/* snap_down/3 */
/* =========== */

snap(X,X1) :-
  float(X), !,
  once(g_user_option(snap,S)),
  snap(X,S,X1).
 
snap(X,X).

snap(X,binary,X1) :- !, X1 is round(128*X)/128.
snap(X,decimal,X1) :- !, X1 is round(100*X)/100.
snap(X,fine,X1) :- !, X1 is round(1000*X)/1000.
snap(X,_,X).

snap_up(X,X1) :-
  float(X), !,
  once(g_user_option(snap,S)),
  snap_up(X,S,X1).

snap_up(X,X).

snap_up(X,binary,X1) :- !, X1 is ceiling(128*X)/128.
snap_up(X,decimal,X1) :- !, X1 is ceiling(100*X)/100.
snap_up(X,fine,X1) :- !, X1 is ceiling(1000*X)/1000.
snap_up(X,_,X).

snap_down(X,X1) :-
  float(X), !,
  once(g_user_option(snap,S)),
  snap_down(X,S,X1).

snap_down(X,X).

snap_down(X,binary,X1) :- !, X1 is floor(128*X)/128.
snap_down(X,decimal,X1) :- !, X1 is floor(100*X)/100.
snap_down(X,fine,X1) :- !, X1 is floor(1000*X)/1000.
snap_down(X,_,X).

snap_equal(X,X1) :-
  once(g_user_option(snap,S)),
  snap_equal(X,S,X1).

snap_equal(X,binary,X1) :- !, abs(X-X1) < 0.0117.
snap_equal(X,decimal,X1) :- !, abs(X-X1) < 0.015.
snap_equal(X,fine,X1) :- !, abs(X-X1) < 0.0015.
snap_equal(X,_,X1) :- abs(X-X1) < 0.000001.

/* ========= */
/* snap_pi/2 */
/* ========= */

snap_pi(A,A1) :-
  float(A),
  D is 2.0*A/pi,
  abs(D-round(D))<0.01,
  !, A1 is round(D)*pi/2.
  
snap_pi(A,A).

/* ================== */
/* load_edge_tiles/1  */
/* load_edge_tiles/2  */
/* extract_tilename/1 */
/* six_atoms/3        */
/* atom/3             */
/* ================== */

load_edge_tiles(InDir) :-
  retractall(edgetile(_)),
  working_directory(WkDir,InDir),
  expand_file_name('*_edge.2da',FileList),
  working_directory(_,WkDir),
  FileList\=[],
  load_edge_tiles(InDir,FileList),
  write('Edge Tiles in '), write(FileList), write(' Loaded'), nl, nl.

load_edge_tiles(_) :-
  write('Cannot load edge tiles from 2da file'), nl, nl.

load_edge_tiles(_,[]) :- !.

load_edge_tiles(InDir,[File|FileList]) :-
  absolute_file_name(File,[relative_to(InDir)],AbsFile),
  open(AbsFile,read,Stream),
  repeat,
  read_line_to_codes(Stream,Line),
  extract_tilename(Line),
  Line=end_of_file,
  close(Stream), !,
  load_edge_tiles(InDir,FileList).

extract_tilename(end_of_file) :- !.

extract_tilename(Line) :-
  phrase(six_atoms(T),Line,_), T\=[32],
  name(TN,T), downcase_atom(TN,Tile),
  (\+edgetile(Tile) -> assertz(edgetile(Tile)); true),
  !.

six_atoms(T) -->
  opt_space,
  atom(_), space,
  atom(_), space,
  atom(_), space,
  atom(_), space,
  atom(_), space,
  atom(T).

opt_space --> space, !.
opt_space --> [].

space --> [32], !, opt_space.
space --> [9], !, opt_space.

atom(Q) --> [34], non_quotes(Q), [34], !.
atom(Q) --> unquoted_atom(Q).

unquoted_atom([C|C0]) --> [C], {C\=32, C\=9, C\=34}, unquoted_atom(C0), !.
unquoted_atom([C]) --> [C], {C\=32, C\=9, C\=34}.

non_quotes([C|C0]) --> [C], {C\=34}, non_quotes(C0), !.
non_quotes([C]) --> [C], {C\=34}.

my_number_chars(X,[46|T]) :- number_chars(X,[48,46|T]), !.

my_number_chars([],[45, 49, 46, 35, 81, 78, 65, 78|_]) :-
  /* -1.#QNAN */
  write('Invalid number encountered in file'), nl, !, fail.

my_number_chars(0,Token) :-
  name(Value,Token),
  downcase_atom(Value,'false'),
  !.

my_number_chars(1,Token) :-
  name(Value,Token),
  downcase_atom(Value,'true'),
  !.

my_number_chars(N,Token) :-
  number_chars(X,Token),
  I is round(X),
  (I =:= X -> N=I ; N=X).

my_downcase(N,N) :- number(N), !.
my_downcase(A,L) :- downcase_atom(A,L).
