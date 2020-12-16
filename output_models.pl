/* ================================================== */ 
/*                                                    */
/* Predicates for writing NWN ascii .mdl files        */
/* Part of the CleanModels 3 suite by OldMansBeard    */
/*                                                    */
/* This version dated 2013-08-10                      */
/* Later modifications by orth                        */
/*                                                    */
/* ================================================== */ 

/* ============= */
/* output_models */
/* ============= */

output_models :-
  mdl_file(File),
  \+ load_failed(File),
  \+ check_failed(File),
  downcase_atom(File,F1), write(F1), nl, !,
  output_file(File),
  fail.

output_models :-
  nl,
  (predicate_property(mdl_file(_),number_of_clauses(NFiles)) -> true; NFiles=0),
  (predicate_property(load_failed(_),number_of_clauses(NLoadFailed)) -> true; NLoadFailed=0),
  (predicate_property(check_failed(_),number_of_clauses(NCheckFailed)) -> true; NCheckFailed=0),
  NOutput is NFiles - NLoadFailed - NCheckFailed,
  write(NOutput), write(' Models exported.'), nl,
  nl.

/* ============= */
/* output_file/1 */
/* ============= */

output_file(File) :-
  tell(File),
  fail.

output_file(_) :-
  get_time(T), convert_time(T,TimeStamp),
  version(Version),
  write('# Rewritten by '), write(Version), tab(1), write(TimeStamp), nl,
  write('#MAXMODEL ASCII'), nl,
  fail.
  
output_file(File) :-
  gotdata(File,[],Data),
  print(Data), nl,
  fail.

output_file(File) :-
  gotdata(File,Model,newmodel(Model)),
  write(newmodel), tab(1), write(Model), nl,
  output_model(File,Model),
  write(donemodel), tab(1), write(Model), nl,
  fail.

output_file(File) :-
  told,
  write(File), tab(1), write('written.'), nl,
  fail.

output_file(File) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  atom_concat(Model,'.wok',WokFile),
  tell(WokFile),
  get_time(T), convert_time(T,TimeStamp),
  version(Version),
  write('# Rewritten by '), write(Version), tab(1), write(TimeStamp), nl,
  write('#MAXWALKMESH  ASCII'), nl,
  write(beginwalkmeshgeom), tab(1), write(Model), nl,
  output_node(File,Model,NodeName,NodeRef),
  write(endwalkmeshgeom), tab(1), write(Model), nl,
  told,
  write(WokFile), tab(1), write('written'), nl,
  fail.

output_file(_).

/* ============== */
/* output_model/2 */
/* ============== */

output_model(File,Model) :-
  gotdata(File,Model,classification(C)),
  tab(8), write(classification), tab(1), write(C), nl,
  fail.

/* Suppressed in CM343f
output_model(File,Model) :-
  gotdata(File,Model,bmin(X1,Y1,Z1)),
  tab(2), write(bmin), tab(1), write(X1), tab(1), write(Y1), tab(1), write(Z1), nl,
  fail.

output_model(File,Model) :-
  gotdata(File,Model,bmax(X2,Y2,Z2)),
  tab(2), write(bmax), tab(1), write(X2), tab(1), write(Y2), tab(1), write(Z2), nl,
  fail.

output_model(File,Model) :-
  gotdata(File,Model,radius(R)),
  tab(2), write(radius), tab(1), write(R), nl,
  fail.
*/

output_model(File,Model) :-
  gotdata(File,Model,setsupermodel(Model,S)),
  tab(8), write(setsupermodel), tab(1), write(Model), tab(1), print(S), nl,
  fail.

output_model(File,Model) :-
  (gotdata(File,Model,setanimationscale(S)) -> true; S=1),
  tab(8), print(setanimationscale(S)), nl,
  fail.

/* Suppressed in CM343f
output_model(File,Model) :-
  gotdata(File,Model,ignorefog(F)),
  F=\=0,
  tab(2), write(ignorefog), tab(1), write(F), nl,
  fail.
*/

output_model(File,Model) :-
  gotdata(File,Model,beginmodelgeom(Model)),
  write('#MAXGEOM ASCII'), nl,
  write(beginmodelgeom), tab(1), write(Model), nl,
  output_tree(File,Model,'NULL'),
  write(endmodelgeom), tab(1), write(Model), nl,
  fail.

output_model(File,Model) :-
  setof(A,gotdata(File,Model,newanim(A,Model)),AnimList),
  member(AnimName,AnimList),
  nl,
  write('#MAXANIM ASCII'), nl,
  write(newanim), tab(1), write(AnimName), tab(1), write(Model), nl,
  output_anim(File,Model,AnimName),
  write(doneanim), tab(1), write(AnimName), tab(1), write(Model), nl,
  fail.

output_model(_,_).

/* ============= */
/* output_tree/3 */
/* ============= */

output_tree(File,Model,Parent) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent)),
  \+atom_concat(Model,'a',NodeName),
  \+gotdata(File,Model,node(skin,NodeName)),
  \+is_transparent_node(File,Model,NodeName,NodeRef),
  output_node(File,Model,NodeName,NodeRef),
  output_tree(File,Model,NodeName/NodeRef),
  fail.

output_tree(File,Model,Parent) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent)),
  \+atom_concat(Model,'a',NodeName),
  \+gotdata(File,Model,node(skin,NodeName)),
  is_transparent_node(File,Model,NodeName,NodeRef),
  output_node(File,Model,NodeName,NodeRef),
  output_tree(File,Model,NodeName/NodeRef),
  fail.

output_tree(File,Model,Parent) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent)),
  atom_concat(Model,'a',NodeName),
  output_node(File,Model,NodeName,NodeRef),
  output_tree(File,Model,NodeName/NodeRef),
  fail.

output_tree(File,Model,Parent) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent)),
  \+atom_concat(Model,'a',NodeName),
  gotdata(File,Model,node(skin,NodeName)),
  output_node(File,Model,NodeName,NodeRef),
  output_tree(File,Model,NodeName/NodeRef),
  fail.

output_tree(_,_,_).

/* ============= */
/* output_node/4 */
/* ============= */
  
output_node(File,Model,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  tab(4), write(node), tab(1), write(NodeType), tab(1), write(NodeName), nl,
  fail.

output_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,parent(P)),
  (P=P1/_ -> Data=parent(P1) ; Data=parent(P)),
  tab(8), print(Data), nl,
  fail.

output_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,'#part-number'(P)),
  tab(8), write('#part-number'), tab(1), write(P), nl,
  fail.

output_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,Data),
  Data=..[A|_], \+outlistparm(A,_), A\=='parent', A\=='#part-number', A\=='sampleperiod',
  tab(8), print(Data), nl,
  fail.

output_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,sampleperiod(_)),
  tab(8), print(sampleperiod(0)), nl,
  fail.

output_node(File,Model,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  outlistparm(A,_), Q=..[A,N],
  \+ ((A=multimaterial, NodeType=aabb)),
  gotdata(File,Model,NodeName,NodeRef,Q),
  tab(8), write(A), tab(1), (A\=aabb -> write(N), nl;true),
  output_list(File,Model,NodeName,NodeRef,A,N,0),
  fail.

output_node(_,_,_,_) :-
  tab(4), write(endnode), nl.

/* ============= */
/* output_list/7 */
/* ============= */

output_list(_,_,_,_,_,N,N1) :- N1>=N, !.

output_list(File,Model,NodeName,NodeRef,colors,N,N1) :-
  Q=..[colors,N1,R,G,B], 
  gotdata(File,Model,NodeName,NodeRef,Q),
  R1 is R/255, G1 is G/255, B1 is B/255,
  tab(12), printlist([R1,G1,B1]), nl, !,
  N2 is N1+1, output_list(File,Model,NodeName,NodeRef,colors,N,N2). 

output_list(File,Model,NodeName,NodeRef,A,N,N1) :-
  A\==colors, outlistparm(A,W), W1 is W+1, functor(Q,A,W1), Q=..[A,N1|L], 
  gotdata(File,Model,NodeName,NodeRef,Q),
  tab(12), printlist(L), nl, !,
  N2 is N1+1, output_list(File,Model,NodeName,NodeRef,A,N,N2). 

outlistparm(multimaterial,1).
outlistparm(verts,3).
outlistparm(faces,8).
outlistparm(tverts,3).
outlistparm(tverts1,3).
outlistparm(tverts2,3).
outlistparm(tverts3,3).
outlistparm(texindices0,3).
outlistparm(texindices1,3).
outlistparm(texindices2,3).
outlistparm(texindices3,3).
outlistparm(aabb,7).
outlistparm(constraints,1).
outlistparm(colors,3).
outlistparm(texturenames,1).
outlistparm(flaresizes,1).
outlistparm(flarepositions,1).
outlistparm(flarecolorshifts,3).
/* outlistparm(animverts,3).  */
/* outlistparm(animtverts,3). */
outlistparm(weights,1).
outlistparm(normals,3).
outlistparm(tangents,4).

/* ============= */
/* output_anim/3 */
/* ============= */

output_anim(File,Model,AnimName) :-
  gotdata(File,Model,anim(AnimName),length(L)),
  tab(8), print(length(L)), nl,
  fail.

output_anim(File,Model,AnimName) :-
  gotdata(File,Model,anim(AnimName),transtime(L)),
  tab(8), print(transtime(L)), nl,
  fail.

output_anim(File,Model,AnimName) :-
  gotdata(File,Model,anim(AnimName),event(N,E)),
  tab(8), print(event(N,E)), nl,
  fail.

output_anim(File,Model,AnimName) :-
  gotdata(File,Model,anim(AnimName),animroot(Node)),
  tab(8), print(animroot(Node)), nl, !,
  output_anim_tree(File,Model,AnimName,'NULL').

/* ================== */
/* output_anim_tree/4 */
/* ================== */

output_anim_tree(File,Model,AnimName,NodeName/NodeRef) :-
  output_anim_node(File,Model,AnimName,NodeName,NodeRef),
  fail.

output_anim_tree(File,Model,_,'NULL') :-
  external_node_parent(File,Model,Child,'NULL'),
  output_external_node_tree(File,Model,Child),
  fail.

output_anim_tree(File,Model,_,NodeName/_) :-
  external_node_parent(File,Model,Child,NodeName),
  output_external_node_tree(File,Model,Child),
  fail.

output_anim_tree(File,Model,AnimName,Parent) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent)),
  \+atom_concat(Model,'a',NodeName),
  \+gotdata(File,Model,node(skin,NodeName)),
  \+is_transparent_node(File,Model,NodeName,NodeRef),
  output_anim_tree(File,Model,AnimName,NodeName/NodeRef),
  fail.

output_anim_tree(File,Model,AnimName,Parent) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent)),
  \+atom_concat(Model,'a',NodeName),
  \+gotdata(File,Model,node(skin,NodeName)),
  is_transparent_node(File,Model,NodeName,NodeRef),
  output_anim_tree(File,Model,AnimName,NodeName/NodeRef),
  fail.

output_anim_tree(File,Model,AnimName,Parent) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent)),
  atom_concat(Model,'a',NodeName),
  \+gotdata(File,Model,node(skin,NodeName)),
  output_anim_tree(File,Model,AnimName,NodeName/NodeRef),
  fail.

output_anim_tree(File,Model,AnimName,Parent) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent)),
  \+atom_concat(Model,'a',NodeName),
  gotdata(File,Model,node(skin,NodeName)),
  output_anim_tree(File,Model,AnimName,NodeName/NodeRef),
  fail.

output_anim_tree(_,_,_,_).

/* ================== */
/* output_anim_node/5 */
/* ================== */

output_anim_node(File,Model,AnimName,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(Type,NodeName)),true,NodeRef),
  (once((Type=light; Type=emitter; Type=reference; gotdata(File,Model,NodeName,NodeRef,AnimName,_))) -> OType=Type; OType=dummy),
  tab(8), write(node), tab(1), write(OType), tab(1), write(NodeName), nl,
  gotdata(File,Model,NodeName,NodeRef,parent(P)),
  tab(12), write(parent), tab(1), (P=Parent/_ -> write(Parent) ; write(P)), nl,
  fail.

output_anim_node(File,Model,_,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,'#part-number'(P)),
  tab(12), write('#part-number'), tab(1), write(P), nl,
  fail.

output_anim_node(File,Model,AnimName,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q),
  Q=..[Q0|_], once((paramtype(NodeType,T,Q0), atom(T))),
  tab(12), print(Q), nl,
  fail.

output_anim_node(File,Model,AnimName,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  node_type(NType,NodeType),
  /* All controllers for this NodeType */
  clause(controller_type(_,NType,P),R), R\=compiler(nwnmdlcomp),
  paramtype(NodeType,T,P), atom(T),
  (T==man -> NArgs=3; T==mn1 -> NArgs=3; T==mn3 -> NArgs=5; T==mn4 -> NArgs=6; fail),
  atom_concat(P,'key',Key),
  Q=..[Key,N],
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q),
  functor(Qk,Key,NArgs), /* Check added in CM344k */
  once(gotdata(File,Model,NodeName,NodeRef,AnimName,Qk)),
  tab(12), print(Key), nl,
  output_animlist(File,Model,NodeName,NodeRef,AnimName,Key,NArgs,N,0),
  tab(12), write(endlist), nl,
  fail.

output_anim_node(File,Model,AnimName,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  once((
    gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(_));
    gotdata(File,Model,NodeName,NodeRef,AnimName,animtverts(_))
      )),
  member(Q0,[verts,faces,tverts,tverts1,tverts2,tverts3]),
  Q=..[Q0,N],
  gotdata(File,Model,NodeName,NodeRef,Q),
  tab(12), print(Q0), tab(1), write(N), nl,
  output_list(File,Model,NodeName,NodeRef,Q0,N,0),
  fail.

output_anim_node(File,Model,AnimName,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  member(Q0,[animverts,animtverts]),
  Q=..[Q0,N],
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q),
  tab(12), print(Q0), tab(1), write(N), nl,
  output_animlist(File,Model,NodeName,NodeRef,AnimName,Q0,4,N,0),
  fail.

output_anim_node(_,_,_,_,_) :-
  tab(8), write(endnode), nl.
  
/* ================== */
/* output_animlist/9  */
/* ================== */

output_animlist(_,_,_,_,_,_,_,N,N1) :- N1>=N, !.

output_animlist(File,Model,NodeName,NodeRef,AnimName,Key,NArgs,N,N1) :- 
  functor(Q,Key,NArgs), Q=..[Key,N1|F],
  /* CM3.6.2k : skip missing keys */
  (gotdata(File,Model,NodeName,NodeRef,AnimName,Q) -> tab(6), printlist(F), nl ; true),
  !,
  N2 is N1+1, output_animlist(File,Model,NodeName,NodeRef,AnimName,Key,NArgs,N,N2).

/* =========================== */
/* output_external_node_tree/3 */
/* =========================== */

output_external_node_tree(File,Model,NodeName) :-
  tab(8), write('node dummy '), write(NodeName), nl,
  external_node_parent(File,Model,NodeName,ParentName),
  tab(12), write('parent '), write(ParentName), nl,
  tab(12), write('#part-number -1'), nl,
  tab(8), write('endnode'), nl,
  fail.

output_external_node_tree(File,Model,NodeName) :-
  external_node_parent(File,Model,ChildName,NodeName),
  output_external_node_tree(File,Model,ChildName),
  fail.  

output_external_node_tree(_,_,_) :- !.

/* ======= */
/* "other" */
/* ======= */

portray(X) :- secret(raw_output), !, write(X).

/* changed in 3.5 2013-07-18 */
portray(X) :- float(X), !, format('~g',X).

portray(X) :- atom(X), !, (camel(X,Y) -> write(Y); write(X)).
portray(X) :- is_list(X), !, printlist(X).
portray(X) :- X=..[K|K0], K0\==[], !, printlist([K|K0]).

printlist([]).
printlist([L]) :- is_list(L), !, printlist(L).
printlist([L]) :- print(L), !.
printlist([L|L0]) :- print(L), tab(1), !, printlist(L0).

camel(ParamKey,CamelKey) :-
  atom_concat(Param,'bezierkey',ParamKey),
  !,
  (clause(camel(Param,Camel),true) -> atom_concat(Camel,'bezierkey',CamelKey) ; CamelKey = ParamKey).

camel(ParamKey,CamelKey) :-
  atom_concat(Param,'key',ParamKey),
  !,
  (clause(camel(Param,Camel),true) -> atom_concat(Camel,'key',CamelKey) ; CamelKey = ParamKey).

camel(ndynamictype,nDynamicType).
camel(iconsize,iconSize).
camel(colorstart,colorStart).
camel(colormid,colorMid).
camel(colorend,colorEnd).
camel(alphastart,alphaStart).
camel(alphamid,alphaMid).
camel(alphaend,alphaEnd).
camel(sizestart,sizeStart).
camel(sizemid,sizeMid).
camel(sizeend,sizeEnd).
camel(sizestart_y,sizeStart_y).
camel(sizemid_y,sizeMid_y).
camel(sizeend_y,sizeEnd_y).
camel(percentstart,percentStart).
camel(percentmid,percentMid).
camel(percentend,percentEnd).
camel(lifeexp,lifeExp).
camel(particlerot,particleRot).
camel(m_istinted,m_isTinted).
camel(affectedbywind,affectedByWind).
camel(framestart,frameStart).
camel(frameend,frameEnd).
camel(lightningdelay,lightningDelay).
camel(lightningradius,lightningRadius).
camel(lightningscale,lightningScale).
camel(lightningsubdiv,lightningSubDiv).
camel(blastradius,blastRadius).
camel(blastlength,blastLength).
camel(chunkname,chunkName).
/* Emitter update, render and blend parameters - may be incomplete */
camel(fountain,'Fountain').
camel(single,'Single').
camel(explosion,'Explosion').
camel(lightning,'Lightning').
camel(normal,'Normal').
camel(linked,'Linked').
camel(billboard_to_local_z,'Billboard_to_Local_Z').
camel(billboard_to_world_z,'Billboard_to_World_Z').
camel(aligned_to_world_z,'Aligned_to_World_Z').
camel(aligned_to_particle_dir,'Aligned_to_Particle_Dir').
camel(motion_blur,'Motion_Blur').
camel('punch-through','Punch-Through').
camel(lighten,'Lighten').
camel(trail,'Trail').
camel(bezier,'Bezier').
camel(gravity,'Gravity').
camel(refmodel,'refModel').
camel(dummy,'Dummy').


/* ===================== */
/* is_transparent_node/4 */
/* ===================== */

is_transparent_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,alpha(A)), A<1, !.

is_transparent_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,_,alpha(A)), A<1, !.

is_transparent_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,_,alphakey(_,_,A)), A<1, !.

is_transparent_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater\=no,
  is_watery(File,Model,NodeName,NodeRef), !.

is_transparent_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(foliage,Foliage)), Foliage==animate,
  is_foliage(File,Model,NodeName,NodeRef), !.

is_transparent_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(splotch,Splotch)), Splotch==animate,
  is_splotch(File,Model,NodeName,NodeRef), !.

is_transparent_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,classification('CHARACTER')),
  once(g_user_option(placeable_with_transparency,Option)), Option\=no,
  is_transparency(File,Model,NodeName,NodeRef), !.

