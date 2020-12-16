/* ================================================== */ 
/*                                                    */
/* Predicates for testing for anomalies in gotdata    */
/* Part of the CleanModels 3 suite by OldMansBeard    */
/*                                                    */
/* This version dated 2014-05-09                      */
/*                                                    */
/* ================================================== */ 

:- dynamic face_normal/6.
:- dynamic check_failed/1.
:- dynamic tvert_group/6.
:- dynamic fixlist/1.
:- dynamic slice_height/2.
:- dynamic hashlist/3.
:- dynamic renumbering/2.
:- dynamic vertex_normal/6.
:- dynamic vertex_normal_part/6.

cm3_verbose :- secret(verbose). /* changed in 3.5.1a because we no longer use optimise */

/* =========================== */
/* Checks on the model itself  */
/* =========================== */

check_for(_,File,_) :-
  retractall(bug_count(File,_)),
  asserta(bug_count(File,0)),
  fail.

check_for(not_model,File,SmallLogStream) :-
  \+ gotdata(File,Model,newmodel(Model)),
  report_error([File,'does not contain a model'],SmallLogStream),
  bad_error(File),
  fail.

check_for(classification,File,_) :-
  once(g_user_option(classification,Classification)),
  Classification\==automatic,
  upcase_atom(Classification,CLASS),
  clause(gotdata(File,Model,classification(ModelClass)),true,Eref),
  ModelClass\==CLASS,
  erase(Eref),
  asserta(gotdata(File,Model,classification(CLASS))),
  tab(2), write('model '), write(Model), write(' changed to classification '), write(CLASS), nl,
  increment_bug_count(File),
  fail.

check_for(classification,File,_) :-
  once(g_user_option(classification,Classification)),
  Classification\==automatic,
  upcase_atom(Classification,CLASS),
  gotdata(File,Model,newmodel(Model)),
  \+ gotdata(File,Model,classification(_)),
  asserta(gotdata(File,Model,classification(CLASS))),
  tab(2), write('model '), write(Model), write(' changed to classification '), write(CLASS), nl,
  increment_bug_count(File),
  fail.

check_for(wrong_model_name,File,SmallLogStream) :-
  atom_concat(TileName,'.mdl',File),
  gotdata(File,Model,newmodel(Model)),
  Model\=TileName,
  report_error([Model,'is in the wrong file'],SmallLogStream),
  bad_error(File),
  fail.

check_for(duplicate_models,File,SmallLogStream) :-
  clause(gotdata(File,Model,newmodel(Model)),true,Ref1),
  clause(gotdata(File,Model,newmodel(Model)),true,Ref2),
  Ref2@>Ref1,
  report_error([Model,'is duplicated'],SmallLogStream),
  bad_error(File),
  fail.

check_for(too_many_walkmeshes,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(aabb,_)),true,Ref1),
  clause(gotdata(File,Model,node(aabb,_)),true,Ref2),
  Ref2@>Ref1,
  report_error([Model,'has more than one walkmesh defined'],SmallLogStream),
  bad_error(File),
  fail.
  
check_for(duplicate_animations,File,SmallLogStream) :-
  clause(gotdata(File,Model,newanim(AnimName,Model)),true,Ref1),
  clause(gotdata(File,Model,newanim(AnimName,Model)),true,Ref2),
  Ref2@<Ref1,
  erase(Ref2),
  report_warning(['animation',AnimName,'was repeated - keys merged'],SmallLogStream),
  increment_bug_count(File),
  fail.

/* ======================= */
/* Checks on special nodes */
/* ======================= */

check_for(special_node_names,File,_) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  downcase_atom(NodeName,DownCaseName), DownCaseName\==NodeName,
  clause(gotdata(File,Model,ChildName,ChildRef,parent(DownCaseName/_)),true,Cref),
  erase(Cref),
  asserta(gotdata(File,Model,ChildName,ChildRef,parent(NodeName/NodeRef))),
  fail.

check_for(special_node_names,File,_) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  (NodeType='light' ; NodeType='dummy'),
  is_wrong_tilename(Model,NodeName,NodeType,RightName),
  \+ gotdata(File,Model,node(_,RightName)),
  rename_node(File,Model,NodeName,NodeRef,RightName),
  tab(2), write('node '), write(NodeName), write(' corrected to '), write(RightName), nl,
  increment_bug_count(File),
  fail.

/* ============================== */
/* Checks on the base dummy node  */
/* ============================== */

check_for(duplicate_base_dummy,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(_,Model)),true,Ref1),
  once((clause(gotdata(File,Model,node(_,Model)),true,Ref2),Ref2@>Ref1)),
  report_error(['duplicate model base'],SmallLogStream),
  bad_error(File),
  fail.

check_for(base_dummy_wrong_type,File,SmallLogStream) :-
  gotdata(File,Model,node(NodeType,Model)), NodeType\='dummy',
  report_error(['model base node is not of type dummy'],SmallLogStream),
  bad_error(File),
  fail.

check_for(base_dummy_has_data,File,_) :-
  clause(gotdata(File,Model,node(dummy,Model)),true,NodeRef),
  clause(gotdata(File,Model,Model,NodeRef,position(X,Y,Z)),true,Cref),
  X=:=0, Y=:=0, Z=:=0,
  erase(Cref),
  tab(2), write('explicit zero position deleted from base dummy'), nl,
  increment_bug_count(File),
  fail.

check_for(base_dummy_has_data,File,_) :-
  clause(gotdata(File,Model,node(dummy,Model)),true,NodeRef),
  clause(gotdata(File,Model,Model,NodeRef,orientation(_,_,_,A)),true,Cref),
  A=:=0,
  erase(Cref),
  tab(2), write('explicit null orientation deleted from base dummy'), nl,
  increment_bug_count(File),
  fail.

check_for(base_dummy_has_data,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(dummy,Model)),true,NodeRef),
  gotdata(File,Model,Model,NodeRef,Q), Q\=parent(_), Q\='#part-number'(_),
  report_error(['model base has invalid data:',Q],SmallLogStream),
  bad_error(File),
  fail.

check_for(base_dummy_exists,File,_) :-
  gotdata(File,Model,beginmodelgeom(Model)),
  \+ gotdata(File,Model,node(_,Model)),
  asserta(gotdata(File,Model,node(dummy,Model)),NodeRef),
  asserta(gotdata(File,Model,Model,NodeRef,parent('NULL'))),
  tab(2), write('dummy node '), write(Model), write(' created'), nl,
  increment_bug_count(File),
  fail.

check_for(base_dummy_parent_NULL,File,_) :-
  clause(gotdata(File,Model,node(dummy,Model)),true,NodeRef),
  \+ gotdata(File,Model,Model,NodeRef,parent(_)),
  asserta(gotdata(File,Model,Model,NodeRef,parent('NULL'))),
  tab(2), write('root node did not have parent NULL - fixed'), nl,
  increment_bug_count(File),
  fail.

check_for(base_dummy_parent_NULL,File,_) :-
  clause(gotdata(File,Model,node(dummy,Model)),true,NodeRef),
  gotdata(File,Model,Model,NodeRef,parent(Parent)), Parent\='NULL',
  retract(gotdata(File,Model,Model,NodeRef,parent(Parent))),
  asserta(gotdata(File,Model,Model,NodeRef,parent('NULL'))),
  tab(2), write('root node did not have parent NULL - fixed'), nl,
  increment_bug_count(File),
  fail.

check_for(animated_base_dummy,File,_) :-
  once(gotdata(File,Model,Model,_,_,_)),
  retractall(gotdata(File,Model,Model,_,_,_)),
  tab(2), write('deleted animation keys on root node'), nl,
  increment_bug_count(File),
  fail.

/* =================================================== */
/* CM3.6.2d : silently remove spaces from weight lists */
/* =================================================== */

check_for(spaces_in_weight_lists,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,weights(V,W)),true,Eref),
  unspace_weight_list(W,W1),
  W1\=W,
  erase(Eref),
  assertz(gotdata(File,Model,NodeName,NodeRef,weights(V,W1))),
  fail.

/* ========================================================= */
/* CM3.6.2q : unscramble orientations of Morrowind skeletons */
/* ========================================================= */

check_for(twisted_skeleton,File,_) :-
  secret(untwist_skeleton),
  gotdata(File,Model,newmodel(Model)),
  reset_orientations(File,Model),
  tab(2), write('orientations reset'), nl,
  fail.
  
/* ================================================= */
/* CM3.6.2h : repivot existing skins to model origin */
/* ================================================= */

check_for(skins_pivoted_to_origin,File,_) :-
  clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef),
  \+gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  raise_to_tile(File,Model,NodeName,NodeRef),
  tab(2), write('skinmesh '), write(NodeName), write(' re-parented to Aurora base'), nl,
  fail.

check_for(skins_pivoted_to_origin,File,_) :-
  clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef),
  \+gotdata(File,Model,NodeName,NodeRef,orientation(0,0,0,0)),
  set_zero_orientation(File,Model,NodeName,NodeRef),
  tab(2), write('skinmesh '), write(NodeName), write(' re-aligned to origin'), nl,
  fail.

check_for(skins_pivoted_to_origin,File,_) :-
  clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef),
  \+gotdata(File,Model,NodeName,NodeRef,position(0,0,0)),
  set_zero_position(File,Model,NodeName,NodeRef),
  tab(2), write('skinmesh '), write(NodeName), write(' re-pivoted to origin'), nl,
  fail.

/* ============================== */
/* Delete clutter from edge tiles */
/* ============================== */

check_for(clutter_in_edge_tile,File,_) :-
  gotdata(File,Model,classification('TILE')),
  edgetile(Model),
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  retractall(gotdata(File,Model,NodeName,NodeRef,_)),
  retract(gotdata(File,Model,node(aabb,NodeName))),
  increment_bug_count(File),
  tab(2), write('edge tile - deleted walkmesh'), nl,
  increment_bug_count(File),
  fail.

check_for(clutter_in_edge_tile,File,_) :-
  gotdata(File,Model,classification('TILE')),
  edgetile(Model),
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  retractall(gotdata(File,Model,NodeName,NodeRef,_)),
  retract(gotdata(File,Model,node(emitter,NodeName))),
  tab(2), write('edge tile - deleted emitter '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(clutter_in_edge_tile,File,_) :-
  gotdata(File,Model,classification('TILE')),
  edgetile(Model),
  clause(gotdata(File,Model,node(light,NodeName)),true,NodeRef),
  retractall(gotdata(File,Model,NodeName,NodeRef,_)),
  retract(gotdata(File,Model,node(light,NodeName))),
  tab(2), write('edge tile - deleted light '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(clutter_in_edge_tile,File,_) :-
  gotdata(File,Model,classification('TILE')),
  edgetile(Model),
  gotdata(File,Model,newanim(Anim,Model)),
  retractall(gotdata(File,Model,anim(Anim),_)),
  retractall(gotdata(File,Model,_,_,_,_)),
  retract(gotdata(File,Model,newanim(Anim,Model))),
  tab(2), write('edge tile - deleted animation '), write(Anim), nl,
  increment_bug_count(File),
  fail.

/* ======================================================= */
/* Pre-checks on animation keys to delete meaningless ones */
/* ======================================================= */

check_for(parameters_that_cannot_be_animated,File,_) :-
  paramtype(anim,mx(N),K),
  atom_concat(P,'key',K),
  \+clause(controller_type(_,_,P),_),
  Q0=..[K,_],
  once(gotdata(_,_,_,_,_,Q0)), 
  Arity is N+1, functor(Q,K,Arity),
  retractall(gotdata(_,_,_,_,_,Q0)),
  retractall(gotdata(_,_,_,_,_,Q)),
  tab(2), write('deleted '), write(K), write('s - '), write(P), write(' cannot be animated'), nl,
  increment_bug_count(File),
  fail.

check_for(parameters_that_cannot_be_animated,File,_) :-
  paramtype(NodeType,M,P), NodeType\='anim', NodeType\='animmesh',
  \+clause(controller_type(_,_,P),_),
  once((M=man -> Arity=1 ; M=mn1 -> Arity=1 ; M=mn3 -> Arity=3 ; M=mn4 -> Arity=4 ; fail)),
  functor(Q,P,Arity),
  once(gotdata(_,_,_,_,_,Q)),
  retractall(gotdata(_,_,_,_,_,Q)),
  tab(2), write('deleted '), write(P), write(' data from animations'), nl,
  increment_bug_count(File),
  fail.

/* ======================= */
/* Checks on the 'a' node  */
/* ======================= */

check_for(a_dummy_wrong_type,File,_) :-
  gotdata(File,Model,classification(Classification)),
  once(g_user_option(placeable_with_transparency,PlaceableWithTransparency)),
  once((Classification=='TILE' ; PlaceableWithTransparency==yes)),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(NodeType,Adummy)),true,ARef), NodeType\='dummy',
  once((between(1,inf,N), atom_concat('cm3_repair_',N,NewName), \+ gotdata(File,Model,node(_,NewName)))),
  rename_node(File,Model,Adummy,ARef,NewName),
  tab(2), write(NodeType), tab(1), write(Adummy), write(' renamed to '), write(NewName), nl,
  increment_bug_count(File),
  fail.

check_for(a_dummy_wrong_position,File,_) :-
  gotdata(File,Model,classification(Classification)),
  once(g_user_option(placeable_with_transparency,PlaceableWithTransparency)),
  once((Classification=='TILE' ; PlaceableWithTransparency==yes)),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  gotdata(File,Model,Adummy,ARef,position(X,Y,Z)),
  \+((X=:=0, Y=:=0, Z=:=0)),
  once((between(1,inf,N), atom_concat('cm3_repair_',N,NewName), \+ gotdata(File,Model,node(_,NewName)))),
  rename_node(File,Model,Adummy,ARef,NewName),
  tab(2), write(Adummy), write(' not at tile origin - renamed to '), write(NewName), nl,
  increment_bug_count(File),
  fail.

check_for(a_dummy_wrong_orientation,File,_) :-
  gotdata(File,Model,classification(Classification)),
  once(g_user_option(placeable_with_transparency,PlaceableWithTransparency)),
  once((Classification=='TILE' ; PlaceableWithTransparency==yes)),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,Aref),
  gotdata(File,Model,Adummy,Aref,orientation(_,_,_,A)), A=\=0,
  once((between(1,inf,N), atom_concat('cm3_repair_',N,NewName), \+ gotdata(File,Model,node(_,NewName)))),
  rename_node(File,Model,Adummy,Aref,NewName),
  tab(2), write(Adummy), write(' has non-null orientation - renamed to '), write(NewName), nl,
  increment_bug_count(File),
  fail.

check_for(a_dummy_has_animations,File,_) :-
  gotdata(File,Model,classification(Classification)),
  once(g_user_option(placeable_with_transparency,PlaceableWithTransparency)),
  once((Classification=='TILE' ; PlaceableWithTransparency==yes)),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,Aref),
  once((
    gotdata(File,Model,Adummy,Aref,_,position(X,Y,Z)), \+ (X=:=0, Y=:=0, Z=:=0);
    gotdata(File,Model,Adummy,Aref,_,positionkey(_,_,X,Y,Z)), \+ (X=:=0, Y=:=0, Z=:=0);
    gotdata(File,Model,Adummy,Aref,_,orientation(_,_,_,A)), A=\=0;
    gotdata(File,Model,Adummy,Aref,_,orientationkey(_,_,_,_,_,A)), A=\=0
      )),
  once((between(1,inf,N), atom_concat('cm3_repair_',N,NewName), \+ gotdata(File,Model,node(_,NewName)))),
  rename_node(File,Model,Adummy,Aref,NewName),
  tab(2), write(Adummy), write(' has animation data - renamed to '), write(NewName), nl,
  increment_bug_count(File),
  fail.

check_for(a_dummy_has_animations,File,_) :-
  gotdata(File,Model,classification(Classification)),
  once(g_user_option(placeable_with_transparency,PlaceableWithTransparency)),
  once((Classification=='TILE' ; PlaceableWithTransparency==yes)),
  atom_concat(Model,'a',Adummy),
  once(gotdata(File,Model,Adummy,_,_,_)),
  retractall(gotdata(File,Model,Adummy,_,_,_)),
  tab(2), write('null animation data deleted from '), write(Adummy), nl,
  increment_bug_count(File),
  fail.

check_for(a_dummy_exists,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once((gotdata(File,Model,_,_,_,_); gotdata(File,Model,node(emitter,_)))),
  atom_concat(Model,'a',Adummy),
  \+ gotdata(File,Model,node(dummy,Adummy)),
  asserta(gotdata(File,Model,node(dummy,Adummy)),NodeRef),
  clause(gotdata(File,Model,node(dummy,Model)),true,ModelRef),
  asserta(gotdata(File,Model,Adummy,NodeRef,parent(Model/ModelRef))),
  asserta(gotdata(File,Model,Adummy,NodeRef,position(0,0,0))),
  asserta(gotdata(File,Model,Adummy,NodeRef,orientation(0,0,0,0))),
  asserta(gotdata(File,Model,Adummy,NodeRef,wirecolor(1,0,0))),
  tab(2), write('new dummy node '), write(Adummy), write(' created'), nl,
  increment_bug_count(File),
  fail.

check_for(a_dummy_exists,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater\=no,
  gotdata(File,Model,newmodel(Model)),
  \+ edgetile(Model),
  once(is_watery(File,Model,_,_)),
  atom_concat(Model,'a',Adummy),
  \+ gotdata(File,Model,node(dummy,Adummy)),
  asserta(gotdata(File,Model,node(dummy,Adummy)),NodeRef),
  clause(gotdata(File,Model,node(dummy,Model)),true,ModelRef),
  asserta(gotdata(File,Model,Adummy,NodeRef,parent(Model/ModelRef))),
  asserta(gotdata(File,Model,Adummy,NodeRef,position(0,0,0))),
  asserta(gotdata(File,Model,Adummy,NodeRef,orientation(0,0,0,0))),
  asserta(gotdata(File,Model,Adummy,NodeRef,wirecolor(1,0,0))),
  tab(2), write('new dummy node '), write(Adummy), write(' created'), nl,
  increment_bug_count(File),
  fail.

check_for(a_dummy_exists,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(foliage,Foliage)), Foliage==animate,
  gotdata(File,Model,newmodel(Model)),
  \+ edgetile(Model),
  once(is_foliage(File,Model,_,_)),
  atom_concat(Model,'a',Adummy),
  \+ gotdata(File,Model,node(dummy,Adummy)),
  asserta(gotdata(File,Model,node(dummy,Adummy)),NodeRef),
  clause(gotdata(File,Model,node(dummy,Model)),true,ModelRef),
  asserta(gotdata(File,Model,Adummy,NodeRef,parent(Model/ModelRef))),
  asserta(gotdata(File,Model,Adummy,NodeRef,position(0,0,0))),
  asserta(gotdata(File,Model,Adummy,NodeRef,orientation(0,0,0,0))),
  asserta(gotdata(File,Model,Adummy,NodeRef,wirecolor(1,0,0))),
  tab(2), write('new dummy node '), write(Adummy), write(' created'), nl,
  increment_bug_count(File),
  fail.

check_for(a_dummy_exists,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(splotch,Splotch)), Splotch==animate,
  gotdata(File,Model,newmodel(Model)),
  \+ edgetile(Model),
  once(is_splotch(File,Model,_,_)),
  atom_concat(Model,'a',Adummy),
  \+ gotdata(File,Model,node(dummy,Adummy)),
  asserta(gotdata(File,Model,node(dummy,Adummy)),NodeRef),
  clause(gotdata(File,Model,node(dummy,Model)),true,ModelRef),
  asserta(gotdata(File,Model,Adummy,NodeRef,parent(Model/ModelRef))),
  asserta(gotdata(File,Model,Adummy,NodeRef,position(0,0,0))),
  asserta(gotdata(File,Model,Adummy,NodeRef,orientation(0,0,0,0))),
  asserta(gotdata(File,Model,Adummy,NodeRef,wirecolor(1,0,0))),
  tab(2), write('new dummy node '), write(Adummy), write(' created'), nl,
  increment_bug_count(File),
  fail.

check_for(a_dummy_exists,File,_) :-
  once(g_user_option(placeable_with_transparency,PlaceableWithTransparency)),
  PlaceableWithTransparency\=no,
  gotdata(File,Model,newmodel(Model)),
  once(is_transparency(File,Model,_,_)),
  atom_concat(Model,'a',Adummy),
  \+ gotdata(File,Model,node(dummy,Adummy)),
  asserta(gotdata(File,Model,node(dummy,Adummy)),NodeRef),
  clause(gotdata(File,Model,node(dummy,Model)),true,ModelRef),
  asserta(gotdata(File,Model,Adummy,NodeRef,parent(Model/ModelRef))),
  asserta(gotdata(File,Model,Adummy,NodeRef,position(0,0,0))),
  asserta(gotdata(File,Model,Adummy,NodeRef,orientation(0,0,0,0))),
  asserta(gotdata(File,Model,Adummy,NodeRef,wirecolor(1,0,0))),
  tab(2), write('new dummy node '), write(Adummy), write(' created'), nl,
  increment_bug_count(File),
  fail.

/* ============================ */
/* Checks for duplicated nodes  */
/* ============================ */

check_for(duplicated_mainlights,File,_) :-
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(light,NodeName)),true,Ref1),
  clause(gotdata(File,Model,node(light,NodeName)),true,Ref2),
  Ref2@>Ref1,
  atom_concat(Model,'ml',BaseName),
  atom_concat(BaseName,Serial,NodeName), name(Serial,NS), name(D,NS), integer(D),
  \+ gotdata(File,Model,NodeName,_,_,_),
  \+ gotdata(File,Model,_,_,parent(NodeName/Ref2)),
  once((between(1,2,NewSerial), atom_concat(BaseName,NewSerial,NewName), \+ gotdata(File,Model,node(_,NewName)))),
  rename_node(File,Model,NodeName,Ref2,NewName),
  tab(2), write('duplicate light '), write(NodeName), write(' renumbered to '), write(NewName), nl,
  increment_bug_count(File),
  fail.

check_for(duplicated_sourcelights,File,_) :-
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(light,NodeName)),true,Ref1),
  clause(gotdata(File,Model,node(light,NodeName)),true,Ref2),
  Ref2@>Ref1,
  atom_concat(Model,'sl',BaseName),
  atom_concat(BaseName,Serial,NodeName), name(Serial,NS), name(D,NS), integer(D),
  \+ gotdata(File,Model,NodeName,_,_,_),
  \+ gotdata(File,Model,_,_,parent(NodeName/Ref2)),
  once((between(1,2,NewSerial), atom_concat(BaseName,NewSerial,NewName), \+ gotdata(File,Model,node(_,NewName)))),
  rename_node(File,Model,NodeName,Ref2,NewName),
  tab(2), write('duplicate light '), write(NodeName), write(' renumbered to '), write(NewName), nl,
  increment_bug_count(File),
  fail.

check_for(duplicated_u_or_d_dummies,File,_) :-
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(dummy,NodeName)),true,Ref1),
  clause(gotdata(File,Model,node(dummy,NodeName)),true,Ref2),
  Ref2@>Ref1,
  (atom_concat(Model,'_D0',BaseName) ; atom_concat(Model,'_U0',BaseName)),
  atom_concat(BaseName,Serial,NodeName), name(Serial,NS), name(D,NS), integer(D),
  \+ gotdata(File,Model,NodeName,_,_,_),
  \+ gotdata(File,Model,_,_,parent(NodeName/Ref2)),
  once((between(1,9,NewSerial), atom_concat(BaseName,NewSerial,NewName), \+ gotdata(File,Model,node(_,NewName)))),
  rename_node(File,Model,NodeName,Ref2,NewName),
  tab(2), write('duplicate dummy '), write(NodeName), write(' renumbered to '), write(NewName), nl,
  increment_bug_count(File),
  fail.

check_for(duplicate_node_names,File,_) :-
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  gotdata(File,Model,node(NodeType,NodeName)),
  \+ ((atom_concat(Model,_,NodeName), (NodeType='light' ; NodeType='dummy'))),
  \+ gotdata(File,Model,NodeName,_,_,_),
  \+ ((gotdata(File,Model,_,_,weights(_,W)), member(NodeName,W))),
  clause(gotdata(File,Model,node(_,NodeName)),true,Ref1),
  \+ (clause(gotdata(File,Model,node(_,NodeName)),true,Ref0), Ref0@<Ref1),
  clause(gotdata(File,Model,node(_,NodeName)),true,Ref2), Ref2@>Ref1,
  \+ gotdata(File,Model,_,_,parent(NodeName/Ref2)),
  renumber_node(NodeName,NewNodeName),
  rename_node(File,Model,NodeName,Ref2,NewNodeName),
  tab(2), write('duplicate node '), write(NodeName), write(' renumbered as '), write(NewNodeName), nl,
  increment_bug_count(File),
  fail.

/** Comment out this clause when animnodes are disambiguated on import **/

check_for(duplicate_node_names,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,Ref1),
  once((clause(gotdata(File,Model,node(_,NodeName)),true,Ref2),Ref2@>Ref1)),
  once((gotdata(File,Model,NodeName,_,_,_))),
  report_error(['animated node',NodeName,'is duplicated'],SmallLogStream),
  bad_error(File),
  fail.


check_for(duplicate_node_names,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,Ref1),
  once((clause(gotdata(File,Model,node(_,NodeName)),true,Ref2),Ref2@>Ref1)),
  once((gotdata(File,Model,Skin,_,weights(_,W)), member(NodeName,W))),
  report_error(['ambiguous bone name',NodeName,'in skin',Skin],SmallLogStream),
  bad_error(File),
  fail.

check_for(duplicate_node_names,File,SmallLogStream) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(_,NodeName)),true,Ref1),
  atom_concat(Model,Suffix,NodeName), Suffix@>'',
  once((clause(gotdata(File,Model,node(_,NodeName)),true,Ref2),Ref2@>Ref1)),
  report_error(['tile node',NodeName,'is duplicated'],SmallLogStream),
  bad_error(File),
  fail.

/* ========================================= */
/* Checks for duplicated or missing parents  */
/* ========================================= */

check_for(missing_parents,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,parent(_)),
  report_error(['node',NodeName,'has no parent node defined'],SmallLogStream),
  bad_error(File),
  fail.

check_for(missing_parents,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  NodeName\=Model,
  gotdata(File,Model,NodeName,NodeRef,parent(Parent/ -1)), Parent\==NodeName,
  report_error(['the parent',Parent,'of node',NodeName,'does not exist'],SmallLogStream),
  bad_error(File),
  fail.

check_for(missing_parents,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  NodeName\=Model,
  gotdata(File,Model,NodeName,NodeRef,parent(NodeName/ -1)),
  report_error(['node',NodeName,'is parented back to itself'],SmallLogStream),
  bad_error(File),
  fail.

/* ========================================================= */
/* Lights and aabb nodes should be parented to the tile.     */
/* Emitters should descend from the 'a' node.                */
/* ========================================================= */

check_for(light_has_child_nodes,File,_) :-
  \+ check_failed(File),
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(light,NodeName)),true,NodeRef),
  atom_concat(Model,Suffix,NodeName),
  once((atom_concat('ml',_,Suffix); atom_concat('sl',_,Suffix))),
  once(gotdata(File,Model,_,_,parent(NodeName/NodeRef))),
  once((between(1,inf,N), atom_concat('cm3_repair_',N,NewName), \+ gotdata(File,Model,node(_,NewName)))),
  asserta(gotdata(File,Model,node(dummy,NewName)),NewRef),
  gotdata(File,Model,NodeName,NodeRef,parent(P)),
  asserta(gotdata(File,Model,NewName,NewRef,parent(P))),
  gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z)),
  asserta(gotdata(File,Model,NewName,NewRef,position(X,Y,Z))),
  gotdata(File,Model,NodeName,NodeRef,orientation(U,V,W,A)),
  asserta(gotdata(File,Model,NewName,NewRef,orientation(U,V,W,A))),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,position(X1,Y1,Z1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,position(X1,Y1,Z1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,orientation(U1,V1,W1,A1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,orientation(U1,V1,W1,A1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(N1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,positionkey(N1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(N1,T1,X1,Y1,Z1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,positionkey(N1,T1,X1,Y1,Z1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,orientationkey(N1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N1,T1,U1,V1,W1,A1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,orientationkey(N1,T1,U1,V1,W1,A1))),
   fail ; true),
  (retract(gotdata(File,Model,Child,ChildRef,parent(NodeName/NodeRef))),
   asserta(gotdata(File,Model,Child,ChildRef,parent(NewName/NewRef))),
   fail ; true),
  tab(2), write('transferred child nodes of light '), write(NodeName), write(' to new dummy '), write(NewName), nl,
  increment_bug_count(File),
  fail.

check_for(lights_parented_to_tile,File,_) :-
  \+ check_failed(File),
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(light,NodeName)),true,NodeRef),
  atom_concat(Model,Suffix,NodeName),
  once((atom_concat('ml',_,Suffix); atom_concat('sl',_,Suffix))),
  \+gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  raise_to_tile(File,Model,NodeName,NodeRef),
  tab(2), write('light '), write(NodeName), write(' re-parented to tile dummy'), nl,
  increment_bug_count(File),
  fail.

check_for(aabb_has_child_nodes,File,_) :-
  \+ check_failed(File),
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  once(gotdata(File,Model,_,_,parent(NodeName/NodeRef))),
  once((between(1,inf,N), atom_concat('cm3_repair_',N,NewName), \+ gotdata(File,Model,node(_,NewName)))),
  asserta(gotdata(File,Model,node(dummy,NewName)),NewRef),
  gotdata(File,Model,NodeName,NodeRef,parent(P)),
  asserta(gotdata(File,Model,NewName,NewRef,parent(P))),
  gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z)),
  asserta(gotdata(File,Model,NewName,NewRef,position(X,Y,Z))),
  gotdata(File,Model,NodeName,NodeRef,orientation(U,V,W,A)),
  asserta(gotdata(File,Model,NewName,NewRef,orientation(U,V,W,A))),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,position(X1,Y1,Z1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,position(X1,Y1,Z1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,orientation(U1,V1,W1,A1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,orientation(U1,V1,W1,A1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(N1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,positionkey(N1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(N1,T1,X1,Y1,Z1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,positionkey(N1,T1,X1,Y1,Z1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,orientationkey(N1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N1,T1,U1,V1,W1,A1)),
   asserta(gotdata(File,Model,NewName,NewRef,AnimName,orientationkey(N1,T1,U1,V1,W1,A1))),
   fail ; true),
  (retract(gotdata(File,Model,Child,ChildRef,parent(NodeName/NodeRef))),
   asserta(gotdata(File,Model,Child,ChildRef,parent(NewName/NewRef))),
   fail ; true),
  tab(2), write('transferred child nodes of aabb '), write(NodeName), write(' to new dummy '), write(NewName), nl,
  increment_bug_count(File),
  fail.

check_for(aabb_parented_to_base,File,_) :-
  \+ check_failed(File),
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  raise_to_tile(File,Model,NodeName,NodeRef),
  tab(2), write('aabb node '), write(NodeName), write(' re-parented to base dummy'), nl,
  increment_bug_count(File),
  fail.

check_for(emitter_parent,File,_) :-
  \+ check_failed(File),
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  retract(gotdata(File,Model,NodeName,NodeRef,parent(Model/_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,parent(Adummy/ARef))),
  tab(2), write('emitter '), write(NodeName), write(' re-parented to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.
  
check_for(emitter_parent,File,_) :-
  \+ check_failed(File),
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  \+ superparent(File,Model,NodeName,NodeRef,Adummy,ARef),
  once((superparent(File,Model,NodeName,NodeRef,Super,SuperRef),
        gotdata(File,Model,Super,SuperRef,parent(Model/_)))),
  retract(gotdata(File,Model,Super,SuperRef,parent(Model/_))),
  asserta(gotdata(File,Model,Super,SuperRef,parent(Adummy/ARef))),
  tab(2), write('node '), write(Super), write(' re-parented to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.  

/* ===================================== */
/* Special checks on dwk and pwk nodes   */
/* ===================================== */

check_for(door_dummy_nodes,File,SmallLogStream) :-
  gotdata(File,Model,classification('DOOR')),
  member(Suffix,[grnd,head,hhit,impc]),
  atom_concat(Model,Suffix,NodeName),
  gotdata(File,Model,node(NodeType,NodeName)),
  NodeType\=='dummy',
  report_warning(['reserved door node',NodeName,'is not a dummy node'],SmallLogStream),
  fail.

check_for(door_dummy_nodes,File,SmallLogStream) :-
  gotdata(File,Model,classification('DOOR')),
  member(Suffix,[grnd,head,hhit,impc]),
  atom_concat(Model,Suffix,NodeName),
  \+ gotdata(File,Model,node(_,NodeName)),
  report_warning(['this door model has no',Suffix,'node'],SmallLogStream),
  fail.

check_for(door_dwk_dummy,File,_) :-
  gotdata(File,Model,classification('DOOR')),
  atom_concat(Model,'_dwk',NodeName),
  external_node(File,Model,NodeName),
  once(( external_node_parent(File,Model,NodeName,Parent), Parent\==Model )),
  retractall(external_node_parent(File,Model,NodeName,_)),
  asserta(external_node_parent(File,Model,NodeName,Model)),
  tab(2), write('dummy node '), write(NodeName), write(' re-parented to model in animations'), nl,
  increment_bug_count(File),
  fail.

check_for(door_dwk_dummy,File,_) :-
  gotdata(File,Model,classification('DOOR')),
  atom_concat(Model,'_dwk',NodeName),
  external_node(File,Model,NodeName),
  \+ external_node_parent(File,Model,NodeName,_),
  asserta(external_node_parent(File,Model,NodeName,Model)),
  tab(2), write('dummy node '), write(NodeName), write(' re-parented to model in animations'), nl,
  increment_bug_count(File),
  fail.

check_for(door_dwk_dummy,File,SmallLogStream) :-
  gotdata(File,Model,classification('DOOR')),
  atom_concat(Model,'_dwk',DwkName),
  external_node(File,Model,NodeName), NodeName\==DwkName,
  external_node_parent(File,Model,NodeName,Parent), Parent\==DwkName,
  report_warning(['dummy node',NodeName,'is not parented to',DwkName,'in animations'],SmallLogStream),
  increment_bug_count(File),
  fail.

check_for(door_dwk_dummy,File,_) :-
  gotdata(File,Model,classification('DOOR')),
  atom_concat(Model,'_dwk',DwkName),
  external_node(File,Model,NodeName), NodeName\==DwkName,
  \+ external_node_parent(File,Model,NodeName,_),
  asserta(external_node_parent(File,Model,NodeName,DwkName)),
  tab(2), write('dummy node '), write(NodeName), write(' parented to DWK in animations'), nl,
  increment_bug_count(File),
  fail.

check_for(door_dummy_names,File,SmallLogStream) :-
  gotdata(File,Model,classification('DOOR')),
  atom_concat(Model,'_dwk',DwkName),
  external_node(File,Model,NodeName), NodeName\==DwkName,
  \+ (( member(Suffix,['_wg_open1','_wg_open2','_wg_closed','_dp_open1_01','_dp_open1_02','_dp_open2_01','_dp_open2_02','dp_closed_01','dp_closed_02']), atom_concat(_,Suffix,NodeName) )),
  report_warning(['dummy node ',NodeName,'has unexpected suffix'],SmallLogStream),
  fail.

check_for(placeable_pwk_dummy,File,_) :-
  gotdata(File,Model,classification('CHARACTER')),
  atom_concat(Model,'_pwk',NodeName),
  external_node(File,Model,NodeName),
  once(( external_node_parent(File,Model,NodeName,Parent), Parent\==Model )),
  retractall(external_node_parent(File,Model,NodeName,_)),
  asserta(external_node_parent(File,Model,NodeName,Model)),
  tab(2), write('dummy node '), write(NodeName), write(' re-parented to model in animations'), nl,
  increment_bug_count(File),
  fail.

check_for(placeable_pwk_dummy,File,_) :-
  gotdata(File,Model,classification('CHARACTER')),
  atom_concat(Model,'_pwk',NodeName),
  external_node(File,Model,NodeName),
  \+ external_node_parent(File,Model,NodeName,_),
  asserta(external_node_parent(File,Model,NodeName,Model)),
  tab(2), write('dummy node '), write(NodeName), write(' re-parented to model in animations'), nl,
  increment_bug_count(File),
  fail.

/* ===================================== */
/* Checks for strange emitter parameters */
/* ===================================== */

check_for(strange_emitter_parameters,File,_) :-
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  retract(gotdata(File,Model,NodeName,NodeRef,p2p_type(P))), P=='bezier',
  \+ gotdata(File,Model,NodeName,NodeRef,p2p_sel(1)),
  retractall(gotdata(File,Model,NodeName,NodeRef,p2p_sel(_))),  
  asserta(gotdata(File,Model,NodeName,NodeRef,p2p_sel(1))),
  tab(2), write('in emitter '), write(NodeName), write(', "p2p_type bezier" interpreted as "p2p_sel 1"'), nl,
  increment_bug_count(File),
  fail.

check_for(strange_emitter_parameters,File,_) :-
  Checklist = ['normal','linked','billboard_to_local_z','billboard_to_world_z','aligned_to_world_z','aligned_to_particle_dir','motion_blur'],
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  retract(gotdata(File,Model,NodeName,NodeRef,render_sel(N))), nth1(N,Checklist,R1),
  \+ ((gotdata(File,Model,NodeName,NodeRef,render(R)), member(R,Checklist))),
  retractall(gotdata(File,Model,NodeName,NodeRef,render(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,render(R1))),
  tab(2), write('in emitter '), write(NodeName), write(', "render_sel '), write(N), write('" interpreted as "render '), write(R1), write('"'), nl,
  increment_bug_count(File),
  fail.

check_for(strange_emitter_parameters,File,_) :-
  Checklist = ['normal','punch_through','lighten'],
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  retract(gotdata(File,Model,NodeName,NodeRef,blend_sel(N))), nth1(N,Checklist,R1),
  \+ ((gotdata(File,Model,NodeName,NodeRef,blend(R)), member(R,Checklist))),
  retractall(gotdata(File,Model,NodeName,NodeRef,blend(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,blend(R1))),
  tab(2), write('in emitter '), write(NodeName), write(', "blend_sel '), write(N), write('" interpreted as "blend '), write(R1), write('"'), nl,  
  increment_bug_count(File),
  fail.

check_for(strange_emitter_parameters,File,_) :-
  Checklist = ['fountain','single','explosion','lightning'],
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  retract(gotdata(File,Model,NodeName,NodeRef,update_sel(N))), nth1(N,Checklist,R1),
  \+ ((gotdata(File,Model,NodeName,NodeRef,update(R)), member(R,Checklist))),
  retractall(gotdata(File,Model,NodeName,NodeRef,update(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,update(R1))),
  tab(2), write('in emitter '), write(NodeName), write(', "update_sel '), write(N), write('" interpreted as "update '), write(R1), write('"'), nl,  
  increment_bug_count(File),
  fail.

check_for(strange_emitter_parameters,File,_) :-
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  retract(gotdata(File,Model,NodeName,NodeRef,opacity(A))),
  \+ gotdata(File,Model,NodeName,NodeRef,alpha(_)),
  asserta(gotdata(File,Model,NodeName,NodeRef,alpha(A))),
  tab(2), write('in emitter '), write(NodeName), write(', "opacity" interpreted as "alpha"'), nl,  
  increment_bug_count(File),
  fail.

check_for(strange_emitter_parameters,File,_) :-
  retractall(gotdata(File,_,_,_,spawntype_sel(_))),
  retractall(gotdata(File,_,_,_,lockaxes(_))),
  retractall(gotdata(File,_,_,_,chunky(_))),
  fail.

/* ==================== */
/* Miscellaneous checks */
/* ==================== */

check_for(null_zero_orientations1,File,_) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  clause(gotdata(File,Model,NodeName,NodeRef,orientation(X,Y,Z,A)),true,Cref),
  A=:=0, \+ (X==0, Y==0, Z==0, A==0),
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,orientation(0,0,0,0))),
  increment_bug_count(File),
  fail.

check_for(null_zero_orientations1,File,_) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  clause(gotdata(File,Model,NodeName,NodeRef,orientation(X,Y,Z,A)),true,Cref),
  X=:=0, Y=:=0, Z=:=0, A=\=0,
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,orientation(0,0,0,0))),
  increment_bug_count(File),
  fail.

check_for(null_zero_orientations1,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,orientation(X,Y,Z,A)),true,Cref),
  A=:=0, \+ (X==0, Y==0, Z==0, A==0),
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,orientation(0,0,0,0))),
  fail.

check_for(null_zero_orientations1,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,orientation(X,Y,Z,A)),true,Cref),
  X=:=0, Y=:=0, Z=:=0, A=\=0,
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,orientation(0,0,0,0))),
  increment_bug_count(File),
  fail.

check_for(null_zero_orientations1,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N,T,X,Y,Z,A)),true,Cref),
  A=:=0, \+ (X==0, Y==0, Z==0, A==0),
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N,T,0,0,0,0))),
  fail.

check_for(null_zero_orientations1,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N,T,X,Y,Z,A)),true,Cref),
  X=:=0, Y=:=0, Z=:=0, A=\=0,
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N,T,0,0,0,0))),
  increment_bug_count(File),
  fail.

check_for(faceless_trimesh,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  \+ ((gotdata(File,Model,NodeName,NodeRef,faces(N)), N>0)),
  \+ gotdata(File,Model,_,_,parent(NodeName/NodeRef)),
  retract(gotdata(File,Model,node(trimesh,NodeName))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_)),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,_)),
  (cm3_verbose -> tab(2), write('deleted faceless trimesh node '), write(NodeName), nl ; true),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  increment_bug_count(File),
  fail.

check_for(faceless_trimesh,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('deleted '), write(N), write(' faceless trimesh nodes'), nl,
  fail.

check_for(faceless_trimesh,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('deleted faceless trimesh node(s) '), write(List), nl,
  fail.

check_for(faceless_trimesh,File,_) :-
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  \+ ((gotdata(File,Model,NodeName,NodeRef,faces(N)), N>0)),
  once(gotdata(File,Model,_,_,parent(NodeName/NodeRef))),
  gotdata(File,Model,NodeName,NodeRef,parent(P)),
  gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z)),
  gotdata(File,Model,NodeName,NodeRef,orientation(U,V,W,A)),
  once((gotdata(File,Model,NodeName,NodeRef,scale(S));S=[])),
  once((gotdata(File,Model,NodeName,NodeRef,inheritcolor(I));I=[])),
  retract(gotdata(File,Model,node(trimesh,NodeName))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_)),
  asserta(gotdata(File,Model,node(dummy,NodeName)),NewRef),
  asserta(gotdata(File,Model,NodeName,NewRef,parent(P))),
  asserta(gotdata(File,Model,NodeName,NewRef,position(X,Y,Z))),
  asserta(gotdata(File,Model,NodeName,NewRef,orientation(U,V,W,A))),
  (S\=[] -> asserta(gotdata(File,Model,NodeName,NewRef,scale(S))) ; true),
  (I\=[] -> asserta(gotdata(File,Model,NodeName,NewRef,inheritcolor(I))) ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,position(X1,Y1,Z1)),
   asserta(gotdata(File,Model,NodeName,NewRef,AnimName,position(X1,Y1,Z1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,orientation(U1,V1,W1,A1)),
   asserta(gotdata(File,Model,NodeName,NewRef,AnimName,orientation(U1,V1,W1,A1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,scale(S1)),
   asserta(gotdata(File,Model,NodeName,NewRef,AnimName,scale(S1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(N1)),
   asserta(gotdata(File,Model,NodeName,NewRef,AnimName,positionkey(N1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(N1,T1,X1,Y1,Z1)),
   asserta(gotdata(File,Model,NodeName,NewRef,AnimName,positionkey(N1,T1,X1,Y1,Z1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N1)),
   asserta(gotdata(File,Model,NodeName,NewRef,AnimName,orientationkey(N1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N1,T1,U1,V1,W1,A1)),
   asserta(gotdata(File,Model,NodeName,NewRef,AnimName,orientationkey(N1,T1,U1,V1,W1,A1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,scalekey(N1)),
   asserta(gotdata(File,Model,NodeName,NewRef,AnimName,scalekey(N1))),
   fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,scalekey(N1,T1,S1)),
   asserta(gotdata(File,Model,NodeName,NewRef,AnimName,scalekey(N1,T1,S1))),
   fail ; true),
  (retract(gotdata(File,Model,Child,ChildRef,parent(NodeName/NodeRef))),
   asserta(gotdata(File,Model,Child,ChildRef,parent(NodeName/NewRef))),
   fail ; true),
   (retract(gotdata(File,Model,ChildName,ChildRef,parent(NodeName/NodeRef))),
   asserta(gotdata(File,Model,ChildName,ChildRef,parent(NodeName/NewRef))),
   fail ; true),
  tab(2), write('converted faceless trimesh node '), write(NodeName), write(' to dummy'), nl,
  increment_bug_count(File),
  fail.

/* Fixed in CM3.5 from trapping "d"=100 to "D"=68 because that's how door dummies are held */
check_for(misnamed_door_dummies,File,SmallLogStream) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,node(dummy,DoorNode)),
  name(Model,ModelName), name(DoorNode,DoorName), append(ModelName,Suffix,DoorName),
  (Suffix=[95,68,N] ; Suffix=[95,68,48,N] ; Suffix=[68,N] ; Suffix=[68,48,N]), (N<49 ; N>52),
  report_warning([DoorNode,'may be a mis-numbered door node'],SmallLogStream),
  fail.

check_for(number_of_danglymesh_constraints,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(danglymesh,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,verts(N)),
  \+gotdata(File,Model,NodeName,NodeRef,constraints(N)),
  report_error(['danglymesh node',NodeName,'has the wrong number of constraints'],SmallLogStream),
  bad_error(File),
  fail.

check_for(lens_flare_counts,File,SmallLogStream) :-
  gotdata(File,Model,NodeName,NodeRef,texturenames(N)),
  gotdata(File,Model,NodeName,NodeRef,lensflares(N0)), N0\=N,
  report_warning(['in light',NodeName,'the number of texturenames (',N,') does not match the lensflares (',N0,')'],SmallLogStream),
  fail.

check_for(lens_flare_counts,File,SmallLogStream) :-
  gotdata(File,Model,NodeName,NodeRef,flaresizes(N)),
  gotdata(File,Model,NodeName,NodeRef,lensflares(N0)), N0\=N,
  report_warning(['in light',NodeName,'the number of flaresizes (',N,') does not match the lensflares (',N0,')'],SmallLogStream),
  fail.

check_for(lens_flare_counts,File,SmallLogStream) :-
  gotdata(File,Model,NodeName,NodeRef,flarepositions(N)),
  gotdata(File,Model,NodeName,NodeRef,lensflares(N0)), N0\=N,
  report_warning(['in light',NodeName,'the number of flarepositions (',N,') does not match the lensflares (',N0,')'],SmallLogStream),
  fail.

check_for(lens_flare_counts,File,SmallLogStream) :-
  gotdata(File,Model,NodeName,NodeRef,flarecolorshifts(N)),
  gotdata(File,Model,NodeName,NodeRef,lensflares(N0)), N0\=N,
  report_warning(['in light',NodeName,'the number of flarecolorshifts (',N,') does not match the lensflares (',N0,')'],SmallLogStream),
  fail.

check_for(skin_weight_number,File,SmallLogStream) :-
  gotdata(File,Model,NodeName,NodeRef,weights(N)),
  \+ gotdata(File,Model,NodeName,NodeRef,verts(N)),
  report_error([NodeName,'has the wrong number of weights'],SmallLogStream),
  bad_error(File),
  fail.

check_for(unknown_bones,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef),
  bagof(W,N^gotdata(File,Model,NodeName,NodeRef,weights(N,W)),Bag), flatten(Bag,FlatBag),
  setof(Bone,(member(Bone,FlatBag), \+number(Bone), \+ gotdata(File,Model,node(_,Bone))),BadBones),
  report_error(['undefined bone(s)',BadBones,'in skin',NodeName],SmallLogStream),
  bad_error(File),
  fail.

check_for(too_many_bones,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef),
  bagof(W,N^gotdata(File,Model,NodeName,NodeRef,weights(N,W)),Bag), flatten(Bag,FlatBag),
  setof(Bone,(member(Bone,FlatBag), \+number(Bone)),AllBones), length(AllBones,NBones),
  (NBones>64 ->
     report_error(['more than 64 bones in',NodeName],SmallLogStream),
     bad_error(File)
     ;
   true),
  fail.

check_for(four_bones_per_vertex,File,_) :-
  clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef),
  once((gotdata(File,Model,NodeName,NodeRef,weights(_,WL0)), length(WL0,N0), N0>8)),
  (clause(gotdata(File,Model,NodeName,NodeRef,weights(V,WL)),true,Eref),
   length(WL,N), N>8,
   enforce4bpv(WL,WL4),
   erase(Eref),
   asserta(gotdata(File,Model,NodeName,NodeRef,weights(V,WL4))),
   fail ; true),
  tab(2), write('skinmesh '), write(NodeName), write(' had verts with too many bones - weakest dropped'), nl,
  increment_bug_count(File),
  fail.
  
check_for(unnormalised_weights,File,_) :-
  clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef),
  once((
    gotdata(File,Model,NodeName,NodeRef,weights(_,W)),
    bagof(N,(member(N,W),number(N)),B), my_sumlist(B,S),
    abs(S-1)>0.00001
      )),
  (clause(gotdata(File,Model,NodeName,NodeRef,weights(V,WL)),true,Eref),
   enforce4bpv(WL,WL4),
   erase(Eref),
   asserta(gotdata(File,Model,NodeName,NodeRef,weights(V,WL4))),
   fail ; true),
  tab(2), write('skinmesh '), write(NodeName), write(' weights normalised'), nl,
  fail.

check_for(ndynamictype_synonyms,File,_) :-
  clause(gotdata(File,Model,node(light,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,ndynamictype(_)),
  clause(gotdata(File,Model,NodeName,NodeRef,isdynamic(_)),true,ERef),
  erase(ERef),
  increment_bug_count(File),
  fail.

check_for(ndynamictype_synonyms,File,_) :-
  clause(gotdata(File,Model,node(light,NodeName)),true,NodeRef),
  clause(gotdata(File,Model,NodeName,NodeRef,isdynamic(N)),true,ERef),
  erase(ERef),
  asserta(gotdata(File,Model,NodeName,NodeRef,ndynamictype(N))),
  increment_bug_count(File),
  fail.

/* ========================================= */
/* Checks on vertex colors - added in CM344k */
/* Mofified in CM344m to hold colors as 255  */
/* ========================================= */

check_for(unwanted_vertex_colors,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,NodeName,NodeRef,colors(_)),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  NodeType\==trimesh, NodeType\==skin,
  retractall(gotdata(File,Model,NodeName,NodeRef,colors(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,colors(_,_,_,_))),
  (cm3_verbose -> tab(2), write('deleted colors from '), write(NodeType), write(' node '), write(NodeName), nl ; true),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  increment_bug_count(File),
  fail.

check_for(unwanted_vertex_colors,File,_) :-
  gotdata(File,Model,NodeName,NodeRef,colors(_)),
  \+((gotdata(File,Model,NodeName,NodeRef,colors(_,R,_,_)), R=\=255)),
  \+((gotdata(File,Model,NodeName,NodeRef,colors(_,_,G,_)), G=\=255)),
  \+((gotdata(File,Model,NodeName,NodeRef,colors(_,_,_,B)), B=\=255)),
  retractall(gotdata(File,Model,NodeName,NodeRef,colors(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,colors(_,_,_,_))),
  (cm3_verbose -> tab(2), write('deleted blank colors from '), write(NodeName), nl ; true),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  increment_bug_count(File),
  fail.

check_for(unwanted_vertex_colors,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('deleted unwanted vertex colors from '), write(N), write(' nodes'), nl,
  fail.

check_for(unwanted_vertex_colors,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('deleted unwanted vertex colors from node(s) '), write(List), nl,
  fail.

check_for(wrong_number_of_vertex_colors,File,SmallLogStream) :-
  gotdata(File,Model,NodeName,NodeRef,colors(C)),
  gotdata(File,Model,NodeName,NodeRef,verts(V)),
  V\=C,
  report_error(['number of colors (',C,') does not match number of verts (',V,') in node',NodeName],SmallLogStream),
  bad_error(File),
  fail.

/* ========== */
/* Wavy water */
/* ========== */

check_for(wavy_water,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater==no,
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  is_watery(File,Model,NodeName,NodeRef),
  reclassify_node(File,Model,NodeName,NodeRef,trimesh,_),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,_)),
  tab(2), write('animmesh '), write(NodeName), write(' changed to trimesh'), nl,
  increment_bug_count(File),
  fail.

check_for(wavy_water,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater==wavy,
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  is_watery(File,Model,NodeName,NodeRef),
  reclassify_node(File,Model,NodeName,NodeRef,animmesh,_),
  tab(2), write('trimesh '), write(NodeName), write(' changed to animmesh'), nl,
  increment_bug_count(File),
  fail.

check_for(wavy_water,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater==wavy,
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  is_watery(File,Model,NodeName,NodeRef),
  raise_to_tile(File,Model,NodeName,NodeRef),
  set_zero_orientation(File,Model,NodeName,NodeRef),
  set_zero_position(File,Model,NodeName,NodeRef),
  tab(2), write('animmesh '), write(NodeName), write(' forced to origin'), nl,
  increment_bug_count(File),
  fail.

check_for(wavy_water,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater==wavy,
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  is_watery(File,Model,NodeName,NodeRef),
  (once(g_user_option(snap,Snap)), Snap\=none -> apply_snap(File,Model,NodeName,NodeRef) ; true),
  gotdata(File,Model,NodeName,NodeRef,verts(NVerts)),
  hash_verts(File,Model,NodeName,NodeRef),
  (has_unwelded_verts(File,Model,NodeName,NodeRef) -> weld_vertices(File,Model,NodeName,NodeRef,NVerts,0,0) ; true),
  tesselate_mesh(File,Model,NodeName,NodeRef,2.0),
  tab(2), write('animmesh '), write(NodeName), write(' tesselated to '), write(2.0), write('m. pitch'), nl,
  increment_bug_count(File),
  fail.

check_for(wavy_water,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater==wavy,
  once((
    clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
    is_watery(File,Model,NodeName,NodeRef)
      )),
  AnimName='default',
  \+gotdata(File,Model,newanim(AnimName,Model)),
  asserta(gotdata(File,Model,newanim(AnimName,Model))),
  asserta(gotdata(File,Model,anim(AnimName),length(10.0))),
  asserta(gotdata(File,Model,anim(AnimName),transtime(0.25))),
  asserta(gotdata(File,Model,anim(AnimName),animroot(Model))),
  tab(2), write(AnimName), write(' animation created'), nl,
  increment_bug_count(File),
  fail.

check_for(wavy_water,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater==wavy,
  AnimName='default',
  gotdata(File,Model,newanim(AnimName,Model)),
  (gotdata(File,Model,anim(AnimName),length(L0)), L0 > 0 -> Length = L0 ;
   retractall(gotdata(File,Model,anim(AnimName),length(_))),
   Length = 10.0, asserta(gotdata(File,Model,anim(AnimName),length(Length)))),
  /*Randomisers moved up to here in CM345a */
  once(g_user_option(wave_height,WaveHeight)),
  R0 is (3+random(6)+random(6)+random(6))*WaveHeight/105.0,
  R1 is (3+random(6)+random(6)+random(6))/21.0,
  R2 is (3+random(6)+random(6)+random(6))/21.0,
  R3 is (3+random(6)+random(6)+random(6))/21.0,
  R4 is (3+random(6)+random(6)+random(6))/21.0,
  (cm3_verbose -> tab(2), write('wave randomisers '), write([R0,R1,R2,R3,R4]), nl ; true),
  /* End of moved part CM345a */
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  is_watery(File,Model,NodeName,NodeRef),
  retractall(gotdata(File,Model,NodeName,_,sampleperiod(_))),
  retractall(gotdata(File,Model,NodeName,_,_,sampleperiod(_))),
  S is Length/5.0, asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,sampleperiod(S))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,clipu(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,clipu(0))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,clipv(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,clipv(0))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,clipw(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,clipw(1))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,cliph(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,cliph(1))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,animverts(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,animverts(_,_,_,_))),
  gotdata(File,Model,NodeName,NodeRef,verts(NV)), AV is 6*NV,
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(AV))),
  /* Randomisers moved up from here in CM345a */
  (gotdata(File,Model,NodeName,NodeRef,verts(V0,X,Y,Z)),
   V1 is V0+NV, V2 is V1+NV, V3 is V2+NV, V4 is V3+NV, V5 is V4+NV,
   perturb(R0,R1,R2,R3,R4,X,Y,Z,Z1,Z2,Z3,Z4),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(V0,X,Y,Z))),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(V1,X,Y,Z1))),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(V2,X,Y,Z2))),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(V3,X,Y,Z3))),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(V4,X,Y,Z4))),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(V5,X,Y,Z))),
   fail ; true),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,animtverts(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,animtverts(_,_,_,_))),
  gotdata(File,Model,NodeName,NodeRef,tverts(NTV)), ATV is 6*NTV,
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animtverts(ATV))),
  (gotdata(File,Model,NodeName,NodeRef,tverts(T0,Tu,Tv,Tw)),
   T1 is T0+NTV, T2 is T1+NTV, T3 is T2+NTV, T4 is T3+NTV, T5 is T4+NTV,
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animtverts(T0,Tu,Tv,Tw))),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animtverts(T1,Tu,Tv,Tw))),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animtverts(T2,Tu,Tv,Tw))),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animtverts(T3,Tu,Tv,Tw))),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animtverts(T4,Tu,Tv,Tw))),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animtverts(T5,Tu,Tv,Tw))),
   fail ; true),
  tab(2), write('animmesh '), write(NodeName), write(' animated'), nl,
  increment_bug_count(File),
  fail.

/* ==================== */
/* Checks on animations */
/* ==================== */

check_for(aabb_has_keys,File,_) :-
  gotdata(File,Model,node(aabb,NodeName)),
  once(gotdata(File,Model,NodeName,_,_,_)),
  retractall(gotdata(File,Model,NodeName,_,_,_)),
  tab(2), write('animation data deleted from aabb node '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(light_has_keys,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,node(light,NodeName)),
  atom_concat(Model,Suffix,NodeName),
  (atom_concat('ml',_,Suffix) ; atom_concat('sl',_,Suffix)),
  once(gotdata(File,Model,NodeName,_,_,_)),
  retractall(gotdata(File,Model,NodeName,_,_,_)),
  tab(2), write('deleted all animation keys for '), write(NodeName), nl,
  increment_bug_count(File),
  fail.  

check_for(animation_length,File,_) :-
  gotdata(File,Model,classification('TILE')),
  (AnimName='day2night' ; AnimName='night2day'),
  gotdata(File,Model,newanim(AnimName,Model)),
  \+ (gotdata(File,Model,anim(AnimName),length(L)), L>=0.03333333),
  retractall(gotdata(File,Model,anim(AnimName),length(_))),
  asserta(gotdata(File,Model,anim(AnimName),length(0.03333333))),
  tab(2), write('length for animation '), write(AnimName), write( ' set to single frame minimum'), nl,
  increment_bug_count(File),
  fail.

check_for(animation_length,File,_) :-
  gotdata(File,Model,newanim(AnimName,Model)),
  \+ gotdata(File,Model,anim(AnimName),length(_)),
  asserta(gotdata(File,Model,anim(AnimName),length(0))),
  tab(2), write('missing length for animation '), write(AnimName), write( ' set to 0'), nl,
  increment_bug_count(File),
  fail.

check_for(animation_transtime,File,_) :-
  gotdata(File,Model,newanim(AnimName,Model)),
  \+ gotdata(File,Model,anim(AnimName),transtime(_)),
  asserta(gotdata(File,Model,anim(AnimName),transtime(0.25))),
  tab(2), write('missing transtime for animation '), write(AnimName), write( ' set to 0.25'), nl,
  increment_bug_count(File),
  fail.

check_for(parameters_clash_with_keys,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,Q),true,Cref),
  functor(Q,P,A), \+ atom_concat(_,'key',P),
  atom_concat(P,'key',K), A2 is A+2, functor(Q2,K,A2),
  once(gotdata(File,Model,NodeName,NodeRef,AnimName,Q2)),
  erase(Cref),
  tab(2), write(NodeName), write(' has '), write(K), write('s in animation '), write(AnimName), write(' - '), write(P), write(' deleted'), nl,
  increment_bug_count(File),
  fail. 

check_for(parameters_back_to_keys,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,Q),true,CRef),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef), NodeType\='animmesh',
  Q=..[P|Q1],
  \+atom_concat(_,'key',P),
  atom_concat(P,'key',K),
  NK=..[K,1],
  DK=..[K,0,0|Q1],
  erase(CRef),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,NK)),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,DK)),
  (cm3_verbose -> tab(2), write(P), write(' changed to '), write(K), write(' for node '), write(NodeName), write(' in animation '), write(AnimName), nl ; true),
  increment_bug_count(File),
  fail.

check_for(non_varying_keys,_,_) :-
  retractall(fixlist(_)),
  fail.

check_for(non_varying_keys,File,_) :-
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  \+ ((gotdata(File,Model,classification('CHARACTER')), gotdata(File,Model,newanim(_,Model)))), 
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef), NodeType\='animmesh',
  /* if there are keys in animations but no corresponding static parameter in the geometry */
  /* and the keys are all the same in all the animations                                   */
  /* make the value static in the geometry and delete all the keys                         */
  gotdata(File,Model,NodeName,NodeRef,_,Q), Q=..[Key,_,_|R], atom_concat(P,'key',Key),
  \+ ((gotdata(File,Model,NodeName,NodeRef,Q3), Q3=..[P|_])),
  \+ (( gotdata(File,Model,NodeName,NodeRef,_,Q2), Q2=..[Key,_,_|R1], R1\=R)),
  Q1=..[P|R], asserta(gotdata(File,Model,NodeName,NodeRef,Q1)),
  Qx1=..[Key,_], retractall(gotdata(File,Model,NodeName,NodeRef,_,Qx1)),
  functor(Q,Key,A), functor(Qx2,Key,A),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,Qx2)),
  (\+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true),
  (cm3_verbose -> tab(2), write('unchanging '), write(Key), write(' values for node '), write(NodeName), write(' moved to geometry'), nl ; true),
  increment_bug_count(File),
  fail.

check_for(non_varying_keys,File,_) :-
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  \+ ((gotdata(File,Model,classification('CHARACTER')), gotdata(File,Model,newanim(_,Model)))), 
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef), NodeType\='animmesh',
  gotdata(File,Model,NodeName,NodeRef,Q), Q=..[Q0|R], atom_concat(Q0,'key',Key),
  \+((gotdata(File,Model,NodeName,NodeRef,_,Q1), Q1=..[Key,_,_|R1], R1\=R)),
  Qx=..[Key,_], once(gotdata(File,Model,NodeName,NodeRef,_,Qx)),
  Qx1=..[Key,_], retractall(gotdata(File,Model,NodeName,NodeRef,_,Qx1)),
  functor(Q,Q0,A), A2 is A+2, functor(Qx2,Key,A2),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,Qx2)),
  (\+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true),
  (cm3_verbose -> tab(2), write('unchanging '), write(Q0), write(' values for node '), write(NodeName), write(' deleted from animations'), nl ; true),
  increment_bug_count(File),
  fail.

check_for(non_varying_keys,File,_) :-
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  \+ ((gotdata(File,Model,classification('CHARACTER')), gotdata(File,Model,newanim(_,Model)))), 
  once(g_user_option(snap,Snap)), Snap\=none,
  gotdata(File,Model,NodeName,NodeRef,position(X0,Y0,Z0)),
  once(gotdata(File,Model,NodeName,NodeRef,_,positionkey(_,_,_,_,_))),
  \+ (gotdata(File,Model,NodeName,NodeRef,_,positionkey(_,_,X2,Y2,Z2)),
      ( \+snap_equal(X2,X0) ; \+snap_equal(Y2,Y0) ; \+snap_equal(Z2,Z0) )), 
  retractall(gotdata(File,Model,NodeName,NodeRef,_,positionkey(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_,positionkey(_,_,_,_,_))),
  (\+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true),
  (cm3_verbose -> tab(2), write('unsnapped positionkeys for '), write(NodeName), write(' deleted from animations'), nl ; true),
  increment_bug_count(File),
  fail.

check_for(non_varying_keys,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('superfluous animation keys deleted from '), write(N), write(' objects'), nl,
  fail.

check_for(non_varying_keys,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('superfluous animation keys deleted from '), write(List), nl,
  fail.

check_for(verts_in_animmesh_animations,File,_) :-
  retract(gotdata(File,Model,NodeName,NodeRef,AnimName,verts(_))),
  tab(2), write('ignoring redefined verts for '), write(NodeName), write(' in animation '), write(AnimName), nl,
  retractall(gotdata(File,Model,NodeName,NodeRef,AnimName,verts(_,_,_,_))),
  fail.

check_for(tverts_in_animmesh_animations,File,_) :-
  retract(gotdata(File,Model,NodeName,NodeRef,AnimName,tverts(_))),
  tab(2), write('ignoring redefined tverts for '), write(NodeName), write(' in animation '), write(AnimName), nl,
  retractall(gotdata(File,Model,NodeName,NodeRef,AnimName,tverts(_,_,_,_))),
  fail.

check_for(faces_in_animmesh_animations,File,_) :-
  retract(gotdata(File,Model,NodeName,NodeRef,AnimName,faces(_))),
  tab(2), write('ignoring redefined faces for '), write(NodeName), write(' in animation '), write(AnimName), nl,
  retractall(gotdata(File,Model,NodeName,NodeRef,AnimName,faces(_,_,_,_,_,_,_,_,_))),
  fail.

check_for(animated_node_parent,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  gotdata(File,Model,NodeName,NodeRef,_,_),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  retract(gotdata(File,Model,NodeName,NodeRef,parent(Model/_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,parent(Adummy/ARef))),
  tab(2), write('animated node '), write(NodeName), write(' re-parented to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.
  
check_for(animated_node_parent,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  once(gotdata(File,Model,NodeName,NodeRef,_,_)),
  \+ superparent(File,Model,NodeName,NodeRef,Adummy,ARef),
  once((superparent(File,Model,NodeName,NodeRef,Super,SuperRef),
        gotdata(File,Model,Super,SuperRef,parent(Model/_)))),
  retract(gotdata(File,Model,Super,SuperRef,parent(Model/_))),
  asserta(gotdata(File,Model,Super,SuperRef,parent(Adummy/ARef))),
  tab(2), write('node '), write(Super), write(' re-parented to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.  

check_for(water_parented_to_a_node,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater\=no,
  gotdata(File,Model,beginmodelgeom(Model)),
  \+ edgetile(Model),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  (NodeType==trimesh ; NodeType=animmesh),
  is_watery(File,Model,NodeName,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  retract(gotdata(File,Model,NodeName,NodeRef,parent(Model/_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,parent(Adummy/ARef))),
  tab(2), write('changed parent of '), write(NodeName), write(' to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.

check_for(water_parented_to_a_node,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater==yes,
  gotdata(File,Model,beginmodelgeom(Model)),
  \+ edgetile(Model),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  (NodeType==trimesh ; NodeType=animmesh),
  is_watery(File,Model,NodeName,NodeRef),
  \+ superparent(File,Model,NodeName,NodeRef,Adummy,ARef),
  once((superparent(File,Model,NodeName,NodeRef,Super,SuperRef),
        gotdata(File,Model,Super,SuperRef,parent(Model/_)))),
  retract(gotdata(File,Model,Super,SuperRef,parent(Model/_))),
  asserta(gotdata(File,Model,Super,SuperRef,parent(Adummy/ARef))),
  tab(2), write('changed parent of '), write(Super), write(' to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.

check_for(foliage_parented_to_a_node,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  once(g_user_option(foliage,Foliage)), Foliage==animate,
  gotdata(File,Model,beginmodelgeom(Model)),
  \+ edgetile(Model),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  is_foliage(File,Model,NodeName,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  retract(gotdata(File,Model,NodeName,NodeRef,parent(Model/_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,parent(Adummy/ARef))),
  tab(2), write('changed parent of '), write(NodeName), write(' to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.

check_for(foliage_parented_to_a_node,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  once(g_user_option(foliage,Foliage)), Foliage==animate,
  gotdata(File,Model,beginmodelgeom(Model)),
  \+ edgetile(Model),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  is_foliage(File,Model,NodeName,NodeRef),
  \+ superparent(File,Model,NodeName,NodeRef,Adummy,ARef),
  once((superparent(File,Model,NodeName,NodeRef,Super,SuperRef),
        gotdata(File,Model,Super,SuperRef,parent(Model/_)))),
  retract(gotdata(File,Model,Super,SuperRef,parent(Model/_))),
  asserta(gotdata(File,Model,Super,SuperRef,parent(Adummy/ARef))),
  tab(2), write('changed parent of '), write(Super), write(' to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.

check_for(splotch_parented_to_a_node,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  once(g_user_option(splotch,Splotch)), Splotch==animate,
  gotdata(File,Model,beginmodelgeom(Model)),
  \+ edgetile(Model),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef), member(NodeType,[trimesh,danglymesh]),
  is_splotch(File,Model,NodeName,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  retract(gotdata(File,Model,NodeName,NodeRef,parent(Model/_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,parent(Adummy/ARef))),
  tab(2), write('changed parent of '), write(NodeName), write(' to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.

check_for(splotch_parented_to_a_node,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  once(g_user_option(splotch,Splotch)), Splotch==animate,
  gotdata(File,Model,beginmodelgeom(Model)),
  \+ edgetile(Model),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef), member(NodeType,[trimesh,danglymesh]),
  is_splotch(File,Model,NodeName,NodeRef),
  \+ superparent(File,Model,NodeName,NodeRef,Adummy,ARef),
  once((superparent(File,Model,NodeName,NodeRef,Super,SuperRef),
        gotdata(File,Model,Super,SuperRef,parent(Model/_)))),
  retract(gotdata(File,Model,Super,SuperRef,parent(Model/_))),
  asserta(gotdata(File,Model,Super,SuperRef,parent(Adummy/ARef))),
  tab(2), write('changed parent of '), write(Super), write(' to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.

check_for(transparency_parented_to_a_node,File,_) :-
  gotdata(File,Model,newmodel(Model)),
  gotdata(File,Model,classification('CHARACTER')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  once(g_user_option(placeable_with_transparency,PlaceableWithTransparency)),
  PlaceableWithTransparency==yes,
  once(is_transparency(File,Model,_,_)),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  gotdata(File,Model,NodeName,NodeRef,faces(_)),
  is_transparency(File,Model,NodeName,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  retract(gotdata(File,Model,NodeName,NodeRef,parent(Model/_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,parent(Adummy/ARef))),
  tab(2), write('changed parent of '), write(NodeName), write(' to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.

check_for(transparency_parented_to_a_node,File,_) :-
  gotdata(File,Model,newmodel(Model)),
  gotdata(File,Model,classification('CHARACTER')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  once(g_user_option(placeable_with_transparency,PlaceableWithTransparency)),
  PlaceableWithTransparency==yes,
  once(is_transparency(File,Model,_,_)),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  gotdata(File,Model,NodeName,NodeRef,faces(_)),
  is_transparency(File,Model,NodeName,NodeRef),
  \+ superparent(File,Model,NodeName,NodeRef,Adummy,ARef),
  once((superparent(File,Model,NodeName,NodeRef,Super,SuperRef),
        gotdata(File,Model,Super,SuperRef,parent(Model/_)))),
  retract(gotdata(File,Model,Super,SuperRef,parent(Model/_))),
  asserta(gotdata(File,Model,Super,SuperRef,parent(Adummy/ARef))),
  tab(2), write('changed parent of '), write(Super), write(' to '), write(Adummy), nl,
  increment_bug_count(File),
  fail.

/** Make this an option **/
/**
check_for(clutter_on_a_node,File,_) :-
  gotdata(File,Model,classification('TILE')),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  once((gotdata(File,Model,_,_,parent(Adummy/ARef)))),
  t_cleanse_a_node(File,Model,Adummy),
  fail.
**/

check_for(animation_root,File,_) :-
  gotdata(File,Model,newanim(AnimName,Model)),
  clause(gotdata(File,Model,anim(AnimName),animroot(R)),true,Eref),
  \+ gotdata(File,Model,node(_,R)),
  erase(Eref),
  tab(2), write('animroot '), write(R), write(' of '), write(AnimName), write(' does not exist'), nl,
  increment_bug_count(File),
  fail.
  
check_for(animation_root,File,_) :-
  gotdata(File,Model,newanim(AnimName,Model)),
  \+ gotdata(File,Model,classification('TILE')),
  \+ gotdata(File,Model,anim(AnimName),animroot(_)),
  gotdata(File,Model,node(_,'rootdummy')),
  asserta(gotdata(File,Model,anim(AnimName),animroot('rootdummy'))),
  tab(2), write('animroot of '), write(AnimName), write(' set to rootdummy'), nl,
  increment_bug_count(File),
  fail.

check_for(animation_root,File,_) :-
  gotdata(File,Model,newanim(AnimName,Model)),
  \+ gotdata(File,Model,anim(AnimName),animroot(_)),
  asserta(gotdata(File,Model,anim(AnimName),animroot(Model))),
  tab(2), write('animroot of '), write(AnimName), write(' set to '), write(Model), nl,
  increment_bug_count(File),
  fail.

check_for(animation_root,File,_) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,anim(AnimName),animroot(R)),true,Cref),
  R\==Model,
  erase(Cref),
  asserta(gotdata(File,Model,anim(AnimName),animroot(Model))),
  tab(2), write('animroot of '), write(AnimName), write(' changed to '), write(Model), nl,
  increment_bug_count(File),
  fail.

/* Added in CM343c: delete keys from nodes that are not, and do not descend from, the animroot */
/* Suppressed in CM344e - some problem with joint dummies */
/*
check_for(animation_root,File,_) :-
  gotdata(File,Model,anim(AnimName),animroot(AnimRoot)),
  AnimRoot\=Model,
  clause(gotdata(File,Model,node(_,AnimRoot)),true,RootRef),
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  NodeName\==AnimRoot, NodeRef\==RootRef,
  \+superparent(File,Model,NodeName,NodeRef,AnimRoot,RootRef),
  once(gotdata(File,Model,NodeName,NodeRef,AnimName,_)),
  retractall(gotdata(File,Model,NodeName,NodeRef,AnimName,_)),
  tab(2), write('deleted keys on node '), write(NodeName), write(' in animation '), write(AnimName), write(' - outside animroot hierarchy'), nl,
  fail.
*/  

/* CM345a: do not rename if there are animmesh nodes (possibly wavywater) */
check_for(inappropriate_default_animation,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,newanim('default',Model)),
  \+ gotdata(File,Model,node(animmesh,_)),
  \+ gotdata(File,Model,newanim('tiledefault',Model)),
  rename_animation(File,Model,'default','tiledefault'),
  tab(2), write('default animation renamed to tiledefault in '), write(Model), nl,
  increment_bug_count(File),
  fail.

check_for(animated_emitters,File,_) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  \+(gotdata(File,Model,NodeName,NodeRef,update(lightning))),
  \+((member(A,[default,day,night]), gotdata(File,Model,newanim(A,Model)))),
  \+((gotdata(File,Model,NodeName,NodeRef,'tiledefault',birthratekey(_,_,B1)), B1=:=0)),
  \+((gotdata(File,Model,NodeName,NodeRef,'tiledefault',birthrate(B2)), B2=:=0)),
  (gotdata(File,Model,newanim('tiledefault',Model)) -> true;
   asserta(gotdata(File,Model,newanim('tiledefault',Model))),
   asserta(gotdata(File,Model,anim('tiledefault'),animroot(Model))),
   asserta(gotdata(File,Model,anim('tiledefault'),length(0))),
   asserta(gotdata(File,Model,anim('tiledefault'),transtime(0.25))),
   increment_bug_count(File)),
  retractall(gotdata(File,Model,NodeName,NodeRef,'tiledefault',birthrate(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,'tiledefault',birthratekey(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,'tiledefault',birthratekey(_,_,_))),
  gotdata(File,Model,anim('tiledefault'),length(L)),
  (L=:=0 ->
    asserta(gotdata(File,Model,NodeName,NodeRef,'tiledefault',birthratekey(1))),
    asserta(gotdata(File,Model,NodeName,NodeRef,'tiledefault',birthratekey(0,0,0)))
    ;
    asserta(gotdata(File,Model,NodeName,NodeRef,'tiledefault',birthratekey(2))),
    asserta(gotdata(File,Model,NodeName,NodeRef,'tiledefault',birthratekey(0,0,0))),
    asserta(gotdata(File,Model,NodeName,NodeRef,'tiledefault',birthratekey(1,L,0)))
  ),  
  tab(2), write('emitter '), write(NodeName), write(' assigned birthratekey 0 in tiledefault animation'), nl,
  increment_bug_count(File),
  fail.

check_for(animated_emitters,File,_) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  \+(gotdata(File,Model,NodeName,NodeRef,update(lightning))),
  \+((member(A,[default,day,night]), gotdata(File,Model,newanim(A,Model)))),
  gotdata(File,Model,NodeName,NodeRef,birthrate(B)), B>0,
  \+((gotdata(File,Model,NodeName,NodeRef,_,birthratekey(_,_,B1)), B1>0)),
  \+((gotdata(File,Model,NodeName,NodeRef,_,birthrate(B2)), B2>0)),
  (gotdata(File,Model,newanim('animloop01',Model)) -> true;
   asserta(gotdata(File,Model,newanim('animloop01',Model))),
   asserta(gotdata(File,Model,anim('animloop01'),animroot(Model))),
   asserta(gotdata(File,Model,anim('animloop01'),length(0))),
   asserta(gotdata(File,Model,anim('animloop01'),transtime(0.25))),
   increment_bug_count(File)),
  retractall(gotdata(File,Model,NodeName,NodeRef,'animloop01',birthrate(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,'animloop01',birthratekey(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,'animloop01',birthratekey(_,_,_))),
  gotdata(File,Model,anim('animloop01'),length(L)),
  (L=:=0 ->
    asserta(gotdata(File,Model,NodeName,NodeRef,'animloop01',birthratekey(1))),
    asserta(gotdata(File,Model,NodeName,NodeRef,'animloop01',birthratekey(0,0,B)))
    ;
    asserta(gotdata(File,Model,NodeName,NodeRef,'animloop01',birthratekey(2))),
    asserta(gotdata(File,Model,NodeName,NodeRef,'animloop01',birthratekey(0,0,B))),
    asserta(gotdata(File,Model,NodeName,NodeRef,'animloop01',birthratekey(1,L,B)))
  ),  
  tab(2), write('emitter '), write(NodeName), write(' assigned birthratekey '), write(B), write(' in animloop01'), nl,
  increment_bug_count(File),
  fail.

check_for(animated_lightning_emitters,File,_) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,update(lightning)),
  gotdata(File,Model,NodeName,NodeRef,birthrate(B)), B>0,
  retract(gotdata(File,Model,NodeName,NodeRef,AnimName,birthratekey(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,AnimName,birthratekey(_,_,_))),
  tab(2), write('birthratekey(s) deleted from '), write(AnimName), write(' for lightning emitter '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(animated_lightning_emitters,File,_) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,update(lightning)),
  gotdata(File,Model,NodeName,NodeRef,birthrate(B)), B>0,
  retract(gotdata(File,Model,NodeName,NodeRef,AnimName,birthrate(_))),
  tab(2), write('birthrate deleted from '), write(AnimName), write(' for lightning emitter '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(modeldummy_animation,File,_) :-
  gotdata(File,Model,newanim(AnimName,Model)),
  once(gotdata(File,Model,Model,MRef,AnimName,_)),
  retractall(gotdata(File,Model,Model,MRef,AnimName,_)),
  tab(2), write('animation keys for base dummy deleted from '), write(AnimName), nl,
  increment_bug_count(File),
  fail.

check_for(wrong_default_keys,File,_) :-
  gotdata(File,Model,classification('TILE')),
  member(AnimName,['tiledefault','default']),
  gotdata(File,Model,newanim(AnimName,Model)),
  gotdata(File,Model,NodeName,NodeRef,Q0), Q0=..[P|R0], P\=='birthrate',
  atom_concat(P,'key',Key), clause(paramtype(anim,_,Key),true),
  Qxa1=..[Key,1], Qxb1=..[Key,0,0|R0],
  Qxa=..[Key,_], functor(Q0,P,A), A2 is A+2, functor(Qxb,Key,A2),
  once(gotdata(File,Model,NodeName,NodeRef,_,Qxa)),
  \+ (( gotdata(File,Model,NodeName,NodeRef,AnimName,Qxa1), gotdata(File,Model,NodeName,NodeRef,AnimName,Qxb1) )),
  retractall(gotdata(File,Model,NodeName,NodeRef,AnimName,Qxa)),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,Qxa1)),
  retractall(gotdata(File,Model,NodeName,NodeRef,AnimName,Qxb)),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,Qxb1)),
  tab(2), write(Key), write(' data for '), write(NodeName), write(' corrected in '), write(AnimName), write(' animation'), nl,
  increment_bug_count(File),
  fail.

check_for(daynight_sequence,File,_) :-
  gotdata(File,Model,classification('TILE')),
  DayNightList = ['day','night','day2night','night2day'],
  once(( member(A, DayNightList), gotdata(File,Model,newanim(A,Model)) )),
  \+ (( member(A1, DayNightList), gotdata(File,Model,_,_,A1,_) )),
  \+ (( member(A12, DayNightList), gotdata(File,Model,anim(A12),event(_,_)) )),
  ( member(A2, DayNightList),
    retract(gotdata(File,Model,newanim(A2,Model))),
    retractall(gotdata(File,Model,anim(A2),_)),
    fail; true
  ),
  tab(2), write('empty day/night animations deleted from model '), write(Model), nl,
  increment_bug_count(File),
  fail.

check_for(daynight_sequence,File,_) :-
  gotdata(File,Model,classification('TILE')),
  DayNightList = [['day',0.3],['night',0.3],['day2night',0.03333333],['night2day',0.03333333]],
  once(( member([A,_], DayNightList), gotdata(File,Model,newanim(A,Model)) )),
  member([AnimName,Length],DayNightList),
  \+ gotdata(File,Model,newanim(AnimName,Model)),
  asserta(gotdata(File,Model,newanim(AnimName,Model))),
  asserta(gotdata(File,Model,anim(AnimName),animroot(Model))),
  asserta(gotdata(File,Model,anim(AnimName),length(Length))),
  asserta(gotdata(File,Model,anim(AnimName),transtime(0.25))),
  tab(2), write('dummy animation '), write(AnimName), write(' created'), nl,
  increment_bug_count(File),
  fail.

check_for(pointless_animation,File,_) :-
  gotdata(File,Model,classification('TILE')),
  DayNightList = ['day','night','day2night','night2day'],
  gotdata(File,Model,newanim(AnimName,Model)),
  /* \+ atom_concat('animloop0',_,AnimName), */
  \+ member(AnimName, DayNightList),
  \+ gotdata(File,Model,_,_,AnimName,_),
  \+ gotdata(File,Model,anim(AnimName),event(_,_)),
  retract(gotdata(File,Model,newanim(AnimName,Model))),
  retractall(gotdata(File,Model,anim(AnimName),_)),
  tab(2), write('empty animation '), write(AnimName), write(' deleted'), nl,
  increment_bug_count(File),
  fail.

check_for(door_animation_names,File,SmallLogStream) :-
  gotdata(File,Model,classification('DOOR')),
  DoorAnimList = [opened1,opened2,opening1,opening2,closing1,closing2,closed,damage,die,dead,trans,default],
  gotdata(File,Model,newanim(AnimName,Model)),
  \+ member(AnimName,DoorAnimList),
  report_warning(['animation',AnimName,'has an unexpected name'],SmallLogStream),
  fail.
  
check_for(binary_flags,File,_) :-
  boolean(Param), Q=..[Param,Value],
  gotdata(File,Model,NodeName,NodeRef,Q),
  Value\=0, Value\=1,
  (Value<0 -> NewValue=0; NewValue=1),
  Q1=..[Param,NewValue],
  retract(gotdata(File,Model,NodeName,NodeRef,Q)),
  asserta(gotdata(File,Model,NodeName,NodeRef,Q1)),
  tab(2), write(Param), write(' parameter for '), write(NodeName), write(' was not 0 or 1 - '), write(NewValue), write(' assumed'), nl,
  increment_bug_count(File),
  fail.

check_for(missing_end_keys,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,anim(AnimName),length(Length)),
  AnimName\='day2night', AnimName\='night2day',
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef), NodeType\=animmesh,
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q), Q=..[Key,NKeys], atom_concat(_,'key',Key), integer(NKeys), LastKey is NKeys-1,
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q0), Q0=..[Key,0,_|V0],
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q1), Q1=..[Key,LastKey,LastTime|_],
  round(30*LastTime)<round(30*Length),
  Q2=..[Key,NKeys,Length|V0],
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,Q2)),
  NewNKeys is NKeys+1, Q3=..[Key,NewNKeys],
  retract(gotdata(File,Model,NodeName,NodeRef,AnimName,Q)),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,Q3)),
  tab(2), write('animation '), write(AnimName), write(': endkey added for '), write(Key), write('s on '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(missing_end_keys,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,anim(AnimName),length(Length)),
  (AnimName=='day2night' -> AnimName2='night' ; AnimName=='night2day' -> AnimName2='day' ; fail),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef), NodeType\=animmesh,
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q), Q=..[Key,NKeys], atom_concat(_,'key',Key), integer(NKeys), LastKey is NKeys-1,
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q1), Q1=..[Key,LastKey,LastTime|_],
  gotdata(File,Model,NodeName,NodeRef,AnimName2,Q0), Q0=..[Key,0,_|V0],
  round(30*LastTime)<round(30*Length),
  Q2=..[Key,NKeys,Length|V0],
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,Q2)),
  NewNKeys is NKeys+1, Q3=..[Key,NewNKeys],
  retract(gotdata(File,Model,NodeName,NodeRef,AnimName,Q)),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,Q3)),
  tab(2), write('animation '), write(AnimName), write(': endkey added for '), write(Key), write('s on '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(mismatching_start_end_keys,File,SmallLogStream) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,anim(AnimName),length(Length)),
  AnimName\='day2night', AnimName\='night2day',
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef), NodeType\=animmesh,
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q), Q=..[Key,NKeys], atom_concat(_,'key',Key), integer(NKeys), LastKey is NKeys-1,
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q0), Q0=..[Key,0,_|V0],
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q1), Q1=..[Key,LastKey,LastTime|V1],
  round(30*LastTime)=:=round(30*Length),
  \+ maplist(=:=,V0,V1),
  report_warning(['in animation',AnimName,'start and end keys differ for',Key,'on',NodeName],SmallLogStream),
  fail.

check_for(mismatching_start_end_keys,File,SmallLogStream) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,anim(AnimName),length(Length)),
  (AnimName=='day2night' -> AnimName2='night' ; AnimName=='night2day' -> AnimName2='day' ; fail),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef), NodeType\=animmesh,
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q), Q=..[Key,NKeys], atom_concat(_,'key',Key), integer(NKeys), LastKey is NKeys-1,
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q1), Q1=..[Key,LastKey,LastTime|V1],
  gotdata(File,Model,NodeName,NodeRef,AnimName2,Q0), Q0=..[Key,0,_|V0],
  round(30*LastTime)=:=round(30*Length),
  \+ maplist(=:=,V0,V1),
  report_warning(['endkey of',AnimName,'does not match startkey of',AnimName2,'for',Key,'on',NodeName],SmallLogStream),
  fail.

check_for(mismatching_start_end_keys,File,SmallLogStream) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,anim(AnimName),length(Length)),
  (AnimName=='day2night' -> AnimName2='day' ; AnimName=='night2day' -> AnimName2='night' ; fail),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef), NodeType\=animmesh,
  gotdata(File,Model,NodeName,NodeRef,AnimName2,Q), Q=..[Key,NKeys], atom_concat(_,'key',Key), integer(NKeys), LastKey is NKeys-1,
  gotdata(File,Model,NodeName,NodeRef,AnimName2,Q1), Q1=..[Key,LastKey,LastTime|V1],
  gotdata(File,Model,NodeName,NodeRef,AnimName,Q0), Q0=..[Key,0,_|V0],
  round(30*LastTime)=:=round(30*Length),
  \+ maplist(=:=,V0,V1),
  report_warning(['startkey of',AnimName,'does not match endkey of',AnimName2,'for',Key,'on',NodeName],SmallLogStream),
  fail.

check_for(animmesh_ratios_agree,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,verts(V)),
  gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(AV)),
  \+ ((V>0, Rv is round(AV/V), Rv>=2, AV=:=Rv*V)),
  report_error(['bad number of animverts for',NodeName,'in animation',AnimName],SmallLogStream),
  bad_error(File),
  fail.

check_for(animmesh_ratios_agree,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,tverts(TV)),
  gotdata(File,Model,NodeName,NodeRef,AnimName,animtverts(ATV)),
  \+ ((TV>0, Rt is round(ATV/TV), Rt>=2, ATV=:=Rt*TV)),
  report_error(['bad number of animtverts for',NodeName,'in animation',AnimName],SmallLogStream),
  bad_error(File),
  fail.

check_for(animmesh_ratios_agree,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,verts(V)),
  gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(AV)),
  V>0, Rv is round(AV/V), Rv>=2, AV=:=Rv*V,
  gotdata(File,Model,NodeName,NodeRef,tverts(TV)),
  gotdata(File,Model,NodeName,NodeRef,AnimName,animtverts(ATV)),
  TV>0, Rt is round(ATV/TV), Rt>=2, ATV=:=Rt*TV,
  Rt=\=Rv,
  report_error(['bad number of animtverts for',NodeName,'in animation',AnimName],SmallLogStream),
  bad_error(File),
  fail.

check_for(animmesh_ratios_agree,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  gotdata(File,Model,anim(AnimName),length(L)),
  gotdata(File,Model,NodeName,NodeRef,AnimName,sampleperiod(S)),
  Sf is round(S*30), Lf is round(L*30),
  \+ ((Sf>0, Rf is round(Lf/Sf), Lf=:=Rf*Sf)),
  report_error(['bad length or sampleperiod for animmesh',NodeName,'in animation',AnimName],SmallLogStream),
  bad_error(File),
  fail.

check_for(animmesh_ratios_agree,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,verts(V)),
  gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(AV)),
  V>0, Rv is round(AV/V), Rv>=2, AV=:=Rv*V,
  gotdata(File,Model,anim(AnimName),length(L)),
  gotdata(File,Model,NodeName,NodeRef,AnimName,sampleperiod(S)),
  Sf is round(S*30), Lf is round(L*30),
  Sf>0, Rf is round(Lf/Sf), Lf=:=Rf*Sf,
  Rf=\=Rv-1,
  report_error(['number of animverts does not match the length and sampleperiod for animmesh',NodeName,'in animation',AnimName],SmallLogStream),
  bad_error(File),
  fail.

/* ============================================================= */ 
/* Check on forcing shadows on or off globally. Moved up to here */
/* in CM343j so that it is performed before bitmaps are checked. */
/* CM345b: also apply this to danglymeshes in character models   */
/* CM345d: also apply this to skinmeshes in character models     */
/* ============================================================= */ 

check_for(render_everything,File,_) :-
  once(g_user_option(render,RenderAll)),
  (RenderAll=all -> Rdel=0, Radd=1; RenderAll=none -> Rdel=1, Radd=0; fail),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  once((retract(gotdata(File,Model,NodeName,NodeRef,render(Rdel))); true)),
  (\+gotdata(File,Model,NodeName,NodeRef,render(Radd)) ->
     asserta(gotdata(File,Model,NodeName,NodeRef,render(Radd))),
     tab(2), write('render set to '), write(Radd), write(' in trimesh '), write(NodeName), nl,
     increment_bug_count(File)
     ;
     true
  ),
  fail.

check_for(shadow_everything,File,_) :- 
  once(g_user_option(shadow,Shadow)),
  (Shadow=all -> Sdel=0, Sadd=1; Shadow=none -> Sdel=1, Sadd=0; fail),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  once((retract(gotdata(File,Model,NodeName,NodeRef,shadow(Sdel))); true)),
  (\+gotdata(File,Model,NodeName,NodeRef,shadow(Sadd)) ->
     asserta(gotdata(File,Model,NodeName,NodeRef,shadow(Sadd))),
     tab(2), write('shadow set to '), write(Sadd), write(' in trimesh '), write(NodeName), nl,
     increment_bug_count(File)
     ;
     true
  ),
  fail.

check_for(shadow_everything,File,_) :- 
  gotdata(File,Model,classification('CHARACTER')),
  once(g_user_option(shadow,Shadow)),
  (Shadow=all -> Sdel=0, Sadd=1; Shadow=none -> Sdel=1, Sadd=0; fail),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef), member(NodeType,[danglymesh,skin]),
  once((retract(gotdata(File,Model,NodeName,NodeRef,shadow(Sdel))); true)),
  (\+gotdata(File,Model,NodeName,NodeRef,shadow(Sadd)) ->
     asserta(gotdata(File,Model,NodeName,NodeRef,shadow(Sadd))),
     tab(2), write('shadow set to '), write(Sadd), write(' in '), write(NodeType), tab(1), write(NodeName), nl,
     increment_bug_count(File)
     ;
     true
  ),
  fail.

/* ========================================================= */
/* Added in CM3.5.1a: force aabb nodes to render 0, shadow 0 */
/* ========================================================= */

check_for(aabb_render0_shadow0,File,_) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,render(0)),
  retractall(gotdata(File,Model,NodeName,NodeRef,render(_))),
  assertz(gotdata(File,Model,NodeName,NodeRef,render(0))),
  tab(2), write('aabb node '), write(NodeName), write(' set to render 0'), nl,
  increment_bug_count(File),
  fail.

check_for(aabb_render0_shadow0,File,_) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,shadow(0)),
  retractall(gotdata(File,Model,NodeName,NodeRef,shadow(_))),
  assertz(gotdata(File,Model,NodeName,NodeRef,shadow(0))),
  tab(2), write('aabb node '), write(NodeName), write(' set to shadow 0'), nl,
  increment_bug_count(File),
  fail.

/* ======================================================== */
/* Checks on bitmaps. These cannot be fixed automatically   */
/* except that for unrendered nodes they can be set to null */
/* ======================================================== */

check_for(unsupported_multimaterials,File,_) :-
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  NodeType\=aabb,
  once((gotdata(File,Model,NodeName,NodeRef,multimaterial(_,Bitmap)), Bitmap\='NULL')),
  \+ gotdata(File,Model,NodeName,NodeRef,bitmap(_)),
  asserta(gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap))),
  retractall(gotdata(File,Model,NodeName,NodeRef,multimaterial(_,_))),
  retract(gotdata(File,Model,NodeName,NodeRef,multimaterial(_))),
  tab(2), write('unsupported multimaterial changed to bitmap '), write(Bitmap), write(' in object '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(unsupported_multimaterials,File,_) :-
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  NodeType\=aabb,
  gotdata(File,Model,NodeName,NodeRef,multimaterial(_)),
  retractall(gotdata(File,Model,NodeName,NodeRef,multimaterial(_,_))),
  retract(gotdata(File,Model,NodeName,NodeRef,multimaterial(_))),
  tab(2), write('unsupported multimaterial deleted in object '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(unwanted_bitmaps,File,_) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  Bitmap\='NULL',
  retract(gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap))),
  asserta(gotdata(File,Model,NodeName,NodeRef,bitmap('NULL'))),
  tab(2), write('unused bitmap '), write(Bitmap), write(' in aabb '), write(NodeName), write(' set to NULL'), nl,
  increment_bug_count(File),
  fail.

check_for(unwanted_bitmaps,File,_) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,bitmap(_)),
  asserta(gotdata(File,Model,NodeName,NodeRef,bitmap('NULL'))),
  tab(2), write('bitmap in aabb '), write(NodeName), write(' set to NULL'), nl,
  increment_bug_count(File),
  fail.

check_for(unwanted_bitmaps,File,_) :-
  \+ ((gotdata(File,Model,classification('CHARACTER')), gotdata(File,Model,newanim(_,Model)))), 
  gotdata(File,Model,NodeName,NodeRef,render(0)),
  \+ gotdata(File,Model,NodeName,NodeRef,shadow(1)),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  Bitmap\='NULL',
  retract(gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap))),
  asserta(gotdata(File,Model,NodeName,NodeRef,bitmap('NULL'))),
  tab(2), write('unused bitmap '), write(Bitmap), write(' in unrendered node '), write(NodeName), write(' set to NULL'), nl,
  increment_bug_count(File),
  fail.

check_for(black_shadow_bitmaps,File,_) :-
  gotdata(File,Model,NodeName,NodeRef,render(0)),
  gotdata(File,Model,NodeName,NodeRef,shadow(1)),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  \+atom_concat(_,'black',Bitmap),
  retract(gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap))),
  asserta(gotdata(File,Model,NodeName,NodeRef,bitmap('black'))),
  tab(2), write('bitmap '), write(Bitmap), write(' in shadow node '), write(NodeName), write(' changed to black'), nl,
  increment_bug_count(File),
  fail.

check_for(unlikely_bitmaps,File,SmallLogStream) :-
  gotdata(File,Model,newmodel(Model)),
  \+ ((gotdata(File,Model,classification('CHARACTER')), gotdata(File,Model,newanim(_,Model)))), 
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,bitmap(NodeName)),
  report_warning(['bitmap',NodeName,'in',NodeType,NodeName,'is unlikely'],SmallLogStream),
  fail.

check_for(unlikely_bitmaps,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,texture(NodeName)),
  report_warning(['texture',NodeName,'in emitter',NodeName,'is unlikely'],SmallLogStream),
  fail.

check_for(unlikely_bitmaps,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(light,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,texturenames(_,NodeName)),
  report_warning(['texturename',NodeName,'in light',NodeName,'is unlikely'],SmallLogStream),
  fail.

check_for(null_bitmaps,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,NodeName,NodeRef,bitmap('NULL')),
  \+clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  \+gotdata(File,Model,NodeName,NodeRef,render(0)),
  ( gotdata(File,Model,NodeName,NodeRef,selfillumcolor(R,G,B)) -> R=0, G=0, B=0 ; true ),
  \+gotdata(File,Model,NodeName,NodeRef,_,selfillumcolorkey(_,_,_,_,_)),
  \+gotdata(File,Model,NodeName,NodeRef,alpha(0)), 
  \+gotdata(File,Model,NodeName,NodeRef,_,alphakey(_,_,_)), 
  retract(gotdata(File,Model,NodeName,NodeRef,bitmap('NULL'))),
  asserta(gotdata(File,Model,NodeName,NodeRef,bitmap(black))),
  increment_bug_count(File),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('NULL bitmap in rendered object '), write(NodeName), write(' set to black'), nl ; true),
  fail.

check_for(null_bitmaps,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('NULL bitmap set to black in '), write(N), write(' rendered objects'), nl,
  fail.

check_for(null_bitmaps,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('NULL bitmap set to black in rendered object(s) '), write(List), nl,
  fail.

check_for(null_bitmaps,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(emitter,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,texture('NULL')),
  report_warning(['NULL texture in emitter',NodeName,'should be checked'],SmallLogStream),
  fail.

check_for(null_bitmaps,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(light,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,texturenames(_,'NULL')),
  report_warning(['NULL texturename in light',NodeName,'should be checked'],SmallLogStream),
  fail.

check_for(missing_bitmaps,File,_) :-
  gotdata(File,Model,NodeName,NodeRef,render(1)),
  \+ gotdata(File,Model,NodeName,NodeRef,bitmap(_)),
  asserta(gotdata(File,Model,NodeName,NodeRef,bitmap(toolcolors))),
  tab(2), write('missing bitmap for rendered node '), write(NodeName), write(' set to toolcolors'), nl,
  increment_bug_count(File),
  fail.

check_for(missing_bitmaps,File,_) :-
  gotdata(File,Model,NodeName,NodeRef,render(0)),
  gotdata(File,Model,NodeName,NodeRef,shadow(1)),
  \+ gotdata(File,Model,NodeName,NodeRef,bitmap(_)),
  asserta(gotdata(File,Model,NodeName,NodeRef,bitmap(black))),
  tab(2), write('bitmap for shadow node '), write(NodeName), write(' set to black'), nl,
  increment_bug_count(File),
  fail.

check_for(black_ambient0,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)), atom_concat(_,'black',Bitmap),
  \+ ((gotdata(File,Model,NodeName,NodeRef,ambient(A1,A2,A3)), A1=:=0, A2=:=0, A3=:=0)),
  once((retract(gotdata(File,Model,NodeName,NodeRef,ambient(_,_,_)));true)),
  asserta(gotdata(File,Model,NodeName,NodeRef,ambient(0,0,0))),
  increment_bug_count(File),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('black node '), write(NodeName), write(' set to ambient 0 0 0'), nl ; true),
  fail.

check_for(black_ambient0,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('set ambient to 0 0 0 in '), write(N), write(' black objects'), nl,
  fail.

check_for(black_ambient0,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('set ambient to 0 0 0 in black object(s) '), write(List), nl,
  fail.

check_for(black_diffuse0,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)), atom_concat(_,'black',Bitmap),
  \+ ((gotdata(File,Model,NodeName,NodeRef,diffuse(A1,A2,A3)), A1=:=0, A2=:=0, A3=:=0)),
  once((retract(gotdata(File,Model,NodeName,NodeRef,diffuse(_,_,_)));true)),
  asserta(gotdata(File,Model,NodeName,NodeRef,diffuse(0,0,0))),
  increment_bug_count(File),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('black node '), write(NodeName), write(' set to diffuse 0 0 0'), nl ; true),
  fail.

check_for(black_diffuse0,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('set diffuse to 0 0 0 in '), write(N), write(' black objects'), nl,
  fail.

check_for(black_diffuse0,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('set diffuse to 0 0 0 in black object(s) '), write(List), nl,
  fail.

check_for(non_black_ambient1,File,_) :-
  retractall(fixlist(_)),
  once(g_user_option(force_white,ForceWhite)), ForceWhite==yes,
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  member(NodeType,['trimesh','danglymesh','animmesh','skin']),
  \+ ((gotdata(File,Model,NodeName,NodeRef,render(0)))),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)), \+atom_concat(_,'black',Bitmap),
  \+ ((gotdata(File,Model,NodeName,NodeRef,ambient(A1,A2,A3)), A1=:=1, A2=:=1, A3=:=1)),
  once((retract(gotdata(File,Model,NodeName,NodeRef,ambient(_,_,_)));true)),
  asserta(gotdata(File,Model,NodeName,NodeRef,ambient(1,1,1))),
  increment_bug_count(File),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('node '), write(NodeName), write(' set to ambient 1 1 1'), nl ; true),
  fail.

check_for(non_black_ambient1,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('set ambient to 1 1 1 in '), write(N), write(' objects'), nl,
  fail.

check_for(non_black_ambient1,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('set ambient to 1 1 1 in object(s) '), write(List), nl,
  fail.

check_for(non_black_diffuse1,File,_) :-
  retractall(fixlist(_)),
  once(g_user_option(force_white,ForceWhite)), ForceWhite==yes,
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  member(NodeType,['trimesh','danglymesh','animmesh','skin']),
  \+ ((gotdata(File,Model,NodeName,NodeRef,render(0)))),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)), \+atom_concat(_,'black',Bitmap),
  \+ ((gotdata(File,Model,NodeName,NodeRef,diffuse(A1,A2,A3)), A1=:=1, A2=:=1, A3=:=1)),
  once((retract(gotdata(File,Model,NodeName,NodeRef,diffuse(_,_,_)));true)),
  asserta(gotdata(File,Model,NodeName,NodeRef,diffuse(1,1,1))),
  increment_bug_count(File),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('node '), write(NodeName), write(' set to diffuse 1 1 1'), nl ; true),
  fail.

check_for(non_black_diffuse1,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('set diffuse to 1 1 1 in '), write(N), write(' objects'), nl,
  fail.

check_for(non_black_diffuse1,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('set diffuse to 1 1 1 in object(s) '), write(List), nl,
  fail.

/*
check_for('bioware_shiny_water',File,_) :-
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  shiny_water_replacement(Bitmap,NewBitmap), NewBitmap\=Bitmap,
  retract(gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap))),
  asserta(gotdata(File,Model,NodeName,NodeRef,bitmap(NewBitmap))),
  increment_bug_count(File),
  tab(2), write('replacing bitmap '), write(Bitmap), write(' with '), write(NewBitmap), nl,
  fail.

check_for('bioware_shiny_water',File,_) :-
  gotdata(File,Model,NodeName,NodeRef,texture(Bitmap)),
  shiny_water_replacement(Bitmap,NewBitmap), NewBitmap\=Bitmap,
  retract(gotdata(File,Model,NodeName,NodeRef,texture(Bitmap))),
  asserta(gotdata(File,Model,NodeName,NodeRef,texture(NewBitmap))),
  increment_bug_count(File),
  tab(2), write('replacing texture '), write(Bitmap), write(' with '), write(NewBitmap), nl,
  fail.

check_for('bioware_shiny_water',File,_) :-
  gotdata(File,Model,NodeName,NodeRef,texturename(Bitmap)),
  shiny_water_replacement(Bitmap,NewBitmap), NewBitmap\=Bitmap,
  retract(gotdata(File,Model,NodeName,NodeRef,texturename(Bitmap))),
  asserta(gotdata(File,Model,NodeName,NodeRef,texturename(NewBitmap))),
  increment_bug_count(File),
  tab(2), write('replacing texturename '), write(Bitmap), write(' with '), write(NewBitmap), nl,
  fail.
*/

check_for(rotate_water_texture,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(rotate_water,RotateWater)), RotateWater==1,
  gotdata(File,Model,beginmodelgeom(Model)),
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  is_watery(File,Model,NodeName,NodeRef),
  (retract(gotdata(File,Model,NodeName,NodeRef,rotatetexture(0))), fail ; true),  
  \+ (gotdata(File,Model,NodeName,NodeRef,rotatetexture(1))),
  asserta(gotdata(File,Model,NodeName,NodeRef,rotatetexture(1))),
  tab(2), write('rotatetexture set to 1 for water node '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(rotate_water_texture,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(rotate_water,RotateWater)), RotateWater==0,
  gotdata(File,Model,beginmodelgeom(Model)),
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  is_watery(File,Model,NodeName,NodeRef),
  (retract(gotdata(File,Model,NodeName,NodeRef,rotatetexture(1))), fail ; true),  
  \+ (gotdata(File,Model,NodeName,NodeRef,rotatetexture(0))),
  asserta(gotdata(File,Model,NodeName,NodeRef,rotatetexture(0))),
  tab(2), write('rotatetexture set to 0 for water node '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(rotate_ground_texture,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(rotate_ground,RotateGround)), RotateGround==1,
  gotdata(File,Model,beginmodelgeom(Model)),
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  is_ground(File,Model,NodeName,NodeRef),
  (retract(gotdata(File,Model,NodeName,NodeRef,rotatetexture(0))), fail ; true),  
  \+ (gotdata(File,Model,NodeName,NodeRef,rotatetexture(1))),
  asserta(gotdata(File,Model,NodeName,NodeRef,rotatetexture(1))),
  tab(2), write('rotatetexture set to 1 for ground node '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(rotate_ground_texture,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(rotate_ground,RotateGround)), RotateGround==0,
  gotdata(File,Model,beginmodelgeom(Model)),
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  is_ground(File,Model,NodeName,NodeRef),
  (retract(gotdata(File,Model,NodeName,NodeRef,rotatetexture(1))), fail ; true),  
  \+ (gotdata(File,Model,NodeName,NodeRef,rotatetexture(0))),
  asserta(gotdata(File,Model,NodeName,NodeRef,rotatetexture(0))),
  tab(2), write('rotatetexture set to 0 for ground node '), write(NodeName), nl,
  increment_bug_count(File),
  fail.

check_for(pxh0_bitmaps,File,_) :- /* Added in CM345d */
  gotdata(File,Model,classification('CHARACTER')),
  pxh0_bitmap(Model,Bitmap),
  clause(gotdata(File,Model,NodeName,NodeRef,bitmap(B)),true,Eref),
  \+gotdata(File,Model,NodeName,NodeRef,render(0)),
  pxh0_bitmap(B,_), /* Amended in CM362d to only replace slightly-wrong bitmaps */ 
  B\=Bitmap,
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap))),
  tab(2), write('bitmap on '), write(NodeName), write(' set to '), write(Bitmap), nl,
  increment_bug_count(File),
  fail.

/* ============================================================== */
/* Option to merge non-animated non-shadowing trimeshes by bitmap */
/* Added in V3.4.2b                                               */
/* CM352d: Bugfix in the last clause of append_mesh and duplicate */
/* remove_tilefade prior to merging meshes                        */
/* ============================================================== */

check_for(remove_tilefade,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(slice,Slice)), Slice==undo,
  t_remove_all_tilefades(File,Model),
  fail.


check_for(merge_by_bitmap,File,_) :-
  once(g_user_option(merge_by_bitmap,Merge)), Merge==yes,
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  \+gotdata(File,Model,_,_,parent(NodeName/NodeRef)),
  gotdata(File,Model,NodeName,NodeRef,shadow(0)),
  \+is_unmergeable_node(File,Model,NodeName,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  gotdata(File,Model,NodeName2,NodeRef2,bitmap(Bitmap)),
  clause(gotdata(File,Model,node(trimesh,NodeName2)),true,NodeRef2), NodeRef2@>NodeRef,
  \+gotdata(File,Model,_,_,parent(NodeName2/NodeRef2)),
  gotdata(File,Model,NodeName2,NodeRef2,shadow(0)),
  \+is_unmergeable_node(File,Model,NodeName2,NodeRef2),
  \+ ((
        member(Q0,[scale,alpha,render]),
        Q1=..[Q0,X1], Q2=..[Q0,X2],
        once((gotdata(File,Model,NodeName,NodeRef,Q1); X1=1)),
        once((gotdata(File,Model,NodeName2,NodeRef2,Q2); X2=1)),
        X2=\=X1
      )),
  \+ ((
        member(Q0,[inheritcolor,beaming,transparencyhint,tilefade,lightmapped,rotatetexture]),
        Q1=..[Q0,X1], Q2=..[Q0,X2],
        once((gotdata(File,Model,NodeName,NodeRef,Q1); X1=0)),
        once((gotdata(File,Model,NodeName2,NodeRef2,Q2); X2=0)),
        X2=\=X1
      )),
  \+ ((
        member(Q0,[selfillumcolor, diffuse, ambient, specular]),
        Q1=..[Q0,U1,V1,W1], Q2=..[Q0,U2,V2,W2],
        once((gotdata(File,Model,NodeName,NodeRef,Q1); [U1,V1,W1]=[0,0,0])),
        once((gotdata(File,Model,NodeName2,NodeRef2,Q2); [U2,V2,W2]=[0,0,0])),
        once((U1=\=U2; V1=\=V2 ; W1=\=W2))
      )),
  (gotdata(File,Model,NodeName,NodeRef,parent(Model/_)) -> true;
   raise_to_tile(File,Model,NodeName,NodeRef),
   tab(2), write('trimesh node '), write(NodeName), write(' relinked to model base'), nl),
  set_zero_orientation(File,Model,NodeName,NodeRef),
  set_zero_position(File,Model,NodeName,NodeRef),
  (gotdata(File,Model,NodeName2,NodeRef2,parent(Model/_)) -> true;
   raise_to_tile(File,Model,NodeName2,NodeRef2),
   tab(2), write('trimesh node '), write(NodeName2), write(' relinked to model base'), nl),
  set_zero_orientation(File,Model,NodeName2,NodeRef2),
  set_zero_position(File,Model,NodeName2,NodeRef2),
  append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2),
  tab(2), write('merged trimesh '), write(NodeName2), write(' into '), write(NodeName), nl,
  fail.

/* ============================= */
/* Checks on vertices and faces. */
/* These can generally be fixed. */
/* ============================= */

check_for(vertex_snap,File,_) :-
  \+ check_failed(File),
  once(g_user_option(snap,Snap)), Snap\=none,
  tab(2), write(Snap), write(' snap applied to node positions and mesh vertices'), nl,
  clause(gotdata(File,Model,node(dummy,Model)),true,MRef),
  gotdata(File,Model,NodeName,NodeRef,parent(Model/MRef)),
  apply_snap(File,Model,NodeName,NodeRef),
  fail.

check_for(out_of_range_vertices,File,_) :-
  gotdata(File,Model,NodeName,NodeRef,faces(_)),
  (gotdata(File,Model,NodeName,NodeRef,verts(NVerts)) -> true; NVerts=1),
  once((gotdata(File,Model,NodeName,NodeRef,faces(_,V1,V2,V3,_,_,_,_,_)), (V1>=NVerts; V2>=NVerts; V3>=NVerts))),
  fix_out_of_range_verts(File,Model,NodeName,NodeRef,NVerts),
  tab(2), write('out of range vert(s) in face(s) of '), write(NodeName), write(' set to zero'), nl,
  fail.

/* Vertex welding changed in CM362h */
check_for(unwelded_vertices,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,NodeName,NodeRef,verts(NVerts)),
  \+ clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  hash_verts(File,Model,NodeName,NodeRef),
  has_unwelded_verts(File,Model,NodeName,NodeRef),
  weld_vertices(File,Model,NodeName,NodeRef,NVerts,0,0),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('vertices welded in '), write(NodeName), nl ; true),
  fail.

check_for(unwelded_vertices,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('vertices welded in '), write(N), write(' objects'), nl,
  fail.

check_for(unwelded_vertices,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('vertices welded in '), write(List), nl,
  fail.

check_for(null_faces,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)),
  /* \+ clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef), */
  once((gotdata(File,Model,NodeName,NodeRef,faces(_,V1,V2,V3,_,_,_,_,_)),(V1=V2; V2=V3; V3=V1))),
  delete_null_faces(File,Model,NodeName,NodeRef,NFaces,0,0),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('null face(s) deleted from '), write(NodeName), nl ; true),
  fail.

check_for(null_faces,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('null faces deleted from '), write(N), write(' objects'), nl,
  fail.

check_for(null_faces,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('null faces deleted from '), write(List), nl,
  fail.

check_for(duplicate_faces,File,_) :-
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)),
  /* \+ clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef), */
  once((gotdata(File,Model,NodeName,NodeRef,faces(N1,V1,V2,V3,_,_,_,_,_)),
        same_face(V1,V2,V3,Vx1,Vx2,Vx3),
        gotdata(File,Model,NodeName,NodeRef,faces(N2,Vx1,Vx2,Vx3,_,_,_,_,_)),
        N2\=N1)),
  delete_duplicate_faces(File,Model,NodeName,NodeRef,NFaces,0,0),
  tab(2), write('duplicate face(s) deleted from '), write(NodeName), nl,
  fail.

check_for(trimesh_degenerate_faces,File,_) :-
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  setof(F,is_degenerate_face(File,Model,NodeName,NodeRef,F),Flist),
  length(Flist,L0), L0>0,
  fix_degenerate_faces(File,Model,NodeName,NodeRef,Flist),
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)),
  delete_null_faces(File,Model,NodeName,NodeRef,NFaces,0,0),
  tab(2), write('repaired '), write(L0), write(' degenerate face(s) in trimesh node '), write(NodeName), nl,
  fail.

check_for(trimesh_torn_faces,File,_) :-
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  setof(T,is_tear_in_mesh(File,Model,NodeName,NodeRef,T),Tlist),
  length(Tlist,L0), L0>0,
  fix_tears_in_mesh(File,Model,NodeName,NodeRef,Tlist),
  tab(2), write('stitched '), write(L0), write(' tear(s) in trimesh node '), write(NodeName), nl,
  fail.

check_for(trimesh_overlapping_faces,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  setof(Pair,is_overlapping_face_pair(File,Model,NodeName,NodeRef,Pair),PairList),
  length(PairList,L),
  report_warning([L,'overlapping face(s) in trimesh',NodeName],SmallLogStream),
  fail.

check_for(unused_vertices,File,_) :-
  gotdata(File,Model,NodeName,NodeRef,verts(_)),
  \+ clause(gotdata(File,Model,node(danglymesh,NodeName)),true,NodeRef),
  \+ clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  /* \+ clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef), */
  once((
    gotdata(File,Model,NodeName,NodeRef,verts(V,_,_,_)),
    \+ gotdata(File,Model,NodeName,NodeRef,faces(_,V,_,_,_,_,_,_,_)),
    \+ gotdata(File,Model,NodeName,NodeRef,faces(_,_,V,_,_,_,_,_,_)),
    \+ gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,V,_,_,_,_,_))
      )),
  delete_unused_vertices(File,Model,NodeName,NodeRef),
  tab(2), write('unused vertices deleted from '), write(NodeName), nl,
  fail.

check_for(danglymesh_has_shadow_0,File,_) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(danglymesh,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,shadow(0)),
  once((retract(gotdata(File,Model,NodeName,NodeRef,shadow(1))) ; true)),
  asserta(gotdata(File,Model,NodeName,NodeRef,shadow(0))),
  tab(2), write('danglymesh node '), write(NodeName), write(' set to shadow 0'), nl,
  increment_bug_count(File),
  fail.

check_for(animated_nodes_have_shadow_0,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,beginmodelgeom(Model)),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  once((gotdata(File,Model,NodeName,NodeRef,parent(Adummy/ARef)) ; superparent(File,Model,NodeName,NodeRef,Adummy,ARef))),
  once((retract(gotdata(File,Model,NodeName,NodeRef,shadow(1))); true)),
  \+ gotdata(File,Model,NodeName,NodeRef,shadow(0)),
  asserta(gotdata(File,Model,NodeName,NodeRef,shadow(0))),
  tab(2), write('animated node '), write(NodeName), write(' set to shadow 0'), nl,
  increment_bug_count(File),
  fail.

check_for(overlays_have_shadow_0,File,_) :-
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  atom_concat(_,'_2_',NodeName),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  make_3D(_,Bitmap),
  \+ gotdata(File,Model,NodeName,NodeRef,shadow(0)),
  retractall(gotdata(File,Model,NodeName,NodeRef,shadow(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,shadow(0))),
  tab(2), write('overlay '), write(NodeName), write(' set to shadow 0'), nl,
  increment_bug_count(File),
  fail.
  
/*
check_for(no_shadows_below_z,File,_) :-
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,shadow(0)),
  f_transform_constraint(File,Model,NodeName,NodeRef,[0,0,0,1],[A,B,C,D]),
  \+ ((gotdata(File,Model,NodeName,NodeRef,verts(_,X,Y,Z)), A+B*X+C*Y+D*Z > 0.0)),
  once((retract(gotdata(File,Model,NodeName,NodeRef,shadow(1))); true)),
  asserta(gotdata(File,Model,NodeName,NodeRef,shadow(0))),
  increment_bug_count(File),
  tab(2), write('trimesh node '), write(NodeName), write(' is at or below Z=0 - setting to shadow 0'), nl,
  fail.
*/

/* ================================ */
/* Checks on tverts and UVW mapping */
/* Re-ordered in version 3.2.1u     */
/* ================================ */

check_for(unwanted_tverts,File,_) :-
  \+ ((gotdata(File,Model,classification('CHARACTER')), gotdata(File,Model,newanim(_,Model)))), 
  gotdata(File,Model,NodeName,NodeRef,tverts(_)),
  gotdata(File,Model,NodeName,NodeRef,render(0)),
  \+ gotdata(File,Model,NodeName,NodeRef,shadow(1)),
  retractall(gotdata(File,Model,NodeName,NodeRef,tverts(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,tverts(_,_,_,_))),
  tab(2), write('unused tverts in unrendered node '), write(NodeName), write(' deleted'), nl,
  increment_bug_count(File),
  fail.

check_for(unwanted_tverts,File,_) :-
  gotdata(File,Model,NodeName,NodeRef,tverts(_)),
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  retractall(gotdata(File,Model,NodeName,NodeRef,tverts(_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,tverts(_,_,_,_))),
  tab(2), write('unused tverts in aabb node '), write(NodeName), write(' deleted'), nl,
  increment_bug_count(File),
  fail.

check_for(uv_remapping,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(do_water,DoWater)), DoWater\=no,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater\=wavy,
  once(g_user_option(tile_water,TileWaterRepeatCount)), integer(TileWaterRepeatCount),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  is_watery(Bitmap),
  remap_uv_to_tile(File,Model,NodeName,NodeRef,TileWaterRepeatCount),
  tab(2), write('water plane '), write(NodeName), write(' mapped to '), write(TileWaterRepeatCount), write('x'), write(TileWaterRepeatCount), write(' repeating pattern'), nl,
  fail.

check_for(uv_remapping,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(tile_ground,TileGroundRepeatCount)), integer(TileGroundRepeatCount),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  is_ground(Bitmap),
  remap_uv_to_tile(File,Model,NodeName,NodeRef,TileGroundRepeatCount),
  tab(2), write('ground plane '), write(NodeName), write(' with bitmap '), write(Bitmap), write(' UVW mapped '), write(TileGroundRepeatCount), write('x'), write(TileGroundRepeatCount), write(' across tile'), nl,
  fail.

check_for(out_of_range_tverts,File,_) :-
  gotdata(File,Model,NodeName,NodeRef,faces(_)),
  (gotdata(File,Model,NodeName,NodeRef,tverts(NVerts)) -> true; NVerts=1),
  once((gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,_,T1,T2,T3,_)), (T1>=NVerts; T2>=NVerts; T3>=NVerts))),
  fix_out_of_range_tverts(File,Model,NodeName,NodeRef,NVerts),
  tab(2), write('out of range tvert(s) in face(s) of '), write(NodeName), write(' set to zero'), nl,
  fail.

check_for(unused_tverts,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,NodeName,NodeRef,tverts(_)),
  \+ clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  once((
    gotdata(File,Model,NodeName,NodeRef,tverts(V,_,_,_)),
    \+ gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,_,V,_,_,_)),
    \+ gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,_,_,V,_,_)),
    \+ gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,_,_,_,V,_))
      )),
  delete_unused_tverts(File,Model,NodeName,NodeRef),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('unused tverts deleted from '), write(NodeName), nl ; true),
  fail.

check_for(unused_tverts,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('unused tverts deleted from '), write(N), write(' objects'), nl,
  fail.

check_for(unused_tverts,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('unused tverts deleted from '), write(List), nl,
  fail.

check_for(tvert_snap,File,_) :-
  once(g_user_option(tvert_snap,TSnap)), integer(TSnap),
  tab(2), write('tverts snapped to 1/'), write(TSnap), nl,
  clause(gotdata(File,Model,NodeName,NodeRef,tverts(T,U,V,W)),true,Eref),
  U1 is floor(TSnap*U+0.5)/TSnap, V1 is floor(TSnap*V+0.5)/TSnap,
  once((abs(U-U1)>6.0E-06 ; abs(V-V1)>6.0E-06 ; W=\=0)),
  /* retract(gotdata(File,Model,NodeName,NodeRef,tverts(T,U,V,W))), */
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,tverts(T,U1,V1,0))),
  increment_bug_count(File),
  fail.

check_for(unwelded_tverts,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,NodeName,NodeRef,tverts(NTVerts)),
  \+ clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  hash_tverts(File,Model,NodeName,NodeRef),
  has_unwelded_tverts(File,Model,NodeName,NodeRef),
  weld_tverts(File,Model,NodeName,NodeRef,NTVerts,0,0),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('tverts welded in '), write(NodeName), nl ; true),
  fail.

check_for(unwelded_tverts,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('tverts welded in '), write(N), write(' objects'), nl,
  fail.

check_for(unwelded_tverts,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('tverts welded in '), write(List), nl,
  fail.

check_for(crazy_tverts,File,SmallLogStream) :-
  retractall(fixlist(_)),
  gotdata(File,Model,NodeName,NodeRef,tverts(NTVerts)),
  once((
    gotdata(File,Model,NodeName,NodeRef,tverts(_,U,_,_)), \+((U>=0, U=<1));
    gotdata(File,Model,NodeName,NodeRef,tverts(_,_,V,_)), \+((V>=0, V=<1));
    gotdata(File,Model,NodeName,NodeRef,tverts(_,_,_,W)), \+((W>=0, W=<1))
      )),
  renormalize_tverts(File,Model,NodeName,NodeRef,NTVerts,NGroups),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  NGroups>=10,
  report_warning(['tverts of',NodeName,'are fragmented in',NGroups,'groups'],SmallLogStream),
  fail.

check_for(crazy_tverts,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('renormalized tverts in '), write(N), write(' objects'), nl,
  fail.

check_for(crazy_tverts,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('renormalized tverts in '), write(List), nl,
  fail.

check_for(silent_tvert_resnap,File,_) :-
  once(g_user_option(tvert_snap,TSnap)), integer(TSnap),
  clause(gotdata(File,Model,NodeName,NodeRef,tverts(T,U,V,W)),true,Eref),
  U1 is floor(TSnap*U+0.5)/TSnap, V1 is floor(TSnap*V+0.5)/TSnap,
  once((abs(U-U1)>6.0E-06 ; abs(V-V1)>6.0E-06 ; W=\=0)),
  /* retract(gotdata(File,Model,NodeName,NodeRef,tverts(T,U,V,W))), */
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,tverts(T,U1,V1,0))),
  increment_bug_count(File),
  fail.

check_for(silent_tverts_reweld,File,_) :-
  gotdata(File,Model,NodeName,NodeRef,tverts(NTVerts)),
  \+ clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  hash_tverts(File,Model,NodeName,NodeRef),
  has_unwelded_tverts(File,Model,NodeName,NodeRef),
  weld_tverts(File,Model,NodeName,NodeRef,NTVerts,0,0),
  (cm3_verbose -> tab(2), write('tverts re-welded in '), write(NodeName), nl ; true),
  fail.

/* ==================== */
/* Checks on walkmeshes */
/* ==================== */

check_for(change_aabb_material,File,_) :-
  once(g_user_option(map_aabb_material,MapAABB)), MapAABB=yes,
  once(g_user_option(map_aabb_from,From)),
  once(g_user_option(map_aabb_to,To)),
  To\=From,
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  once(gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,_,_,_,_,From))),
  tab(2), write('changing walkmesh material from '), write(From), write(' to '), write(To), write(' in '), write(NodeName), nl,
  retract(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,S,T1,T2,T3,From))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,S,T1,T2,T3,To))),
  increment_bug_count(File),
  fail.

check_for(zero_orientation_aabb_node,File,_) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  gotdata(File,Model,NodeName,NodeRef,orientation(_,_,_,A)), A=\=0,
  set_zero_orientation(File,Model,NodeName,NodeRef),
  tab(2), write('forced aabb node '), write(NodeName), write(' to null orientation'), nl,
  increment_bug_count(File),
  fail.  

check_for(zero_position_aabb_node,File,_) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z)),
  \+ ((X=:=0.0, Y=:=0.0, Z=:=0.0)),
  set_zero_position(File,Model,NodeName,NodeRef),
  tab(2), write('re-pivoted walkmesh to model origin'), nl,
  increment_bug_count(File),
  fail.  

/** Enable for Babylon & Q-compatible water tiles **/
/*
check_for(water_walkable_for_fishes,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,_,_,bitmap(tced1_waterwavy)),
  Zwater=0.5, Zdeep= -1,
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  clause(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,S,T1,T2,T3,17)),true,FRef),
  clause(gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),true,V1Ref),
  clause(gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),true,V2Ref),
  clause(gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,Z3)),true,V3Ref),
  Z1=<Zwater, Z2=<Zwater, Z3=<Zwater,
  /* erase(FRef), */
  /* asserta(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,S,T1,T2,T3,11))), */ /* Puddles */
  (Z1=:= Zdeep -> true ; erase(V1Ref), asserta(gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Zdeep)))),
  (Z2=:= Zdeep -> true ; erase(V2Ref), asserta(gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Zdeep)))),
  (Z3=:= Zdeep -> true ; erase(V3Ref), asserta(gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,Zdeep)))),
  tab(2), write('walkmesh face '), write(N), write(' adjusted to '), write(Zdeep), nl,
  fail.
*/

check_for(unwelded_aabb_vertices,File,_) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,verts(NVerts)),
  hash_verts(File,Model,NodeName,NodeRef),
  has_unwelded_verts(File,Model,NodeName,NodeRef),
  weld_vertices(File,Model,NodeName,NodeRef,NVerts,0,0),
  tab(2), write('vertices re-welded in aabb node '), write(NodeName), nl,
  fail.

check_for(aabb_null_faces,File,_) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)),
  once((gotdata(File,Model,NodeName,NodeRef,faces(_,V1,V2,V3,_,_,_,_,_)),(V1=V2; V2=V3; V3=V1))),
  delete_null_faces(File,Model,NodeName,NodeRef,NFaces,0,0),
  tab(2), write('null face(s) deleted from aabb node '), write(NodeName), nl,
  fail.

check_for(aabb_degenerate_faces,File,_) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  setof(F,is_degenerate_face(File,Model,NodeName,NodeRef,F),Flist),
  length(Flist,L0), L0>0,
  fix_degenerate_faces(File,Model,NodeName,NodeRef,Flist),
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)),
  delete_null_faces(File,Model,NodeName,NodeRef,NFaces,0,0),
  tab(2), write('repaired '), write(L0), write(' degenerate face(s) in aabb node '), write(NodeName), nl,
  fail.

check_for(aabb_torn_faces,File,_) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  setof(T,is_tear_in_mesh(File,Model,NodeName,NodeRef,T),Tlist),
  length(Tlist,L0), L0>0,
  fix_tears_in_mesh(File,Model,NodeName,NodeRef,Tlist),
  tab(2), write('stitched '), write(L0), write(' tear(s) in aabb node '), write(NodeName), nl,
  fail.

check_for(aabb_overlapping_faces,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  setof(Pair,is_overlapping_face_pair(File,Model,NodeName,NodeRef,Pair),PairList),
  length(PairList,L),
  report_warning([L,'overlapping faces in walkmesh',NodeName],SmallLogStream),
  fail.

check_for(aabb_fits_tile,File,SmallLogStream) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  w_facelist(File,Model,NodeName,NodeRef,FaceList),
  w_bounding_box(File,Model,NodeName,NodeRef,FaceList,[Xmin,Ymin,_,Xmax,Ymax,_]),
  \+ (abs(Xmin+5)=<0.025, abs(Ymin+5)=<0.025, abs(Xmax-5)=<0.025, abs(Ymax-5)=<0.025),
  report_warning(['aabb node',NodeName,'does not fit tile'],SmallLogStream),
  fail. 

check_for(aabb_fits_tile,File,_) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  w_facelist(File,Model,NodeName,NodeRef,FaceList),
  w_bounding_box(File,Model,NodeName,NodeRef,FaceList,[Xmin,Ymin,_,Xmax,Ymax,_]),
  abs(Xmin+5)=<0.025, abs(Ymin+5)=<0.025, abs(Xmax-5)=<0.025, abs(Ymax-5)=<0.025,
  retractall(fixlist(_)),
  snap_aabb_to_tile(File,Model,NodeName,NodeRef),
  predicate_property(fixlist(_),number_of_clauses(N)), N>0,
  tab(2), write(N), write(' edges of aabb node '), write(NodeName), write(' snapped to tile'), nl,
  fail. 

check_for(inverted_aabb_faces,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(fix_overhangs,FixOverhangs)), FixOverhangs=='yes',
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  setof(E,is_undercut_edge(File,Model,NodeName,NodeRef,E),Edgelist),
  length(Edgelist,L0), L0>0,
  fix_undercut_edges(File,Model,NodeName,NodeRef,Edgelist),
  tab(2), write('repaired '), write(L0), write(' aabb zigzags down tile edges '), nl,
  fail.

check_for(inverted_aabb_faces,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(fix_overhangs,FixOverhangs)), FixOverhangs\=='no',
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)), NFaces>0,
  prepare_face_normals(File,Model,NodeName,NodeRef),
  setof(F,is_undercut_face(File,Model,NodeName,NodeRef,F),Facelist),
  length(Facelist,L0), L0>0,
  fix_undercut_faces(File,Model,NodeName,NodeRef,Facelist,Residue),
  length(Residue,L1), L2 is L0-L1, L2>0,
  (cm3_verbose -> tab(2), write('repaired '),
   (L2<L0 -> write(L2), write(' of ') ; true),
    write(L0), write(' overhung faces in aabb node '), write(NodeName), nl ; true),
  fail.

check_for(inverted_aabb_faces,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(fix_overhangs,FixOverhangs)), FixOverhangs\=='no',
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)), NFaces>0,
  prepare_face_normals(File,Model,NodeName,NodeRef),
  setof(T,is_tuck(File,Model,NodeName,NodeRef,T),Tucklist),
  fix_tucks_in_list(File,Model,NodeName,NodeRef,Tucklist),
  length(Tucklist,L),
  tab(2), write('eliminated '), write(L), write(' tuck(s) in aabb node '), write(NodeName), nl,
  fail.

check_for(inverted_aabb_faces,File,_) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)), NFaces>0,
  prepare_face_normals(File,Model,NodeName,NodeRef),
  setof(F,is_inverted_duplicate(File,Model,NodeName,NodeRef,F),Facelist),
  Facelist\=[], length(Facelist,NDeleted),
  f_delete_faces_in_list(File,Model,NodeName,NodeRef,Facelist),
  tab(2), write('deleted '), write(NDeleted), write(' inverted duplicate face(s) from aabb node '), write(NodeName), nl,
  fail.

check_for(inverted_aabb_faces,File,SmallLogStream) :-
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)), NFaces>0,
  prepare_face_normals(File,Model,NodeName,NodeRef),
  /* once((face_normal(File,Model,NodeName,NodeRef,_,[_,_,Z]),Z < -0.000001)), */
  setof(F,Z^Y^X^(face_normal(File,Model,NodeName,NodeRef,F,[X,Y,Z]),Z < -0.000001),S),
  length(S,L), L>0,
  report_warning(['aabb node',NodeName,'has',L,'downward-pointing face(s)',S],SmallLogStream),
  member(Face,S),
  gotdata(File,Model,NodeName,NodeRef,faces(Face,V1,V2,V3,_,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,Z3)),
  tab(2), write('face '), write(Face), write(': '), write([[X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3]]), nl,
  fail.

check_for(failing_aabb_rebuild,File,SmallLogStream) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  \+ w_rebuild_aabb(File,Model,NodeName,NodeRef),
  report_error(['cannot rebuild aabb data for',NodeName],SmallLogStream),
  bad_error(File),
  fail.

/* ================================= */
/* Checks for slicing and tilefading */
/* ================================= */

check_for(remove_tilefade,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(slice,Slice)), Slice==undo,
  t_remove_all_tilefades(File,Model),
  fail.

check_for(remove_tilefade,File,_) :-
  gotdata(File,Model,classification('TILE')),
  edgetile(Model),
  once((gotdata(File,Model,_,_,tilefade(T)), T\=0)),
  t_remove_all_tilefades(File,Model),
  fail.

check_for(can_slice_and_fade,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(slice,Slice)), Slice==yes,
  gotdata(File,Model,newmodel(Model)),
  \+ edgetile(Model),
  \+ gotdata(File,Model,node(aabb,_)),
  retractall(slice_height(Model,_)),
  tab(2), write(Model), tab(1), write('has no walkmesh - cannot slice and fade'), nl,
  fail.

check_for(can_slice_and_fade,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(slice,Slice)), Slice==yes,
  gotdata(File,Model,newmodel(Model)),
  \+ edgetile(Model),
  once(g_user_option(slice_height,Zs)),
  clause(gotdata(File,Model,node(aabb,WalkmeshName)),true,WalkmeshRef),
  t_walkmesh_ref_height(File,Model,WalkmeshName,WalkmeshRef,Zw),
  (t_dynamic_ref_height(File,Model,Zdyn) -> Zslice is max(Zw+Zs,Zdyn) ; Zslice is Zw+Zs),
  retractall(slice_height(Model,_)),
  asserta(slice_height(Model,Zslice)),
  (cm3_verbose -> tab(2), write('slice height for '), write(Model), write(' is '), write(Zslice), nl ; true),
  fail.

check_for(fading_above_Z,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(slice,Slice)), Slice==yes,
  slice_height(Model,Zslice),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,parent(Adummy/ARef)),
  \+ superparent(File,Model,NodeName,NodeRef,Adummy,ARef),
  \+ gotdata(File,Model,NodeName,NodeRef,tilefade(1)),
  \+ gotdata(File,Model,NodeName,NodeRef,render(0)),
  t_abs_verts(File,Model,NodeName,NodeRef),
  once((abs_vertex(NodeRef,_,[_,_,Za]), Za>Zslice+0.025)),
  once((
        once(g_user_option(foliage,Foliage)),
        Foliage==tilefade,
        is_foliage(File,Model,NodeName,NodeRef)
        ;
        \+((abs_vertex(NodeRef,_,[_,_,Za1]), Za1<Zslice-0.025))
      )),
  retractall(gotdata(File,Model,NodeName,NodeRef,tilefade(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,tilefade(1))),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('trimesh node '), write(NodeName), write(' set to fade'), nl ; true),
  increment_bug_count(File),
  fail.

check_for(fading_above_Z,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('set '), write(N), write(' trimesh nodes to fade'), nl,
  fail.

check_for(fading_above_Z,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('set nodes '), write(List), write(' to fade'), nl,
  fail.

check_for(no_fading_below_Z,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(slice,Slice)), Slice==yes,
  slice_height(Model,Zslice),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,parent(Adummy/ARef)),
  \+ superparent(File,Model,NodeName,NodeRef,Adummy,ARef),
  gotdata(File,Model,NodeName,NodeRef,tilefade(T)), (T==1 ; T==4),
  \+ gotdata(File,Model,NodeName,NodeRef,render(0)),
  t_abs_verts(File,Model,NodeName,NodeRef),
  \+((abs_vertex(NodeRef,_,[_,_,Za]), Za>Zslice+0.025)),
  retractall(gotdata(File,Model,NodeName,NodeRef,tilefade(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,tilefade(0))),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('trimesh node '), write(NodeName), write(' set to no-fade'), nl ; true),
  increment_bug_count(File),
  fail.

check_for(no_fading_below_Z,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('set '), write(N), write(' trimesh nodes to no-fade'), nl,
  fail.

check_for(no_fading_below_Z,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('set nodes '), write(List), write(' to no-fade'), nl,
  fail.

check_for(slicing_at_Z,File,_) :-
  retractall(fixlist(_)),
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(slice,Slice)), Slice==yes,
  slice_height(Model,Zslice),
  atom_concat(Model,'a',Adummy),
  clause(gotdata(File,Model,node(dummy,Adummy)),true,ARef),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,parent(Adummy/ARef)),
  \+ superparent(File,Model,NodeName,NodeRef,Adummy,ARef),
  \+ gotdata(File,Model,NodeName,NodeRef,render(0)),
  once(g_user_option(foliage,Foliage)),
  \+((Foliage==tilefade, is_foliage(File,Model,NodeName,NodeRef))),
  t_abs_verts(File,Model,NodeName,NodeRef),
  once((abs_vertex(NodeRef,_,[_,_,Za]), Za>Zslice+0.025)),
  once((abs_vertex(NodeRef,_,[_,_,Zb]), Zb<Zslice-0.025)),
  t_slice_trimesh(File,Model,NodeName,NodeRef,Zslice,tilefade),
  ( \+ fixlist(NodeName) -> asserta(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('trimesh node '), write(NodeName), write(' sliced'), nl ; true),
  increment_bug_count(File),
  fail.

check_for(slicing_at_Z,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('sliced '), write(N), write(' trimesh nodes'), nl,
  fail.

check_for(slicing_at_Z,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('sliced nodes '), write(List), nl,
  fail.

check_for(black_bases,File,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(slice,Slice)), Slice==yes,
  slice_height(Model,Zslice),
  t_create_black_base(File,Model,Zslice,NodeName),
  tab(2), write('black base '), write(NodeName), write(' created at slice plane'), nl,
  fail.

/* ==================== */
/* Rosenkrantz Chamfers */
/* ==================== */

check_for(rosenkrantz_chamfers,File,_) :-
  once(g_user_option(chamfer,Chamfer)), Chamfer==delete,
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  has_chamfer(File,Model,NodeName,NodeRef),
  delete_chamfer(File,Model,NodeName,NodeRef),
  tab(2), write('Rosenkrantz Chamfer deleted from '), write(NodeName), nl,
  fail.

check_for(rosenkrantz_chamfers,File,_) :-
  once(g_user_option(chamfer,Chamfer)), Chamfer==add,
  is_ground(File,Model,NodeName,NodeRef),
  \+ ((gotdata(File,Model,NodeName,NodeRef,alpha(Alpha)), Alpha<1.0)),
  \+ ((gotdata(File,Model,NodeName,NodeRef,_,alpha(Alpha1)), Alpha1<1.0)),
  \+ ((gotdata(File,Model,NodeName,NodeRef,_,alphakey(_,_,Alpha2)), Alpha2<1.0)),
  add_chamfer(File,Model,NodeName,NodeRef),
  fail.

/* ================ */
/* Tile raise/lower */
/* ================ */

check_for(tile_raise,File,_) :-
  once(g_user_option(tile_raise,TileRaise)),
  TileRaise\=='no',
  once(g_user_option(tile_raise_amount,Height)),
  Height =\= 0.0,
  (TileRaise=='lower' -> Offset is -Height ; Offset is Height),
  gotdata(File,Model,newmodel(Model)),
  gotdata(File,Model,classification('TILE')),
  \+ check_failed(File),
  raise_tile(File,Model,Offset),
  fail.

/* ======================================= */
/* Special 3D masonry for Babylon Rooftops */
/* ======================================= */

check_for(special_3d_masonry,File,_) :-
  gotdata(File,Model,newmodel(Model)),
  gotdata(File,Model,classification('TILE')),
  atom_concat('tced4_',_,Model),
  make_3D(BaseMap,OverlayMap),
  gotdata(File,Model,BaseName,BaseRef,bitmap(BaseMap)),
  atom_concat(BaseName,'_2_',OverlayName),
  gotdata(File,Model,OverlayName,OverlayRef,bitmap(OverlayMap)),
  retract(gotdata(File,Model,BaseName,BaseRef,ambient(_,_,_))),
  asserta(gotdata(File,Model,BaseName,BaseRef,ambient(1,1,1))),
  retract(gotdata(File,Model,BaseName,BaseRef,diffuse(_,_,_))),
  asserta(gotdata(File,Model,BaseName,BaseRef,diffuse(1,1,1))),
  retractall(gotdata(File,Model,node(_,OverlayName))),
  retractall(gotdata(File,Model,OverlayName,OverlayRef,_)),
  retractall(gotdata(File,Model,OverlayName,OverlayRef,_,_)),
  tab(2), write('pulled existing 3D overlay '), write(OverlayName), nl,
  fail.

/*
check_for(special_3d_masonry,File,_) :-
  gotdata(File,Model,newmodel(Model)),
  gotdata(File,Model,classification('TILE')),
  atom_concat('tced4_',_,Model),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  \+ atom_concat(_,'_2_',NodeName),
  gotdata(File,Model,NodeName,NodeRef,bitmap(BaseMap)),
  make_3D(BaseMap,OverlayMap),
  gotdata(File,Model,NodeName,NodeRef,diffuse(R,G,B)), R=:=1, G=:=1, B=:=1,
  atom_concat(NodeName,'_2_',OverlayName),
  \+ gotdata(File,Model,node(_,OverlayName)),
  make_3D_backplane(File,Model,NodeName,NodeRef,OverlayName,BaseMap,OverlayMap),
  tab(2), write('created 3D overlay '), write(OverlayName), nl,
  fail.

check_for(special_3d_masonry,File,_) :-
  gotdata(File,Model,newmodel(Model)),
  gotdata(File,Model,classification('TILE')),
  atom_concat('tced4_',_,Model),
  make_3D(_,OverlayMap),
  once((
    clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
    atom_concat(_,'_2_',NodeName),
    gotdata(File,Model,NodeName,NodeRef,bitmap(OverlayMap))
      )),
  make_3D_fix_edges(File,Model,OverlayMap),
  tab(2), write('fixed edges of 3D overlays with bitmap '), write(OverlayMap), nl,
  fail.
*/

/* ============================== */
/* Checks on pivots and positions */
/* ============================== */

check_for(bad_pivots,_,_) :-
  gotdata(File,Model,classification('TILE')),
  once(g_user_option(repivot,Repivot)), Repivot\=none,
  once(g_user_option('pivots_below_z=0',AllowBelow)), AllowBelow=='slice',
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,shadow(0)),
  \+ gotdata(File,Model,NodeName,NodeRef,_,_),
  t_slice_trimesh(File,Model,NodeName,NodeRef,0.0,shadows),
  tab(2), write(NodeName), write(' sliced at Z=0'), nl,
  fail.
  
check_for(bad_pivots,File,SmallLogStream) :-
  once(g_user_option(repivot,Repivot)), Repivot\=none,
  once(g_user_option('pivots_below_z=0',AllowBelow)),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  (NodeType==trimesh ; NodeType=danglymesh),
  \+ gotdata(File,Model,NodeName,NodeRef,shadow(0)),
  \+ gotdata(File,Model,NodeName,NodeRef,_,_),
  /* once((gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,S,_,_,_,_)),S>0)), */
  prepare_face_normals(File,Model,NodeName,NodeRef),
  relate_to_tile(File,Model,NodeName,NodeRef,[0,0,0],[_,_,Z0]),
  once((Repivot==all ;
        face_normal(File,Model,NodeName,NodeRef,_,depth(D)), D<0 ;
        AllowBelow==disallow, Z0<0.0
      )),
  attempt_to_fix_pivots(File,Model,NodeName,NodeRef,SmallLogStream),
  fail.

check_for(far_out_positions,File,SmallLogStream) :-
  \+ check_failed(File),
  gotdata(File,Model,classification('TILE')),
  clause(gotdata(File,Model,node(Type,NodeName)),true,NodeRef), Type\=dummy,
  w_facelist(File,Model,NodeName,NodeRef,FaceList),
  w_mid_point_average(File,Model,NodeName,NodeRef,FaceList,Centroid),
  relate_to_tile(File,Model,NodeName,NodeRef,Centroid,[X,Y,_]),
  once((abs(X)>5; abs(Y)>5)),
  report_warning([NodeName,'is positioned off-tile at',[X,Y]],SmallLogStream),
  fail.

/* ============================================= */
/* GUI option in CM3.6.2b for rescaling by X,Y,Z */
/* ============================================= */

check_for(rescale_xyz,File,_) :-
  gotdata(File,Model,classification(C)), C\=='TILE',
  once(g_user_option(rescaleXYZ,R)), R=[Xs,Ys,Zs],
  clause(gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z)),true,ERef),
  X1 is X*Xs, Y1 is Y*Ys, Z1 is Z*Zs,
  erase(ERef),
  asserta(gotdata(File,Model,NodeName,NodeRef,position(X1,Y1,Z1))),
  fail.

check_for(rescale_xyz,File,_) :-
  gotdata(File,Model,classification(C)), C\=='TILE',
  once(g_user_option(rescaleXYZ,R)), R=[Xs,Ys,Zs],
  clause(gotdata(File,Model,NodeName,NodeRef,Animname,position(X,Y,Z)),true,ERef),
  X1 is X*Xs, Y1 is Y*Ys, Z1 is Z*Zs,
  erase(ERef),
  asserta(gotdata(File,Model,NodeName,NodeRef,Animname,position(X1,Y1,Z1))),
  fail.

check_for(rescale_xyz,File,_) :-
  gotdata(File,Model,classification(C)), C\=='TILE',
  once(g_user_option(rescaleXYZ,R)), R=[Xs,Ys,Zs],
  clause(gotdata(File,Model,NodeName,NodeRef,Animname,positionkey(N,T,X,Y,Z)),true,ERef),
  X1 is X*Xs, Y1 is Y*Ys, Z1 is Z*Zs,
  erase(ERef),
  asserta(gotdata(File,Model,NodeName,NodeRef,Animname,positionkey(N,T,X1,Y1,Z1))),
  fail.

check_for(rescale_xyz,File,_) :-
  gotdata(File,Model,classification(C)), C\=='TILE',
  once(g_user_option(rescaleXYZ,[Xs,Ys,Zs])),
  clause(gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,Z)),true,ERef),
  X1 is X*Xs, Y1 is Y*Ys, Z1 is Z*Zs,
  erase(ERef),
  asserta(gotdata(File,Model,NodeName,NodeRef,verts(V,X1,Y1,Z1))),
  fail.

check_for(rescale_xyz,File,_) :-
  gotdata(File,Model,classification(C)), C\=='TILE',
  once(g_user_option(rescaleXYZ,R)), R=[_,_,Zs],
  clause(gotdata(File,Model,setanimationscale(AnimationScale)),true,ERef),
  \+sub_atom(Model,5,5,4,'cloak'), \+sub_atom(Model,5,4,3,'robe'),
  A1 is AnimationScale*Zs,
  erase(ERef),
  asserta(gotdata(File,Model,setanimationscale(A1))),
  fail.

check_for(rescale_xyz,File,_) :-
  gotdata(File,Model,classification(C)), C\=='TILE',
  once(g_user_option(rescaleXYZ,R)), R=[Xs,Ys,Zs],
  gotdata(File,Model,newmodel(Model)),
  tab(2), write('rescaled '), write(Model), write(' by '), write([Xs,Ys,Zs]), nl,
  fail.

/* ================================ */
/* Special code for skinmesh bodies */
/* ================================ */

check_for(nodes_match_supermodel,File,_) :-
  once(secret(align_to_supermodel(Align))), Align==yes,
  gotdata(File,Model,setsupermodel(Model,SuperModel)),
  gotdata(SuperFile,SuperModel,newmodel(SuperModel)),
  gotdata(File,Model,NodeName,NodeRef,parent(Parent/_)),
  clause(gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z)),true,Eref),
  gotdata(SuperFile,SuperModel,NodeName,SuperNodeRef,parent(SuperParent/_)),
  once((SuperParent==Parent ; Parent==Model, SuperParent==SuperModel)),
  gotdata(SuperFile,SuperModel,NodeName,SuperNodeRef,position(Xs,Ys,Zs)),
  \+((snap_equal(X,Xs), snap_equal(Y,Ys), snap_equal(Z,Zs))),
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,position(Xs,Ys,Zs))),
  (cm3_verbose ->tab(2), write('aligned position of '), write(NodeName), write(' to supermodel'), nl ; true),
  fail.

check_for(correct_supermodel,File,_) :-
  \+ check_failed(File),
  gotdata(File,Model,classification('CHARACTER')),
  wildcard_match('p[mf][adegho]?_robe???',Model),
  gotdata(File,Model,setsupermodel(Model,Super)),
  wildcard_match('p[mf][adegho]?',Super),
  sub_atom(Model,0,4,8,Pheno),
  Super\==Pheno,
  retractall(gotdata(File,Model,setsupermodel(Model,_))),
  asserta(gotdata(File,Model,setsupermodel(Model,Pheno))),
  tab(2), write('Supermodel set to '), write(Pheno), nl,
  fail.

/* ============== */
/* Final Clean-up */
/* ============== */

/*
check_for(inheritcolor,File,_) :-
  retract(gotdata(File,Model,NodeName,NodeRef,inheritcolor(0))),
  asserta(gotdata(File,Model,NodeName,NodeRef,inheritcolor(1))),
  fail.
*/

/* check_for(shininess_and_specular,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,shininess(S)),true,Cref),
  S=\=1,
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,shininess(1))),
  tab(2), write('corrected shininess to 1 in '), write(NodeName), nl,
  fail.

check_for(shininess_and_specular,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,specular(U,V,W)),true,Cref),
  \+((U =:= 0.05, V =:= 0.05, W =:= 0.05)),
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,specular(0.05,0.05,0.05))),
  tab(2), write('corrected specular to 0.05 0.05 0.05 in '), write(NodeName), nl,
  fail.
*/

check_for(render_0_shadow_0,File,_) :-
  once(g_user_option(invisible_mesh_cull,InvisibleMeshCull)), InvisibleMeshCull\=='no',
  gotdata(File,Model,newmodel(Model)),
  \+gotdata(File,Model,newanim(_,Model)),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  NodeType\==aabb, /* fixed in CM3.5.1a */
  gotdata(File,Model,NodeName,NodeRef,faces(_)),
  gotdata(File,Model,NodeName,NodeRef,render(0)),
  gotdata(File,Model,NodeName,NodeRef,shadow(0)),
  reclassify_node(File,Model,NodeName,NodeRef,dummy,_),
  tab(2), write('invisible '), write(NodeType), tab(1), write(NodeName), write(' converted to dummy node'), nl,
  fail.

check_for(childless_dummies,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(dummy,NodeName)),true,NodeRef),
  NodeName\==Model,
  \+ gotdata(File,Model,_,_,parent(NodeName/NodeRef)),
  \+ gotdata(File,Model,anim(_),animroot(NodeName)),
  \+ (
       atom_concat(Model,Suffix,NodeName),
       (atom_concat('_D0',_,Suffix) ; atom_concat('_U0',_,Suffix))
     ),
  \+ ((gotdata(File,Model,_,_,weights(_,W)), member(NodeName,W))),
  retractall(gotdata(File,Model,node(dummy,NodeName))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_)),
  tab(2), write('unused dummy node '), write(NodeName), write(' deleted'), nl,
  increment_bug_count(File),
  fail.

check_for(childless_dummies,File,_) :-
  gotdata(File,Model,classification('TILE')),
  gotdata(File,Model,setsupermodel(Model,'NULL')),
  clause(gotdata(File,Model,node(dummy,NodeName)),true,NodeRef),
  NodeName\==Model,
  \+ gotdata(File,Model,_,_,parent(NodeName/NodeRef)),
  \+ gotdata(File,Model,anim(_),animroot(NodeName)),
  \+ (
       atom_concat(Model,Suffix,NodeName),
       (atom_concat('_D0',_,Suffix) ; atom_concat('_U0',_,Suffix))
     ),
  \+ ((gotdata(File,Model,_,_,weights(_,W)), member(NodeName,W))),
  retractall(gotdata(File,Model,node(dummy,NodeName))),
  retractall(gotdata(File,Model,NodeName,NodeRef,_)),
  tab(2), write('unused dummy node '), write(NodeName), write(' deleted'), nl,
  increment_bug_count(File),
  fail.

check_for(null_zero_orientations2,File,_) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  clause(gotdata(File,Model,NodeName,NodeRef,orientation(X,Y,Z,A)),true,Cref),
  A=:=0, \+ (X==0, Y==0, Z==0, A==0),
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,orientation(0,0,0,0))),
  fail.

check_for(null_zero_orientations2,File,_) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  clause(gotdata(File,Model,NodeName,NodeRef,orientation(X,Y,Z,A)),true,Cref),
  X=:=0, Y=:=0, Z=:=0, A=\=0,
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,orientation(0,0,0,0))),
  fail.

check_for(null_zero_orientations2,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,orientation(X,Y,Z,A)),true,Cref),
  A=:=0, \+ (X==0, Y==0, Z==0, A==0),
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,orientation(0,0,0,0))),
  fail.

check_for(null_zero_orientations2,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,orientation(X,Y,Z,A)),true,Cref),
  X=:=0, Y=:=0, Z=:=0, A=\=0,
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,orientation(0,0,0,0))),
  fail.

check_for(null_zero_orientations2,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N,T,X,Y,Z,A)),true,Cref),
  A=:=0, \+ (X==0, Y==0, Z==0, A==0),
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N,T,0,0,0,0))),
  fail.

check_for(null_zero_orientations2,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N,T,X,Y,Z,A)),true,Cref),
  X=:=0, Y=:=0, Z=:=0, A=\=0,
  erase(Cref),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N,T,0,0,0,0))),
  fail.

/* V342f: NWMax doesn't recognise alphakeys on skin nodes but it does recognise alpha */
/* So, convert single keys back to parameters and generate a warning for multiple keys */

check_for(alphakeys_on_skins,File,_) :-
  clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef),
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,alphakey(1)),true,ERef1),
  clause(gotdata(File,Model,NodeName,NodeRef,AnimName,alphakey(0,_,A)),true,ERef2),
  erase(ERef1), erase(ERef2),
  asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,alpha(A))),
  (cm3_verbose -> tab(2), write('alphakey converted to static alpha in animation '), write(AnimName), write(' for skin '), write(NodeName), nl ; true),
  fail.

/* Added in v3.6.2d */
check_for(repeated_bone_weights,File,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,weights(V,W)),true,Eref),
  compact_bone_weights(W,W1), W1\=W,
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,weights(V,W1))),
  fail.

check_for(incompatibility,File,SmallLogStream) :-
  gotdata(File,Model,newmodel(Model)),
  setof(Q,incompatible(nwnmdlcomp,Model,Q,error),QL),
  report_warning(['model',Model,'is not compatible with nwnmdlcomp',QL],SmallLogStream),
  fail.

check_for(incompatibility,File,SmallLogStream) :-
  gotdata(File,Model,newmodel(Model)),
  setof(Q,incompatible(engine,Model,Q,ignore),QL),
  report_warning(['model',Model,'contains data that will be ignored by the NWN engine',QL],SmallLogStream),
  fail.

check_for(incompatibility,File,SmallLogStream) :-
  gotdata(File,Model,newmodel(Model)),
  setof(Q,incompatible(nwmax08,Model,Q,ignore),QL),
  report_warning(['model',Model,'contains data that NWMax 0.8b cannot handle',QL],SmallLogStream),
  fail.

/* ==================================== */
/* Special Code for danglymesh envelope */
/* (Commented out unless needed)        */
/* ==================================== */

/*
check_for(danglymesh_envelope,File,_) :-
  clause(gotdata(File,Model,node(danglymesh,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,displacement(Displacement)),
  /* construct the envelope trimesh node  */
  atom_concat(NodeName,'_envelope',NewName),
  asserta(gotdata(File,Model,node(trimesh,NewName)),NewRef),
  (gotdata(File,Model,NodeName,NodeRef,Q), functor(Q,Q0,_), once(paramtype(trimesh,_,Q0)), asserta(gotdata(File,Model,NewName,NewRef,Q)), fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,Q), functor(Q,Q0,_), atom_concat(P,'key',Q0), controller_type(_,33,P), asserta(gotdata(File,Model,NewName,NewRef,AnimName,Q)), fail ; true),
  /* prepare face normals (existing code) */
  prepare_face_normals(File,Model,NewName,NewRef),
  /* for each vertex, total the face normals of the faces around that vertex */
  clause(gotdata(File,Model,NewName,NewRef,verts(V,X,Y,Z)),true,ERef),
  is_vertex_in_facelist(File,Model,NewName,NewRef,FaceList,V),
  face_normal_total(File,Model,NewName,NewRef,FaceList,Total),
  /* normalise it (existing vector predicate) */
  vector_normalise(Total,[Nx,Ny,Nz]),
  /* scale it by the displacement*constraint for that vertex, add 5mm and snap */
  /* shift the vertex that much in the envelope */
  gotdata(File,Model,NodeName,NodeRef,constraints(V,C1)),
  X1 is X + Nx*Displacement*C1/768.0 + 0.001*sign(Nx), snap(X1,Xs),
  Y1 is Y + Ny*Displacement*C1/768.0 + 0.001*sign(Ny), snap(Y1,Ys),
  Z1 is Z + Nz*Displacement*C1/768.0 + 0.001*sign(Nz), snap(Z1,Zs),
  erase(ERef),
  asserta(gotdata(File,Model,NewName,NewRef,verts(V,Xs,Ys,Zs))), 
  fail.
*/

check_for(_,File,_) :-
  bug_count(File,NBugs),
  tab(2), write('Fixes made = '), write(NBugs), nl.

/* ============= */
/* rename_node/5 */
/* ============= */

rename_node(File,Model,OldName,OldRef,NewName) :-
  clause(gotdata(File,Model,node(NodeType,OldName)),true,OldRef),
  asserta(gotdata(File,Model,node(NodeType,NewName)),NewRef),
  erase(OldRef),
  increment_bug_count(File),
  ( clause(gotdata(File,Model,OldName,OldRef,Q),true,Eref),
    erase(Eref),
    asserta(gotdata(File,Model,NewName,NewRef,Q)),
    increment_bug_count(File),
    fail ; true),
  ( clause(gotdata(File,Model,OldName,OldRef,AnimName,Q1),true,Eref1),
    erase(Eref1),
    asserta(gotdata(File,Model,NewName,NewRef,AnimName,Q1)),
    increment_bug_count(File),
    fail ; true),
  ( clause(gotdata(File,Model,ChildNode,ChildRef,parent(OldName/OldRef)),true,Eref2),
    erase(Eref2),
    asserta(gotdata(File,Model,ChildNode,ChildRef,parent(NewName/NewRef))),
    increment_bug_count(File),
    fail ; true),
  fail.

rename_node(_,_,_,_,_).

/* ================= */
/* reclassify_node/5 */
/* ================= */

reclassify_node(File,Model,NodeName,NodeRef,NewType,NewRef) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  asserta(gotdata(File,Model,node(NewType,NodeName)),NewRef),
  erase(NodeRef),
  increment_bug_count(File),
  (clause(gotdata(File,Model,NodeName,NodeRef,Q),true,Eref),
   erase(Eref),
   Q=..[Q0|_],
   paramtype(NewType,_,Q0), 
   asserta(gotdata(File,Model,NodeName,NewRef,Q)),
   increment_bug_count(File),
   fail ; true),
  /* Modified in Version 343e to copy children and valid animation keys also */
  (clause(gotdata(File,Model,Child,ChildRef,parent(NodeName/NodeRef)),true,Eref1),
   erase(Eref1),
   asserta(gotdata(File,Model,Child,ChildRef,parent(NodeName/NewRef))),
   increment_bug_count(File),
   fail ; true),
  node_type(N,NewType),
  (clause(gotdata(File,Model,NodeName,NodeRef,AnimName,Q1),true,Eref2),
   erase(Eref2),
   Q1=..[Key|_], atom_concat(P,'key',Key),
   controller_type(_,N,P),
   asserta(gotdata(File,Model,NodeName,NewRef,AnimName,Q1)),
   increment_bug_count(File),
   fail ; true),
  !.

reclassify_node(_,_,_,_,_,_).

/* ======== */
/* boolean/1 */
/* ======== */

boolean(shadow).
boolean(beaming).
boolean(rotatetexture).
boolean(inheritcolor).
boolean(danglymesh).
boolean(ambientonly).
boolean(isdynamic).
boolean(affectdynamic).
boolean(fadinglight).
boolean(spawntype).

/* emitter flags */

boolean(p2p).
boolean(p2p_sel).
boolean(affectedbywind).
boolean(m_istinted).
boolean(bounce).
boolean(random).
boolean(inherit).
boolean(inheritvel).
boolean(inherit_local).
boolean(splat).
boolean(inherit_part).

/* ============= */
/* superparent/6 */
/* ============= */

superparent(File,Model,NodeName,NodeRef,SuperParent,SuperParentRef) :-
  gotdata(File,Model,NodeName,NodeRef,parent(SuperParent/SuperParentRef)),
  clause(gotdata(File,Model,node(_,SuperParent)),true,SuperParentRef).

superparent(File,Model,NodeName,NodeRef,SuperParent,SuperParentRef) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent/ParentRef)),
  clause(gotdata(File,Model,node(_,Parent)),true,ParentRef),
  superparent(File,Model,Parent,ParentRef,SuperParent,SuperParentRef).

/* =========== */
/* bad_error/1 */
/* =========== */

bad_error(File) :- check_failed(File), !.
bad_error(File) :- asserta(check_failed(File)).

/* ================= */
/* raise_to_tile/4   */
/* raise_to_parent/4 */
/* ================= */

raise_to_tile(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Model/_)), !.

raise_to_tile(File,Model,NodeName,NodeRef) :-
  raise_to_parent(File,Model,NodeName,NodeRef),  
  raise_to_tile(File,Model,NodeName,NodeRef).

raise_to_parent(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent/PRef)), PRef\= -1,
  clause(gotdata(File,Model,node(_,Parent)),true,PRef),
  gotdata(File,Model,Parent,PRef,parent(Super/SuperRef)),
  once((gotdata(File,Model,Parent,PRef,position(Xp,Yp,Zp)) ; [Xp,Yp,Zp]=[0,0,0])),
  once((gotdata(File,Model,Parent,PRef,orientation(Up,Vp,Wp,Ap)) ; [Up,Vp,Wp,Ap]=[0,0,0,0])),
  once((gotdata(File,Model,NodeName,NodeRef,position(X0,Y0,Z0)) ; [X0,Y0,Z0]=[0,0,0])),
  once((gotdata(File,Model,NodeName,NodeRef,orientation(U0,V0,W0,A0)) ; [U0,V0,W0,A0]=[0,0,0,0])),
  f_rotate_vector([X0,Y0,Z0],[Up,Vp,Wp,Ap],[X1,Y1,Z1]),
  vector_add([X1,Y1,Z1],[Xp,Yp,Zp],[X2,Y2,Z2]),
  f_rotate_rotation([U0,V0,W0,A0],[Up,Vp,Wp,Ap],[U1,V1,W1,A1]),
  retract(gotdata(File,Model,NodeName,NodeRef,parent(Parent/PRef))),
  asserta(gotdata(File,Model,NodeName,NodeRef,parent(Super/SuperRef))),
  retractall(gotdata(File,Model,NodeName,NodeRef,position(_,_,_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,position(X2,Y2,Z2))),
  retractall(gotdata(File,Model,NodeName,NodeRef,orientation(_,_,_,_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,orientation(U1,V1,W1,A1))),
  increment_bug_count(File),
  !.

/* ========================= */
/* fix_out_of_range_verts/5 */
/* ========================= */

fix_out_of_range_verts(File,Model,NodeName,NodeRef,NVerts) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M)),true,Eref),
  V1>=NVerts,
  /* retract(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M))), */
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N,0,V2,V3,G,TV1,TV2,TV3,M))),
  increment_bug_count(File),
  fail.

fix_out_of_range_verts(File,Model,NodeName,NodeRef,NVerts) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M)),true,Eref),
  V2>=NVerts,
  /* retract(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M))), */
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,0,V3,G,TV1,TV2,TV3,M))),
  increment_bug_count(File),
  fail.

fix_out_of_range_verts(File,Model,NodeName,NodeRef,NVerts) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M)),true,Eref),
  V3>=NVerts,
  /* retract(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M))), */
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,0,G,TV1,TV2,TV3,M))),
  increment_bug_count(File),
  fail.

fix_out_of_range_verts(_,_,_,_,_) :- !.

/* ========================= */
/* fix_out_of_range_tverts/5 */
/* ========================= */

fix_out_of_range_tverts(File,Model,NodeName,NodeRef,NVerts) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M)),true,Eref),
  TV1>=NVerts,
  /* retract(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M))), */
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,0,TV2,TV3,M))),
  increment_bug_count(File),
  fail.

fix_out_of_range_tverts(File,Model,NodeName,NodeRef,NVerts) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M)),true,Eref),
  TV2>=NVerts,
  /* retract(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M))), */
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,0,TV3,M))),
  increment_bug_count(File),
  fail.

fix_out_of_range_tverts(File,Model,NodeName,NodeRef,NVerts) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M)),true,Eref),
  TV3>=NVerts,
  /* retract(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,TV3,M))), */
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,G,TV1,TV2,0,M))),
  increment_bug_count(File),
  fail.

fix_out_of_range_tverts(_,_,_,_,_) :- !.

/* ========================== */
/* Modified in CM362h         */
/* ========================== */
/* hash_verts/4               */
/* has_unwelded_verts/4       */
/* weld_vertices/7            */
/* renumber_verts_in_faces/4  */
/* ========================== */

hash_verts(File,Model,NodeName,NodeRef) :-
  retractall(hashlist(_,_,_)),
  retractall(renumbering(_,_)),
  clause(gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,Z)),true,Ref),
  once(( gotdata(File,Model,NodeName,NodeRef,weights(V,W)) ; W=[] )),
  once(( gotdata(File,Model,NodeName,NodeRef,constraints(V,C)) ; C=[] )),
  once(( gotdata(File,Model,NodeName,NodeRef,colors(V,R,G,B)) ; R=[], G=[], B=[] )),
  term_hash(foo(X,Y,Z,W,C,R,G,B),Hash),
  asserta(hashlist(Hash,V,Ref)),
  fail.

hash_verts(File,Model,NodeName,NodeRef) :- prepare_face_normals(File,Model,NodeName,NodeRef).

has_unwelded_verts(File,Model,NodeName,NodeRef) :-
  hashlist(Hash,V1,_),
  hashlist(Hash,V2,_),
  V1 \= V2,
  vertex_normal(File,Model,NodeName,NodeRef,V1,U1),
  vertex_normal(File,Model,NodeName,NodeRef,V2,U2),
  vector_dot_product(U1,U2,P), P > -0.9,
  !.

has_unwelded_verts(File,Model,NodeName,NodeRef) :-
  hashlist(_,V,_), \+vertex_normal(File,Model,NodeName,NodeRef,V,_),
  !.

weld_vertices(File,Model,NodeName,NodeRef,NVerts,NVerts,NewNVerts) :-
  /* Terminating clause when all nodes have been done */
  retract(gotdata(File,Model,NodeName,NodeRef,verts(NVerts))),
  asserta(gotdata(File,Model,NodeName,NodeRef,verts(NewNVerts))),
  /* if this node has constaints, weights or colors, change the number of them */
  (clause(gotdata(File,Model,NodeName,NodeRef,constraints(NVerts)),true,CRef) ->
   erase(CRef),
   asserta(gotdata(File,Model,NodeName,NodeRef,constraints(NewNVerts)))
   ;
   true),
  (clause(gotdata(File,Model,NodeName,NodeRef,weights(NVerts)),true,CRef1) ->
   erase(CRef1),
   asserta(gotdata(File,Model,NodeName,NodeRef,weights(NewNVerts)))
   ;
   true),
  (clause(gotdata(File,Model,NodeName,NodeRef,colors(NVerts)),true,CRef2) ->
   erase(CRef2),
   asserta(gotdata(File,Model,NodeName,NodeRef,colors(NewNVerts)))
   ;
   true),
  renumber_verts_in_faces(File,Model,NodeName,NodeRef),
  !.

weld_vertices(File,Model,NodeName,NodeRef,NVerts,ThisVert,NewNVerts) :-
  /* Discard this vert if it is not used in any faces and therefore has no vertex normal computed */
  \+vertex_normal(File,Model,NodeName,NodeRef,ThisVert,_),
  clause(gotdata(File,Model,NodeName,NodeRef,verts(ThisVert,_,_,_)),true,Eref),
  erase(Eref), retract(hashlist(_,ThisVert,Eref)),
  retractall(gotdata(File,Model,NodeName,NodeRef,constraints(ThisVert,_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,weights(ThisVert,_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,colors(ThisVert,_,_,_))),
  increment_bug_count(File),
  NextVert is ThisVert+1, !,
  weld_vertices(File,Model,NodeName,NodeRef,NVerts,NextVert,NewNVerts).


weld_vertices(File,Model,NodeName,NodeRef,NVerts,ThisVert,NewNVerts) :-
  /* Identify this vert with a matching lower-numbered one */
  hashlist(Hash,ThisVert,Eref), hashlist(Hash,ThatVert,_), ThatVert<ThisVert,
  clause(gotdata(File,Model,NodeName,NodeRef,verts(ThisVert,X,Y,Z)),true,Eref),
  gotdata(File,Model,NodeName,NodeRef,verts(ThatVert,X,Y,Z)),
  (gotdata(File,Model,NodeName,NodeRef,constraints(ThisVert,C)) ->
     gotdata(File,Model,NodeName,NodeRef,constraints(ThatVert,C)) ; true),
  (gotdata(File,Model,NodeName,NodeRef,weights(ThisVert,W)) ->
     gotdata(File,Model,NodeName,NodeRef,weights(ThatVert,W)) ; true),
  (gotdata(File,Model,NodeName,NodeRef,colors(ThisVert,R,G,B)) ->
     gotdata(File,Model,NodeName,NodeRef,colors(ThatVert,R,G,B)) ; true),
  (vertex_normal(File,Model,NodeName,NodeRef,ThisVert,NThis),
   vertex_normal(File,Model,NodeName,NodeRef,ThatVert,NThat),
   vector_dot_product(NThis,NThat,P) -> P > -0.9 ; true),
  erase(Eref), retract(hashlist(Hash,ThisVert,Eref)), assertz(renumbering(ThisVert,ThatVert)),
  retractall(gotdata(File,Model,NodeName,NodeRef,constraints(ThisVert,_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,weights(ThisVert,_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,colors(ThisVert,_,_,_))),
  retractall(vertex_normal(File,Model,NodeName,NodeRef,ThisVert,_)),
  increment_bug_count(File),
  NextVert is ThisVert+1, !,
  weld_vertices(File,Model,NodeName,NodeRef,NVerts,NextVert,NewNVerts).

weld_vertices(File,Model,NodeName,NodeRef,NVerts,ThisVert,NewNVerts) :-
  /* Keep this vert but no need to renumber it because all earlier verts were distinct */
  ThisVert = NewNVerts,
  assertz(renumbering(ThisVert,ThisVert)),
  NextVert is ThisVert+1, NextNewNVerts is NewNVerts+1, !,
  weld_vertices(File,Model,NodeName,NodeRef,NVerts,NextVert,NextNewNVerts). 

weld_vertices(File,Model,NodeName,NodeRef,NVerts,ThisVert,NewNVerts) :-
  /* Keep this vert but renumber it to pack down the sequence */
  assertz(renumbering(ThisVert,NewNVerts)),
  retract(hashlist(Hash,ThisVert,Eref)),
  clause(gotdata(File,Model,NodeName,NodeRef,verts(ThisVert,X,Y,Z)),true,Eref),
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,verts(NewNVerts,X,Y,Z)),NewRef),
  asserta(hashlist(Hash,NewNVerts,NewRef)),
  (clause(gotdata(File,Model,NodeName,NodeRef,constraints(ThisVert,C)),true,CRef) ->
   erase(CRef), asserta(gotdata(File,Model,NodeName,NodeRef,constraints(NewNVerts,C)))
   ; true),
  (clause(gotdata(File,Model,NodeName,NodeRef,weights(ThisVert,W)),true,CRef1) ->
   erase(CRef1), asserta(gotdata(File,Model,NodeName,NodeRef,weights(NewNVerts,W)))
   ; true),
  (clause(gotdata(File,Model,NodeName,NodeRef,colors(ThisVert,R,G,B)),true,CRef2) ->
   erase(CRef2), asserta(gotdata(File,Model,NodeName,NodeRef,colors(NewNVerts,R,G,B)))
   ; true),
  (retract(vertex_normal(File,Model,NodeName,NodeRef,ThisVert,U)) ->
   asserta(vertex_normal(File,Model,NodeName,NodeRef,NewNVerts,U))
   ; true),
  increment_bug_count(File),
  NextVert is ThisVert+1, NextNewNVerts is NewNVerts+1, !,
  weld_vertices(File,Model,NodeName,NodeRef,NVerts,NextVert,NextNewNVerts). 

weld_vertices(_,_,NodeName,_,NVerts,ThisVert,NewNVerts) :-
  tab(2), write('!!! CM3 Internal Error: '), write(weld_vertices(NodeName,NVerts,ThisVert,NewNVerts)), write(' failed unexpectedly'), nl,
  break.

renumber_verts_in_faces(File,Model,NodeName,NodeRef) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,G,T1,T2,T3,M)),true,Eref),
  renumbering(V1,V1New), renumbering(V2,V2New), renumbering(V3,V3New),
  [V1,V2,V3] \= [V1New,V2New,V3New], 
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V1New,V2New,V3New,G,T1,T2,T3,M))),
  increment_bug_count(File),
  fail.

renumber_verts_in_faces(_,_,_,_).


/* ========================== */
/* Modified in CM362h         */
/* ========================== */
/* hash_tverts/4              */
/* has_unwelded_tverts/4      */
/* weld_tverts/7              */
/* renumber_tverts_in_faces/4 */
/* ========================== */

hash_tverts(File,Model,NodeName,NodeRef) :-
  retractall(hashlist(_,_,_)),
  retractall(renumbering(_,_)),
  clause(gotdata(File,Model,NodeName,NodeRef,tverts(T,U,V,_)),true,Ref),
  U1 is floor(U*32768), V1 is floor(V*32768), term_hash(foo(U1,V1),Hash),
  assertz(hashlist(Hash,T,Ref)),
  fail ; true.


has_unwelded_tverts(_,_,_,_) :-
  hashlist(Hash,T1,_),
  hashlist(Hash,T2,_),
  T1 \= T2,
  !.

weld_tverts(File,Model,NodeName,NodeRef,NTVerts,NTVerts,NewNTVerts) :-
  retract(gotdata(File,Model,NodeName,NodeRef,tverts(NTVerts))),
  asserta(gotdata(File,Model,NodeName,NodeRef,tverts(NewNTVerts))),
  renumber_tverts_in_faces(File,Model,NodeName,NodeRef),
  !.

weld_tverts(File,Model,NodeName,NodeRef,NTVerts,ThisTVert,NewNTVerts) :-
  hashlist(Hash,ThisTVert,Eref), hashlist(Hash,ThatTVert,_), ThatTVert<ThisTVert,
  erase(Eref), retract(hashlist(Hash,ThisTVert,Eref)), assertz(renumbering(ThisTVert,ThatTVert)),
  increment_bug_count(File),
  NextTVert is ThisTVert+1, !,
  weld_tverts(File,Model,NodeName,NodeRef,NTVerts,NextTVert,NewNTVerts).  

weld_tverts(File,Model,NodeName,NodeRef,NTVerts,ThisTVert,NewNTVerts) :-
  ThisTVert = NewNTVerts,
  assertz(renumbering(ThisTVert,ThisTVert)),
  NextTVert is ThisTVert+1, NextNewNTVerts is NewNTVerts+1, !,
  weld_tverts(File,Model,NodeName,NodeRef,NTVerts,NextTVert,NextNewNTVerts). 

weld_tverts(File,Model,NodeName,NodeRef,NTVerts,ThisTVert,NewNTVerts) :-
  hashlist(Hash,ThisTVert,Eref),
  clause(gotdata(File,Model,NodeName,NodeRef,tverts(ThisTVert,U,V,W)),true,Eref),
  erase(Eref), retract(hashlist(Hash,ThisTVert,Eref)),
  asserta(gotdata(File,Model,NodeName,NodeRef,tverts(NewNTVerts,U,V,W)),NewRef),
  asserta(hashlist(Hash,NewNTVerts,NewRef)),
  assertz(renumbering(ThisTVert,NewNTVerts)),
  increment_bug_count(File),
  NextTVert is ThisTVert+1, NextNewNTVerts is NewNTVerts+1, !,
  weld_tverts(File,Model,NodeName,NodeRef,NTVerts,NextTVert,NextNewNTVerts). 

renumber_tverts_in_faces(File,Model,NodeName,NodeRef) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,G,T1,T2,T3,M)),true,Eref),
  renumbering(T1,T1New), renumbering(T2,T2New), renumbering(T3,T3New),
  [T1,T2,T3] \= [T1New,T2New,T3New], 
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,G,T1New,T2New,T3New,M))),
  increment_bug_count(File),
  fail.

renumber_tverts_in_faces(_,_,_,_).

/* =================== */
/* delete_null_faces/7 */
/* =================== */

delete_null_faces(File,Model,NodeName,NodeRef,NFaces,NFaces,NewNFaces) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))), !.

delete_null_faces(File,Model,NodeName,NodeRef,NFaces,ThisFace,NewNFaces) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(ThisFace,V1,V2,V3,_,_,_,_,_)),true,Eref),
  once((V1=V2; V2=V3; V3=V1)),
  erase(Eref),
  increment_bug_count(File),
  NextFace is ThisFace+1, !,
  delete_null_faces(File,Model,NodeName,NodeRef,NFaces,NextFace,NewNFaces).

delete_null_faces(File,Model,NodeName,NodeRef,NFaces,ThisFace,NewNFaces) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(ThisFace,V1,V2,V3,G,T1,T2,T3,M)),true,Eref),
  V1\=V2, V2\=V3, V3\=V1,
  (ThisFace\=NewNFaces ->
    erase(Eref),
    asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces,V1,V2,V3,G,T1,T2,T3,M))),
    increment_bug_count(File);
    true),
  NextFace is ThisFace+1, NextNewNFaces is NewNFaces+1, !,
  delete_null_faces(File,Model,NodeName,NodeRef,NFaces,NextFace,NextNewNFaces).

/* ======================= */
/* delete_inverted_faces/7 */
/* ======================= */

delete_inverted_faces(File,Model,NodeName,NodeRef,NFaces,NFaces,NewNFaces) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))), !.

delete_inverted_faces(File,Model,NodeName,NodeRef,NFaces,ThisFace,NewNFaces) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(ThisFace,_,_,_,_,_,_,_,_)),true,Eref),
  face_normal(File,Model,NodeName,NodeRef,ThisFace,[_,_,Z]), Z < -0.7071,
  erase(Eref),
  increment_bug_count(File),
  NextFace is ThisFace+1, !,
  delete_inverted_faces(File,Model,NodeName,NodeRef,NFaces,NextFace,NewNFaces).

delete_inverted_faces(File,Model,NodeName,NodeRef,NFaces,ThisFace,NewNFaces) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(ThisFace,V1,V2,V3,G,T1,T2,T3,M)),true,Eref),
  face_normal(File,Model,NodeName,NodeRef,ThisFace,[_,_,Z]), Z >= -0.7071,
  (ThisFace\=NewNFaces ->
    erase(Eref),
    asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces,V1,V2,V3,G,T1,T2,T3,M))),
    increment_bug_count(File);
    true),
  NextFace is ThisFace+1, NextNewNFaces is NewNFaces+1, !,
  delete_inverted_faces(File,Model,NodeName,NodeRef,NFaces,NextFace,NextNewNFaces).

/* ======================== */
/* delete_duplicate_faces/7 */
/* ======================== */

delete_duplicate_faces(File,Model,NodeName,NodeRef,NFaces,NFaces,NewNFaces) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))), !.

delete_duplicate_faces(File,Model,NodeName,NodeRef,NFaces,ThisFace,NewNFaces) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(ThisFace,V1,V2,V3,_,_,_,_,_)),true,Eref),
  same_face(V1,V2,V3,Vx1,Vx2,Vx3),
  gotdata(File,Model,NodeName,NodeRef,faces(ThatFace,Vx1,Vx2,Vx3,_,_,_,_,_)),
  ThatFace<ThisFace,
  erase(Eref),
  increment_bug_count(File),
  NextFace is ThisFace+1, !,
  delete_duplicate_faces(File,Model,NodeName,NodeRef,NFaces,NextFace,NewNFaces).

delete_duplicate_faces(File,Model,NodeName,NodeRef,NFaces,ThisFace,NewNFaces) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(ThisFace,V1,V2,V3,G,T1,T2,T3,M)),true,Eref),
  (ThisFace\=NewNFaces ->
    erase(Eref),
    asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces,V1,V2,V3,G,T1,T2,T3,M))),
    increment_bug_count(File);
    true),
  NextFace is ThisFace+1, NextNewNFaces is NewNFaces+1, !,
  delete_duplicate_faces(File,Model,NodeName,NodeRef,NFaces,NextFace,NextNewNFaces).

/* ======================== */
/* renormalize_tverts/6     */
/* assign_tvert_groups/8    */
/* assign_tvert_to_group/6  */
/* ======================== */

renormalize_tverts(File,Model,NodeName,NodeRef,NTVerts,NGroups):-
  retractall(tvert_group(File,Model,NodeName,NodeRef,_,_)),
  assign_tvert_groups(File,Model,NodeName,NodeRef,NTVerts,0,NGroups,TGroups),
  tvert_group_deltas(File,Model,NodeName,NodeRef,TGroups,0,NGroups,Deltas),
  offset_tverts_by_group(File,Model,NodeName,NodeRef,NTVerts,TGroups,Deltas).

assign_tvert_groups(File,Model,NodeName,NodeRef,NTVerts,G,NGroups,TGroups) :-
  LastTVert is NTVerts-1,
  between(0,LastTVert,T),
  \+ tvert_group(File,Model,NodeName,NodeRef,T,_), !,
  assign_tvert_to_group(File,Model,NodeName,NodeRef,T,G),
  G1 is G+1,
  assign_tvert_groups(File,Model,NodeName,NodeRef,NTVerts,G1,NGroups,TGroups).

assign_tvert_groups(File,Model,NodeName,NodeRef,_,NGroups,NGroups,TGroups) :-
  setof([T,G],tvert_group(File,Model,NodeName,NodeRef,T,G),B),
  bagof(G1,T1^member([T1,G1],B),TGroups).

assign_tvert_to_group(File,Model,NodeName,NodeRef,T,G) :-
  tvert_group(File,Model,NodeName,NodeRef,T,G), !.

assign_tvert_to_group(File,Model,NodeName,NodeRef,T,G) :-
  asserta(tvert_group(File,Model,NodeName,NodeRef,T,G)),
  (gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,_,T,T1,T2,_));
   gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,_,T1,T,T2,_));
   gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,_,T1,T2,T,_))),
  assign_tvert_to_group(File,Model,NodeName,NodeRef,T1,G),
  assign_tvert_to_group(File,Model,NodeName,NodeRef,T2,G),
  fail ; true.
  
/* ======================== */
/* tvert_group_deltas/8     */
/* offset_tverts_by_group/7 */
/* bag_range/7              */
/* ======================== */

tvert_group_deltas(_,_,_,_,_,G,NGroups,[]) :- G>=NGroups, !.

/* Changed in V3.4.4d */
/*
tvert_group_deltas(File,Model,NodeName,NodeRef,TGroups,G,NGroups,[[DeltaU,DeltaV,DeltaW]|Deltas]) :- 
  gotdata(File,Model,NodeName,NodeRef,tverts(Tu,U,_,_)), nth0(Tu,TGroups,G),
  \+ (gotdata(File,Model,NodeName,NodeRef,tverts(T1,U1,_,_)), nth0(T1,TGroups,G), U1<U),
  DeltaU is -floor(U),
  gotdata(File,Model,NodeName,NodeRef,tverts(Tv,_,V,_)), nth0(Tv,TGroups,G),
  \+ (gotdata(File,Model,NodeName,NodeRef,tverts(T1,_,V1,_)), nth0(T1,TGroups,G), V1<V),
  DeltaV is -floor(V),
  gotdata(File,Model,NodeName,NodeRef,tverts(Tw,_,_,W)), nth0(Tw,TGroups,G),
  \+ (gotdata(File,Model,NodeName,NodeRef,tverts(T1,_,_,W1)), nth0(T1,TGroups,G), W1<W),
  DeltaW is -floor(W),
  !,
  G1 is G+1,
  tvert_group_deltas(File,Model,NodeName,NodeRef,TGroups,G1,NGroups,Deltas).
*/

tvert_group_deltas(File,Model,NodeName,NodeRef,TGroups,G,NGroups,[[DeltaU,DeltaV,DeltaW]|Deltas]) :- 
  bagof([Tu,U,V,W],(gotdata(File,Model,NodeName,NodeRef,tverts(Tu,U,V,W)),nth0(Tu,TGroups,G)),Bag),
  bag_range(Bag,Umin,Umax,Vmin,Vmax,Wmin,Wmax),
  DeltaU is -floor((Umin+Umax)/2),
  DeltaV is -floor((Vmin+Vmax)/2),
  DeltaW is -floor((Wmin+Wmax)/2),
  !,
  G1 is G+1,
  tvert_group_deltas(File,Model,NodeName,NodeRef,TGroups,G1,NGroups,Deltas).

offset_tverts_by_group(File,Model,NodeName,NodeRef,NTVerts,TGroups,Deltas) :-
  LastTVert is NTVerts-1,
  between(0,LastTVert,T),
  nth0(T,TGroups,G),
  nth0(G,Deltas,[DeltaU,DeltaV,DeltaW]),
  once((abs(DeltaU)>6.0E-06 ; abs(DeltaV)>6.0E-06 ; abs(DeltaW)>6.0E-06)),
  once(clause(gotdata(File,Model,NodeName,NodeRef,tverts(T,U,V,W)),true,CRef)),
  U1 is U+DeltaU, V1 is V+DeltaV, W1 is W+DeltaW,
  erase(CRef),
  asserta(gotdata(File,Model,NodeName,NodeRef,tverts(T,U1,V1,W1))),
  increment_bug_count(File),
  fail ; true.

bag_range([[_,U,V,W]],U,U,V,V,W,W) :- !.
bag_range([[_,U,V,W]|X0],Umin,Umax,Vmin,Vmax,Wmin,Wmax) :-
  bag_range(X0,Umin0,Umax0,Vmin0,Vmax0,Wmin0,Wmax0),
  (U>Umax0 -> Umax=U; Umax=Umax0), (U<Umin0 -> Umin=U; Umin=Umin0),
  (V>Vmax0 -> Vmax=V; Vmax=Vmax0), (V<Vmin0 -> Vmin=V; Vmin=Vmin0),
  (W>Wmax0 -> Wmax=W; Wmax=Wmax0), (W<Wmin0 -> Wmin=W; Wmin=Wmin0).

/* ============= */
/* tvert_mins/12 */
/* ============= */

tvert_mins(_,_,_,_,NTVerts,NTVerts,U0,V0,W0,U0,V0,W0) :- !.

tvert_mins(File,Model,NodeName,NodeRef,N,NTVerts,U0,V0,W0,Umin,Vmin,Wmin) :-
  gotdata(File,Model,NodeName,NodeRef,tverts(N,Un,Vn,Wn)),
  U1 is min(U0,Un), V1 is min(V0,Vn), W1 is min(W0,Wn), N1 is N+1, !,
  tvert_mins(File,Model,NodeName,NodeRef,N1,NTVerts,U1,V1,W1,Umin,Vmin,Wmin).  

/* =============== */
/* offset_tverts/9 */
/* =============== */

offset_tverts(_,_,_,_,NTVerts,NTVerts,_,_,_) :- !.
  
offset_tverts(File,Model,NodeName,NodeRef,N,NTVerts,DeltaU,DeltaV,DeltaW) :-
  clause(gotdata(File,Model,NodeName,NodeRef,tverts(N,Un,Vn,Wn)),true,Eref),
  erase(Eref),
  U1 is Un+DeltaU, V1 is Vn+DeltaV, W1 is Wn+DeltaW, N1 is N+1,
  asserta(gotdata(File,Model,NodeName,NodeRef,tverts(N,U1,V1,W1))), !,
  increment_bug_count(File),
  offset_tverts(File,Model,NodeName,NodeRef,N1,NTVerts,DeltaU,DeltaV,DeltaW).

/* ====================== */
/* prepare_face_normals/4 */
/* ====================== */

prepare_face_normals(File,Model,NodeName,NodeRef) :-
  retractall(face_normal(File,Model,NodeName,NodeRef,_,_)),
  retractall(vertex_normal_part(File,Model,NodeName,NodeRef,_,_)),
  retractall(vertex_normal(File,Model,NodeName,NodeRef,_,_)),
  fail.

prepare_face_normals(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,_,_,_,_,_)),
  prepare_face_normal(File,Model,NodeName,NodeRef,N,V1,V2,V3),
  fail.

prepare_face_normals(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,verts(NVerts)),
  LastVert is NVerts-1,
  between(0,LastVert,V),
  setof(U,vertex_normal_part(File,Model,NodeName,NodeRef,V,U),S),
  vector_total(S,U1), vector_normalise(U1,Un),
  assertz(vertex_normal(File,Model,NodeName,NodeRef,V,Un)),
  fail.

prepare_face_normals(_,_,_,_).

prepare_face_normal(File,Model,NodeName,NodeRef,N,V1,V2,V3) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,Z3)),
  vector_subtract([X2,Y2,Z2],[X1,Y1,Z1],V12),
  vector_subtract([X3,Y3,Z3],[X1,Y1,Z1],V13),
  vector_cross_product(V12,V13,V123),
  vector_normalise(V123,U),
  asserta(face_normal(File,Model,NodeName,NodeRef,N,U)),
  asserta(vertex_normal_part(File,Model,NodeName,NodeRef,V1,U)),
  asserta(vertex_normal_part(File,Model,NodeName,NodeRef,V2,U)),
  asserta(vertex_normal_part(File,Model,NodeName,NodeRef,V3,U)),
  X0 is (X1+X2+X3)/3, Y0 is (Y1+Y2+Y3)/3, Z0 is (Z1+Z2+Z3)/3,
  vector_dot_product([X0,Y0,Z0],U,A),
  asserta(face_normal(File,Model,NodeName,NodeRef,N,depth(A))),
  !.

/* ======================= */
/* is_inverted_duplicate/5 */
/* is_undercut_face/5      */
/* is_undercut/1           */
/* ======================= */

is_inverted_duplicate(File,Model,NodeName,NodeRef,F) :-
  face_normal(File,Model,NodeName,NodeRef,F,[_,_,Z]), Z < -0.000001,
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,_,_,_,_)),
  (Vn1=V1, Vn2=V3, Vn3=V2 ; Vn1=V3, Vn2=V2, Vn3=V1 ; Vn1=V2, Vn2=V1, Vn3=V3),
  gotdata(File,Model,NodeName,NodeRef,faces(Fn,Vn1,Vn2,Vn3,_,_,_,_,_)),
  face_normal(File,Model,NodeName,NodeRef,Fn,[_,_,Zn]), Zn>0.

is_undercut_face(File,Model,NodeName,NodeRef,F) :-
  face_normal(File,Model,NodeName,NodeRef,F,[_,_,Z]),
  is_undercut(Z) .

is_undercut(Z) :- Z < -0.000001, Z > -0.7.

/* ======================= */
/* is_undercut_edge/5      */
/* edge_line/8             */
/* start_vertex/8          */
/* continuation/9          */
/* clockwise_vertex/8      */
/* anti-clockwise_vertex/8 */
/* monotonic/3             */
/* fix_undercut_edges/5    */
/* fix_zigzag/5            */
/* adjust_abscissae/9      */
/* half_way/3              */
/* ======================= */

is_undercut_edge(File,Model,NodeName,NodeRef,[Vertlist,Abscissae,XorY,Order]) :-
  edge_line(File,Model,NodeName,NodeRef,Vertlist,Abscissae,XorY,Order),
  length(Vertlist,L), L>=2,
  \+ monotonic(Abscissae,Order,Abscissae).

edge_line(File,Model,NodeName,NodeRef,[V|V0],[A|A0],XorY,Order) :-
  start_vertex(File,Model,NodeName,NodeRef,XorY,Order,V,A),
  continuation(File,Model,NodeName,NodeRef,XorY,Order,V,V0,A0).

start_vertex(File,Model,NodeName,NodeRef,x,ascending,V,X) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,_)), Y =:= -5,
  \+ (anti_clockwise_vertex(File,Model,NodeName,NodeRef,V,_,_,Y1), Y1 =:= -5).

start_vertex(File,Model,NodeName,NodeRef,y,ascending,V,Y) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,_)), X =:= 5,
  \+ (anti_clockwise_vertex(File,Model,NodeName,NodeRef,V,_,X1,_), X1 =:= 5).

start_vertex(File,Model,NodeName,NodeRef,x,descending,V,X) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,_)), Y =:= 5,
  \+ (anti_clockwise_vertex(File,Model,NodeName,NodeRef,V,_,_,Y1), Y1 =:= 5).

start_vertex(File,Model,NodeName,NodeRef,y,descending,V,Y) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,_)), X =:= -5,
  \+ (anti_clockwise_vertex(File,Model,NodeName,NodeRef,V,_,X1,_), X1 =:= -5).

continuation(File,Model,NodeName,NodeRef,x,ascending,V,[V1|V0],[X1|X0]) :-
  clockwise_vertex(File,Model,NodeName,NodeRef,V,V1,X1,Y1,_,_,Y2), Y1 =:= -5, Y2 =\= -5,
  \+anti_clockwise_vertex(File,Model,NodeName,NodeRef,V,V1,X1,Y1), !,
  continuation(File,Model,NodeName,NodeRef,x,ascending,V1,V0,X0).

continuation(File,Model,NodeName,NodeRef,y,ascending,V,[V1|V0],[Y1|Y0]) :-
  clockwise_vertex(File,Model,NodeName,NodeRef,V,V1,X1,Y1,_,X2,_), X1 =:=  5, X2 =\=  5,
  \+anti_clockwise_vertex(File,Model,NodeName,NodeRef,V,V1,X1,Y1), !,
  continuation(File,Model,NodeName,NodeRef,y,ascending,V1,V0,Y0).

continuation(File,Model,NodeName,NodeRef,x,descending,V,[V1|V0],[X1|X0]) :-
  clockwise_vertex(File,Model,NodeName,NodeRef,V,V1,X1,Y1,_,_,Y2), Y1 =:=  5, Y2 =\=  5,
  \+anti_clockwise_vertex(File,Model,NodeName,NodeRef,V,V1,X1,Y1), !,
  continuation(File,Model,NodeName,NodeRef,x,descending,V1,V0,X0).

continuation(File,Model,NodeName,NodeRef,y,descending,V,[V1|V0],[Y1|Y0]) :-
  clockwise_vertex(File,Model,NodeName,NodeRef,V,V1,X1,Y1,_,X2,_), X1 =:= -5, X2 =\= -5,
  \+anti_clockwise_vertex(File,Model,NodeName,NodeRef,V,V1,X1,Y1), !,
  continuation(File,Model,NodeName,NodeRef,y,descending,V1,V0,Y0).

continuation(_,_,_,_,_,_,_,[],[]).

clockwise_vertex(File,Model,NodeName,NodeRef,V,V1,X1,Y1,V2,X2,Y2):-
  (gotdata(File,Model,NodeName,NodeRef,faces(_,V,V1,V2,_,_,_,_,_)) ;
   gotdata(File,Model,NodeName,NodeRef,faces(_,V2,V,V1,_,_,_,_,_)) ;
   gotdata(File,Model,NodeName,NodeRef,faces(_,V1,V2,V,_,_,_,_,_))),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,_)).

anti_clockwise_vertex(File,Model,NodeName,NodeRef,V,V1,X1,Y1):-
  (gotdata(File,Model,NodeName,NodeRef,faces(_,V1,V,_,_,_,_,_,_)) ;
   gotdata(File,Model,NodeName,NodeRef,faces(_,_,V1,V,_,_,_,_,_)) ;
   gotdata(File,Model,NodeName,NodeRef,faces(_,V,_,V1,_,_,_,_,_))),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,_)).

monotonic(Abscissae,ascending,Sorted)  :- msort(Abscissae,Sorted), !.
monotonic(Abscissae,descending,Sorted) :- reverse(Abscissae,Revlist), msort(Revlist,RevSorted), reverse(RevSorted,Sorted), !.

fix_undercut_edges(_,_,_,_,[]) :- !.

fix_undercut_edges(File,Model,NodeName,NodeRef,[E|E0]) :-
  fix_zigzag(File,Model,NodeName,NodeRef,E), !,
  fix_undercut_edges(File,Model,NodeName,NodeRef,E0).

fix_zigzag(_,_,_,_,[_,Abscissae,_,Order]) :- 
  monotonic(Abscissae,Order,Abscissae), !.

fix_zigzag(File,Model,NodeName,NodeRef,[Vertlist,Abscissae,XorY,Order]) :-
  monotonic(Abscissae,Order,Sorted),
  adjust_abscissae(File,Model,NodeName,NodeRef,Vertlist,Abscissae,XorY,Sorted,NewAbscissae), !,
  fix_zigzag(File,Model,NodeName,NodeRef,[Vertlist,NewAbscissae,XorY,Order]).

adjust_abscissae(_,_,_,_,[],[],_,[],[]) :- !.

adjust_abscissae(File,Model,NodeName,NodeRef,[_|V0],[Same|A0],XorY,[Same|S0],[Same|N0]) :-
  !,
  adjust_abscissae(File,Model,NodeName,NodeRef,V0,A0,XorY,S0,N0).
  
adjust_abscissae(File,Model,NodeName,NodeRef,[V|V0],[A|A0],x,[S|S0],[N|N0]) :-
  half_way(A,S,N),
  clause(gotdata(File,Model,NodeName,NodeRef,verts(V,A,Y,Z)),true,Eref),
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,verts(V,N,Y,Z))),
  increment_bug_count(File),
  !,
  adjust_abscissae(File,Model,NodeName,NodeRef,V0,A0,x,S0,N0).

adjust_abscissae(File,Model,NodeName,NodeRef,[V|V0],[A|A0],y,[S|S0],[N|N0]) :-
  half_way(A,S,N),
  clause(gotdata(File,Model,NodeName,NodeRef,verts(V,X,A,Z)),true,Eref),
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,verts(V,X,N,Z))),
  increment_bug_count(File),
  !,
  adjust_abscissae(File,Model,NodeName,NodeRef,V0,A0,y,S0,N0).

half_way(X,X,X) :- !.

half_way(X1,X2,X) :- M is 0.5*(X1+X2), snap(M,X), !.
  
/* ======================== */
/* fix_undercut_faces/6     */
/* improve_undercut_faces/7 */
/* shift_vertex/5           */
/* relax_vertex/10          */
/* recompute_face_normals/6 */
/* ======================== */

fix_undercut_faces(_,_,_,_,[],[]) :- !.

fix_undercut_faces(File,Model,NodeName,NodeRef,Facelist,Residue) :-
  improve_undercut_faces(File,Model,NodeName,NodeRef,Facelist,Improved,Unimproved), !,
  fix_undercut_faces(File,Model,NodeName,NodeRef,Improved,SubResidue),
  append(Unimproved,SubResidue,Residue).

improve_undercut_faces(_,_,_,_,[],[],[]) :- !.
  
improve_undercut_faces(File,Model,NodeName,NodeRef,[F|F0],L0,U) :-
  /* recompute_face_normals(File,Model,NodeName,NodeRef,[F],nocheck), */
  \+ is_undercut_face(File,Model,NodeName,NodeRef,F), !,
  tab(2), write('repaired overhang on '), write(NodeName), write(' face '), write(F), nl,
  increment_bug_count(File),
  improve_undercut_faces(File,Model,NodeName,NodeRef,F0,L0,U).

improve_undercut_faces(File,Model,NodeName,NodeRef,[F|F0],[F|L0],U) :-
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,Z3)),
  face_normal(File,Model,NodeName,NodeRef,F,[Nx,Ny,Nz]),
  Zc is (Z1+Z2+Z3)/3.0,
  Px is 0.33*Nx*Nz/(1.0-Nz*Nz), Py is 0.33*Ny*Nz/(1.0-Nz*Nz),
  (abs(X1)=\=5, abs(Y1)=\=5,
   shift_vertex(X1,Y1,Px,Py,Z1,Zc,NewX1,NewY1),
   \+ (gotdata(File,Model,NodeName,NodeRef,verts(VV1,NewX1,NewY1,Z1)), VV1\=V1)
   ;
   NewX1=X1, NewY1=Y1),
  (abs(X2)=\=5, abs(Y2)=\=5,
   shift_vertex(X2,Y2,Px,Py,Z2,Zc,NewX2,NewY2),
   \+ (gotdata(File,Model,NodeName,NodeRef,verts(VV2,NewX2,NewY2,Z2)), VV2\=V2)
   ;
   NewX2=X2, NewY2=Y2),
  (abs(X3)=\=5, abs(Y3)=\=5,
   shift_vertex(X3,Y3,Px,Py,Z3,Zc,NewX3,NewY3),
   \+ (gotdata(File,Model,NodeName,NodeRef,verts(VV3,NewX3,NewY3,Z3)), VV3\=V3)
   ;
   NewX3=X3, NewY3=Y3),
  \+ maplist(=:=,[X1,Y1,X2,Y2,X3,Y3],[NewX1,NewY1,NewX2,NewY2,NewX3,NewY3]),
  relax_vertex(File,Model,NodeName,NodeRef,V1,X1,Y1,NewX1,NewY1,Pass),
  relax_vertex(File,Model,NodeName,NodeRef,V2,X2,Y2,NewX2,NewY2,Pass),
  relax_vertex(File,Model,NodeName,NodeRef,V3,X3,Y3,NewX3,NewY3,Pass),
  Pass==yes, !,
  improve_undercut_faces(File,Model,NodeName,NodeRef,F0,L0,U).

improve_undercut_faces(File,Model,NodeName,NodeRef,[F|F0],L0,[F|U]) :-
  improve_undercut_faces(File,Model,NodeName,NodeRef,F0,L0,U).

shift_vertex(X,Y,Px,Py,Z,Zc,NewX,NewY) :-
  Dx is Px*(Z-Zc), Dy is Py*(Z-Zc),
  (abs(Dx)>=0.5*abs(Dy) -> (Dx>0 -> Dx1 is Dx+0.005; Dx<0 -> Dx1 is Dx-0.005 ; Dx1 is 0) ; Dx1=Dx),
  (abs(Dy)>=0.5*abs(Dx) -> (Dy>0 -> Dy1 is Dy+0.005; Dy<0 -> Dy1 is Dy-0.005 ; Dy1 is 0) ; Dy1=Dy),
  FuzzyX is X+Dx1,
  (
    FuzzyX > 5 -> NewX = 5;
    FuzzyX < -5 -> NewX = -5;
    snap(FuzzyX,NewX)
  ),
  FuzzyY is Y+Dy1,
  (
    FuzzyY > 5.0 -> NewY = 5;
    FuzzyY < -5.0 -> NewY = -5;
    snap(FuzzyY,NewY)
  ), !.

relax_vertex(_,_,_,_,_,X1,Y1,X1,Y1,_) :- !.

relax_vertex(File,Model,NodeName,NodeRef,V,X,Y,NewX,NewY,Pass) :-
  clause(gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,Z)),true,Eref),
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,verts(V,NewX,NewY,Z))),
  is_vertex_in_facelist(File,Model,NodeName,NodeRef,FL,V),
  recompute_face_normals(File,Model,NodeName,NodeRef,FL,check),
  Pass=yes,
  increment_bug_count(File).

relax_vertex(File,Model,NodeName,NodeRef,V,X,Y,NewX,NewY,_) :-
  clause(gotdata(File,Model,NodeName,NodeRef,verts(V,NewX,NewY,Z)),true,Eref),
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,Z))),
  is_vertex_in_facelist(File,Model,NodeName,NodeRef,FL,V),
  recompute_face_normals(File,Model,NodeName,NodeRef,FL,nocheck), !.
  
recompute_face_normals(_,_,_,_,[],_) :- !.

recompute_face_normals(File,Model,NodeName,NodeRef,[F|FL],Check) :-
  recompute_face_normals(File,Model,NodeName,NodeRef,FL,Check), !,
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,_,_,_,_)),
  clause(face_normal(File,Model,NodeName,NodeRef,F,depth(_)),true,Cref1), erase(Cref1),
  clause(face_normal(File,Model,NodeName,NodeRef,F,[_,_,Z]),true,Cref2), erase(Cref2),
  prepare_face_normal(File,Model,NodeName,NodeRef,F,V1,V2,V3),
  face_normal(File,Model,NodeName,NodeRef,F,[_,_,Z1]),
  ( Check=nocheck ; Z1 >= -0.000001 ; Z1 >= Z*1.1 ), !.

/* =================== */
/* is_tuck/5           */
/* inside_triangle/8   */
/* is_left_of/3        */
/* fix_tucks_in_list/5 */
/* fix_tuck/5          */
/* =================== */

is_tuck(File,Model,NodeName,NodeRef,[F1,F2,V1,V2,V3,V4]) :-
  face_normal(File,Model,NodeName,NodeRef,F1,[_,_,Z1]), Z1 =:= -1,
  (gotdata(File,Model,NodeName,NodeRef,faces(F1,V1,V3,V2,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(F1,V3,V2,V1,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(F1,V2,V1,V3,_,_,_,_,_))),
  (gotdata(File,Model,NodeName,NodeRef,faces(F2,V1,V2,V4,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(F2,V2,V4,V1,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(F2,V4,V1,V2,_,_,_,_,_))),
  face_normal(File,Model,NodeName,NodeRef,F2,[_,_,Z2]), Z2 =:= 1,
  inside_triangle(File,Model,NodeName,NodeRef,V1,V2,V4,V3).

inside_triangle(File,Model,NodeName,NodeRef,V1,V2,V4,V3) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V4,X4,Y4,_)),
  is_left_of([X1,Y1],[X2,Y2],[X3,Y3]),
  is_left_of([X2,Y2],[X4,Y4],[X3,Y3]),
  is_left_of([X4,Y4],[X1,Y1],[X3,Y3]).

is_left_of([X1,Y1],[X2,Y2],[X3,Y3]) :- (X2-X1)*(Y3-Y1) > (Y2-Y1)*(X3-X1).

fix_tucks_in_list(_,_,_,_,[]).

fix_tucks_in_list(File,Model,NodeName,NodeRef,[T|T0]) :-
  fix_tuck(File,Model,NodeName,NodeRef,T), !,
  fix_tucks_in_list(File,Model,NodeName,NodeRef,T0).

fix_tuck(File,Model,NodeName,NodeRef,[F1,F2,V1,V2,V3,V4]) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(F2,_,_,_,S,_,_,_,M))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(F2,V1,V3,V4,S,0,0,0,M))),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(F1,_,_,_,_,_,_,_,_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(F1,V3,V2,V4,S,0,0,0,M))),
  retract(face_normal(File,Model,NodeName,NodeRef,F1,[_,_,_])),
  asserta(face_normal(File,Model,NodeName,NodeRef,F1,[0.0,0.0,1.0])),
  !.

/* ======================= */
/* is_degenerate_face/5    */
/* is_degenerate_face/8    */
/* fix_degenerate_faces/5  */
/* is_tear_in_mesh/5       */
/* open_edge/8             */
/* fix_tears_in_mesh/5     */
/* edge_divide/7           */
/* interpolate_tvert/10    */
/* interpolate_tvert/6     */
/* ======================= */

is_degenerate_face(File,Model,NodeName,NodeRef,F) :-
  is_degenerate_face(File,Model,NodeName,NodeRef,F,_,_,_).

is_degenerate_face(File,Model,NodeName,NodeRef,F,VV1,VV2,VV3) :-
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,Z3)),
  vector_subtract([X3,Y3,Z3],[X1,Y1,Z1],V13),
  vector_subtract([X2,Y2,Z2],[X1,Y1,Z1],V12),
  vector_magnitude(V13,R13),
  vector_magnitude(V12,R12),
  vector_cross_product(V12,V13,Up),
  vector_magnitude(Up,Rup),
  Rup < 0.001*R12*R13,
  vector_subtract([X3,Y3,Z3],[X2,Y2,Z2],V23),
  vector_magnitude(V23,R23),
  ( R13>R12, R13>R23 -> [VV1,VV2,VV3]=[V1,V2,V3] ;
    R23>R12, R23>R13 -> [VV1,VV2,VV3]=[V3,V1,V2] ;
    R12>R23, R12>R13 -> [VV1,VV2,VV3]=[V2,V3,V1] ;
    fail).

fix_degenerate_faces(_,_,_,_,[]) :- !.

fix_degenerate_faces(File,Model,NodeName,NodeRef,[F|F0]) :-
  is_degenerate_face(File,Model,NodeName,NodeRef,F,V1,V2,V3),
  edge_divide(File,Model,NodeName,NodeRef,V1,V2,V3),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(F,_,_,_,S,_,_,_,M))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V2,V2,V2,S,0,0,0,M))),
  increment_bug_count(File),
  !,
  fix_degenerate_faces(File,Model,NodeName,NodeRef,F0).

fix_degenerate_faces(File,Model,NodeName,NodeRef,[_|F0]) :-
  fix_degenerate_faces(File,Model,NodeName,NodeRef,F0).

is_tear_in_mesh(File,Model,NodeName,NodeRef,[VV1,VV2,VV3]) :-
  open_edge(File,Model,NodeName,NodeRef,V1,V2,V12,R12),
  open_edge(File,Model,NodeName,NodeRef,V2,V3,V23,R23),
  open_edge(File,Model,NodeName,NodeRef,V3,V1,_,R31),
  vector_cross_product(V12,V23,Up),
  vector_magnitude(Up,Rup),
  Rup < 0.001*R12*R23,
  ( R31>R12, R31>R23 -> [VV1,VV2,VV3]=[V1,V2,V3] ;
    R23>R12, R23>R31 -> [VV1,VV2,VV3]=[V3,V1,V2] ;
    R12>R23, R12>R31 -> [VV1,VV2,VV3]=[V2,V3,V1] ;
    fail).
  
open_edge(File,Model,NodeName,NodeRef,V1,V2,V12,R12) :-
  (gotdata(File,Model,NodeName,NodeRef,faces(_,V1,V2,_,_,_,_,_,_));  
   gotdata(File,Model,NodeName,NodeRef,faces(_,_,V1,V2,_,_,_,_,_));  
   gotdata(File,Model,NodeName,NodeRef,faces(_,V2,_,V1,_,_,_,_,_))),
  \+ gotdata(File,Model,NodeName,NodeRef,faces(_,V2,V1,_,_,_,_,_,_)),  
  \+ gotdata(File,Model,NodeName,NodeRef,faces(_,_,V2,V1,_,_,_,_,_)),  
  \+ gotdata(File,Model,NodeName,NodeRef,faces(_,V1,_,V2,_,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  vector_subtract([X2,Y2,Z2],[X1,Y1,Z1],V12),
  vector_magnitude(V12,R12).
    
fix_tears_in_mesh(_,_,_,_,[]) :- !.

fix_tears_in_mesh(File,Model,NodeName,NodeRef,[[V1,V2,V3]|T0]) :-
  is_tear_in_mesh(File,Model,NodeName,NodeRef,[V1,V2,V3]),
  edge_divide(File,Model,NodeName,NodeRef,V1,V2,V3),
  !,
  fix_tears_in_mesh(File,Model,NodeName,NodeRef,T0).

fix_tears_in_mesh(File,Model,NodeName,NodeRef,[_|T0]) :-
  fix_tears_in_mesh(File,Model,NodeName,NodeRef,T0).

edge_divide(File,Model,NodeName,NodeRef,V1,V2,V3) :-
  (clause(gotdata(File,Model,NodeName,NodeRef,faces(F1,V0,V1,V3,S,T0,T1,T3,M)),true,FRef1), V0\==V2;
   clause(gotdata(File,Model,NodeName,NodeRef,faces(F1,V1,V3,V0,S,T1,T3,T0,M)),true,FRef1), V0\==V2;
   clause(gotdata(File,Model,NodeName,NodeRef,faces(F1,V3,V0,V1,S,T3,T0,T1,M)),true,FRef1), V0\==V2),
  \+ is_degenerate_face(File,Model,NodeName,NodeRef,F1),
  interpolate_tvert(File,Model,NodeName,NodeRef,V1,T1,V3,T3,V2,T2),
  erase(FRef1),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(F1,V0,V1,V2,S,T0,T1,T2,M))),
  increment_bug_count(File),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  NewNFaces is NFaces+1,
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))),
  increment_bug_count(File),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(NFaces,V0,V2,V3,S,T0,T2,T3,M))),
  increment_bug_count(File),
  fail.

edge_divide(File,Model,NodeName,NodeRef,V1,V2,V3) :-
  (clause(gotdata(File,Model,NodeName,NodeRef,faces(F1,V0,V3,V1,S,T0,T3,T1,M)),true,FRef1), V0\==V2;
   clause(gotdata(File,Model,NodeName,NodeRef,faces(F1,V3,V1,V0,S,T3,T1,T0,M)),true,FRef1), V0\==V2;
   clause(gotdata(File,Model,NodeName,NodeRef,faces(F1,V1,V0,V3,S,T1,T0,T3,M)),true,FRef1), V0\==V2),
  \+ is_degenerate_face(File,Model,NodeName,NodeRef,F1),
  interpolate_tvert(File,Model,NodeName,NodeRef,V1,T1,V3,T3,V2,T2),
  erase(FRef1),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(F1,V0,V2,V1,S,T0,T2,T1,M))),
  increment_bug_count(File),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  NewNFaces is NFaces+1,
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))),
  increment_bug_count(File),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(NFaces,V0,V3,V2,S,T0,T3,T2,M))),
  increment_bug_count(File),
  fail.

edge_divide(_,_,_,_,_,_,_).

interpolate_tvert(_,_,_,_,_,T,_,T,_,T) :- !.

interpolate_tvert(File,Model,NodeName,NodeRef,V1,T1,V3,T3,V2,T2) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,tverts(T1,Tu1,Tv1,Tw1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,Z3)),
  gotdata(File,Model,NodeName,NodeRef,tverts(T3,Tu3,Tv3,Tw3)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  vector_subtract([X2,Y2,Z2],[X1,Y1,Z1],V12), vector_magnitude(V12,R12),
  vector_subtract([X3,Y3,Z3],[X1,Y1,Z1],V13), vector_magnitude(V13,R13),
  R12=<R13,
  M is R12/R13,
  Tu2 is Tu1+M*(Tu3-Tu1), Tv2 is Tv1+M*(Tv3-Tv1), Tw2 is Tw1+M*(Tw3-Tw1),
  interpolate_tvert(File,Model,NodeName,NodeRef,[Tu2,Tv2,Tw2],T2),
  !.
  
interpolate_tvert(File,Model,NodeName,NodeRef,[U,V,W],T) :-
  gotdata(File,Model,NodeName,NodeRef,tverts(T,U,V,W)), !.

interpolate_tvert(File,Model,NodeName,NodeRef,[U,V,W],T) :-
  gotdata(File,Model,NodeName,NodeRef,tverts(N)),
  T=N, !,
  asserta(gotdata(File,Model,NodeName,NodeRef,tverts(T,U,V,W))),
  increment_bug_count(File),
  retract(gotdata(File,Model,NodeName,NodeRef,tverts(N))),
  N1 is N+1,
  asserta(gotdata(File,Model,NodeName,NodeRef,tverts(N1))).

/* ======================= */
/* is_vertex_in_facelist/6 */
/* is_vertex_in_face/6     */
/* ======================= */

is_vertex_in_facelist(File,Model,NodeName,NodeRef,Facelist,V) :-
  is_list(Facelist),
  member(F,Facelist),
  is_vertex_in_face(File,Model,NodeName,NodeRef,F,V).

is_vertex_in_facelist(File,Model,NodeName,NodeRef,Facelist,V) :-
   nonvar(V),
   (setof(F,is_vertex_in_face(File,Model,NodeName,NodeRef,F,V),Facelist) ; Facelist=[]), !.

is_vertex_in_face(File,Model,NodeName,NodeRef,F,V) :- gotdata(File,Model,NodeName,NodeRef,faces(F,V,_,_,_,_,_,_,_)).
is_vertex_in_face(File,Model,NodeName,NodeRef,F,V) :- gotdata(File,Model,NodeName,NodeRef,faces(F,_,V,_,_,_,_,_,_)).
is_vertex_in_face(File,Model,NodeName,NodeRef,F,V) :- gotdata(File,Model,NodeName,NodeRef,faces(F,_,_,V,_,_,_,_,_)).

/* ======================= */
/* attempt_to_fix_pivots/4 */
/* ======================= */

attempt_to_fix_pivots(_,_,_,_,_) :- \+ clause(f_find_pivot(_,_,_,_,_),_), !.
attempt_to_fix_pivots(_,_,_,_,_) :- \+ clause(f_repivot(_,_,_,_,_),_), !.
attempt_to_fix_pivots(File,_,_,_,_) :- check_failed(File), !.

/*
attempt_to_fix_pivots(File,Model,NodeName,NodeRef,_) :-
  prepare_face_normals(File,Model,NodeName,NodeRef),
  fail.
*/

attempt_to_fix_pivots(File,Model,NodeName,NodeRef,SmallLogStream) :-
  (gotdata(File,Model,NodeName,NodeRef,_,orientation(_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,_,orientationkey(_));
   gotdata(File,Model,NodeName,NodeRef,_,position(_,_,_));
   gotdata(File,Model,NodeName,NodeRef,_,positionkey(_))), !,
  once((face_normal(File,Model,NodeName,NodeRef,_,depth(D)),D<0)),
  report_warning(['bad pivot on animated node',NodeName,'- cannot repair'],SmallLogStream).

/*
attempt_to_fix_pivots(File,Model,NodeName,NodeRef,_) :-
  f_transform_constraint(File,Model,NodeName,NodeRef,[0,0,0,1],[A,B,C,D]),
  \+ ((gotdata(File,Model,NodeName,NodeRef,verts(_,X,Y,Z)), A+B*X+C*Y+D*Z > 0.0)),
  once((retract(gotdata(File,Model,NodeName,NodeRef,shadow(1))); true)),
  asserta(gotdata(File,Model,NodeName,NodeRef,shadow(0))),
  tab(2), write('trimesh node '), write(NodeName), write(' is at or below Z=0 - setting to shadow 0'), nl,
  w_facelist(File,Model,NodeName,NodeRef,FaceList),
  w_mid_point_average(File,Model,NodeName,NodeRef,FaceList,Centre),
  apply_pivot_shift(File,Model,NodeName,NodeRef,Centre), !.
*/

attempt_to_fix_pivots(File,Model,NodeName,NodeRef,_) :-
  f_find_pivot(File,Model,NodeName,NodeRef,Pivot),
  apply_pivot_shift(File,Model,NodeName,NodeRef,Pivot),
  !.

attempt_to_fix_pivots(File,Model,NodeName,NodeRef,SmallLogStream) :-
  once(g_user_option(allow_split,Split)), Split=yes,
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)),maxfaces(_,Max), NFaces<Max,
  f_detach_subobjects(File,Model,NodeName,NodeRef,_,NewObjList,SmallLogStream),
  length(NewObjList,L), L>=1,
  prepare_face_normals(File,Model,NodeName,NodeRef),
  attempt_to_fix_pivots(File,Model,NodeName,NodeRef,SmallLogStream),
  !.

/*
attempt_to_fix_pivots(File,Model,NodeName,NodeRef,_) :-
  \+ gotdata(File,Model,NodeName,NodeRef,render(0)),
  w_facelist(File,Model,NodeName,NodeRef,FaceList),
  w_mid_point_average(File,Model,NodeName,NodeRef,FaceList,Centre),
  f_repivot(File,Model,NodeName,NodeRef,Centre),
  tab(2), write('moved pivot of '), write(NodeName), write(' by '), format('[~7f, ~7f, ~7f]',Centre), nl,
  create_shadow_mesh(File,Model,NodeName,NodeRef),
  !.
*/

attempt_to_fix_pivots(File,Model,NodeName,NodeRef,SmallLogStream) :-
  once(g_user_option(move_bad_pivots,MoveBadPivots)), MoveBadPivots==top,
  once((
    gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z0)),
    \+ (gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z1)), Z1>Z0)
      )),
  bagof([N,X,Y],gotdata(File,Model,NodeName,NodeRef,verts(N,X,Y,Z0)),B),
  middle(B,X0,Y0),
  f_repivot(File,Model,NodeName,NodeRef,[X0,Y0,Z0]),
  retractall(gotdata(File,Model,NodeName,NodeRef,wirecolor(_,_,_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,wirecolor(1,0,0))),
  write(SmallLogStream,'  Bad Pivot on '), write(SmallLogStream,NodeName), nl(SmallLogStream),
  tab(2), write('*** bad pivot on '), write(NodeName), write(' placed at top centre'), nl,
  !.

attempt_to_fix_pivots(File,Model,NodeName,NodeRef,SmallLogStream) :-
  once(g_user_option(move_bad_pivots,MoveBadPivots)), MoveBadPivots==middle,
  f_transform_constraint(File,Model,NodeName,NodeRef,[0,0,0,1],[A,B,C,D]),
  bagof([N,X,Y,Z],(gotdata(File,Model,NodeName,NodeRef,verts(N,X,Y,Z)), A+B*X+C*Y+D*Z > 0.0),Bag),
  middle3(Bag,X0,Y0,Z0),
  \+((X0=:=0, Y0=:=0, Z0=:=0)),
  f_repivot(File,Model,NodeName,NodeRef,[X0,Y0,Z0]),
  retractall(gotdata(File,Model,NodeName,NodeRef,wirecolor(_,_,_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,wirecolor(1,0,0))),
  write(SmallLogStream,'  Bad Pivot on '), write(SmallLogStream,NodeName), nl(SmallLogStream),
  tab(2), write('*** re-centred bad pivot on '), write(NodeName), nl,
  !.
    
attempt_to_fix_pivots(File,Model,NodeName,NodeRef,SmallLogStream) :-
  once(g_user_option(move_bad_pivots,MoveBadPivots)), MoveBadPivots==bottom,
  once((
    gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z0)),
    \+ (gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z1)), Z1<Z0)
      )),
  bagof([N,X,Y],gotdata(File,Model,NodeName,NodeRef,verts(N,X,Y,Z0)),B),
  middle(B,X0,Y0),
  f_repivot(File,Model,NodeName,NodeRef,[X0,Y0,Z0]),
  retractall(gotdata(File,Model,NodeName,NodeRef,wirecolor(_,_,_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,wirecolor(1,0,0))),
  write(SmallLogStream,'  Bad Pivot on '), write(SmallLogStream,NodeName), nl(SmallLogStream),
  tab(2), write('*** bad pivot on '), write(NodeName), write(' placed at bottom centre'), nl,
  !.

attempt_to_fix_pivots(File,Model,NodeName,NodeRef,SmallLogStream) :-
  retractall(gotdata(File,Model,NodeName,NodeRef,wirecolor(_,_,_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,wirecolor(1,0,0))),
  write(SmallLogStream,'  Bad Pivot on '), write(SmallLogStream,NodeName), nl(SmallLogStream),
  tab(2), write('*** bad pivot on '), write(NodeName), nl.

/* =================== */
/* apply_pivot_shift/5 */
/* =================== */

apply_pivot_shift(File,Model,NodeName,NodeRef,[X,Y,Z]) :-
  abs(X)<0.000001, abs(Y)<0.000001, abs(Z)<0.000001,
  \+ (face_normal(File,Model,NodeName,NodeRef,_,depth(D)),D<0), !, 
  tab(2), write('no pivot shift needed for '), write(NodeName), nl.

apply_pivot_shift(File,Model,NodeName,NodeRef,Pivot) :-
  f_repivot(File,Model,NodeName,NodeRef,Pivot),
  (
   gotdata(File,Model,NodeName,NodeRef,shadow(0))
   ;
   prepare_face_normals(File,Model,NodeName,NodeRef),
   \+ (face_normal(File,Model,NodeName,NodeRef,_,depth(D)),D< -0.000001)
  ),  
  tab(2), write('repivoted '), write(NodeName), write(' by '), format('[~7f, ~7f, ~7f]',Pivot), nl, !.

apply_pivot_shift(File,Model,NodeName,NodeRef,Pivot) :-
  tab(2), write('warning - repivoting '), write(NodeName), write(' by '),
  format('[~7f, ~7f, ~7f]',Pivot), write(' was marginal'), nl,
  (face_normal(File,Model,NodeName,NodeRef,N,depth(D)), D<0,
   tab(4), write('face '), write(N), write(' has residual depth '), write(D), nl,
   fail ; true), !.

/* ==================================== */
/* various vector arithmetic predicates */
/* ==================================== */

vector_add([X1,Y1,Z1],[X2,Y2,Z2],[X,Y,Z]) :-
  X is X2+X1, Y is Y2+Y1, Z is Z2+Z1.

vector_subtract([X2,Y2,Z2],[X1,Y1,Z1],[X,Y,Z]) :-
  X is X2-X1, Y is Y2-Y1, Z is Z2-Z1.

vector_cross_product([X1,Y1,Z1],[X2,Y2,Z2],[X,Y,Z]) :-
  X is Y1*Z2-Z1*Y2, Y is Z1*X2-X1*Z2, Z is X1*Y2-Y1*X2.

vector_dot_product([X1,Y1,Z1],[X2,Y2,Z2],D) :-
  D is X1*X2+Y1*Y2+Z1*Z2.

vector_normalise([X,Y,Z],N) :-
  R is sqrt(X*X+Y*Y+Z*Z),
  (R>0.0 -> X0 is X/R, Y0 is Y/R, Z0 is Z/R, N = [X0,Y0,Z0]; N=[]).

vector_magnitude([X,Y,Z],R) :-
  R is sqrt(X*X+Y*Y+Z*Z).

vector_total([[]],[0,0,0]) :- !.
vector_total([U],U) :- !.
vector_total([H|T],S) :- H=[], !, vector_total(T,S). 
vector_total([H|T],S) :- vector_total(T,S1), vector_add(H,S1,S). 

/* ==================== */
/* create_shadow_mesh/6 */
/* ==================== */

create_shadow_mesh(File,Model,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  atom_concat(NodeName,'_shdw',NS),
  once((gotdata(File,Model,node(_,NS)) -> f_newname(NS,NewName) ; NewName=NS)),
  asserta(gotdata(File,Model,node(trimesh,NewName)),NewRef),
  increment_bug_count(File),
  once((
    gotdata(File,Model,NodeName,NodeRef,Q),
    Q=..[Q0|_], \+ member(Q0,[wirecolor,render,bitmap,faces,tverts,colors]),
    asserta(gotdata(File,Model,NewName,NewRef,Q)),
    increment_bug_count(File),
    fail ; true
      )),
  gotdata(File,Model,NodeName,NodeRef,faces(N1)), N2 is 2*N1,
  asserta(gotdata(File,Model,NewName,NewRef,faces(N2))),
  once((
    gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,_,_,_,_)),
    F1 is 2*F, F2 is F1+1,
    asserta(gotdata(File,Model,NewName,NewRef,faces(F1,V1,V2,V3,0,0,0,0,0))),
    increment_bug_count(File),
    asserta(gotdata(File,Model,NewName,NewRef,faces(F2,V2,V1,V3,0,0,0,0,0))),
    increment_bug_count(File),
    fail ; true
      )),
  Red is random(256)/256, Green is random(256)/256, Blue is random(256)/256,
  asserta(gotdata(File,Model,NewName,NewRef,wirecolor(Red,Green,Blue))),
  increment_bug_count(File),
  asserta(gotdata(File,Model,NewName,NewRef,render(0))),
  increment_bug_count(File),
  asserta(gotdata(File,Model,NewName,NewRef,bitmap(black))),
  increment_bug_count(File),
  once((retract(gotdata(File,Model,NodeName,NodeRef,shadow(1))); true)),
  asserta(gotdata(File,Model,NodeName,NodeRef,shadow(0))),
  increment_bug_count(File),
  tab(2), write('shadow mesh created for '), write(NodeName), nl, !.

/* =================== */
/* flip_inward_faces/4 */
/* =================== */

flip_inward_faces(File,Model,NodeName,NodeRef) :-
  face_normal(File,Model,NodeName,NodeRef,N,depth(A)), A<0.0,
  clause(gotdata(File,Model,NodeName,NodeRef,faces(N,V1,V2,V3,S,T1,T2,T3,M)),true,Eref),
  erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N,V2,V1,V3,S,T1,T2,T3,M))),
  increment_bug_count(File),
  clause(face_normal(File,Model,NodeName,NodeRef,N,depth(A)),true,Eref),
  erase(Eref), A1 is -A,
  asserta(face_normal(File,Model,NodeName,NodeRef,N,depth(A1))),
  fail.  

flip_inward_faces(_,_,_,_).

/* ======================== */
/* delete_unused_vertices/4 */
/* delete_unused_vertices/7 */
/* ======================== */

delete_unused_vertices(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,verts(NVerts)),
  delete_unused_vertices(File,Model,NodeName,NodeRef,NVerts,0,0).

delete_unused_vertices(_,_,_,_,NVerts,NVerts,NVerts) :- !.

delete_unused_vertices(File,Model,NodeName,NodeRef,NVerts,NVerts,NewNVerts) :-
  retractall(gotdata(File,Model,NodeName,NodeRef,verts(_))),
  increment_bug_count(File),
  (NewNVerts>0 -> asserta(gotdata(File,Model,NodeName,NodeRef,verts(NewNVerts))) ; true),
  (clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef) ->
    retractall(gotdata(File,Model,NodeName,NodeRef,weights(_))),
    increment_bug_count(File),
    (NewNVerts>0 -> asserta(gotdata(File,Model,NodeName,NodeRef,weights(NewNVerts))) ; true)
    ;
    true),
  (gotdata(File,Model,NodeName,NodeRef,constraints(_)) ->
    retractall(gotdata(File,Model,NodeName,NodeRef,constraints(_))),
    increment_bug_count(File),
    (NewNVerts>0 -> asserta(gotdata(File,Model,NodeName,NodeRef,constraints(NewNVerts))) ; true)
    ;
    true),
  (gotdata(File,Model,NodeName,NodeRef,colors(_)) ->
    retractall(gotdata(File,Model,NodeName,NodeRef,colors(_))),
    increment_bug_count(File),
    (NewNVerts>0 -> asserta(gotdata(File,Model,NodeName,NodeRef,colors(NewNVerts))) ; true)
    ;
    true),
 !.

delete_unused_vertices(File,Model,NodeName,NodeRef,NVerts,ThisVert,NewNVerts) :-
  \+ gotdata(File,Model,NodeName,NodeRef,faces(_,ThisVert,_,_,_,_,_,_,_)),
  \+ gotdata(File,Model,NodeName,NodeRef,faces(_,_,ThisVert,_,_,_,_,_,_)),
  \+ gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,ThisVert,_,_,_,_,_)),
  retractall(gotdata(File,Model,NodeName,NodeRef,verts(ThisVert,_,_,_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,weights(ThisVert,_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,constraints(ThisVert,_))),
  retractall(gotdata(File,Model,NodeName,NodeRef,colors(ThisVert,_,_,_))),
  increment_bug_count(File),
  NextVert is ThisVert+1, !,
  delete_unused_vertices(File,Model,NodeName,NodeRef,NVerts,NextVert,NewNVerts).

delete_unused_vertices(File,Model,NodeName,NodeRef,NVerts,ThisVert,NewNVerts) :-
  (ThisVert\=NewNVerts ->
    retract(gotdata(File,Model,NodeName,NodeRef,verts(ThisVert,X,Y,Z))),
    asserta(gotdata(File,Model,NodeName,NodeRef,verts(NewNVerts,X,Y,Z))),
    increment_bug_count(File),
    once((clause(gotdata(File,Model,NodeName,NodeRef,weights(ThisVert,W)),true,Eref),
          erase(Eref),
          asserta(gotdata(File,Model,NodeName,NodeRef,weights(NewNVerts,W))),
          increment_bug_count(File),
          fail; true)),
    once((clause(gotdata(File,Model,NodeName,NodeRef,constraints(ThisVert,C)),true,Eref),
          erase(Eref),
          asserta(gotdata(File,Model,NodeName,NodeRef,constraints(NewNVerts,C))),
          increment_bug_count(File),
          fail; true)),
    once((clause(gotdata(File,Model,NodeName,NodeRef,colors(ThisVert,R,G,B)),true,Eref),
          erase(Eref),
          asserta(gotdata(File,Model,NodeName,NodeRef,colors(NewNVerts,R,G,B))),
          increment_bug_count(File),
          fail; true)),
    once((clause(gotdata(File,Model,NodeName,NodeRef,faces(F,ThisVert,V2,V3,S,T1,T2,T3,M)),true,Eref1),
          erase(Eref1),
          asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,NewNVerts,V2,V3,S,T1,T2,T3,M))),
          increment_bug_count(File),
          fail; true)),
    once((clause(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,ThisVert,V3,S,T1,T2,T3,M)),true,Eref2),
          erase(Eref2),
          asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,NewNVerts,V3,S,T1,T2,T3,M))),
          increment_bug_count(File),
          fail; true)),
    once((clause(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,ThisVert,S,T1,T2,T3,M)),true,Eref3),
          erase(Eref3),
          asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,NewNVerts,S,T1,T2,T3,M))),
          increment_bug_count(File),
          fail; true))
    ; true),
  NextVert is ThisVert+1, NextNewNVerts is NewNVerts+1, !,
  delete_unused_vertices(File,Model,NodeName,NodeRef,NVerts,NextVert,NextNewNVerts).

/* ====================== */
/* delete_unused_tverts/4 */
/* delete_unused_tverts/7 */
/* ====================== */

delete_unused_tverts(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,tverts(NVerts)),
  delete_unused_tverts(File,Model,NodeName,NodeRef,NVerts,0,0).

delete_unused_tverts(_,_,_,_,NVerts,NVerts,NVerts) :- !.

delete_unused_tverts(File,Model,NodeName,NodeRef,NVerts,NVerts,NewNVerts) :-
  retract(gotdata(File,Model,NodeName,NodeRef,tverts(NVerts))),
  (NewNVerts>0 -> asserta(gotdata(File,Model,NodeName,NodeRef,tverts(NewNVerts))) ; true), !.

delete_unused_tverts(File,Model,NodeName,NodeRef,NVerts,ThisVert,NewNVerts) :-
  \+ gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,_,ThisVert,_,_,_)),
  \+ gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,_,_,ThisVert,_,_)),
  \+ gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,_,_,_,ThisVert,_)),
  clause(gotdata(File,Model,NodeName,NodeRef,tverts(ThisVert,_,_,_)),true,Eref),
  erase(Eref),
  increment_bug_count(File),
  NextVert is ThisVert+1, !,
  delete_unused_tverts(File,Model,NodeName,NodeRef,NVerts,NextVert,NewNVerts).

delete_unused_tverts(File,Model,NodeName,NodeRef,NVerts,ThisVert,NewNVerts) :-
  (ThisVert\=NewNVerts ->
    clause(gotdata(File,Model,NodeName,NodeRef,tverts(ThisVert,X,Y,Z)),true,Eref),
    erase(Eref),
    asserta(gotdata(File,Model,NodeName,NodeRef,tverts(NewNVerts,X,Y,Z))),
    increment_bug_count(File),
    once((clause(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,ThisVert,T2,T3,M)),true,Eref1), erase(Eref1),
          asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,NewNVerts,T2,T3,M))),
          increment_bug_count(File),
          fail; true)),
    once((clause(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,T1,ThisVert,T3,M)),true,Eref2), erase(Eref2),
          asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,T1,NewNVerts,T3,M))),
          increment_bug_count(File),
          fail; true)),
    once((clause(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,T1,T2,ThisVert,M)),true,Eref3), erase(Eref3),
          asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,T1,T2,NewNVerts,M))),
          increment_bug_count(File),
          fail; true))
    ; true),
  NextVert is ThisVert+1, NextNewNVerts is NewNVerts+1, !,
  delete_unused_tverts(File,Model,NodeName,NodeRef,NVerts,NextVert,NextNewNVerts).

/* ====================== */
/* set_zero_orientation/4 */
/* set_zero_position/4    */
/* ====================== */

set_zero_orientation(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,orientation(_,_,_,A)),
  A=:=0.0, !.

set_zero_orientation(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,_,_,parent(NodeName/NodeRef)), !, fail.

set_zero_orientation(File,Model,NodeName,NodeRef) :-
  once(gotdata(File,Model,NodeName,NodeRef,orientation(U,V,W,A))),
  gotdata(File,Model,NodeName,NodeRef,verts(NVerts)), LastVert is NVerts-1,
  (between(0,LastVert,ThisVert),
    clause(gotdata(File,Model,NodeName,NodeRef,verts(ThisVert,X,Y,Z)),true,Eref),
    erase(Eref),
    f_rotate_vector([X,Y,Z],[U,V,W,A],[X1,Y1,Z1]),
    (abs(X1)<0.000001 -> X2=0 ; X2=X1),
    (abs(Y1)<0.000001 -> Y2=0 ; Y2=Y1),
    (abs(Z1)<0.000001 -> Z2=0 ; Z2=Z1),
    asserta(gotdata(File,Model,NodeName,NodeRef,verts(ThisVert,X2,Y2,Z2))),
    increment_bug_count(File),
    fail ; true),
  retract(gotdata(File,Model,NodeName,NodeRef,orientation(U,V,W,A))),
  asserta(gotdata(File,Model,NodeName,NodeRef,orientation(0,0,0,0))),
  increment_bug_count(File), !.
 
set_zero_position(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z)),
  X=:=0.0, Y=:=0.0, Z=:=0.0, !.

set_zero_position(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,_,_,parent(NodeName/NodeRef)), !, fail.

set_zero_position(File,Model,NodeName,NodeRef) :-
  retract(gotdata(File,Model,NodeName,NodeRef,position(X0,Y0,Z0))),
  asserta(gotdata(File,Model,NodeName,NodeRef,position(0,0,0))),
  increment_bug_count(File),
  (gotdata(File,Model,NodeName,NodeRef,verts(NVerts)) ->
    LastVert is NVerts-1,
    ( between(0,LastVert,ThisVert),
      clause(gotdata(File,Model,NodeName,NodeRef,verts(ThisVert,X,Y,Z)),true,Eref), erase(Eref),
      X1 is X+X0, (abs(X1)<0.000001 -> X2=0 ; X2=X1),
      Y1 is Y+Y0, (abs(Y1)<0.000001 -> Y2=0 ; Y2=Y1),
      Z1 is Z+Z0, (abs(Z1)<0.000001 -> Z2=0 ; Z2=Z1),
      asserta(gotdata(File,Model,NodeName,NodeRef,verts(ThisVert,X2,Y2,Z2))),
      increment_bug_count(File),
      fail ; true)
    ; true), !.

/* ================ */
/* relate_to_tile/6 */
/* ================ */

relate_to_tile(File,Model,NodeName,NodeRef,Vector,AbsVector) :-
  (gotdata(File,Model,NodeName,NodeRef,orientation(Xa,Ya,Za,A)) ->
    f_rotate_vector(Vector,[Xa,Ya,Za,A],Vector1) ; Vector1=Vector),
  (gotdata(File,Model,NodeName,NodeRef,position(Xp,Yp,Zp)) ->
    vector_add(Vector1,[Xp,Yp,Zp],Vector2); Vector2=Vector1),
  gotdata(File,Model,NodeName,NodeRef,parent(Parent)),
  (Parent='NULL' -> AbsVector=Vector2;
   Parent=ParentName/PRef, PRef\= -1,
   clause(gotdata(File,Model,node(_,ParentName)),true,PRef),
   relate_to_tile(File,Model,ParentName,PRef,Vector2,AbsVector)).

/* =================== */
/* snap_aabb_to_tile/4 */
/* exterior_edge/6     */
/* clockwise_edge/6    */
/* clockwise_edge/7    */
/* snap_edge_to_5/14   */
/* =================== */

snap_aabb_to_tile(File,Model,NodeName,NodeRef) :-
  exterior_edge(File,Model,NodeName,NodeRef,V1,V2),
  clause(gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),true,Eref1),
  clause(gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),true,Eref2),
  snap_edge_to_5(File,Model,NodeName,NodeRef,V1,X1,Y1,Z1,Eref1,V2,X2,Y2,Z2,Eref2),
  asserta(fixlist([V1,V2])),
  fail ; true.

exterior_edge(File,Model,NodeName,NodeRef,V1,V2) :-
  clockwise_edge(File,Model,NodeName,NodeRef,V1,V2),
  \+ clockwise_edge(File,Model,NodeName,NodeRef,V2,V1).

clockwise_edge(File,Model,NodeName,NodeRef,V1,V2) :-
  gotdata(File,Model,NodeName,NodeRef,faces(_,V1,V2,_,_,_,_,_,_)).

clockwise_edge(File,Model,NodeName,NodeRef,V1,V2) :-
  gotdata(File,Model,NodeName,NodeRef,faces(_,_,V1,V2,_,_,_,_,_)).

clockwise_edge(File,Model,NodeName,NodeRef,V1,V2) :-
  gotdata(File,Model,NodeName,NodeRef,faces(_,V2,_,V1,_,_,_,_,_)).

clockwise_edge(File,Model,NodeName,NodeRef,V1,V2,V3) :-
  gotdata(File,Model,NodeName,NodeRef,faces(_,V1,V2,V3,_,_,_,_,_)).

clockwise_edge(File,Model,NodeName,NodeRef,V1,V2,V3) :-
  gotdata(File,Model,NodeName,NodeRef,faces(_,V3,V1,V2,_,_,_,_,_)).

clockwise_edge(File,Model,NodeName,NodeRef,V1,V2,V3) :-
  gotdata(File,Model,NodeName,NodeRef,faces(_,V2,V3,V1,_,_,_,_,_)).

snap_edge_to_5(_,_,_,_,_,5,_,_,_,_,5,_,_,_) :- !, fail.
snap_edge_to_5(_,_,_,_,_,_,5,_,_,_,_,5,_,_) :- !, fail.
snap_edge_to_5(_,_,_,_,_,-5,_,_,_,_,-5,_,_,_) :- !, fail.
snap_edge_to_5(_,_,_,_,_,_,-5,_,_,_,_,-5,_,_) :- !, fail.

snap_edge_to_5(File,Model,NodeName,NodeRef,V1,X1,Y1,Z1,Eref1,V2,X2,Y2,Z2,Eref2) :-
  abs(X1-5)=<0.025, abs(X2-5)=<0.025,
  (X1==5 -> true ; erase(Eref1), asserta(gotdata(File,Model,NodeName,NodeRef,verts(V1,5,Y1,Z1))), increment_bug_count(File)),
  (X2==5 -> true ; erase(Eref2), asserta(gotdata(File,Model,NodeName,NodeRef,verts(V2,5,Y2,Z2))), increment_bug_count(File)),
  !.

snap_edge_to_5(File,Model,NodeName,NodeRef,V1,X1,Y1,Z1,Eref1,V2,X2,Y2,Z2,Eref2) :-
  abs(Y1-5)=<0.025, abs(Y2-5)=<0.025,
  (Y1==5 -> true ; erase(Eref1), asserta(gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,5,Z1))), increment_bug_count(File)),
  (Y2==5 -> true ; erase(Eref2), asserta(gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,5,Z2))), increment_bug_count(File)),
  !.

snap_edge_to_5(File,Model,NodeName,NodeRef,V1,X1,Y1,Z1,Eref1,V2,X2,Y2,Z2,Eref2) :-
  abs(X1+5)=<0.025, abs(X2+5)=<0.025,
  (X1== -5 -> true ; erase(Eref1), asserta(gotdata(File,Model,NodeName,NodeRef,verts(V1,-5,Y1,Z1))), increment_bug_count(File)),
  (X2== -5 -> true ; erase(Eref2), asserta(gotdata(File,Model,NodeName,NodeRef,verts(V2,-5,Y2,Z2))), increment_bug_count(File)),
  !.

snap_edge_to_5(File,Model,NodeName,NodeRef,V1,X1,Y1,Z1,Eref1,V2,X2,Y2,Z2,Eref2) :-
  abs(Y1+5)=<0.025, abs(Y2+5)=<0.025,
  (Y1== -5 -> true ; erase(Eref1), asserta(gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,-5,Z1))), increment_bug_count(File)),
  (Y2== -5 -> true ; erase(Eref2), asserta(gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,-5,Z2))), increment_bug_count(File)),
  !.

/* =================== */
/* snap_aabb_to_tile/6 */
/* ragged_edges/4      */
/* =================== */

snap_aabb_to_tile(File,Model,NodeName,NodeRef,[Xmin,Ymin,Xmax,Ymax]) :-
  abs(Xmin+5)=<0.025, abs(Ymin+5)=<0.025, abs(Xmax-5)=<0.025, abs(Ymax-5)=<0.025,
  \+ ragged_edges(File,Model,NodeName,NodeRef), !, fail.
    
snap_aabb_to_tile(File,Model,NodeName,NodeRef,[Xmin,Ymin,Xmax,Ymax]) :-
  abs(Xmin+5)=<0.025, abs(Ymin+5)=<0.025, abs(Xmax-5)=<0.025, abs(Ymax-5)=<0.025,
  gotdata(File,Model,NodeName,NodeRef,verts(NVerts)), LastVert is NVerts-1,
  (between(0,LastVert,V),
   clause(gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,Z)),true,Eref),
   once(( X >=  4.975 -> X1 =  5; X =< -4.975 -> X1 = -5; X1 = X )),
   once(( Y >=  4.975 -> Y1 =  5; Y =< -4.975 -> Y1 = -5; Y1 = Y )),
   \+ (X1=:=X, Y1=:=Y),
   erase(Eref),
   asserta(gotdata(File,Model,NodeName,NodeRef,verts(V,X1,Y1,Z))),
   increment_bug_count(File),
   fail ; true).

ragged_edges(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,verts(_,X,_,_)),
  (X =< -4.975, X =\= -5 ; X >= 4.95, X =\= 5), !.
  
ragged_edges(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,verts(_,_,Y,_)),
  (Y =< -4.975, Y =\= -5 ; Y >= 4.95, Y =\= 5), !.

/* ==================== */
/* reset_orientations/2 */
/* ==================== */

reset_orientations(File,Model) :-
  clause(gotdata(File,Model,node(dummy,Model)),true,MRef),
  gotdata(File,Model,NodeName,NodeRef,parent(Model/MRef)),
  reset_orientations(File,Model,NodeName,NodeRef),
  fail; true.

reset_orientations(_,Model,NodeName,_) :-
  atom_concat(Model,_,NodeName), !.

reset_orientations(File,Model,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(Type,NodeName)),true,NodeRef),
  (Type=light; Type=emitter), !.

reset_orientations(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,orientation(Xa,Ya,Za,A)), A=\=0,
  (gotdata(File,Model,NodeName,NodeRef,verts(NVerts)), NVerts>0, LastVert is NVerts-1,
   between(0,LastVert,V),
   clause(gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,Z)),true,Eref), erase(Eref),
   f_rotate_vector([X,Y,Z],[Xa,Ya,Za,A],[X1,Y1,Z1]),
   asserta(gotdata(File,Model,NodeName,NodeRef,verts(V,X1,Y1,Z1))),
   increment_bug_count(File),
   fail ; true),
  retract(gotdata(File,Model,NodeName,NodeRef,orientation(Xa,Ya,Za,A))),
  asserta(gotdata(File,Model,NodeName,NodeRef,orientation(0,0,0,0))),
  (gotdata(File,Model,Child,ChildRef,parent(NodeName/NodeRef)),
   clause(gotdata(File,Model,Child,ChildRef,position(Xc,Yc,Zc)),true,Eref1), erase(Eref1),
   f_rotate_vector([Xc,Yc,Zc],[Xa,Ya,Za,A],[Xc1,Yc1,Zc1]),
   asserta(gotdata(File,Model,Child,ChildRef,position(Xc1,Yc1,Zc1))),
   increment_bug_count(File),
   clause(gotdata(File,Model,Child,ChildRef,orientation(Xac,Yac,Zac,Ac)),true,Eref2), erase(Eref2),
   f_rotate_rotation([Xac,Yac,Zac,Ac],[Xa,Ya,Za,A],[Xac1,Yac1,Zac1,Ac1]),
   asserta(gotdata(File,Model,Child,ChildRef,orientation(Xac1,Yac1,Zac1,Ac1))),
   increment_bug_count(File),
   fail ; true),
  fail.

reset_orientations(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,Child,ChildRef,parent(NodeName/NodeRef)),
  reset_orientations(File,Model,Child,ChildRef),
  fail ; true.

/* ============ */
/* apply_snap/4 */
/* apply_snap/5 */
/* ============ */

/* Changes in RC1c:                                                                                      */
/* (1) Snap the positions of all nodes.                                                                  */
/* (2) When you adjust the position of a node to a snap point, apply the same adjustment to position and */
/*     positionkeys in its animations but do not snap those values (there may be fine-grained movement). */
/* (3) Compute a back-adjustment allowing for a possible non-null orientation of the node.               */
/* (4) Apply the back-adjustment to all the verts and animverts of the node.                             */
/* (5) When back-adjusting verts, snap their adjusted positions unless it is an animmesh node.           */
/* (6) Include the back-adjustment when snapping child nodes.                                            */
/* Change in V3.2.1:                                                                                     */
/* Don't snap the vertices of splotches, just their positions                                            */

apply_snap(File,Model,NodeName,NodeRef) :-
  apply_snap(File,Model,NodeName,NodeRef,[0,0,0]).

apply_snap(File,Model,NodeName,NodeRef,Offset) :-
  retract(gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z))),
  vector_add([X,Y,Z],Offset,[X1,Y1,Z1]),
  once((snap(X1,Xs), snap(Y1,Ys), snap(Z1,Zs))),
  asserta(gotdata(File,Model,NodeName,NodeRef,position(Xs,Ys,Zs))),
  DX is Xs-X, DY is Ys-Y, DZ is Zs-Z,
  (clause(gotdata(File,Model,NodeName,NodeRef,AnimName,position(Xa,Ya,Za)),true,Aref),
   vector_add([Xa,Ya,Za],[DX,DY,DZ],[Xa1,Ya1,Za1]),
   erase(Aref),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,position(Xa1,Ya1,Za1))),
   fail ; true),
  (clause(gotdata(File,Model,NodeName,NodeRef,AnimName2,positionkey(N,T,Xa2,Ya2,Za2)),true,Aref2),
   vector_add([Xa2,Ya2,Za2],[DX,DY,DZ],[Xa3,Ya3,Za3]),
   erase(Aref2),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName2,positionkey(N,T,Xa3,Ya3,Za3))),
   fail ; true),
  gotdata(File,Model,NodeName,NodeRef,orientation(U,V,W,A)),
  A_ is -A, DX_ is X1-Xs, DY_ is Y1-Ys, DZ_ is Z1-Zs,
  f_rotate_vector([DX_,DY_,DZ_],[U,V,W,A_],[Bx,By,Bz]),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  (NodeType\=='animmesh', \+is_splotch(File,Model,NodeName,NodeRef),
   gotdata(File,Model,NodeName,NodeRef,verts(NVerts)), LastVert is NVerts-1,
   between(0,LastVert,V1),
   clause(gotdata(File,Model,NodeName,NodeRef,verts(V1,Xv,Yv,Zv)),true,CRef),
   once((Xv1 is Xv+Bx, snap(Xv1,Xvs), Yv1 is Yv+By, snap(Yv1,Yvs), Zv1 is Zv+Bz, snap(Zv1,Zvs))),
   erase(CRef),
   asserta(gotdata(File,Model,NodeName,NodeRef,verts(V1,Xvs,Yvs,Zvs))),
   (Xvs==Xv, Yvs==Yv, Zvs==Zv -> true ; increment_bug_count(File)),
   fail; true),
  (once((NodeType=='animmesh' ; is_splotch(File,Model,NodeName,NodeRef))),
   gotdata(File,Model,NodeName,NodeRef,verts(NVerts2)), LastVert2 is NVerts2-1,
   between(0,LastVert2,V2),
   clause(gotdata(File,Model,NodeName,NodeRef,verts(V2,Xv2,Yv2,Zv2)),true,CRef2),
   once((Xv3 is Xv2+Bx, Yv3 is Yv2+By, Zv3 is Zv2+Bz)),
   erase(CRef2),
   asserta(gotdata(File,Model,NodeName,NodeRef,verts(V2,Xv3,Yv3,Zv3))),
   (Xv3==Xv2, Yv3==Yv2, Zv3==Zv2 -> true ; increment_bug_count(File)),
   fail; true),
  (gotdata(File,Model,NodeName, NodeRef,AnimName,animverts(NAVerts)), LastAVert is NAVerts-1,
   between(0,LastAVert,AV),
   clause(gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(AV,XA,YA,ZA)),true,CARef),
   once((XA1 is XA+Bx, YA1 is YA+By, ZA1 is ZA+Bz)),
   erase(CARef),
   asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(AV,XA1,YA1,ZA1))),
   (XA1==XA, YA1==YA, ZA1==ZA -> true ; increment_bug_count(File)),
   fail; true),
   !,
  (gotdata(File,Model,Child,ChildRef,parent(NodeName/NodeRef)),
   apply_snap(File,Model,Child,ChildRef,[Bx,By,Bz]),
   fail; true), !.

/* ============== */
/* abs_position/7 */
/* ============== */

abs_position(File,Model,NodeName,NodeRef,X,Y,Z) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z)), !.

abs_position(File,Model,NodeName,NodeRef,X,Y,Z) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent/PRef)), Parent\=Model, PRef\= -1,
  abs_position(File,Model,Parent,PRef,Xp,Yp,Zp),
  gotdata(File,Model,NodeName,NodeRef,position(X1,Y1,Z1)),
  X is Xp+X1, Y is Yp+Y1, Z is Zp+Z1, !.

/* ================== */
/* rename_animation/4 */
/* ================== */

rename_animation(File,Model,OldName,NewName) :-
  retract(gotdata(File,Model,newanim(OldName,Model))),
  asserta(gotdata(File,Model,newanim(NewName,Model))),
  increment_bug_count(File),
  fail.

rename_animation(File,Model,OldName,NewName) :-
  clause(gotdata(File,Model,anim(OldName),Q),true,Eref), erase(Eref),
  asserta(gotdata(File,Model,anim(NewName),Q)),
  increment_bug_count(File),
  fail.

rename_animation(File,Model,OldName,NewName) :-
  clause(gotdata(File,Model,NodeName,NodeRef,OldName,Q),true,Eref), erase(Eref),
  asserta(gotdata(File,Model,NodeName,NodeRef,NewName,Q)),
  increment_bug_count(File),
  fail.

rename_animation(_,_,_,_) :- !.

/* ========================= */
/* shiny_water_replacement/2 */
/* ========================= */

shiny_water_replacement(tbw01_water01,ttf01_water01).
shiny_water_replacement(tdt01_water01,tcn01_water01).
shiny_water_replacement(tni01_water02,tin01_water02).
shiny_water_replacement(tni02_water01,tcn01_water01).
shiny_water_replacement(tno01_water01,tin01_water02).
shiny_water_replacement(tno01_wtsea01,tin01_water02).
shiny_water_replacement(twc03_wtsea01,tin01_water02).
shiny_water_replacement(tdm02_water01,tcn01_water01).

/* ================= */
/* is_watery/4       */
/* is_watery/1       */
/* is_ground/4       */
/* is_ground/1       */
/* is_foliage/4      */
/* is_foliage/1      */
/* is_splotch/4      */
/* is_splotch/1      */
/* is_transparency/4 */
/* is_transparency/1 */
/* ================= */

is_watery(File,Model,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  is_watery(Bitmap).

is_watery(Bitmap) :- shiny_water_replacement(Bitmap,_), !.

is_watery(Bitmap) :-
  once(g_user_option(water_key,W)), downcase_atom(W,W1),
  concat_atom(Keylist,' ',W1),
  member(X,Keylist), X\='',
  sub_atom(Bitmap,_,_,_,X), !.

is_ground(File,Model,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  is_ground(Bitmap).

is_ground(Bitmap) :-
  once(g_user_option(ground_key,W)), downcase_atom(W,W1),
  concat_atom(Keylist,' ',W1),
  member(X,Keylist), X\='',
  sub_atom(Bitmap,_,_,_,X), !.

is_foliage(File,Model,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  is_foliage(Bitmap).

is_foliage(Bitmap) :- sub_atom(Bitmap,_,_,_,'trefol').
is_foliage(Bitmap) :- sub_atom(Bitmap,_,_,_,'treefol').
is_foliage(Bitmap) :- sub_atom(Bitmap,_,_,_,'leaf').
is_foliage(Bitmap) :- sub_atom(Bitmap,_,_,_,'leaves').

is_foliage(Bitmap) :-
  once(g_user_option(foliage_key,K)), downcase_atom(K,K1),
  concat_atom(Keylist,' ',K1),
  member(X,Keylist), X\='',
  sub_atom(Bitmap,_,_,_,X), !.

is_splotch(_,_,_,_) :-
  once(g_user_option(splotch,Splotch)),
  Splotch==ignore, !, fail.

is_splotch(File,Model,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  is_splotch(Bitmap).

is_splotch(Bitmap) :- sub_atom(Bitmap,_,_,_,'splotch').

is_splotch(Bitmap) :-
  once(g_user_option(splotch_key,K)), downcase_atom(K,K1),
  concat_atom(Keylist,' ',K1),
  member(X,Keylist), X\='',
  sub_atom(Bitmap,_,_,_,X), !.

is_transparency(_,_,_,_) :-
  once(g_user_option(placeable_with_transparency,Option)),
  Option==no,
  !, fail.

is_transparency(File,Model,NodeName,NodeRef) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,bitmap(Bitmap)),
  is_transparency(Bitmap).

is_transparency(Bitmap) :-
  once(g_user_option(transparency_key,K)), downcase_atom(K,K1),
  concat_atom(Keylist,' ',K1),
  member(X,Keylist), X\='',
  sub_atom(Bitmap,_,_,_,X), !.

/* ===================== */
/* increment_bug_count/1 */
/* ===================== */

increment_bug_count(File) :-
  clause(bug_count(File,NBugs),true,Eref),
  erase(Eref),
  NBugs1 is NBugs+1,
  asserta(bug_count(File,NBugs1)),
  once((clause(total_fixes(Total),true,Eref2), erase(Eref2) ; Total is 0)),
  Total1 is Total+1,
  asserta(total_fixes(Total1)).

/* ========= */
/* middle/3  */
/* middle/4  */
/* middle3/4 */
/* middle3/5 */
/* ========= */

middle(Bag,Xs,Ys) :- middle(Bag,N,Xtot,Ytot), X is Xtot/N, snap(X,Xs), Y is Ytot/N, snap(Y,Ys).

middle([[_,X,Y]],1,X1,Y1) :- X1 is float(X), Y1 is float(Y), !.
middle([[_,X,Y]|Bag],N1,X1,Y1) :- middle(Bag,Nb,Xb,Yb), N1 is Nb+1, X1 is Xb+X, Y1 is Yb+Y, !.

middle3(Bag,Xs,Ys,Zs) :-
  middle3(Bag,N,Xtot,Ytot,Ztot),
  X is Xtot/N, snap(X,Xs),
  Y is Ytot/N, snap(Y,Ys),
  Z is Ztot/N, snap(Z,Zs).

middle3([[_,X,Y,Z]],1,X1,Y1,Z1) :- X1 is float(X), Y1 is float(Y), Z1 is float(Z), !.
middle3([[_,X,Y,Z]|Bag],N1,X1,Y1,Z1) :- middle3(Bag,Nb,Xb,Yb,Zb), N1 is Nb+1, X1 is Xb+X, Y1 is Yb+Y, Z1 is Zb+Z, !.

/* =========== */
/* same_face/6 */
/* =========== */

same_face(V11,V12,V13,V21,V22,V23) :- V11=V21, V12=V22, V13=V23.
same_face(V11,V12,V13,V21,V22,V23) :- V11=V22, V12=V23, V13=V21.
same_face(V11,V12,V13,V21,V22,V23) :- V11=V23, V12=V21, V13=V22.

/* =================== */
/* is_wrong_tilename/4 */
/* is_special_suffix/3 */
/* =================== */

is_wrong_tilename(Model,BadName,NodeType,GoodName) :-
  name(Model,[T1,T2,T3,T4,T5,95,G1,G2,G3,95,N1,N2]),
  name(BadName,[T1,T2,T3,T4,T5,95,Q1,Q2,Q3,95,Q4,Q5|Suf]),
  [Q1,Q2,Q3,Q4,Q5]\==[G1,G2,G3,N1,N2],
  is_special_suffix(NodeType,Suf,Suffix),
  atom_concat(Model,Suffix,GoodName).

is_wrong_tilename(Model,BadName,NodeType,GoodName) :-
  name(Model,[T1,T2,T3,T4,T5,95,G1,G2,G3,95,N1,N2]),
  name(BadName,[Q1,Q2,Q3,Q4,Q5,95,G1,G2,G3,95,N1,N2|Suf]),
  [Q1,Q2,Q3,Q4,Q5]\==[T1,T2,T3,T4,T5],
  is_special_suffix(NodeType,Suf,Suffix),
  atom_concat(Model,Suffix,GoodName).

is_special_suffix('dummy',[97],'a').            /* a    */
is_special_suffix('light',[109,108,49],'ml1').  /* ml1  */
is_special_suffix('light',[109,108,50],'ml2').  /* ml2  */
is_special_suffix('light',[115,108,49],'sl1').  /* sl1  */
is_special_suffix('light',[115,108,50],'sl2').  /* sl2  */
is_special_suffix('light',[115,108,51],'sl3').  /* sl3  */
is_special_suffix('dummy',[95,68,48,N],Suffix)  :- name(Suffix,[95,68,48,N]). /* _D0? */
is_special_suffix('dummy',[95,100,48,N],Suffix) :- name(Suffix,[95,68,48,N]). /* _d0? */
is_special_suffix('dummy',[95,85,48,N],Suffix)  :- name(Suffix,[95,85,48,N]). /* _U0? */
is_special_suffix('dummy',[95,117,48,N],Suffix) :- name(Suffix,[95,85,48,N]). /* _u0? */

/* =============== */
/* renumber_node/2 */
/* =============== */

renumber_node(OldName,NewName) :-
  NewName==OldName, !, fail.

renumber_node(_,NewName) :-
  nonvar(NewName), !,
  \+ gotdata(_,_,node(_,NewName)).

renumber_node(OldName,NewName) :-
  var(NewName),
  between(10,99,N0), atom_concat(Prefix,N0,OldName),
  between(10,99,N1), N1\=N0, atom_concat(Prefix,N1,NewName),
  \+ gotdata(_,_,node(_,NewName)),
  !.

renumber_node(OldName,NewName) :-
  var(NewName),
  between(1,9,N0), atom_concat(Prefix,N0,OldName),
  between(1,9,N1), N1\=N0, atom_concat(Prefix,N1,NewName),
  \+ gotdata(_,_,node(_,NewName)),
  !.

/* ============ */
/* my_sumlist/2 */
/* ============ */

my_sumlist([],0).
my_sumlist([H|T],N) :- number(H), !, my_sumlist(T,N1), N is N1+H.
my_sumlist([_|T],N) :- my_sumlist(T,N).


/* ========== */
/* perturb/20 */
/* ========== */

perturb(R0,R1,R2,R3,R4,X,Y,Z,Z1,Z2,Z3,Z4) :-
  F0 is R0*2*(5-X)*(5+X)*(5-Y)*(5+Y)/625,
  F1 is 0.2*R1*F0*(X+1.25)*(Y+1.25),
  F2 is 0.2*R2*F0*(X-1.25)*(Y+1.25),
  F3 is 0.2*R3*F0*(X+1.25)*(Y-1.25),
  F4 is 0.2*R4*F0*(X-1.25)*(Y-1.25),

  Z1 is Z+0.66*F0-0.71*F1+0.66*F2+0.50*F3+0.16*F4,
  Z2 is Z+0.14*F0+0.05*F1+0.37*F2-0.70*F3+0.61*F4,
  Z3 is Z-0.22*F0+0.52*F1-0.60*F2-0.30*F3-0.16*F4,
  Z4 is Z-0.58*F0+0.14*F1-0.43*F2+0.50*F3-0.61*F4.
  
/* ==================== */
/* has_chamfer/4        */
/* add_chamfer/4        */
/* delete_chamfer/4     */
/* face_is_chamfer/5    */
/* is_chamfer/3         */
/* chamfer_edge/6       */
/* add_chamfer/6        */
/* add_chamfer_vertex/6 */
/* chamfer_add_vertex/6 */
/* add_chamfer_face/7   */
/* vertex_to_local/6    */
/* abs_face_normal/4    */
/* coplanar/4           */
/* ==================== */

has_chamfer(File,Model,NodeName,NodeRef) :-
  once(gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,1048576,_,_,_,_))),
  t_abs_verts(File,Model,NodeName,NodeRef),
  once(face_is_chamfer(File,Model,NodeName,NodeRef,_)),
  !.

add_chamfer(File,Model,NodeName,NodeRef) :-
  retractall(fixlist(_)),
  t_abs_verts(File,Model,NodeName,NodeRef),
  fail.

add_chamfer(File,Model,NodeName,NodeRef) :-
  chamfer_edge(File,Model,NodeName,NodeRef,V1,V2),
  add_chamfer(File,Model,NodeName,NodeRef,V1,V2),
  asserta(fixlist([V1,V2])),
  fail.

add_chamfer(_,_,NodeName,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0,
  tab(2), write('applied Rosenkrantz Chamfer to '), write(N), write(' edges in '), write(NodeName), nl,
  !.

delete_chamfer(File,Model,NodeName,NodeRef) :-
  t_abs_verts(File,Model,NodeName,NodeRef),
  setof(F,face_is_chamfer(File,Model,NodeName,NodeRef,F),FaceList),
  f_delete_faces_in_list(File,Model,NodeName,NodeRef,FaceList),
  delete_unused_vertices(File,Model,NodeName,NodeRef),
  delete_unused_tverts(File,Model,NodeName,NodeRef),
  (cm3_verbose -> tab(2), write('deleted chamfer faces '), write(FaceList), write(' from '), write(NodeName), nl ; true),
  !.

face_is_chamfer(File,Model,NodeName,NodeRef,F) :-
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,1048576,_,_,_,_)),
  abs_vertex(NodeRef,V1,[X1,Y1,Z1]),
  abs_vertex(NodeRef,V2,[X2,Y2,Z2]),
  abs_vertex(NodeRef,V3,[X3,Y3,Z3]),
  once(is_chamfer([X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3])).

is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X1 =:= -5, X2 =:= -5, X3 < -5.01, X3 > -5.05.  
is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X3 =:= -5, X1 =:= -5, X2 < -5.01, X2 > -5.05.  
is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X2 =:= -5, X3 =:= -5, X1 < -5.01, X1 > -5.05.  
is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X1 =:=  5, X2 =:=  5, X3 <  5.05, X3 >  5.01.  
is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X3 =:=  5, X1 =:=  5, X2 <  5.05, X2 >  5.01.  
is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X2 =:=  5, X3 =:=  5, X1 <  5.05, X1 >  5.01.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y1 =:= -5, Y2 =:= -5, Y3 < -5.01, Y3 > -5.05.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y3 =:= -5, Y1 =:= -5, Y2 < -5.01, Y2 > -5.05.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y2 =:= -5, Y3 =:= -5, Y1 < -5.01, Y1 > -5.05.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y1 =:=  5, Y2 =:=  5, Y3 <  5.05, Y3 >  5.01.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y3 =:=  5, Y1 =:=  5, Y2 <  5.05, Y2 >  5.01.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y2 =:=  5, Y3 =:=  5, Y1 <  5.05, Y1 >  5.01.  

is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X1 =:= -5, X2 =:= X3, X3 < -5.01, X3 > -5.05.  
is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X3 =:= -5, X1 =:= X2, X2 < -5.01, X2 > -5.05.  
is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X2 =:= -5, X3 =:= X1, X1 < -5.01, X1 > -5.05.  
is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X1 =:=  5, X2 =:= X3, X3 <  5.05, X3 >  5.01.  
is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X3 =:=  5, X1 =:= X2, X2 <  5.05, X2 >  5.01.  
is_chamfer([X1,_,_],[X2,_,_],[X3,_,_]) :- X2 =:=  5, X3 =:= X1, X1 <  5.05, X1 >  5.01.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y1 =:= -5, Y2 =:= Y3, Y3 < -5.01, Y3 > -5.05.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y3 =:= -5, Y1 =:= Y2, Y2 < -5.01, Y2 > -5.05.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y2 =:= -5, Y3 =:= Y1, Y1 < -5.01, Y1 > -5.05.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y1 =:=  5, Y2 =:= Y3, Y3 <  5.05, Y3 >  5.01.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y3 =:=  5, Y1 =:= Y2, Y2 <  5.05, Y2 >  5.01.  
is_chamfer([_,Y1,_],[_,Y2,_],[_,Y3,_]) :- Y2 =:=  5, Y3 =:= Y1, Y1 <  5.05, Y1 >  5.01.  

chamfer_edge(File,Model,NodeName,NodeRef,V1,V2) :-
  abs_vertex(NodeRef,V1,[X1,Y1,Z1]), abs(X1) =:= 5,
  clockwise_edge(File,Model,NodeName,NodeRef,V1,V2,V3),
  abs_vertex(NodeRef,V2,[X2,Y2,Z2]), X2 =:= X1,
  \+ clockwise_edge(File,Model,NodeName,NodeRef,V2,V1),
  abs_vertex(NodeRef,V3,[X3,Y3,Z3]),
  abs_face_normal([X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3],[U,_,W]),
  abs(U) < 0.5, W > 0.866.

chamfer_edge(File,Model,NodeName,NodeRef,V1,V2) :-
  abs_vertex(NodeRef,V1,[X1,Y1,Z1]), abs(Y1) =:= 5,
  clockwise_edge(File,Model,NodeName,NodeRef,V1,V2,V3),
  abs_vertex(NodeRef,V2,[X2,Y2,Z2]), Y2 =:= Y1,
  \+ clockwise_edge(File,Model,NodeName,NodeRef,V2,V1),
  abs_vertex(NodeRef,V3,[X3,Y3,Z3]),
  abs_face_normal([X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3],[_,V,W]),
  abs(V) < 0.5, W > 0.866.

add_chamfer(File,Model,NodeName,NodeRef,V1,V2) :-
  abs_vertex(NodeRef,V1,[X1,Y1,Z1]),
  add_chamfer_vertex(File,Model,NodeName,NodeRef,[X1,Y1,Z1],V3),
  abs_vertex(NodeRef,V2,[X2,Y2,Z2]),
  add_chamfer_vertex(File,Model,NodeName,NodeRef,[X2,Y2,Z2],V4),
  add_chamfer_face(File,Model,NodeName,NodeRef,V1,V3,V2),
  add_chamfer_face(File,Model,NodeName,NodeRef,V3,V4,V2).

add_chamfer_vertex(File,Model,NodeName,NodeRef,[X1,Y1,Z1],V2) :-
  snap(0.03,Delta),
  (X1 =:= -5 -> X2 is X1-Delta; X1 =:= 5 -> X2 is X1+Delta; X2 is X1),
  (Y1 =:= -5 -> Y2 is Y1-Delta; Y1 =:= 5 -> Y2 is Y1+Delta; Y2 is Y1),
  Z2 is Z1-Delta,
  vertex_to_local(File,Model,NodeName,NodeRef,[X2,Y2,Z2],[X3,Y3,Z3]),
  chamfer_add_vertex(File,Model,NodeName,NodeRef,[X2,Y2,Z2],[X3,Y3,Z3],V2).

chamfer_add_vertex(File,Model,NodeName,NodeRef,_,[X,Y,Z],V) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V,X1,Y1,Z1)),
  X1=:=X, Y1=:=Y, Z1=:=Z, !.

chamfer_add_vertex(File,Model,NodeName,NodeRef,Abs,[X,Y,Z],V) :-
  retract(gotdata(File,Model,NodeName,NodeRef,verts(V))),
  asserta(gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,Z))),
  NewNVerts is V+1,
  asserta(gotdata(File,Model,NodeName,NodeRef,verts(NewNVerts))),
  asserta(abs_vertex(NodeRef,V,Abs)),
  (retract(gotdata(File,Model,NodeName,NodeRef,colors(V))) ->
     asserta(gotdata(File,Model,NodeName,NodeRef,colors(V,255,255,255))),
     asserta(gotdata(File,Model,NodeName,NodeRef,colors(NewNVerts)))
     ;
     true
  ).

add_chamfer_face(File,Model,NodeName,NodeRef,V1,V2,V3) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(NFaces,V1,V2,V3,1048576,0,1,2,21))),
  NewNFaces is NFaces+1,
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))).

vertex_to_local(File,Model,NodeName,NodeRef,[X1,Y1,Z1],[X2,Y2,Z2]) :-
  abs_vertex(NodeRef,V,[X1a,Y1a,Z1a]), snap(X1a,X1), snap(Y1a,Y1), snap(Z1a,Z1),
  gotdata(File,Model,NodeName,NodeRef,verts(V,X2,Y2,Z2)),
  !.

vertex_to_local(File,Model,NodeName,NodeRef,AbsVector,Vector) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Model)),
  (gotdata(File,Model,NodeName,NodeRef,position(Xp,Yp,Zp)) ->
    vector_subtract(AbsVector,[Xp,Yp,Zp],Vector1); Vector1=AbsVector),
  (gotdata(File,Model,NodeName,NodeRef,orientation(Xa,Ya,Za,A)) ->
    f_rotate_vector(Vector1,[Xa,Ya,Za,-A],Vector) ; Vector=Vector1),
  !.

vertex_to_local(File,Model,NodeName,NodeRef,AbsVector,Vector) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent/PRef)), Parent\=Model, Parent\='NULL', PRef\= -1,
  clause(gotdata(File,Model,node(_,Parent)),true,PRef),
  vertex_to_local(File,Model,Parent,PRef,AbsVector,V1),
  (gotdata(File,Model,NodeName,NodeRef,position(Xp,Yp,Zp)) ->
    vector_subtract(V1,[Xp,Yp,Zp],V2); V2=V1),
  (gotdata(File,Model,NodeName,NodeRef,orientation(Xa,Ya,Za,A)) ->
    f_rotate_vector(V2,[Xa,Ya,Za,-A],Vector) ; Vector=V2),
  !.

abs_face_normal([X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3],N) :-
  vector_subtract([X2,Y2,Z2],[X1,Y1,Z1],V12),
  vector_subtract([X3,Y3,Z3],[X1,Y1,Z1],V13),
  vector_cross_product(V12,V13,V123),
  vector_normalise(V123,N).

coplanar(P1,P2,P3,P4) :-
  vector_subtract(P2,P1,P12),
  vector_subtract(P3,P1,P13),
  vector_subtract(P4,P1,P14),
  vector_cross_product(P12,P13,P123), vector_magnitude(P123,R123),
  vector_dot_product(P123,P14,D),
  vector_magnitude(P14,R14),
  D < 0.1*R123*R14,
  vector_cross_product(P12,P14,P124), vector_magnitude(P124,R124),
  vector_dot_product(P123,P124,A),
  A > 0.9*R123*R124.

/* ============ */
/* raise_tile/3 */
/* ============ */

raise_tile(File,Model,Offset) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Model/_)),
  \+ atom_concat(Model,'a',NodeName),
  \+ clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  retract(gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z))),
  Z1 is Z+Offset,
  asserta(gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z1))),
  increment_bug_count(File),
  fail.

raise_tile(File,Model,Offset) :-
  clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef),
  (gotdata(File,Model,NodeName,NodeRef,verts(Nverts)), Nverts>0,
    LastVert is Nverts-1,
    between(0,LastVert,N),
    retract(gotdata(File,Model,NodeName,NodeRef,verts(N,X,Y,Z))),
    Z1 is Z+Offset,
    asserta(gotdata(File,Model,NodeName,NodeRef,verts(N,X,Y,Z1))),
    increment_bug_count(File),
    fail ; true),
  (cm3_verbose -> tab(2), write('walkmesh vertices adjusted for raise/lower'), nl ; true),
  fail.

raise_tile(File,Model,Offset) :-
  atom_concat(Model,'a',ANodeName),
  clause(gotdata(File,Model,node(dummy,ANodeName)),true,ARef),
  gotdata(File,Model,NodeName,NodeRef,parent(ANodeName/ARef)),
  \+ clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  retract(gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z))),
  Z1 is Z+Offset,
  asserta(gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z1))),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(NKeys)),
    LastKey is NKeys-1,
    between(0,LastKey,Na),
    retract(gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(Na,Ta,Xa,Ya,Za))),
    Za1 is Za+Offset,
    asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(Na,Ta,Xa,Ya,Za1))),
    increment_bug_count(File),
    fail ; true),
  fail.

raise_tile(File,Model,Offset) :-
  atom_concat(Model,'a',ANodeName),
  clause(gotdata(File,Model,node(dummy,ANodeName)),true,ARef),
  gotdata(File,Model,NodeName,NodeRef,parent(ANodeName/ARef)),
  clause(gotdata(File,Model,node(animmesh,NodeName)),true,NodeRef),
  (gotdata(File,Model,NodeName,NodeRef,verts(Nverts)), Nverts>0,
    LastVert is Nverts-1,
    between(0,LastVert,N),
    retract(gotdata(File,Model,NodeName,NodeRef,verts(N,X,Y,Z))),
    Z1 is Z+Offset,
    asserta(gotdata(File,Model,NodeName,NodeRef,verts(N,X,Y,Z1))),
    increment_bug_count(File),
    fail ; true),
  (gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(Nn)),
    LastAvert is Nn-1,
    between(0,LastAvert,Nb),
    retract(gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(Nb,Xb,Yb,Zb))),
    Zb1 is Zb+Offset,
    asserta(gotdata(File,Model,NodeName,NodeRef,AnimName,animverts(Nb,Xb,Yb,Zb1))),
    increment_bug_count(File),
    fail ; true),
  fail.

raise_tile(File,Model,Offset) :-
  tab(2), write(Model),
  (Offset<0 -> write(' lowered by '); write(' raised by ')),
  AbsOffset is abs(Offset), write(AbsOffset), write('m'), nl,
  (clause(gotdata(File,Model,node(aabb,NodeName)),true,NodeRef) -> w_rebuild_aabb(File,Model,NodeName,NodeRef) ; true).
  
/* ================== */
/* remap_uv_to_tile/6 */
/* ================== */
  
remap_uv_to_tile(File,Model,NodeName,NodeRef,RepeatCount) :-
  gotdata(File,Model,NodeName,NodeRef,verts(NVerts)), LastVert is NVerts-1,
  clause(gotdata(File,Model,NodeName,NodeRef,tverts(NTverts)),true,Tref),
  (NTverts=:=NVerts ->
    true
    ;
    erase(Tref),
    asserta(gotdata(File,Model,NodeName,NodeRef,tverts(NVerts))),
    increment_bug_count(File)
  ), 
  retractall(gotdata(File,Model,NodeName,NodeRef,tverts(_,_,_,_))),
  t_abs_verts(File,Model,NodeName,NodeRef),
  (between(0,LastVert,N),
   abs_vertex(NodeRef,N,[X,Y,_]),
   U is (X+5)*RepeatCount*0.1,
   V is (Y+5)*RepeatCount*0.1,
   asserta(gotdata(File,Model,NodeName,NodeRef,tverts(N,U,V,0))),
   increment_bug_count(File),
   fail; true),
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)), LastFace is NFaces-1,
  (between(0,LastFace,F),
   retract(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,_,_,_,M))),
   asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,V1,V2,V3,M))),
   increment_bug_count(File),
   fail; true).

/* ========================== */
/* is_overlapping_face_pair/5 */
/* overlap_type/5             */
/* ========================== */

is_overlapping_face_pair(File,Model,NodeName,NodeRef,[F1,F2,V1,V2,V3,V4,Type]) :-
  (
   gotdata(File,Model,NodeName,NodeRef,faces(F1,V1,V2,V3,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(F1,V3,V1,V2,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(F1,V2,V3,V1,_,_,_,_,_))
  ),
  (
   gotdata(File,Model,NodeName,NodeRef,faces(F2,V1,V2,V4,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(F2,V4,V1,V2,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(F2,V2,V4,V1,_,_,_,_,_))
  ), F2@>F1, V4\=V3,
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,Z3)),
  gotdata(File,Model,NodeName,NodeRef,verts(V4,X4,Y4,Z4)),
  coplanar([X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3],[X4,Y4,Z4]),
  overlap_type([X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3],[X4,Y4,Z4],Type),
  (cm3_verbose -> tab(2), write(overlap_type(F1,F2,Type)), nl ; true).

overlap_type(A1,A2,A3,A4,type(T1,T2)) :-
  vector_subtract(A2,A1,A12),
  vector_subtract(A3,A1,A13),
  vector_cross_product(A12,A13,A213), vector_normalise(A213,N),
  vector_subtract(A4,A1,A14),
  vector_cross_product(A14,A13,A413), vector_normalise(A413,N413),
  (N413=[] ->
     T1=zero
    ;
     vector_dot_product(N413,N,S1),
     (S1 >= 0.001 -> T1=plus ; S1 =< -0.001 -> T1=minus ; T1=zero)
  ),
  vector_subtract(A4,A2,A24),
  vector_subtract(A3,A2,A23),
  vector_cross_product(A23,A24,A324), vector_normalise(A324,N324),
  (N324=[] ->
     T2=zero
    ;
     vector_dot_product(N324,N,S2),
     (S2 >= 0.001 -> T2=plus ; S2 =< -0.001 -> T2=minus ; T2=zero)
  ), !.

overlap_type(_,_,_,_,unknown).

/* ================== */
/* fix_aabb_overlap/5 */
/* ================== */

/*
fix_aabb_overlap(_,_,_,_,[_,_,_,_,_,_,unknown]) :- !, fail.
fix_aabb_overlap(_,_,_,_,[_,_,_,_,_,_,type(zero,zero)]) :- !, fail.

fix_aabb_overlap(File,Model,NodeName,NodeRef,[F1,F2,V1,V2,V3,V4,type(minus,zero)]) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(F2,_,_,_,S,_,_,_,M)),true,Cref),
  erase(Cref),
  !, asserta(gotdata(File,Model,NodeName,NodeRef,faces(F2,V1,V3,V4,S,0,0,0,M))).

fix_aabb_overlap(File,Model,NodeName,NodeRef,[F1,F2,V1,V2,V3,V4,type(zero,minus)]) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(F2,_,_,_,S,_,_,_,M)),true,Cref),
  erase(Cref),
  !, asserta(gotdata(File,Model,NodeName,NodeRef,faces(F2,V3,V2,V4,S,0,0,0,M))).

fix_aabb_overlap(File,Model,NodeName,NodeRef,[F1,F2,V1,V2,V3,V4,type(plus,zero)]) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(F1,_,_,_,S,_,_,_,M)),true,Cref),
  erase(Cref),
  !, asserta(gotdata(File,Model,NodeName,NodeRef,faces(F1,V1,V4,V3,S,0,0,0,M))).

fix_aabb_overlap(File,Model,NodeName,NodeRef,[F1,F2,V1,V2,V3,V4,type(zero,plus)]) :-
  /* erase F1 (V1V2V3) and replace it with F1 (V4V2V3) but keep the same material */
  clause(gotdata(File,Model,NodeName,NodeRef,faces(F1,_,_,_,S,_,_,_,M)),true,Cref),
  erase(Cref),
  !, asserta(gotdata(File,Model,NodeName,NodeRef,faces(F1,V4,V2,V3,S,0,0,0,M))).

fix_aabb_overlap(File,Model,NodeName,NodeRef,[F1,F2,V1,V2,V3,V4,type(minus,minus)]) :-
  fix_aabb_overlap(File,Model,NodeName,NodeRef,[F2,F1,V1,V2,V4,V3,type(plus,plus)]).

fix_aabb_overlap(File,Model,NodeName,NodeRef,[F1,F2,V1,V2,V3,V4,type(minus,plus)]) :-
  fix_aabb_overlap(File,Model,NodeName,NodeRef,[F2,F1,V1,V2,V4,V3,type(plus,minus)]).

fix_aabb_overlap(File,Model,NodeName,NodeRef,[F1,F2,V1,V2,V3,V4,type(plus,minus)]) :-
  /* find or create a vertex V5 at the intersection of  V1V4 and V2V3 */
  /* erase F1 (V1V2V3) and replace it with F1 (V1V5V3) */
  /* erase F2 (V1V2V4) and replace it with F2 (V5V2V4) */
  /* find or create a new face F3 (V1V2V5) */
  fail.
   
fix_aabb_overlap(File,Model,NodeName,NodeRef,[F1,F2,V1,V2,V3,V4,type(plus,plus)]) :-
  /* erase F1 (V1V2V3) and replace it with F1 (V1V4V3) */
  /* keep F2 (V1V2V4) */
  /* find or create a face F3 (V2V3V4) */
  fail.
*/

/* =================== */
/* make_3D/2           */
/* make_3D_backplane/7 */
/* make_3D_pushverts/4 */
/* =================== */

make_3D('tced0_creambrick','tced0_creambr_2_').
make_3D('tced0_creamstone','tced0_creamst_2_').
make_3D('tced4_stone01','tced4_stone01_2_').

make_3D_backplane(File,Model,NodeName,NodeRef,OverlayName,BaseMap,OverlayMap) :-
  asserta(gotdata(File,Model,node(trimesh,OverlayName)),OverlayRef),
  increment_bug_count(File),
  (
    gotdata(File,Model,NodeName,NodeRef,Q),
    asserta(gotdata(File,Model,OverlayName,OverlayRef,Q)),
    increment_bug_count(File),
    fail ; true
  ),
  /* Modify the original mesh to be the back layer */
  retractall(gotdata(File,Model,NodeName,NodeRef,ambient(_,_,_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,ambient(0.7,0.7,0.7))),
  retractall(gotdata(File,Model,NodeName,NodeRef,diffuse(_,_,_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,diffuse(0.7,0.7,0.7))),
  /* Modify the _2_ clone to be the overlay */
  retract(gotdata(File,Model,OverlayName,OverlayRef,bitmap(BaseMap))),
  asserta(gotdata(File,Model,OverlayName,OverlayRef,bitmap(OverlayMap))),
  make_3D_pushverts(File,Model,OverlayName,OverlayRef),
  retractall(gotdata(File,Model,OverlayName,OverlayRef,shadow(_))),
  asserta(gotdata(File,Model,OverlayName,OverlayRef,shadow(0))).

make_3D_pushverts(File,Model,NodeName,NodeRef) :-
  prepare_face_normals(File,Model,NodeName,NodeRef),
  gotdata(File,Model,NodeName,NodeRef,verts(NVerts)),
  LastVert is NVerts-1,
  (
    between(0,LastVert,V),
    make_3D_delta(File,Model,NodeName,NodeRef,V,[Dx,Dy,Dz]),
    retract(gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,Z))),
    X1 is X+Dx, snap(X1,Xs),
    Y1 is Y+Dy, snap(Y1,Ys),
    Z1 is Z+Dz, snap(Z1,Zs),
    asserta(gotdata(File,Model,NodeName,NodeRef,verts(V,Xs,Ys,Zs))),
    (cm3_verbose -> tab(2), write('pushed vertex '), write(V), write(' by '), write([Dx,Dy,Dz]), nl ; true),
    fail
    ;
    true
  ).

make_3D_delta(File,Model,NodeName,NodeRef,V,[Dx,Dy,Dz]) :-
  make_3D_deltaX(File,Model,NodeName,NodeRef,V,Dx),
  make_3D_deltaY(File,Model,NodeName,NodeRef,V,Dy),
  make_3D_deltaZ(File,Model,NodeName,NodeRef,V,Dz),
  !.

make_3D_deltaX(File,Model,NodeName,NodeRef,V,0) :-
  is_vertex_in_face(File,Model,NodeName,NodeRef,F1,V),
  face_normal(File,Model,NodeName,NodeRef,F1,[N1,_,_]),
  N1 > 0.1,
  is_vertex_in_face(File,Model,NodeName,NodeRef,F2,V),
  face_normal(File,Model,NodeName,NodeRef,F2,[N2,_,_]),
  N2 < -0.1, !.

make_3D_deltaX(File,Model,NodeName,NodeRef,V,0.01) :-
  is_vertex_in_face(File,Model,NodeName,NodeRef,F,V),
  face_normal(File,Model,NodeName,NodeRef,F,[N,_,_]),
  N > 0.1, !.

make_3D_deltaX(File,Model,NodeName,NodeRef,V,-0.01) :-
  is_vertex_in_face(File,Model,NodeName,NodeRef,F,V),
  face_normal(File,Model,NodeName,NodeRef,F,[N,_,_]),
  N < -0.1, !.

make_3D_deltaX(_,_,_,_,_,0).

make_3D_deltaY(File,Model,NodeName,NodeRef,V,0) :-
  is_vertex_in_face(File,Model,NodeName,NodeRef,F1,V),
  face_normal(File,Model,NodeName,NodeRef,F1,[_,N1,_]),
  N1 > 0.1,
  is_vertex_in_face(File,Model,NodeName,NodeRef,F2,V),
  face_normal(File,Model,NodeName,NodeRef,F2,[_,N2,_]),
  N2 < -0.1, !.

make_3D_deltaY(File,Model,NodeName,NodeRef,V,0.01) :-
  is_vertex_in_face(File,Model,NodeName,NodeRef,F,V),
  face_normal(File,Model,NodeName,NodeRef,F,[_,N,_]),
  N > 0.1, !.

make_3D_deltaY(File,Model,NodeName,NodeRef,V,-0.01) :-
  is_vertex_in_face(File,Model,NodeName,NodeRef,F,V),
  face_normal(File,Model,NodeName,NodeRef,F,[_,N,_]),
  N < -0.1, !.

make_3D_deltaY(_,_,_,_,_,0).

make_3D_deltaZ(File,Model,NodeName,NodeRef,V,0) :-
  is_vertex_in_face(File,Model,NodeName,NodeRef,F1,V),
  face_normal(File,Model,NodeName,NodeRef,F1,[_,_,N1]),
  N1 > 0.1,
  is_vertex_in_face(File,Model,NodeName,NodeRef,F2,V),
  face_normal(File,Model,NodeName,NodeRef,F2,[_,_,N2]),
  N2 < -0.1, !.

make_3D_deltaZ(File,Model,NodeName,NodeRef,V,0.01) :-
  is_vertex_in_face(File,Model,NodeName,NodeRef,F,V),
  face_normal(File,Model,NodeName,NodeRef,F,[_,_,N]),
  N > 0.1, !.

make_3D_deltaZ(File,Model,NodeName,NodeRef,V,-0.01) :-
  is_vertex_in_face(File,Model,NodeName,NodeRef,F,V),
  face_normal(File,Model,NodeName,NodeRef,F,[_,_,N]),
  N < -0.1, !.

make_3D_deltaZ(File,Model,NodeName,NodeRef,V,0.01) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V,_,_,Z)),
  \+ (gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z1)), Z1<Z), !.

make_3D_deltaZ(File,Model,NodeName,NodeRef,V,-0.01) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V,_,_,Z)),
  \+ (gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z1)), Z1>Z), !.

make_3D_deltaZ(_,_,_,_,_,0).

make_3D_fix_edges(File,Model,OverlayMap) :-
  make_3D(BaseMap,OverlayMap),
  gotdata(File,Model,NodeName,NodeRef,bitmap(OverlayMap)),
  atom_concat(BaseNode,'_2_',NodeName),
  gotdata(File,Model,BaseNode,BaseRef,bitmap(BaseMap)),
  gotdata(File,Model,BaseNode1,BaseRef1,bitmap(BaseMap)), BaseRef1@>BaseRef,
  atom_concat(BaseNode1,'_2_',NodeName1),
  gotdata(File,Model,NodeName1,NodeRef1,bitmap(OverlayMap)),
  setof([V,V1],coincident_verts(File,Model,BaseNode,BaseRef,BaseNode1,BaseRef1,[V,V1]),VertList),
  make_3D_join_verts(NodeRef,NodeRef1,BaseRef,BaseRef1,VertList),
  fail ; true.

coincident_verts(File,Model,BaseNode,BaseRef,BaseNode1,BaseRef1,[V,V1]) :-
  gotdata(File,Model,BaseNode,BaseRef,verts(V,X,Y,Z)),
  relate_to_tile(File,Model,BaseNode,BaseRef,[X,Y,Z],[X0,Y0,Z0]),
  gotdata(File,Model,BaseNode1,BaseRef1,verts(V1,X1,Y1,Z1)),
  relate_to_tile(File,Model,BaseNode1,BaseRef1,[X1,Y1,Z1],[X00,Y00,Z00]),
  abs(X00-X0)<0.005, abs(Y00-Y0)<0.005, abs(Z00-Z0)<0.005.

make_3D_join_verts(_,_,_,_,[]) :- !.

make_3D_join_verts(NodeRef,NodeRef1,BaseRef,BaseRef1,[Pair|More]) :-
  make_3D_join_vert_pair(NodeRef,NodeRef1,BaseRef,BaseRef1,Pair),
  !,
  make_3D_join_verts(NodeRef,NodeRef1,BaseRef,BaseRef1,More).

make_3D_join_vert_pair(NodeRef,NodeRef1,BaseRef,BaseRef1,[V,V1]) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,Z)),
  gotdata(File,Model,NodeName1,NodeRef1,verts(V1,X1,Y1,Z1)),
  gotdata(_,_,_,BaseRef,verts(V,Xb,Yb,Zb)),
  gotdata(_,_,_,BaseRef1,verts(V1,Xb1,Yb1,Zb1)),
  combine_delta(X-Xb,X1-Xb1,Dx),
  combine_delta(Y-Yb,Y1-Yb1,Dy),
  combine_delta(Z-Zb,Z1-Zb1,Dz),
  Xnew is Xb+Dx, snap(Xnew,Xs),
  Ynew is Yb+Dy, snap(Ynew,Ys),
  Znew is Zb+Dz, snap(Znew,Zs),
  ( Xs=:=X, Ys=:=Y, Zs=:=Z -> true;
    retract(gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,Z))),
    asserta(gotdata(File,Model,NodeName,NodeRef,verts(V,Xs,Ys,Zs))),
    increment_bug_count(File),
    (cm3_verbose -> tab(2), write('tweaked vertex '), write(NodeName/V), nl ; true)
  ),
  X1new is Xb1+Dx, snap(X1new,X1s),
  Y1new is Yb1+Dy, snap(Y1new,Y1s),
  Z1new is Zb1+Dz, snap(Z1new,Z1s),
  ( X1s=:=X1, Y1s=:=Y1, Z1s=:=Z1 -> true;
    retract(gotdata(File,Model,NodeName1,NodeRef1,verts(V1,X1,Y1,Z1))),
    asserta(gotdata(File,Model,NodeName1,NodeRef1,verts(V1,X1s,Y1s,Z1s))),
    increment_bug_count(File),
    (cm3_verbose -> tab(2), write('tweaked vertex '), write(NodeName1/V1), nl ; true )
  ).

combine_delta(D1,D2,D2) :- D1>=0, D2>=D1, !.
combine_delta(D1,D2,D1) :- D2>=0, D1>=D2, !.
combine_delta(D1,D2,D2) :- D1=<0, D2=<D1, !.
combine_delta(D1,D2,D1) :- D2=<0, D1=<D2, !.
combine_delta(_,_,0).

/* ================ */
/* tesselate_mesh/5 */
/* ================ */

tesselate_mesh(File,Model,NodeName,NodeRef,EdgeMax) :-
  setof(E,long_edge(File,Model,NodeName,NodeRef,EdgeMax,E),EdgeList),
  tesselate_mesh(File,Model,NodeName,NodeRef,EdgeMax,EdgeList).

long_edge(File,Model,NodeName,NodeRef,EdgeMax,[V1,V2,L]) :-
  face_edge(File,Model,NodeName,NodeRef,V1,V2),
  V2>V1,
  edge_length(File,Model,NodeName,NodeRef,V1,V2,L),
  L>EdgeMax.

face_edge(File,Model,NodeName,NodeRef,V1,V2) :- gotdata(File,Model,NodeName,NodeRef,faces(_,V1,V2,_,_,_,_,_,_)).
face_edge(File,Model,NodeName,NodeRef,V1,V2) :- gotdata(File,Model,NodeName,NodeRef,faces(_,_,V1,V2,_,_,_,_,_)).
face_edge(File,Model,NodeName,NodeRef,V1,V2) :- gotdata(File,Model,NodeName,NodeRef,faces(_,V2,_,V1,_,_,_,_,_)).
face_edge(File,Model,NodeName,NodeRef,V1,V2) :- gotdata(File,Model,NodeName,NodeRef,faces(_,V2,V1,_,_,_,_,_,_)).
face_edge(File,Model,NodeName,NodeRef,V1,V2) :- gotdata(File,Model,NodeName,NodeRef,faces(_,_,V2,V1,_,_,_,_,_)).
face_edge(File,Model,NodeName,NodeRef,V1,V2) :- gotdata(File,Model,NodeName,NodeRef,faces(_,V1,_,V2,_,_,_,_,_)).

edge_length(File,Model,NodeName,NodeRef,V1,V2,L) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  vector_subtract([X2,Y2,Z2],[X1,Y1,Z1],D),
  vector_magnitude(D,L).

tesselate_mesh(_,_,_,_,_,[]) :- !.

tesselate_mesh(File,Model,NodeName,NodeRef,EdgeMax,[[V1,V2,L]|List]) :-
  make_midpoint_vertex(File,Model,NodeName,NodeRef,V1,V2,V12),
  bisect_edge(File,Model,NodeName,NodeRef,V1,V2,V12,V3),
  bisect_edge(File,Model,NodeName,NodeRef,V2,V1,V12,V4),
  L1 is L/2,
  (L1>EdgeMax -> append(List,[[V1,V12,L1],[V2,V12,L1]],List1) ; List1=List),
  (V3 \= -1, edge_length(File,Model,NodeName,NodeRef,V3,V12,L3), L3>EdgeMax ->
   append(List1,[[V3,V12,L3]],List2) ; List2=List1),
  (V4 \= -1, edge_length(File,Model,NodeName,NodeRef,V4,V12,L4), L4>EdgeMax ->
   append(List2,[[V4,V12,L4]],List3) ; List3=List2),
  !,
  tesselate_mesh(File,Model,NodeName,NodeRef,EdgeMax,List3).

/* Warning - make_midpoint_vertex does not maintain colors  */
/* but there shouldn't be any, on animmesh wavywater planes */

make_midpoint_vertex(File,Model,NodeName,NodeRef,V1,V2,V12) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  X is (X1+X2)/2, Y is (Y1+Y2)/2, Z is (Z1+Z2)/2,
  snap(X,Xs), snap(Y,Ys), snap(Z,Zs),
  (gotdata(File,Model,NodeName,NodeRef,verts(V12,Xs,Ys,Zs)) -> true ;
     retract(gotdata(File,Model,NodeName,NodeRef,verts(V12))),
     asserta(gotdata(File,Model,NodeName,NodeRef,verts(V12,Xs,Ys,Zs))),
     NVerts is V12+1,
     asserta(gotdata(File,Model,NodeName,NodeRef,verts(NVerts)))
  ).

bisect_edge(File,Model,NodeName,NodeRef,V1,V2,V12,V3) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,G,T1,T2,T3,S)),true,CRef),
  make_midpoint_tvert(File,Model,NodeName,NodeRef,T1,T2,T12),
  erase(CRef),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(N1))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V12,V3,G,T1,T12,T3,S))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N1,V12,V2,V3,G,T12,T2,T3,S))),
  N2 is N1+1,
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N2))),
  (cm3_verbose -> tab(2), write('edge '), write(V1-V2), write(' divided'), nl ; true),
  !.

bisect_edge(File,Model,NodeName,NodeRef,V1,V2,V12,V3) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(F,V3,V1,V2,G,T3,T1,T2,S)),true,CRef),
  make_midpoint_tvert(File,Model,NodeName,NodeRef,T1,T2,T12),
  erase(CRef),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(N1))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V3,V1,V12,G,T3,T1,T12,S))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N1,V3,V12,V2,G,T3,T12,T2,S))),
  N2 is N1+1,
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N2))),
  (cm3_verbose -> tab(2), write('edge '), write(V1-V2), write(' divided'), nl ; true),
  !.

bisect_edge(File,Model,NodeName,NodeRef,V1,V2,V12,V3) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(F,V2,V3,V1,G,T2,T3,T1,S)),true,CRef),
  make_midpoint_tvert(File,Model,NodeName,NodeRef,T1,T2,T12),
  erase(CRef),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(N1))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(F,V12,V3,V1,G,T12,T3,T1,S))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N1,V2,V3,V12,G,T2,T3,T12,S))),
  N2 is N1+1,
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N2))),
  (cm3_verbose -> tab(2), write('edge '), write(V1-V2), write(' divided'), nl ; true),
  !.

bisect_edge(_,_,_,_,_,_,_,-1).

make_midpoint_tvert(File,Model,NodeName,NodeRef,T1,T2,T12) :-
  gotdata(File,Model,NodeName,NodeRef,tverts(T1,U1,V1,_)),
  gotdata(File,Model,NodeName,NodeRef,tverts(T2,U2,V2,_)),
  U is (U1+U2)/2, V is (V1+V2)/2,
  /** You might want to do tvert snapping here **/
  (gotdata(File,Model,NodeName,NodeRef,tverts(T12,U,V,_)) -> true ;
     retract(gotdata(File,Model,NodeName,NodeRef,tverts(T12))),
     asserta(gotdata(File,Model,NodeName,NodeRef,tverts(T12,U,V,0))),
     NTVerts is T12+1,
     asserta(gotdata(File,Model,NodeName,NodeRef,tverts(NTVerts)))
  ).

/* ============== */
/* incompatible/4 */
/* ============== */

incompatible(nwnmdlcomp,Model,Q0,error) :-
  Catchlist = [alphamid,colormid,percentstart,percentmid,percentend,sizemid,sizemid_y],
  member(Q0,Catchlist),  Q=..[Q0,_],
  once(gotdata(_,Model,_,_,Q)).

incompatible(nwnmdlcomp,Model,Key,error) :-
  Catchlist = [alphamid,colormid,percentstart,percentmid,percentend,sizemid,sizemid_y],
  member(Q0,Catchlist),  atom_concat(Q0,'key',Key), Q=..[Key,_],
  once(gotdata(_,Model,_,_,_,Q)).

incompatible(engine,Model,Q0,ignore) :-
  Catchlist = [lightningsubdiv,negativelight,lensflares,gizmo,texindices0,clipu,clipv,clipw,cliph],
  member(Q0,Catchlist),  Q=..[Q0,_],
  once(gotdata(_,Model,_,_,Q)).

incompatible(engine,Model,Q0,ignore) :-
  Catchlist = [showdispl,displtype,p2p_type,render_sel,blend_sel,update_sel,spawntype_sel,opacity,iconsize,lockaxes,chunky],
  member(Q0,Catchlist),  Q=..[Q0,_],
  once(gotdata(_,Model,_,_,Q)).

incompatible(engine,Model,Key,ignore) :-
  Catchlist = [gizmokey,lightningsubdivkey],
  member(Key,Catchlist), Q=..[Key,_],
  once(gotdata(_,Model,_,_,_,Q)).

incompatible(nwmax08,Model,Q0,ignore) :-
  Catchlist = [alphamid,colormid,percentstart,percentmid,percentend,sizemid,sizestart_y,sizemid_y,sizeend_y],
  member(Q0,Catchlist),  Q=..[Q0,_],
  once(gotdata(_,Model,_,_,Q)).

incompatible(nwmax08,Model,Q0,ignore) :-
  Catchlist = [blurlength,threshold,shadowradius,verticaldisplacement],
  member(Q0,Catchlist),  Q=..[Q0,_],
  once(gotdata(_,Model,_,_,Q)).

incompatible(nwmax08,Model,Q0,ignore) :-
  Catchlist = [tverts1,tverts2,tverts3,texindices0,texindices1,texindices2,texindices3,texture1,texture2,texture3],
  member(Q0,Catchlist),  Q=..[Q0,_],
  once(gotdata(_,Model,_,_,Q)).

incompatible(nwmax08,Model,Key,ignore) :-
  Catchlist = [alphamid,colormid,percentstart,percentmid,percentend,sizemid,sizestart_y,sizemid_y,sizeend_y],
  member(Q0,Catchlist),  atom_concat(Q0,'key',Key), Q=..[Key,_],
  once(gotdata(_,Model,_,_,_,Q)).

incompatible(nwmax08,Model,Key,ignore) :-
  Catchlist = [blurlength,threshold,shadowradius,verticaldisplacement],
  member(Q0,Catchlist),  atom_concat(Q0,'key',Key), Q=..[Key,_],
  once(gotdata(_,Model,_,_,_,Q)).

incompatible(nwmax08,Model,animmesh,ignore) :-
  once(gotdata(_,Model,node(animmesh,_))).

/* Added in V342f */

incompatible(nwmax08,Model,'skin mesh alpha in animations',ignore) :-
  once((
        clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef),
        gotdata(File,Model,NodeName,NodeRef,_,alpha(_))
      )).

incompatible(nwmax08,Model,'skin mesh alphakeys',ignore) :-
  once((
        clause(gotdata(File,Model,node(skin,NodeName)),true,NodeRef),
        gotdata(File,Model,NodeName,NodeRef,_,alphakey(_))
      )).

/* =================== */
/* face_normal_total/6 */
/* =================== */

face_normal_total(_,_,_,_,[],[0,0,0]) :- !.

face_normal_total(File,Model,NodeName,NodeRef,[F0|FaceList],Total) :-
  face_normal_total(File,Model,NodeName,NodeRef,FaceList,Total1),
  face_normal(File,Model,NodeName,NodeRef,F0,N),
  vector_add(Total1,N,Total),
  !.


/* ===================== */
/* is_unmergeable_node/4 */
/* ===================== */

is_unmergeable_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,_,_), !.

is_unmergeable_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent/_)),
  atom_concat(Model,'a',Parent), !.

is_unmergeable_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,scale(S)),
  S=\=1, !.

is_unmergeable_node(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent/PRef)),
  is_unmergeable_node(File,Model,Parent,PRef), !.


/* ============= */
/* append_mesh/6 */
/* append_mesh/9 */
/* ============= */

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2) :-
  gotdata(File,Model,NodeName,NodeRef,constraints(_)),
  gotdata(File,Model,NodeName2,NodeRef2,verts(N)), N>0,
  \+gotdata(File,Model,NodeName2,NodeRef2,constraints(_)),
  !, fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2) :-
  gotdata(File,Model,NodeName2,NodeRef2,constraints(_)),
  gotdata(File,Model,NodeName,NodeRef,verts(N)), N>0,
  \+gotdata(File,Model,NodeName,NodeRef,constraints(_)),
  !, fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2) :-
  gotdata(File,Model,NodeName,NodeRef,weights(_)),
  gotdata(File,Model,NodeName2,NodeRef2,verts(N)), N>0,
  \+gotdata(File,Model,NodeName2,NodeRef2,weights(_)),
  !, fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2) :-
  gotdata(File,Model,NodeName2,NodeRef2,weights(_)),
  gotdata(File,Model,NodeName,NodeRef,verts(N)), N>0,
  \+gotdata(File,Model,NodeName,NodeRef,weights(_)),
  !, fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2) :-
  gotdata(File,Model,NodeName,NodeRef,colors(C)), C>0,
  \+gotdata(File,Model,NodeName2,NodeRef2,colors(_)),
  gotdata(File,Model,NodeName2,NodeRef2,verts(N)), N>0,
  asserta(gotdata(File,Model,NodeName2,NodeRef2,colors(N))),
  LastV is N-1,
  between(0,LastV,V),
  asserta(gotdata(File,Model,NodeName2,NodeRef2,colors(V,255,255,255))),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2) :-
  gotdata(File,Model,NodeName2,NodeRef2,colors(C)), C>0,
  \+gotdata(File,Model,NodeName,NodeRef,colors(_)),
  gotdata(File,Model,NodeName,NodeRef,verts(N)), N>0,
  asserta(gotdata(File,Model,NodeName,NodeRef,colors(N))),
  LastV is N-1,
  between(0,LastV,V),
  asserta(gotdata(File,Model,NodeName,NodeRef,colors(V,255,255,255))),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2) :-
  once((gotdata(File,Model,NodeName,NodeRef,verts(NVerts)) ; NVerts=0)),
  once((gotdata(File,Model,NodeName,NodeRef,tverts(NTVerts)) ; NTVerts=0)),
  once((gotdata(File,Model,NodeName,NodeRef,faces(NFaces)) ; NFaces=0)),
  !,
  append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,NVerts,NTVerts,NFaces).

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,NVerts,_,_) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,verts(N,X,Y,Z)),true,ERef),
  N1 is N+NVerts,
  asserta(gotdata(File,Model,NodeName,NodeRef,verts(N1,X,Y,Z))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,NVerts,_,_) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,verts(N)),true,ERef), N>0,
  N1 is N+NVerts,
  retractall(gotdata(File,Model,NodeName,NodeRef,verts(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,verts(N1))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,NVerts,_,_) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,constraints(N,C)),true,ERef),
  N1 is N+NVerts,
  asserta(gotdata(File,Model,NodeName,NodeRef,constraints(N1,C))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,NVerts,_,_) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,constraints(N)),true,ERef), N>0,
  N1 is N+NVerts,
  retractall(gotdata(File,Model,NodeName,NodeRef,constraints(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,constraints(N1))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,NVerts,_,_) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,weights(N,W)),true,ERef),
  N1 is N+NVerts,
  asserta(gotdata(File,Model,NodeName,NodeRef,weights(N1,W))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,NVerts,_,_) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,weights(N)),true,ERef), N>0,
  N1 is N+NVerts,
  retractall(gotdata(File,Model,NodeName,NodeRef,weights(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,weights(N1))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,NVerts,_,_) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,colors(N,R,G,B)),true,ERef),
  N1 is N+NVerts,
  asserta(gotdata(File,Model,NodeName,NodeRef,colors(N1,R,G,B))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,NVerts,_,_) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,colors(N)),true,ERef), N>0,
  N1 is N+NVerts,
  retractall(gotdata(File,Model,NodeName,NodeRef,colors(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,colors(N1))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,_,NTVerts,_) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,tverts(N,U,V,W)),true,ERef),
  N1 is N+NTVerts,
  asserta(gotdata(File,Model,NodeName,NodeRef,tverts(N1,U,V,W))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,_,NTVerts,_) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,tverts(N)),true,ERef), N>0,
  N1 is N+NTVerts,
  retractall(gotdata(File,Model,NodeName,NodeRef,tverts(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,tverts(N1))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,NVerts,NTVerts,NFaces) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,faces(N,V1,V2,V3,G,T1,T2,T3,M)),true,ERef),
  N1 is N+NFaces,
  VV1 is V1+NVerts, VV2 is V2+NVerts, VV3 is V3+NVerts,
  TT1 is T1+NTVerts, TT2 is T2+NTVerts, TT3 is T3+NTVerts,
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N1,VV1,VV2,VV3,G,TT1,TT2,TT3,M))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,_,_,NFaces) :-
  clause(gotdata(File,Model,NodeName2,NodeRef2,faces(N)),true,ERef), N>0,
  N1 is N+NFaces,
  retractall(gotdata(File,Model,NodeName,NodeRef,faces(_))),
  asserta(gotdata(File,Model,NodeName,NodeRef,faces(N1))),
  erase(ERef),
  fail.

append_mesh(File,Model,NodeName,NodeRef,NodeName2,NodeRef2,_,_,_) :-
  clause(gotdata(File,Model,Child,ChildRef,parent(NodeName2/NodeRef2)),true,ERef),
  asserta(gotdata(File,Model,Child,ChildRef,parent(NodeName,NodeRef))),
  erase(ERef),
  fail.

/* CM352d: Fixed bug here that occurs if there is another node with the same name as the one being merged */
append_mesh(File,Model,_,_,NodeName2,NodeRef2,_,_,_) :-
  erase(NodeRef2),
  retractall(gotdata(File,Model,NodeName2,NodeRef2,_)),
  !.

/* ============= */
/* pxh0_bitmap/2 */
/* (CM345d)      */
/* ============= */

pxh0_bitmap(Model,Bitmap) :-
  sub_atom(Model,0,2,_,PFM), once((PFM='pf' ; PFM='pm')),
  sub_atom(Model,4,5,_,'_head'),
  !,
  Bitmap = Model.

pxh0_bitmap(Model,Bitmap) :-
  sub_atom(Model,0,2,_,PFM), once((PFM='pf' ; PFM='pm')),
  sub_atom(Model,5,6,3,'cloak_'),
  !,
  sub_atom(Model,5,9,0,Bitmap). 
  
pxh0_bitmap(Model,Bitmap) :- 
  sub_atom(Model,0,2,_,PFM), once((PFM='pf' ; PFM='pm')),
  sub_atom(Model,4,1,_,'_'),
  atom_length(Model,L1),
  L2 is L1-5,
  sub_atom(Model,5,L2,0,Part),
  concat_atom([PFM,'h0_',Part],Bitmap),
  !.

/* ================== */
/* report_error/2     */
/* report_warning/2   */
/* report_to_stream/2 */
/* ================== */

report_error(Error,SmallLogStream) :-
  Report = ['  *** Error ***'|Error],
  report_to_stream(current_output,Report),
  report_to_stream(SmallLogStream,Report).

report_warning(Warning,SmallLogStream) :-
  Report = ['  warning -'|Warning],
  report_to_stream(current_output,Report),
  report_to_stream(SmallLogStream,Report).

report_message(Message,SmallLogStream) :-
  report_to_stream(current_output,Message),
  report_to_stream(SmallLogStream,Message).

report_message(Message) :-
  report_to_stream(current_output,Message).

report_to_stream(Stream,[])    :- !, nl(Stream).
report_to_stream(Stream,[H|T]) :- write(Stream,H), tab(Stream,1), !, report_to_stream(Stream,T).


/* ===================== */
/* unspace_weight_list/2 */
/* unspace_weight_list/3 */
/* ===================== */

unspace_weight_list([],[]) :- !.
unspace_weight_list([N|L],[N|L1]) :- number(N), !, unspace_weight_list(L,L1).
unspace_weight_list([B|L],W) :- \+number(B), !, unspace_weight_list([B],L,W).

unspace_weight_list(BList,[N|L],[B,N|L1]) :- number(N), !, atomic_list_concat(BList,B), unspace_weight_list(L,L1).
unspace_weight_list(BList,[B|L],L1)  :- append(BList,[B],BL1), !, unspace_weight_list(BL1,L,L1).


/* ============================= */
/* compact_bone_weights/2        */
/* compact_sorted_bone_weights/2 */
/* list_pairs/2                  */
/* ============================= */

compact_bone_weights([],[]) :- !.
compact_bone_weights(W,W1) :- list_pairs(W,W2), sort(W2,W2s), flatten(W2s,Ws), !, compact_sorted_bone_weights(Ws,W1).

compact_sorted_bone_weights([],[]).
compact_sorted_bone_weights([B,W],[B,W]).
compact_sorted_bone_weights([B1,W1,B2,W2|W],[B1,W1|L]) :- B2\=B1, !, compact_sorted_bone_weights([B2,W2|W],L).
compact_sorted_bone_weights([B,W1,B,W2|W],L) :- W3 is W1+W2, !, compact_sorted_bone_weights([B,W3|W],L).

list_pairs([],[]).
list_pairs([B,W|L],[[B,W]|L1]) :- list_pairs(L,L1).

/* ================= */
/* vector2rotation/3 */
/* Added in CM3.6.2q */
/* ================= */

vector2rotation(V1,V2,[X,Y,Z,A]) :-
  vector_normalise(V1,N1), N1\==[],
  vector_normalise(V2,N2), N2\==[],
  vector_cross_product(N1,N2,N1xN2),
  vector_normalise(N1xN2,[X,Y,Z]),
  vector_magnitude(N1xN2,S),
  vector_dot_product(N1,N2,C),
  A is atan2(S,C).
