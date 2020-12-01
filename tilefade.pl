/* ================================================== */ 
/*                                                    */
/* Predicates for slicing and tilefading              */
/* Part of the CleanModels 3 suite by OldMansBeard    */
/*                                                    */
/* This version dated 2013-07-15                      */
/*                                                    */
/* ================================================== */ 

:- dynamic abs_vertex/3.
:- dynamic(t_base_edge/5).
:- dynamic(t_base_corner/10).

:- op(700,xfx,'~=').

  X1 ~= X1.
  X1 ~= X2 :- number(X1), number(X2), X1\=X2, abs(X1-X2)<0.0001.

/* ================= */
/* t_slice_trimesh/6 */
/* ================= */

t_slice_trimesh(File,Model,NodeName,NodeRef,_,_) :-
  t_abs_verts(File,Model,NodeName,NodeRef),
  fail.

t_slice_trimesh(File,Model,NodeName,NodeRef,Z,_) :-
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,_,_,_,_)),
  abs_vertex(NodeRef,V1,[_,_,Zv1]), D1 is Zv1-Z,
  abs_vertex(NodeRef,V2,[_,_,Zv2]), D2 is Zv2-Z,
  abs_vertex(NodeRef,V3,[_,_,Zv3]), D3 is Zv3-Z,
  once((D1 < -0.025 ; D2 < -0.025 ; D3 < -0.025)), once((D1 > 0.025 ; D2 > 0.025 ; D3 > 0.025)),
  t_slice_face(File,Model,NodeName,NodeRef,Z,F),
  fail.

t_slice_trimesh(File,Model,NodeName,NodeRef,Z,TileFadeOrShadows) :-
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)),
  setof(F,t_face_above_Z(File,Model,NodeName,NodeRef,Z,F),FacesAboveZ),
  length(FacesAboveZ,N), N<NFaces,
  t_split_node_by_face_list(File,Model,NodeName,NodeRef,FacesAboveZ,TileFadeOrShadows), !.

/* ============= */
/* t_abs_verts/4 */
/* ============= */

t_abs_verts(File,Model,NodeName,NodeRef) :-
  retractall(abs_vertex(_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,Z)),
  relate_to_tile(File,Model,NodeName,NodeRef,[X,Y,Z],Abs),
  assertz(abs_vertex(NodeRef,V,Abs)),
  fail ; true.

/* ============== */
/* t_slice_face/6 */
/* ============== */

t_slice_face(File,Model,NodeName,NodeRef,Z,F) :-
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,T1,T2,T3,M)),
  abs_vertex(NodeRef,V1,[_,_,Z1]),
  abs_vertex(NodeRef,V2,[_,_,Z2]),
  abs_vertex(NodeRef,V3,[_,_,Z3]),
  t_divide_edge(File,Model,NodeName,NodeRef,V1,T1,V2,T2,Z1,Z2,Z,V12,T12),
  t_divide_edge(File,Model,NodeName,NodeRef,V2,T2,V3,T3,Z2,Z3,Z,V23,T23),
  t_divide_edge(File,Model,NodeName,NodeRef,V3,T3,V1,T1,Z3,Z1,Z,V31,T31),
  t_divide_face(File,Model,NodeName,NodeRef,F,V1,V2,V3,S,T1,T2,T3,M,V12,T12,V23,T23,V31,T31).

/* ================ */
/* t_divide_face/19 */
/* ================ */

t_divide_face(_,_,_,_,_,_,_,_,_,_,_,_,_,-1,-1,-1,-1,-1,-1) :- !.

t_divide_face(File,Model,NodeName,NodeRef,F,V1,V2,V3,S,T1,T2,T3,M,V4,T4,-1,-1,-1,-1) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  NewNFaces is NFaces+1,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,T1,T2,T3,M))),
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V4,V3,S,T1,T4,T3,M))),
  Fnew is NewNFaces-1,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(Fnew,V4,V2,V3,S,T4,T2,T3,M))), !.

t_divide_face(File,Model,NodeName,NodeRef,F,V1,V2,V3,S,T1,T2,T3,M,-1,-1,V4,T4,-1,-1) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  NewNFaces is NFaces+1,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,T1,T2,T3,M))),
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V4,V3,S,T1,T4,T3,M))),
  Fnew is NewNFaces-1,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(Fnew,V1,V2,V4,S,T1,T2,T4,M))), !.

t_divide_face(File,Model,NodeName,NodeRef,F,V1,V2,V3,S,T1,T2,T3,M,-1,-1,-1,-1,V4,T4) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  NewNFaces is NFaces+1,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,T1,T2,T3,M))),
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V4,S,T1,T2,T4,M))),
  Fnew is NewNFaces-1,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(Fnew,V2,V3,V4,S,T2,T3,T4,M))), !.

t_divide_face(File,Model,NodeName,NodeRef,F,V1,V2,V3,S,T1,T2,T3,M,V4,T4,V5,T5,-1,-1) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  NewNFaces is NFaces+2,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,T1,T2,T3,M))),
  F1 is NFaces, F2 is NFaces+1,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F,V4,V2,V5,S,T4,T2,T5,M))),
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F1,V1,V4,V5,S,T1,T4,T5,M))),
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F2,V1,V5,V3,S,T1,T5,T3,M))), !.

t_divide_face(File,Model,NodeName,NodeRef,F,V1,V2,V3,S,T1,T2,T3,M,V4,T4,-1,-1,V5,T5) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  NewNFaces is NFaces+2,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,T1,T2,T3,M))),
  F1 is NFaces, F2 is NFaces+1,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V4,V5,S,T1,T4,T5,M))),
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F1,V5,V4,V2,S,T5,T4,T2,M))),
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F2,V5,V2,V3,S,T5,T2,T3,M))), !.

t_divide_face(File,Model,NodeName,NodeRef,F,V1,V2,V3,S,T1,T2,T3,M,-1,-1,V4,T4,V5,T5) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  NewNFaces is NFaces+2,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,S,T1,T2,T3,M))),
  F1 is NFaces, F2 is NFaces+1,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F,V4,V3,V5,S,T4,T3,T5,M))),
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F1,V1,V2,V5,S,T1,T2,T5,M))),
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F2,V2,V4,V5,S,T2,T4,T5,M))), !.

/* ================ */
/* t_divide_edge/13 */
/* ================ */

t_divide_edge(File,Model,NodeName,NodeRef,Vx1,T1,Vx2,T2,Za1,Za2,Z,NewV,NewT) :-
  Za1>Z, Za2<Z,
  Lambda is (Z-Za2)/(Za1-Za2),
  gotdata(File,Model,NodeName,NodeRef,verts(Vx1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(Vx2,X2,Y2,Z2)),
  Xnew is X2+Lambda*(X1-X2), Ynew is Y2+Lambda*(Y1-Y2), Znew is Z2+Lambda*(Z1-Z2),
  t_get_vert(File,Model,NodeName,NodeRef,NewV,Xnew,Ynew,Znew),
  gotdata(File,Model,NodeName,NodeRef,tverts(T1,U1,V1,W1)),
  gotdata(File,Model,NodeName,NodeRef,tverts(T2,U2,V2,W2)),
  Unew is U2+Lambda*(U1-U2), Vnew is V2+Lambda*(V1-V2), Wnew is W2+Lambda*(W1-W2),
  t_get_tvert(File,Model,NodeName,NodeRef,NewT,Unew,Vnew,Wnew), !.

t_divide_edge(File,Model,NodeName,NodeRef,Vx1,T1,Vx2,T2,Za1,Za2,Z,NewV,NewT) :-
  Za1<Z, Za2>Z,
  Lambda is (Z-Za1)/(Za2-Za1),
  gotdata(File,Model,NodeName,NodeRef,verts(Vx1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(Vx2,X2,Y2,Z2)),
  Xnew is X1+Lambda*(X2-X1), Ynew is Y1+Lambda*(Y2-Y1), Znew is Z1+Lambda*(Z2-Z1),
  t_get_vert(File,Model,NodeName,NodeRef,NewV,Xnew,Ynew,Znew),
  gotdata(File,Model,NodeName,NodeRef,tverts(T1,U1,V1,W1)),
  gotdata(File,Model,NodeName,NodeRef,tverts(T2,U2,V2,W2)),
  Unew is U1+Lambda*(U2-U1), Vnew is V1+Lambda*(V2-V1), Wnew is W1+Lambda*(W2-W1),
  t_get_tvert(File,Model,NodeName,NodeRef,NewT,Unew,Vnew,Wnew), !.

t_divide_edge(_,_,_,_,_,_,_,_,_,_,_,-1,-1).

/* ============= */
/* t_get_vert/8  */
/* t_get_tvert/8 */
/* ============= */

t_get_vert(File,Model,NodeName,NodeRef,V,X,Y,Z) :-
  snap(X,Xs), snap(Y,Ys), snap(Z,Zs),
  gotdata(File,Model,NodeName,NodeRef,verts(V,Xs,Ys,Zs)), !.

t_get_vert(File,Model,NodeName,NodeRef,V,X,Y,Z) :-
  retract(gotdata(File,Model,NodeName,NodeRef,verts(V))),
  NewNVerts is V+1,
  assertz(gotdata(File,Model,NodeName,NodeRef,verts(NewNVerts))),
  snap(X,Xs), snap(Y,Ys), snap(Z,Zs),
  assertz(gotdata(File,Model,NodeName,NodeRef,verts(V,Xs,Ys,Zs))),
  relate_to_tile(File,Model,NodeName,NodeRef,[Xs,Ys,Zs],Abs),
  assertz(abs_vertex(NodeRef,V,Abs)),
  (retract(gotdata(File,Model,NodeName,NodeRef,colors(V))) ->
     assertz(gotdata(File,Model,NodeName,NodeRef,colors(V,255,255,255))),
     assertz(gotdata(File,Model,NodeName,NodeRef,colors(NewNVerts)))
     ;
     true
  ).

t_get_tvert(File,Model,NodeName,NodeRef,T,U,V,W) :-
  gotdata(File,Model,NodeName,NodeRef,tverts(T,U,V,W)), !.

t_get_tvert(File,Model,NodeName,NodeRef,T,U,V,W) :-
  once(g_user_option(tvert_snap,TSnap)), integer(TSnap),
  Us is floor(TSnap*U+0.5)/TSnap,
  Vs is floor(TSnap*V+0.5)/TSnap,
  Ws is floor(TSnap*W+0.5)/TSnap,
  gotdata(File,Model,NodeName,NodeRef,tverts(T,Us,Vs,Ws)), !.

t_get_tvert(File,Model,NodeName,NodeRef,T,U,V,W) :-
  retract(gotdata(File,Model,NodeName,NodeRef,tverts(T))),
  NewNTVerts is T+1,
  assertz(gotdata(File,Model,NodeName,NodeRef,tverts(NewNTVerts))),
  assertz(gotdata(File,Model,NodeName,NodeRef,tverts(T,U,V,W))).

/* ================ */
/* t_face_above_Z/6 */
/* ================ */

t_face_above_Z(File,Model,NodeName,NodeRef,Z,F) :-
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,_,_,_,_)),
  once((
    abs_vertex(NodeRef,V1,[_,_,Zv1]), Zv1>Z+0.025 ;
    abs_vertex(NodeRef,V2,[_,_,Zv2]), Zv2>Z+0.025 ;
    abs_vertex(NodeRef,V3,[_,_,Zv3]), Zv3>Z+0.025
      )).

/* =========================== */
/* t_split_node_by_face_list/6 */
/* t_copy_trimesh/7            */
/* =========================== */

t_split_node_by_face_list(File,Model,NodeName,NodeRef,FaceList,TileFadeOrShadows) :-
  length(FaceList,L), L>0,
  t_copy_trimesh(File,Model,NodeName,NodeRef,NewName,NewRef,FaceList),
  f_delete_faces_in_list(File,Model,NodeName,NodeRef,FaceList),
  delete_unused_vertices(File,Model,NodeName,NodeRef),
  delete_unused_tverts(File,Model,NodeName,NodeRef),
  (gotdata(File,Model,NewName,NewRef,tverts(NewNT)) -> renormalize_tverts(File,Model,NewName,NewRef,NewNT,_)),
  (TileFadeOrShadows==tilefade ->
    retractall(gotdata(File,Model,NewName,NewRef,tilefade(_))),
    assertz(gotdata(File,Model,NewName,NewRef,tilefade(1))),
    retractall(gotdata(File,Model,NodeName,NodeRef,tilefade(_))),
    assertz(gotdata(File,Model,NodeName,NodeRef,tilefade(0)))
    ;
    retractall(gotdata(File,Model,NodeName,NodeRef,shadow(_))),
    assertz(gotdata(File,Model,NodeName,NodeRef,shadow(0)))
  ),
  !.

t_copy_trimesh(File,Model,NodeName,NodeRef,NewName,NewRef,FaceList) :-
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  \+ gotdata(File,Model,NodeName,NodeRef,_,_),
  f_newname(NodeName,NewName),
  assertz(gotdata(File,Model,node(trimesh,NewName)),NewRef),
  once((
    gotdata(File,Model,NodeName,NodeRef,Q),
    Q=..[Q0|_], \+ member(Q0,[verts,colors,tverts,faces,wirecolor,tilefade]),
    assertz(gotdata(File,Model,NewName,NewRef,Q)),
    fail ; true
      )),
  Red is random(256)/256, Green is random(256)/256, Blue is random(256)/256,
  assertz(gotdata(File,Model,NewName,NewRef,wirecolor(Red,Green,Blue))),
  f_copy_geometry(File,Model,NodeName,NodeRef,NewName,NewRef,FaceList).

/* ======================= */
/* t_walkmesh_ref_height/5 */
/* t_walkable_face_Z/5     */
/* ======================= */

t_walkmesh_ref_height(File,Model,WalkmeshName,WalkmeshRef,Zw) :-
  t_abs_verts(File,Model,WalkmeshName,WalkmeshRef),
  setof(Z,t_walkable_face_Z(File,Model,WalkmeshName,WalkmeshRef,Z),Set),
  last(Set,Zmax), Zw is 0.1*floor(10*Zmax).

t_walkable_face_Z(File,Model,WalkmeshName,WalkmeshRef,Z) :-
  gotdata(File,Model,WalkmeshName,WalkmeshRef,faces(_,V1,V2,V3,_,_,_,_,M)),
  t_walkable(M),
  abs_vertex(WalkmeshRef,V1,[_,_,Z1]),
  abs_vertex(WalkmeshRef,V2,[_,_,Z2]),
  abs_vertex(WalkmeshRef,V3,[_,_,Z3]),
  Z is max(Z1,max(Z2,Z3)).

/* ============ */
/* t_walkable/1 */
/* ============ */

t_walkable(1).
t_walkable(3).
t_walkable(4).
t_walkable(5).
t_walkable(6).
t_walkable(9).
t_walkable(10).
t_walkable(11).
t_walkable(12).
t_walkable(13).
t_walkable(14).
t_walkable(18).
t_walkable(19).
t_walkable(20).
t_walkable(21).
t_walkable(30).

/* ====================== */
/* t_dynamic_ref_height/3 */
/* t_dynamic_max_height/5 */
/* ====================== */

t_dynamic_ref_height(File,Model,Zd) :-
  atom_concat(Model,'a',ANodeName),
  clause(gotdata(File,Model,node(dummy,ANodeName)),true,ANodeRef),
  setof(Zmax,t_dynamic_max_height(File,Model,ANodeName,ANodeRef,Zmax),S),
  last(S,Z),
  Zd is 0.1*ceiling(10*Z).

t_dynamic_max_height(File,Model,ANodeName,ANodeRef,Zmax) :-
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  superparent(File,Model,NodeName,NodeRef,ANodeName,ANodeRef),
  t_abs_verts(File,Model,NodeName,NodeRef),
  setof(Z,abs_vertex(NodeRef,_,[_,_,Z]),S),
  last(S,Zmax).

/* ================== */
/* t_cleanse_a_node/3 */
/* ================== */

t_cleanse_a_node(File,Model,Adummy) :-
  retractall(fixlist(_)),
  gotdata(File,Model,NodeName,NodeRef,parent(Adummy/_)),
  clause(gotdata(File,Model,node(NodeType,NodeName)),true,NodeRef),
  \+ t_needs_to_be_dynamic(File,Model,NodeName,NodeType,NodeRef),
  t_remove_from_a_node(File,Model,NodeName,NodeRef,Adummy),
  ( \+ fixlist(NodeName) -> assertz(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('removed '), write(NodeName), write(' from '), write(Adummy), nl ; true),
  fail.

t_cleanse_a_node(_,_,Adummy) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('removed '), write(N), write(' static objects from '), write(Adummy), nl, !.

t_cleanse_a_node(_,_,Adummy) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('removed static object(s) from '), write(Adummy), tab(1), write(List), nl, !.

/* ======================= */
/* t_needs_to_be_dynamic/5 */
/* ======================= */

t_needs_to_be_dynamic(_,_,_,emitter,_) :- !.

t_needs_to_be_dynamic(_,_,_,animmesh,_) :- !.

t_needs_to_be_dynamic(File,Model,NodeName,_,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,_,_), !.

t_needs_to_be_dynamic(File,Model,NodeName,_,NodeRef) :-
  is_watery(File,Model,NodeName,NodeRef),
  once(g_user_option(do_water,DoWater)), DoWater==no, !.

t_needs_to_be_dynamic(File,Model,NodeName,_,NodeRef) :-
  is_watery(File,Model,NodeName,NodeRef),
  once(g_user_option(do_water,DoWater)), DoWater==yes,
  once(g_user_option(dynamic_water,DynamicWater)), DynamicWater==yes, !.

t_needs_to_be_dynamic(File,Model,NodeName,_,NodeRef) :-
  is_foliage(File,Model,NodeName,NodeRef),
  once(g_user_option(foliage,Foliage)), (Foliage==animate; Foliage==no_change), !.

t_needs_to_be_dynamic(File,Model,NodeName,_,NodeRef) :-
  is_splotch(File,Model,NodeName,NodeRef),
  once(g_user_option(splotch,Splotch)), Splotch==animate, !.

t_needs_to_be_dynamic(File,Model,NodeName,_,NodeRef) :-
  gotdata(File,Model,Child,ChildRef,parent(NodeName/NodeRef)),
  t_needs_to_be_dynamic(File,Model,Child,_,ChildRef).

/* ====================== */
/* t_remove_from_a_node/5 */
/* ====================== */

t_remove_from_a_node(File,Model,NodeName,NodeRef,Adummy) :-
  retract(gotdata(File,Model,NodeName,NodeRef,parent(Adummy/_))),
  clause(gotdata(File,Model,node(dummy,Model)),true,MRef),
  assertz(gotdata(File,Model,NodeName,NodeRef,parent(Model/MRef))), !.

/* ======================== */
/* t_remove_all_tilefades/2 */
/* ======================== */

t_remove_all_tilefades(File,Model) :-
  retractall(fixlist(_)),
  gotdata(File,Model,NodeName,_,tilefade(2)),
  retractall(gotdata(File,Model,NodeName,_,_)),
  retractall(gotdata(File,Model,node(_,NodeName))),
  tab(2), write('tilefade base '), write(NodeName), write(' deleted'), nl,
  fail.

t_remove_all_tilefades(File,Model) :-
  clause(gotdata(File,Model,NodeName,NodeRef,tilefade(T)),true,Eref),
  T\=0, erase(Eref), assertz(gotdata(File,Model,NodeName,NodeRef,tilefade(0))),
  ( \+ fixlist(NodeName) -> assertz(fixlist(NodeName)) ; true ),
  (cm3_verbose -> tab(2), write('set '), write(NodeName), write(' to non-fading'), nl ; true),
  fail.

t_remove_all_tilefades(_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  tab(2), write('set '), write(N), write(' objects to non-fading'), nl, !.

t_remove_all_tilefades(_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  tab(2), write('removed tilefade from '), write(List), nl, !.

/* ===================== */
/* t_create_black_base/4 */
/* t_create_base_node/5  */
/* t_add_vertices/5      */
/* t_add_vertices/6      */
/* t_add_faces/5         */
/* t_add_faces/6         */
/* ===================== */

t_create_black_base(File,Model,_,NodeName) :-
  gotdata(File,Model,NodeName,_,tilefade(2)),
  retractall(gotdata(File,Model,NodeName,_,_)),
  retractall(gotdata(File,Model,node(_,NodeName))),
  tab(2), write('old tilefade base '), write(NodeName), write(' deleted'), nl,
  fail.

t_create_black_base(File,Model,Zslice,NodeName) :-
  t_construct_base(File,Model,Zslice,VertsList,FaceList),
  VertsList\=[], FaceList\=[],
  t_create_base_node(File,Model,Zslice,NodeName,NodeRef),
  t_add_vertices(File,Model,NodeName,NodeRef,VertsList),
  t_add_faces(File,Model,NodeName,NodeRef,FaceList).

t_create_base_node(File,Model,Zslice,NodeName,NodeRef) :-
  once((between(1,inf,N), concat_atom([Model,base,N],'_',NodeName), \+ gotdata(File,Model,node(_,NodeName)))),
  assertz(gotdata(File,Model,node(trimesh,NodeName)),NodeRef),
  clause(gotdata(File,Model,node(dummy,Model)),true,MRef),
  assertz(gotdata(File,Model,NodeName,NodeRef,parent(Model/MRef))),
  assertz(gotdata(File,Model,NodeName,NodeRef,position(0,0,Zslice))),
  assertz(gotdata(File,Model,NodeName,NodeRef,orientation(0,0,0,0))),
  assertz(gotdata(File,Model,NodeName,NodeRef,wirecolor(0,0,0))),
  assertz(gotdata(File,Model,NodeName,NodeRef,scale(1))),
  assertz(gotdata(File,Model,NodeName,NodeRef,ambient(0,0,0))),
  assertz(gotdata(File,Model,NodeName,NodeRef,diffuse(0,0,0))),
  assertz(gotdata(File,Model,NodeName,NodeRef,specular(0,0,0))),
  assertz(gotdata(File,Model,NodeName,NodeRef,shininess(0))),
  assertz(gotdata(File,Model,NodeName,NodeRef,alpha(1))),
  assertz(gotdata(File,Model,NodeName,NodeRef,render(1))),
  assertz(gotdata(File,Model,NodeName,NodeRef,shadow(0))), /* are you sure? */
  assertz(gotdata(File,Model,NodeName,NodeRef,beaming(0))),
  assertz(gotdata(File,Model,NodeName,NodeRef,tilefade(2))),
  assertz(gotdata(File,Model,NodeName,NodeRef,rotatetexture(0))),
  assertz(gotdata(File,Model,NodeName,NodeRef,inheritcolor(0))),
  assertz(gotdata(File,Model,NodeName,NodeRef,selfillumcolor(0,0,0))),
  assertz(gotdata(File,Model,NodeName,NodeRef,bitmap(black))),
  !.

t_add_vertices(File,Model,NodeName,NodeRef,VertsList) :-
  sort(VertsList,SortedList),
  length(SortedList,NVerts),
  assertz(gotdata(File,Model,NodeName,NodeRef,verts(NVerts))), !,
  (cm3_verbose -> tab(2), write('defining '), write(NVerts), write(' vertices ...'), nl ; true),
  t_add_vertices(File,Model,NodeName,NodeRef,0,SortedList).

t_add_vertices(_,_,_,_,_,[]) :- !.

t_add_vertices(File,Model,NodeName,NodeRef,V,[[X,Y]|VertsList]) :-
  assertz(gotdata(File,Model,NodeName,NodeRef,verts(V,X,Y,0))),
  V1 is V+1, !,
  t_add_vertices(File,Model,NodeName,NodeRef,V1,VertsList).

t_add_faces(File,Model,NodeName,NodeRef,FaceList) :-
  length(FaceList,NFaces),
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))), !,
  (cm3_verbose -> tab(2), write('painting '), write(NFaces), write(' faces ...'), nl ; true),
  t_add_faces(File,Model,NodeName,NodeRef,0,FaceList).

t_add_faces(_,_,_,_,_,[]) :- !.

t_add_faces(File,Model,NodeName,NodeRef,F,[[X1,Y1,X2,Y2,X3,Y3]|FaceList]) :-
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1a,Y1a,_)), X1a ~= X1, Y1a ~= Y1,
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2a,Y2a,_)), X2a ~= X2, Y2a ~= Y2,
  gotdata(File,Model,NodeName,NodeRef,verts(V3,X3a,Y3a,_)), X3a ~= X3, Y3a ~= Y3,
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,0,0,0,0,0))),
  F1 is F+1, !,
  t_add_faces(File,Model,NodeName,NodeRef,F1,FaceList).

/* --------------- below here is not finished ------------------ */

/* ==================== */
/* t_construct_base/5   */
/* t_detect_cut_edges/3 */
/* t_prepair_polygons/0 */
/* t_paint_faces/2      */
/* ==================== */

t_construct_base(File,Model,Z,VertsList,FaceList) :-
  t_detect_cut_edges(File,Model,Z),
  t_prepair_polygons,
  t_paint_faces(VertsList,FaceList).

t_detect_cut_edges(File,Model,Z) :-
  retractall(t_base_edge(_,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,tilefade(T)), (T==1 ; T==4),
  t_abs_verts(File,Model,NodeName,NodeRef),
  \+((abs_vertex(NodeRef,_,[_,_,Zz]), Zz<Z-0.025)),
  abs_vertex(NodeRef,V1,[X1,Y1,Z1]), Z1 ~= Z,
  abs_vertex(NodeRef,V2,[X2,Y2,Z2]), V2\=V1, Z2 ~= Z,
  (gotdata(File,Model,NodeName,NodeRef,faces(_,V2,V1,V3,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(_,V1,V3,V2,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(_,V3,V2,V1,_,_,_,_,_))),
  abs_vertex(NodeRef,V3,[_,_,Z3]), Z3>Z,
  L is sqrt((X2-X1)**2+(Y2-Y1)**2), \+((L ~= 0.0)),
  \+ t_base_edge(X1,Y1,X2,Y2,L),
  assertz(t_base_edge(X1,Y1,X2,Y2,L)),
  fail.
  
t_detect_cut_edges(_,_,_) :-
  predicate_property(t_base_edge(_,_,_,_,_),number_of_clauses(N)),
  (cm3_verbose -> tab(2), write(N), write(' cut edges found ...'), nl).

t_prepair_polygons :-
  t_fix_anomalies,
  t_repair_breaks,
  t_make_corners,
  t_repair_indentations.

t_paint_faces([],[]) :-
  \+ t_base_corner(_,_,_,_,_,_,_,_,_,_),
  tab(2), write('base meshed successfully ...'), nl,
  !.

t_paint_faces(VertsList,FaceList) :-
  clause(t_base_corner(_,_,_,_,_,_,_,_,_,Theta2),true,Cref2), Theta2<0.0001,
  erase(Cref2),
  !,
  t_paint_faces(VertsList,FaceList).

t_paint_faces([[X1,Y1],[X2,Y2],[X3,Y3]|VertsList],[[X2,Y2,X1,Y1,X3,Y3]|FaceList]) :-
  clause(t_base_corner(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,_,Theta2),true,Cref2), Theta2<pi,
  clause(t_base_corner(X2b,Y2b,X3b,Y3b,X3ab,Y3ab,X1b,Y1b,_,Theta3),true,Cref3),
  X2b~=X2a, Y2b~=Y2a, X3b~=X3, Y3b~=Y3, X1b~=X1, Y1b~=Y2, Theta3<pi,
  clause(t_base_corner(X3c,Y3c,X1c,Y1c,X1ac,Y1ac,X2c,Y2c,_,Theta1),true,Cref1),
  X3c~=X3ab, Y3c~=Y3ab, X1c~=X1b, Y1c~=Y1b, X1ac~=X1, Y1ac~=Y1, X2c~=X2, Y2c~=Y2, Theta1<pi,
  erase(Cref1), erase(Cref2), erase(Cref3),
  !,
  t_paint_faces(VertsList,FaceList).

t_paint_faces([[X1,Y1],[X2,Y2],[X3,Y3]|VertsList],[[X2,Y2,X1,Y1,X3,Y3]|FaceList]) :-
  clause(t_base_corner(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,_,Theta2),true,Cref2), Theta2 =< 0.5*pi,
  clause(t_base_corner(X2a,Y2a,X3,Y3,X3a,Y3a,X4,Y4,_,Theta3),true,Cref3),
  clause(t_base_corner(X0,Y0,X1a,Y1a,X1,Y1,X2,Y2,_,Theta1),true,Cref1),
  Corner0 = t_base_corner(X0,Y0,X1a,Y1a,X1a,Y1a,X3a,Y3a,Sin0,Theta0),
  t_corner_trig(X0,Y0,X1a,Y1a,X1a,Y1a,X3a,Y3a,Sin0,Theta0),
  Corner4 = t_base_corner(X1a,Y1a,X3a,Y3a,X3a,Y3a,X4,Y4,Sin4,Theta4),
  t_corner_trig(X1a,Y1a,X3a,Y3a,X3a,Y3a,X4,Y4,Sin4,Theta4),
  Theta0<Theta1, Theta4<Theta3,
  erase(Cref1), erase(Cref2), erase(Cref3),
  assertz(Corner0), assertz(Corner4),
  !,
  t_paint_faces(VertsList,FaceList).

t_paint_faces([[X1,Y1],[X2,Y2],[X3,Y3]|VertsList],[[X2,Y2,X1,Y1,X3,Y3]|FaceList]) :-
  clause(t_base_corner(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,_,Theta2),true,Cref2), Theta2 < pi,
  clause(t_base_corner(X2a,Y2a,X3,Y3,X3a,Y3a,X4,Y4,_,Theta3),true,Cref3),
  clause(t_base_corner(X0,Y0,X1a,Y1a,X1,Y1,X2,Y2,_,Theta1),true,Cref1),
  Corner0 = t_base_corner(X0,Y0,X1a,Y1a,X1a,Y1a,X3a,Y3a,Sin0,Theta0),
  t_corner_trig(X0,Y0,X1a,Y1a,X1a,Y1a,X3a,Y3a,Sin0,Theta0),
  Corner4 = t_base_corner(X1a,Y1a,X3a,Y3a,X3a,Y3a,X4,Y4,Sin4,Theta4),
  t_corner_trig(X1a,Y1a,X3a,Y3a,X3a,Y3a,X4,Y4,Sin4,Theta4),
  Theta0<Theta1, Theta4<Theta3,
  erase(Cref1), erase(Cref2), erase(Cref3),
  assertz(Corner0), assertz(Corner4),
  !,
  t_paint_faces(VertsList,FaceList).

t_paint_faces([[X1,Y1],[X2,Y2],[X3,Y3]|VertsList],[[X2,Y2,X1,Y1,X3,Y3]|FaceList]) :-
  clause(t_base_corner(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,_,Theta2),true,Cref2), Theta2 < pi,
  clause(t_base_corner(X2a,Y2a,X3,Y3,X3a,Y3a,X4,Y4,_,Theta3),true,Cref3),
  t_corner_trig(X1,Y1,X3a,Y3a,X3a,Y3a,X4,Y4,Sin4,Theta4),
  Theta4<Theta3,
  erase(Cref2), erase(Cref3),
  assertz(t_base_corner(X1,Y1,X3a,Y3a,X3a,Y3a,X4,Y4,Sin4,Theta4)),
  !,
  t_paint_faces(VertsList,FaceList).

t_paint_faces([[X1,Y1],[X2,Y2],[X3,Y3]|VertsList],[[X2,Y2,X1,Y1,X3,Y3]|FaceList]) :-
  clause(t_base_corner(X1,Y1,X2,Y2,_,_,X3,Y3,_,Theta2),true,Cref2), Theta2 < pi,
  clause(t_base_corner(X0,Y0,X1a,Y1a,X1,Y1,X2,Y2,_,Theta1),true,Cref1),
  t_corner_trig(X0,Y0,X1a,Y1a,X1a,Y1a,X3,Y3,Sin0,Theta0),
  Theta0<Theta1,
  erase(Cref1), erase(Cref2),
  assertz(t_base_corner(X0,Y0,X1a,Y1a,X1a,Y1a,X3,Y3,Sin0,Theta0)),
  !,
  t_paint_faces(VertsList,FaceList).

t_paint_faces([[X1,Y1],[X2,Y2],[X3,Y3]|VertsList],[[X2,Y2,X1,Y1,X3,Y3]|FaceList]) :-
  clause(t_base_corner(X1,Y1,X2,Y2,_,_,X3,Y3,_,Theta2),true,Cref2), Theta2 < pi,
  erase(Cref2),
  !,
  t_paint_faces(VertsList,FaceList).

t_paint_faces([],[]) :-
  tab(2), write('unable to mesh base completely but ...'), nl.


/* ======================= */
/* t_fix_anomalies/0       */
/* t_repair_breaks/0       */
/* t_make_corners/0        */
/* t_repair_indentations/0 */
/* ======================= */

t_fix_anomalies :-
  clause(t_base_edge(X1,Y1,X3,Y3,L13),true,Cref13), X3~=X1,
  clause(t_base_edge(X3a,Y3a,X2,Y2,L32),true,Cref32), X3a~=X2, X3a~=X3, Y3a~=Y3, L32<L13, (Y2-Y3a)*(Y1-Y3)>0,
  clause(t_base_edge(X2a,Y2a,X4,Y4,L24),true,Cref24), X4~=X2a, X2a~=X2, Y2a~=Y2, L24>L32, (Y2-Y3a)*(Y2a-Y4)>0,
  L12 is sqrt((X1-X2)**2+(Y1-Y2)**2), assertz(t_base_edge(X1,Y1,X2,Y2,L12)),
  L23 is sqrt((X2-X3)**2+(Y2-Y3)**2), assertz(t_base_edge(X2,Y2,X3,Y3,L23)),
  L34 is sqrt((X3-X4)**2+(Y3-Y4)**2), assertz(t_base_edge(X3,Y3,X4,Y4,L34)),
  erase(Cref13), erase(Cref32), erase(Cref24),
  (cm3_verbose -> tab(2), write('fixed zigzag at '), write([X2,Y2,to,X3,Y3]), nl ; true),
  fail.

t_fix_anomalies :-
  clause(t_base_edge(X1,Y1,X3,Y3,L13),true,Cref13), Y3~=Y1,
  clause(t_base_edge(X3a,Y3a,X2,Y2,L32),true,Cref32), Y3a~=Y2, X3a~=X3, Y3a~=Y3, L32<L13, (X2-X3a)*(X1-X3)>0,
  clause(t_base_edge(X2a,Y2a,X4,Y4,L24),true,Cref24), Y4~=Y2a, X2a~=X2, Y2a~=Y2, L24>L32, (X2-X3a)*(X2a-X4)>0,
  L12 is sqrt((X1-X2)**2+(Y1-Y2)**2), assertz(t_base_edge(X1,Y1,X2,Y2,L12)),
  L23 is sqrt((X2-X3)**2+(Y2-Y3)**2), assertz(t_base_edge(X2,Y2,X3,Y3,L23)),
  L34 is sqrt((X3-X4)**2+(Y3-Y4)**2), assertz(t_base_edge(X3,Y3,X4,Y4,L34)),
  erase(Cref13), erase(Cref32), erase(Cref24),
  (cm3_verbose -> tab(2), write('fixed zigzag at '), write([X2,Y2,to,X3,Y3]), nl ; true),
  fail.

t_fix_anomalies :-
  t_base_edge(X0,_,X2,Y2,_), X0~=X2,
  \+ ((t_base_edge(X2a,Y2a,_,_,_), X2a~=X2, Y2a~=Y2)),
  clause(t_base_edge(X1,Y1,X3,Y3,_),true,Cref13), Y1~=Y2, Y3~=Y2,
  (X3-X2)*(X2-X1)>0,
  erase(Cref13),
  L12 is abs(X1-X2), assertz(t_base_edge(X1,Y1,X2,Y2,L12)),
  L23 is abs(X2-X3), assertz(t_base_edge(X2,Y2,X3,Y3,L23)),
  (cm3_verbose -> tab(2), write('welded vertex at '), write([X2,Y2]), nl ; true),
  fail.

t_fix_anomalies :-
  t_base_edge(_,Y0,X2,Y2,_), Y0~=Y2,
  \+ ((t_base_edge(X2a,Y2a,_,_,_), X2a~=X2, Y2a~=Y2)),
  clause(t_base_edge(X1,Y1,X3,Y3,_),true,Cref13), X1~=X2, X3~=X2,
  (Y3-Y2)*(Y2-Y1)>0,
  erase(Cref13),
  L12 is abs(Y1-Y2), assertz(t_base_edge(X1,Y1,X2,Y2,L12)),
  L23 is abs(Y2-Y3), assertz(t_base_edge(X2,Y2,X3,Y3,L23)),
  (cm3_verbose -> tab(2), write('welded vertex at '), write([X2,Y2]), nl ; true),
  fail.

t_fix_anomalies :-
  t_base_edge(X2,Y2,X0,_,_), X0~=X2,
  \+ ((t_base_edge(_,_,X2a,Y2a,_), X2a~=X2, Y2a~=Y2)),
  clause(t_base_edge(X1,Y1,X3,Y3,_),true,Cref13), Y1~=Y2, Y3~=Y2,
  (X3-X2)*(X2-X1)>0,
  erase(Cref13),
  L12 is abs(X1-X2), assertz(t_base_edge(X1,Y1,X2,Y2,L12)),
  L23 is abs(X2-X3), assertz(t_base_edge(X2,Y2,X3,Y3,L23)),
  (cm3_verbose -> tab(2), write('welded vertex at '), write([X2,Y2]), nl ; true),
  fail.

t_fix_anomalies :-
  t_base_edge(X2,Y2,_,Y0,_), Y0~=Y2,
  \+ ((t_base_edge(_,_,X2a,Y2a,_), X2a~=X2, Y2a~=Y2)),
  clause(t_base_edge(X1,Y1,X3,Y3,_),true,Cref13), X1~=X2, X3~=X2,
  (Y3-Y2)*(Y2-Y1)>0,
  erase(Cref13),
  L12 is abs(Y1-Y2), assertz(t_base_edge(X1,Y1,X2,Y2,L12)),
  L23 is abs(Y2-Y3), assertz(t_base_edge(X2,Y2,X3,Y3,L23)),
  (cm3_verbose -> tab(2), write('welded vertex at '), write([X2,Y2]), nl ; true),
  fail.

/*
t_fix_anomalies :-
  t_base_edge(X1,Y1,X2,Y2,_),
  clause(t_base_edge(X2a,Y2a,X3,Y3,_),true,Cref3), X2a~=X2, Y2a~=Y2, 
  t_base_edge(X2b,Y2b,X4,Y4,_), X2b~=X2, Y2b~=Y2, \+((X4~=X3)), \+((Y3~=Y3)),
  t_corner_trig(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,_,Theta3),
  t_corner_trig(X1,Y1,X2,Y2,X2b,Y2b,X4,Y4,_,Theta4),
  Theta3<Theta4,
  erase(Cref3),
  (cm3_verbose -> tab(2), write('deleted interior edge '), write([X2a,Y2a,X3,Y3]), nl; true),
  fail.

t_fix_anomalies :-
  t_base_edge(X1,Y1,X2,Y2,_),
  t_base_edge(X2a,Y2a,X3,Y3,_), X2a~=X2, Y2a~=Y2, 
  t_corner_trig(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,_,Theta2),
  clause(t_base_edge(X4,Y4,X2b,Y2b,_),true,Cref4), X2b~=X2, Y2b~=Y2, \+((X4~=X3)), \+((Y3~=Y3)),
  t_corner_trig(X4,Y4,X2b,Y2b,X2a,Y2a,X3,Y3,_,Theta4),
  Theta4<Theta2,
  erase(Cref4),
  (cm3_verbose -> tab(2), write('deleted interior edge '), write([X4,Y4,X2b,Y2b]), nl; true),
  fail.
*/

t_fix_anomalies.
 
t_repair_breaks :-
  t_base_edge(_,_,X1,Y1,_),
  \+ ((t_base_edge(Xa,Ya,_,_,_), Xa~=X1, Ya~=Y1)),
  t_base_edge(X2,Y2,_,_,_), X2~=X1, \+((Y2~=Y1)),
  \+ ((t_base_edge(_,_,Xb,Yb,_), Xb~=X2, Yb~=Y2)),
  L is abs(Y2-Y1),
  assertz(t_base_edge(X1,Y1,X2,Y2,L)),
  (cm3_verbose -> tab(2), write('closed edge '), write([X1,Y1,X2,Y2]), nl ; true),
  !,
  t_repair_breaks.

t_repair_breaks :-
  t_base_edge(_,_,X1,Y1,_),
  \+ ((t_base_edge(Xa,Ya,_,_,_), Xa~=X1, Ya~=Y1)),
  t_base_edge(X2,Y2,_,_,_), Y2~=Y1, \+((X2~=X1)),
  \+ ((t_base_edge(_,_,Xb,Yb,_), Xb~=X2, Yb~=Y2)),
  L is abs(X2-X1),
  assertz(t_base_edge(X1,Y1,X2,Y2,L)),
  (cm3_verbose -> tab(2), write('closed edge '), write([X1,Y1,X2,Y2]), nl ; true),
  !,
  t_repair_breaks.

t_repair_breaks :-
  t_base_edge(_,_,X1,Y1,_), (X1~=5 ; X1~= -5),
  \+ ((t_base_edge(Xa,Ya,_,_,_), Xa~=X1, X1*Ya =< X1*Y1)),
  Y2 is -X1, \+((Y2~=Y1)),
  L is abs(Y2-Y1),
  assertz(t_base_edge(X1,Y1,X1,Y2,L)),
  (cm3_verbose -> tab(2), write('closed edge '), write([X1,Y1,X1,Y2]), nl ; true),
  !,
  t_repair_breaks.

t_repair_breaks :-
  t_base_edge(_,_,X1,Y1,_), (Y1~=5 ; Y1~= -5),
  \+ ((t_base_edge(Xa,Ya,_,_,_), Ya~=Y1, Y1*Xa >= Y1*X1)),
  X2 is Y1, \+((X2~=X1)),
  L is abs(X2-X1),
  assertz(t_base_edge(X1,Y1,X2,Y1,L)),
  (cm3_verbose -> tab(2), write('closed edge '), write([X1,Y1,X2,Y1]), nl ; true),
  !,
  t_repair_breaks.

t_repair_breaks :-
  t_base_edge(X1,Y1,_,_,_), (X1~=5 ; X1~= -5),
  \+ ((t_base_edge(_,_,Xa,Ya,_), Xa~=X1, X1*Ya >= X1*Y1)),
  Y2 is -X1, \+((Y2~=Y1)),
  L is abs(Y2-Y1),
  assertz(t_base_edge(X1,Y2,X1,Y1,L)),
  (cm3_verbose -> tab(2), write('closed edge '), write([X1,Y2,X1,Y1]), nl ; true),
  !,
  t_repair_breaks.

t_repair_breaks :-
  t_base_edge(X1,Y1,_,_,_), (Y1~=5 ; Y1~= -5),
  \+ ((t_base_edge(_,_,Xa,Ya,_), Ya~=Y1, Y1*Xa =< Y1*X1)),
  X2 is Y1, \+((X2~=X1)),
  L is abs(X2-X1),
  assertz(t_base_edge(X2,Y1,X1,Y1,L)),
  (cm3_verbose -> tab(2), write('closed edge '), write([X2,Y1,X1,Y1]), nl ; true),
  !,
  t_repair_breaks.

t_repair_breaks.

t_make_corners :-
  retractall(t_base_corner(_,_,_,_,_,_,_,_,_,_)),
  t_base_edge(X1,Y1,X2,Y2,L12), \+((L12 ~= 0.0)),
  t_base_edge(X2a,Y2a,X3,Y3,L23), X2a ~= X2, Y2a~= Y2, \+((L23 ~= 0.0)),
  \+((X1 ~= X3, Y1 ~= Y3)),
  t_corner_trig(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,Sin,Theta),
  Theta>0.0001, 2*pi-Theta>0.0001,
  \+ t_base_corner(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,Sin,Theta),
  assertz(t_base_corner(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,Sin,Theta)),
  fail.

t_make_corners :-
  t_base_corner(X1,Y1,X2,Y2,_,_,_,_,_,Theta),
  clause(t_base_corner(X1a,Y1a,X2a,Y2a,_,_,_,_,_,Thetaa),true,Cref),
  X1a~=X1, Y1a~=Y1, X2a~=X2, Y2a~=Y2,
  Thetaa<Theta,
  erase(Cref),
  (cm3_verbose -> tab(2), write('inner corner at '), write([X2,Y2]), write(' dropped'), nl ; true),
  fail.

t_make_corners :-
  t_base_corner(_,_,_,_,X2,Y2,X3,Y3,_,Theta),
  clause(t_base_corner(_,_,_,_,X2a,Y2a,X3a,Y3a,_,Thetaa),true,Cref),
  X2a~=X2, Y2a~=Y2, X3a~=X3, Y3a~=Y3,
  Thetaa<Theta,
  erase(Cref),
  (cm3_verbose -> tab(2), write('inner corner at '), write([X2,Y2]), write(' dropped'), nl ; true),
  fail.

/*
t_make_corners :-
  clause(t_base_corner(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,_,Theta),true,Cref),
  t_base_corner(X1b,Y1b,X2b,Y2b,X2ab,Y2ab,X3b,Y3b,_,Thetab),
  X1b~=X1, Y1b~=Y1, X2b~=X2, Y2b~=Y2,
  Thetab<Theta,
  erase(Cref),
  t_corner_trig(X3b,Y3b,X2ab,Y2ab,X2a,Y2a,X3,Y3,Sin1,Theta1),
  \+ t_base_corner(X3b,Y3b,X2ab,Y2ab,X2a,Y2a,X3,Y3,Sin1,Theta1),
  assertz(t_base_corner(X3b,Y3b,X2ab,Y2ab,X2a,Y2a,X3,Y3,Sin1,Theta1)),
  (cm3_verbose -> tab(2), write('corners at '), write([X2,Y2]), write(' differenced'), nl ; true),
  t_back_propagate(X2a,Y2a,X3,Y3),
  fail.

t_make_corners :-
  clause(t_base_corner(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,_,Theta),true,Cref),
  t_base_corner(X1b,Y1b,X2b,Y2b,X2ab,Y2ab,X3b,Y3b,_,Thetab),
  X2ab~=X2a, Y2ab~=Y2a, X3b~=X3, Y3b~=Y3,
  Thetab<Theta,
  erase(Cref),
  t_corner_trig(X1,Y1,X2,Y2,X2b,Y2b,X1b,Y1b,Sin1,Theta1),
  \+ t_base_corner(X1,Y1,X2,Y2,X2b,Y2b,X1b,Y1b,Sin1,Theta1),
  assertz(t_base_corner(X1,Y1,X2,Y2,X2b,Y2b,X1b,Y1b,Sin1,Theta1)),
  (cm3_verbose -> tab(2), write('corners at '), write([X2,Y2]), write(' differenced'), nl ; true),
  t_back_propagate(X2b,Y2b,X1b,Y1b),
  fail.
*/

t_make_corners.
    
t_repair_indentations :-
  clause(t_base_corner(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,_,Theta),true,Cref), Theta>pi,
  t_cut_target(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,Xc,Yc),
  t_corner_trig(X1,Y1,X2,Y2,X2,Y2,Xc,Yc,Sin1,Theta1),
  t_corner_trig(Xc,Yc,X2a,Y2a,X2a,Y2a,X3,Y3,Sin2,Theta2),
  Theta1=<pi, Theta2=<pi,
  clause(t_base_corner(Xc1,Yc1,Xc,Yc,Xca,Yca,Xc2,Yc2,_,Thetac),true,Crefc),
  t_corner_trig(Xc1,Yc1,Xc,Yc,Xc,Yc,X2a,Y2a,Sinc1,Thetac1),
  t_corner_trig(X2,Y2,Xca,Yca,Xca,Yca,Xc2,Yc2,Sinc2,Thetac2),
  abs(Thetac1+Thetac2-Thetac) < 0.0001,
  erase(Cref),
  assertz(t_base_corner(X1,Y1,X2,Y2,X2,Y2,Xc,Yc,Sin1,Theta1)),
  assertz(t_base_corner(Xc,Yc,X2a,Y2a,X2a,Y2a,X3,Y3,Sin2,Theta2)),
  erase(Crefc),
  assertz(t_base_corner(Xc1,Yc1,Xc,Yc,Xc,Yc,X2a,Y2a,Sinc1,Thetac1)),
  assertz(t_base_corner(X2,Y2,Xca,Yca,Xca,Yca,Xc2,Yc2,Sinc2,Thetac2)),
  (cm3_verbose -> tab(2), write('cut from '), write([X2,Y2]), write(' to '), write([Xc,Yc]), nl ; true),
  !,
  t_repair_indentations.

t_repair_indentations.


/* ================ */
/* t_corner_trig/10 */
/* t_cut_target/10  */
/* t_cut_target/19  */
/* ================ */
  
t_corner_trig(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,Sin,Theta) :-
  Sin is ((X1-X2)*(Y3-Y2a)-(X3-X2a)*(Y1-Y2)),
  Cos is ((X1-X2)*(X3-X2a)+(Y1-Y2)*(Y3-Y2a)),
  Theta0 is atan(Sin,Cos),
  (Theta0<0.0 -> Theta is Theta0+2.0*pi ; Theta is Theta0).

t_cut_target(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,Xt,Yt) :-
  setof([R,X,Y],t_cut_target(X1,Y1,X2,Y2,X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,X2a,Y2a,X3,Y3,X,Y,R),[[_,Xt,Yt]|_]).

t_cut_target(Xt,Yt,_,_,_,_,X2,Y2,X2a,Y2a,X3,Y3,_,_,_,_,Xt,Yt,R) :-
  (Xt-X2)*(Y3-Y2a)>=(X3-X2a)*(Yt-Y2),
  R is sqrt((Xt-X2)**2+(Yt-Y2)**2).

t_cut_target(_,_,_,_,X1,Y1,X2,Y2,X2a,Y2a,_,_,_,_,Xt,Yt,Xt,Yt,R) :-
  (X1-X2)*(Yt-Y2a)>=(Xt-X2a)*(Y1-Y2),
  R is sqrt((Xt-X2a)**2+(Yt-Y2a)**2).

t_cut_target(Xa2,Ya2,Xa1,Ya1,X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,Xb1,Yb1,Xb2,Yb2,Xt,Yt,R) :-
  \+((Xa2~=Xb2, Ya2~=Yb2)),
  t_base_corner(Xa3,Ya3,Xa2a,Ya2a,Xa2x,Ya2x,Xa1x,Ya1x,_,_), Xa2x~=Xa2, Ya2x~=Ya2, Xa1x~=Xa1, Ya1x~=Ya1,
  \+((Xa3~=Xb2, Ya3~=Yb2)),
  t_base_corner(Xb1x,Yb1x,Xb2x,Yb2x,Xb2a,Yb2a,Xb3,Yb3,_,_), Xb1x~=Xb1, Yb1x~=Yb1, Xb2x~=Xb2, Yb2x~=Yb2,
  \+((Xb3~= Xa2, Yb3~=Ya2)),
  t_cut_target(Xa3,Ya3,Xa2a,Ya2a,X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,Xb2a,Yb2a,Xb3,Yb3,Xt,Yt,R).


/* ================== */
/* t_back_propagate/4 */
/* ================== */

/*
t_back_propagate(X3,Y3,X2a,Y2a) :-
  \+ t_base_corner(X3,Y3,X2a,Y2a,_,_,_,_,_,_),
  t_base_corner(X1,Y1,X2,Y2,X2a,Y2a,X3,Y3,_,_),
  t_corner_trig(X3,Y3,X2a,Y2a,X2,Y2,X1,Y1,Sin,Theta),
  assertz(t_base_corner(X3,Y3,X2a,Y2a,X2,Y2,X1,Y1,Sin,Theta)),
  (cm3_verbose -> tab(2), write('back propagating '), write([X3,Y3,X2a,Y2a]), nl; true),
  t_back_propagate(X2,Y2,X1,Y1).
*/
  
t_back_propagate(_,_,_,_).
