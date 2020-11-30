/* ================================================== */ 
/*                                                    */
/* Predicates for rebuilding aabb data in walkmeshes  */
/* Part of the CleanModels 3 suite by OldMansBeard    */
/*                                                    */
/* This version dated 2013-07-15                      */
/*                                                    */
/* ================================================== */ 

/*
Explanation
-----------
The aabb data represents a binary tree, written depth-first, one node per line.
The root node is written first (on the same line as the word 'aabb'),
followed by its whole "left" sub-tree, followed by its whole "right" subtree.

Each tree node repesents a bounding box for a subset of the faces in the
walkmesh geometry. The first six numbers in the line are X1,Y1,Z1,X2,Y2,Z2
where [X1,Y1,Z1] and [X2,Y2,Z2] are diagonally opposite corners of the box
such that X1<=X2, Y1<=Y2, Z2<=Z2.

The root node is the bounding box for the whole geometry. Walkmesh geometry is
not allowed to extend beyond the tile boundaries, so X1<-5.0 or X2>5.0 or
Y1<-5.0 or Y2>5.0 would be an error.

The subtrees of a tree node are formed by splitting the subset of faces that 
the node bounds, into non-empty "left" and "right" co-subsets and (recursively)
forming the subtrees for each subset.

Leaf nodes bound single faces, so they do not have subtrees. The number of the
face that a leaf node bounds, is written as the seventh number on the line. If the
tree node is not a leaf node, "-1" is written there.

The way that a set of faces is split into "left" and "right" subsets is to compare
the centroids of each of the faces with the average of the centroids of the whole
list. Those that lie to one side of it along a particular axis go into "left" 
and the rest go into "right". The axis is one of the three orthogonal axes of the 
box. The "long" axis of the box is tried first but if that results in one list or 
the other being empty, another axis is tried. If no axis is successful is must mean
that all of the faces in the list have the same centroids. So they must be overlapping
or crossing in some strange way. If this happens, crash out - the walkmesh geometry
is bad.
*/

/* ================ */
/* w_rebuild_aabb/4 */
/* ================ */

w_rebuild_aabb(File,Model,NodeName,NodeRef) :-
  retract(gotdata(File,Model,NodeName,NodeRef,aabb(_))),
  fail.

w_rebuild_aabb(File,Model,NodeName,NodeRef) :-
  retract(gotdata(File,Model,NodeName,NodeRef,aabb(_,_,_,_,_,_,_,_))),
  fail.

w_rebuild_aabb(File,Model,NodeName,NodeRef) :-
  \+((gotdata(File,Model,NodeName,NodeRef,faces(N)), N>0)),
  tab(2), write('aabb node '), write(NodeName), write(' has no geometry'), nl,
  !, fail.

w_rebuild_aabb(File,Model,NodeName,NodeRef) :-
  w_facelist(File,Model,NodeName,NodeRef,FaceList),
  w_make_tree(File,Model,NodeName,NodeRef,FaceList,0,N),
  assertz(gotdata(File,Model,NodeName,NodeRef,aabb(N))),
  tab(2), write('aabb data rebuilt'), nl.

/* ============== */
/* w_facelist/5   */
/* ============== */

w_facelist(File,Model,NodeName,NodeRef,FaceList) :-
  setof(F,V1^V2^V3^G^T1^T2^T3^M^gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,G,T1,T2,T3,M)),FaceList),
  FaceList\=[].
  
/* ============= */
/* w_make_tree/7 */
/* ============= */

w_make_tree(File,Model,NodeName,NodeRef,[Face],Seq,Next) :-
  w_bounding_box(File,Model,NodeName,NodeRef,[Face],[Xmin,Ymin,Zmin,Xmax,Ymax,Zmax]),
  assertz(gotdata(File,Model,NodeName,NodeRef,aabb(Seq,Xmin,Ymin,Zmin,Xmax,Ymax,Zmax,Face))),
  Next is Seq+1,
  !.

w_make_tree(File,Model,NodeName,NodeRef,FaceList,Seq,Next) :-
  FaceList=[_,_|_],
  w_bounding_box(File,Model,NodeName,NodeRef,FaceList,Box),
  Box=[Xmin,Ymin,Zmin,Xmax,Ymax,Zmax],
  assertz(gotdata(File,Model,NodeName,NodeRef,aabb(Seq,Xmin,Ymin,Zmin,Xmax,Ymax,Zmax,-1))),
  w_split_facelist(File,Model,NodeName,NodeRef,FaceList,Box,Left,Right),
  NLeftStart is Seq+1,
  w_make_tree(File,Model,NodeName,NodeRef,Left,NLeftStart,NRightStart),
  w_make_tree(File,Model,NodeName,NodeRef,Right,NRightStart,Next),
  !.
  
/* ================ */
/* w_bounding_box/6 */
/* ================ */

w_bounding_box(File,Model,NodeName,NodeRef,[Face],[Xmin,Ymin,Zmin,Xmax,Ymax,Zmax]) :-
  gotdata(File,Model,NodeName,NodeRef,faces(Face,V1,V2,V3,_,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,Z3)),
  Xmin is min(X1,min(X2,X3)),
  Ymin is min(Y1,min(Y2,Y3)),
  Zmin is min(Z1,min(Z2,Z3)),
  Xmax is max(X1,max(X2,X3)),
  Ymax is max(Y1,max(Y2,Y3)),
  Zmax is max(Z1,max(Z2,Z3)), !.

w_bounding_box(_,_,NodeName,_,[Face],_) :-
  tab(2), write('cannot form legal bounding box for face '), write(Face), write(' of '), write(NodeName), nl,
  !, fail.

w_bounding_box(File,Model,NodeName,NodeRef,[F|F0],[Xmin,Ymin,Zmin,Xmax,Ymax,Zmax]) :-
  F0=[_|_],
  w_bounding_box(File,Model,NodeName,NodeRef,F0,[XminF0,YminF0,ZminF0,XmaxF0,YmaxF0,ZmaxF0]),
  w_bounding_box(File,Model,NodeName,NodeRef,[F],[XminF,YminF,ZminF,XmaxF,YmaxF,ZmaxF]),
  Xmin is min(XminF0,XminF),
  Ymin is min(YminF0,YminF),
  Zmin is min(ZminF0,ZminF),
  Xmax is max(XmaxF0,XmaxF),
  Ymax is max(YmaxF0,YmaxF),
  Zmax is max(ZmaxF0,ZmaxF), !.

/* ================== */
/* w_split_facelist/8 */
/* w_split_facelist/9 */
/* ================== */

w_split_facelist(File,Model,NodeName,NodeRef,FaceList,Box,Left,Right) :-
  w_mid_point_average(File,Model,NodeName,NodeRef,FaceList,Centre),
  w_try_axis(Box,Axis),
  w_split_facelist(File,Model,NodeName,NodeRef,FaceList,Centre,Axis,Left,Right),
  Left\=[], Right\=[], !.

w_split_facelist(_,_,_,_,FaceList,_,_,_) :-
  tab(2), write('*** cannot split face list '), write(FaceList), nl,
  fail.

w_split_facelist(_,_,_,_,[],_,_,[],[]) :- !.

w_split_facelist(File,Model,NodeName,NodeRef,[F|F0],Centre,Axis,Left,Right) :-
  w_split_facelist(File,Model,NodeName,NodeRef,F0,Centre,Axis,Left0,Right0),
  w_centroid(File,Model,NodeName,NodeRef,F,Centroid),
  (w_left_of(Centroid,Centre,Axis) ->
    Left=[F|Left0], Right=Right0; Left=Left0, Right=[F|Right0]),
  !.

/* ===================== */
/* w_mid_point_average/6 */
/* w_mid_point_sum/6     */
/* ===================== */

w_mid_point_average(_,_,_,_,[],[0,0,0]) :- !.

w_mid_point_average(File,Model,NodeName,NodeRef,FaceList,[X,Y,Z]) :-
  w_mid_point_sum(File,Model,NodeName,NodeRef,FaceList,[Xs,Ys,Zs]),
  length(FaceList,L),
  X is Xs/L, Y is Ys/L, Z is Zs/L, !.

w_mid_point_sum(_,_,_,_,[],[0,0,0]) :- !.

w_mid_point_sum(File,Model,NodeName,NodeRef,[F|F0],[X,Y,Z]) :-
  w_mid_point_sum(File,Model,NodeName,NodeRef,F0,[XF0,YF0,ZF0]),
  w_centroid(File,Model,NodeName,NodeRef,F,[XF,YF,ZF]),
  X is XF+XF0, Y is YF+YF0, Z is ZF+ZF0, !.

/* ============ */
/* w_centroid/6 */
/* ============ */

w_centroid(File,Model,NodeName,NodeRef,Face,[X,Y,Z]) :-
  gotdata(File,Model,NodeName,NodeRef,faces(Face,V1,V2,V3,_,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,Z3)),
  X is (X1+X2+X3)/3,
  Y is (Y1+Y2+Y3)/3,
  Z is (Z1+Z2+Z3)/3, !.

/* ============ */
/* w_try_axis/2 */
/* ============ */

w_try_axis([X1,Y1,Z1,X2,Y2,Z2],Axis) :-
  (X2-X1)>=(Y2-Y1), (Y2-Y1)>=(Z2-Z1), !,
  (Axis=x; Axis=y; Axis=z).

w_try_axis([X1,Y1,Z1,X2,Y2,Z2],Axis) :-
  (X2-X1)>=(Z2-Z1), (Z2-Z1)>=(Y2-Y1), !,
  (Axis=x; Axis=z; Axis=y).

w_try_axis([X1,Y1,Z1,X2,Y2,Z2],Axis) :-
  (Y2-Y1)>=(Z2-Z1), (Z2-Z1)>=(X2-X1), !,
  (Axis=y; Axis=z; Axis=x).

w_try_axis([X1,Y1,Z1,X2,Y2,Z2],Axis) :-
  (Y2-Y1)>=(X2-X1), (X2-X1)>=(Z2-Z1), !,
  (Axis=y; Axis=x; Axis=z).

w_try_axis([X1,Y1,Z1,X2,Y2,Z2],Axis) :-
  (Z2-Z1)>=(X2-X1), (X2-X1)>=(Y2-Y1), !,
  (Axis=z; Axis=x; Axis=y).

w_try_axis([X1,Y1,Z1,X2,Y2,Z2],Axis) :-
  (Z2-Z1)>=(Y2-Y1), (Y2-Y1)>=(X2-X1), !,
  (Axis=z; Axis=y; Axis=x).

/* ============ */
/* w_left_of/3  */
/* ============ */

w_left_of([X,_,_],[X0,_,_],x) :- X<X0.
w_left_of([_,Y,_],[_,Y0,_],y) :- Y<Y0.
w_left_of([_,_,Z],[_,_,Z0],z) :- Z<Z0.

