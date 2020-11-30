/* ================================================== */ 
/*                                                    */
/* Predicates for fixing pivots in gotdata            */
/* Part of the CleanModels 3 suite by OldMansBeard    */
/*                                                    */
/* This version dated 2013-07-15                      */
/*                                                    */
/* ================================================== */ 

small(0.0).
maxfaces(150,400).

:- dynamic face_group/2.
:- dynamic neighbourhood/7.
:- dynamic ground_face/1.

/* ============== */
/* f_find_pivot/5 */
/* ============== */

f_find_pivot(File,Model,NodeName,NodeRef,Pivot) :-
  bagof(C,f_pivot_constraint(File,Model,NodeName,NodeRef,C),Constraints),
  f_top_centre(File,Model,NodeName,NodeRef,TopCentre),
  w_facelist(File,Model,NodeName,NodeRef,FaceList),
  w_mid_point_average(File,Model,NodeName,NodeRef,FaceList,Centre),
  ( f_satisfies_constraints(TopCentre,Constraints) ->
    Pivot=TopCentre
    ;
    f_satisfies_constraints(Centre,Constraints) ->
    Pivot=Centre
    ;
    maxfaces(_,Max), gotdata(File,Model,NodeName,NodeRef,faces(N)), N<Max,
    f_pre_box([-100,-100,-100,100,100,100],Constraints,Box1,_),
    f_solve_pivot(Box1,Constraints,Box), f_centroid(Box,Pivot)
  ).

f_top_centre(File,Model,NodeName,NodeRef,[Xs,Ys,Zs]) :-
  once((
    gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z0)),
    \+ (gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z1)), Z1>Z0)
      )),
  bagof([N,X,Y],gotdata(File,Model,NodeName,NodeRef,verts(N,X,Y,Z0)),B),
  middle(B,X0,Y0),
  snap(X0,Xs), snap(Y0,Ys), snap(Z0,Zs),
  !.
  
/* ==================== */
/* f_pivot_constraint/5 */
/* ==================== */

f_pivot_constraint(File,Model,NodeName,NodeRef,Constraint) :-
  f_transform_constraint(File,Model,NodeName,NodeRef,[5,1,0,0],Constraint).

f_pivot_constraint(File,Model,NodeName,NodeRef,Constraint) :-
  f_transform_constraint(File,Model,NodeName,NodeRef,[5,-1,0,0],Constraint).

f_pivot_constraint(File,Model,NodeName,NodeRef,Constraint) :-
  f_transform_constraint(File,Model,NodeName,NodeRef,[5,0,1,0],Constraint).

f_pivot_constraint(File,Model,NodeName,NodeRef,Constraint) :-
  f_transform_constraint(File,Model,NodeName,NodeRef,[5,0,-1,0],Constraint).

f_pivot_constraint(File,Model,NodeName,NodeRef,Constraint) :-
  once(g_user_option('pivots_below_z=0',AllowBelow)), AllowBelow==disallow,
  f_transform_constraint(File,Model,NodeName,NodeRef,[0,0,0,1],Constraint).

/*
f_pivot_constraint(File,Model,NodeName,NodeRef,Constraint) :-
  f_transform_constraint(File,Model,NodeName,NodeRef,[30,0,0,-1],Constraint).
*/

f_pivot_constraint(File,Model,NodeName,NodeRef,[A,B,C,D]) :-
  face_normal(File,Model,NodeName,NodeRef,N,depth(A)),
  face_normal(File,Model,NodeName,NodeRef,N,[Nx,Ny,Nz]),
  B is -Nx, C is -Ny, D is -Nz.

/* ======================== */
/* f_transform_constraint/6 */
/* ======================== */

f_transform_constraint(File,Model,NodeName,NodeRef,C,C) :-
  gotdata(File,Model,NodeName,NodeRef,parent('NULL')), !.

f_transform_constraint(File,Model,NodeName,NodeRef,Constraint,[A,B,C,D]) :-
  clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef),
  (gotdata(File,Model,NodeName,NodeRef,position(X,Y,Z)) -> true; X=0, Y=0, Z=0),
  (gotdata(File,Model,NodeName,NodeRef,orientation(Xv,Yv,Zv,Rot)) -> true; Xv=0, Yv=0, Zv=0, Rot=0),
  gotdata(File,Model,NodeName,NodeRef,parent(Parent/PRef)),
  f_transform_constraint(File,Model,Parent,PRef,Constraint,[A1,B1,C1,D1]),
  A is A1+(B1*X+C1*Y+D1*Z),
  /* Careful with the sign of this rotation */
  f_rotate_vector([B1,C1,D1],[Xv,Yv,Zv,-Rot],[B,C,D]).

/* =========== */
/* f_pre_box/4 */
/* =========== */

f_pre_box(Box,[],Box,[]) :- !.

f_pre_box(Box0,[[A,B,C,D]|Rest],Box,Constraints) :-
  B=:=0, C=:=0, D=:=0, !, A>0,
  f_pre_box(Box0,Rest,Box,Constraints).

f_pre_box([X1,Y1,Z1,X2,Y2,Z2],[[A,B,C,D]|Rest],Box,Constraints) :-
  B=:=0, C=:=0, D>0, !,
  (Z1=[] -> NewZ1 is -A/D; NewZ1 is max(Z1,-A/D)),
  (Z2=[] -> true; NewZ1=<Z2), 
  f_pre_box([X1,Y1,NewZ1,X2,Y2,Z2],Rest,Box,Constraints).

f_pre_box([X1,Y1,Z1,X2,Y2,Z2],[[A,B,C,D]|Rest],Box,Constraints) :-
  B=:=0, C=:=0, D<0, !,
  (Z2=[] -> NewZ2 is -A/D; NewZ2 is min(Z2,-A/D)),
  (Z1=[] -> true; NewZ2>=Z1),
  f_pre_box([X1,Y1,Z1,X2,Y2,NewZ2],Rest,Box,Constraints).

f_pre_box([X1,Y1,Z1,X2,Y2,Z2],[[A,B,C,D]|Rest],Box,Constraints) :-
  B=:=0, D=:=0, C>0, !,
  (Y1=[] -> NewY1 is -A/C; NewY1 is max(Y1,-A/C)),
  (Y2=[] -> true; NewY1=<Y2),
  f_pre_box([X1,NewY1,Z1,X2,Y2,Z2],Rest,Box,Constraints).

f_pre_box([X1,Y1,Z1,X2,Y2,Z2],[[A,B,C,D]|Rest],Box,Constraints) :-
  B=:=0, D=:=0, C<0, !,
  (Y2=[] -> NewY2 is -A/C; NewY2 is min(Y2,-A/C)),
  (Y1=[] -> true; NewY2>=Y1),
  f_pre_box([X1,Y1,Z1,X2,NewY2,Z2],Rest,Box,Constraints).

f_pre_box([X1,Y1,Z1,X2,Y2,Z2],[[A,B,C,D]|Rest],Box,Constraints) :-
  C=:=0, D=:=0, B>0, !,
  (X1=[] -> NewX1 is -A/B; NewX1 is max(X1,-A/B)),
  (X2=[] -> true; NewX1=<X2),
  f_pre_box([NewX1,Y1,Z1,X2,Y2,Z2],Rest,Box,Constraints).

f_pre_box([X1,Y1,Z1,X2,Y2,Z2],[[A,B,C,D]|Rest],Box,Constraints) :-
  C=:=0, D=:=0, B<0, !,
  (X2=[] -> NewX2 is -A/B; NewX2 is min(X2,-A/B)),
  (X1=[] -> true; NewX2>=X1),
  f_pre_box([X1,Y1,Z1,NewX2,Y2,Z2],Rest,Box,Constraints).

f_pre_box(Box0,[C|C0],Box,[C|Constraints]) :-
  f_pre_box(Box0,C0,Box,Constraints).

/* ============== */
/* f_centroid/2   */
/* f_medial/3     */
/* f_within_box/2 */
/* ============== */

f_centroid([X1,Y1,Z1,X2,Y2,Z2],[X,Y,Z]) :-
  f_medial(X2,X1,X),
  f_medial(Y2,Y1,Y),
  f_medial(Z2,Z1,Z).

f_medial(100,-100,0)    :- !.
f_medial(100,Min,0)   :- Min<0, !.
f_medial(Max,-100,0)   :- Max>0, !.

f_medial(100,Min,X) :- Min>=0, !, snap_up(Min,X).
f_medial(Max,-100,X) :- Max=<0, !, snap_down(Max,X).
f_medial(Max,Min,X)  :-
  Max>=Min,
  Xmean is (Max+Min)/2,
  snap(Xmean,Xs),
  (Xs>=Min, Xs=<Max -> X=Xs ; X=Xmean), !.

f_within_box([X,Y,Z],[X1,Y1,Z1,X2,Y2,Z2]) :- X>=X1, X=<X2, Y>=Y1, Y=<Y2, Z>=Z1, Z=<Z2.

/* =============== */
/* f_solve_pivot/3 */
/* =============== */

f_solve_pivot(Box,[],Box) :- !.

f_solve_pivot(Box1,Constraints,Box) :-
  f_split_by_D(Constraints,D_Zero,D_Plus,D_Minus),
  f_join_over_Z(D_Plus,D_Minus,D_Zero,ConstraintsXY),
  f_trim_with_box(Box1,ConstraintsXY,Box2),
  f_box_valid(Box2),
  maxfaces(Max,_), length(ConstraintsXY,Nxy),
  (Nxy=<Max -> f_tighten_box(Box2,ConstraintsXY,Box3); Box3=Box2),
  f_box_valid(Box3),
  f_solve_Z(Box3,Constraints,Box).

/* =============== */
/* f_split_by_D/4  */
/* =============== */

f_split_by_D([],[],[],[]) :- !.

f_split_by_D([[A,B,C,D]|Rest],L0,[[A1,B1,C1]|L1],L2) :-
  small(Small), D>Small, !, A1 is -A/D, B1 is -B/D, C1 is -C/D,
  f_split_by_D(Rest,L0,L1,L2).

f_split_by_D([[A,B,C,D]|Rest],L0,L1,[[A1,B1,C1]|L2]) :-
  small(Small), D< -Small, !, A1 is -A/D, B1 is -B/D, C1 is -C/D,
  f_split_by_D(Rest,L0,L1,L2).

f_split_by_D([[A,B,C,_]|Rest],[[A,B,C]|L0],L1,L2) :-
  f_split_by_D(Rest,L0,L1,L2).

/* ================= */
/* f_join_over_Z/4   */
/* f_join_two/4      */
/* ================= */

f_join_over_Z([],_,L,L) :- !.

f_join_over_Z([D_Plus|D0],D_Minus,L0,L) :-
  f_join_over_Z(D0,D_Minus,L0,L1), !,
  f_join_two(D_Plus,D_Minus,L1,L).

f_join_two(_,[],L,L) :- !.

f_join_two([A1,B1,C1],[[A2,B2,C2]|D_Minus],L0,[[A,B,C]|L1]) :-
  f_join_two([A1,B1,C1],D_Minus,L0,L1), !,
  A is A2-A1, B is B2-B1, C is C2-C1.

/* ================= */
/* f_trim_with_box/3 */
/* ================= */

f_trim_with_box(Box,[],Box) :- !.

f_trim_with_box(Box1,[C1|C0],Box) :-
  f_truncates_box(C1,Box1,Box2), !,
  f_trim_with_box(Box2,C0,Box).

/* =============== */
/* f_truncates_box */
/* =============== */

f_truncates_box([A,B,C],[X1,Y1,Z1,X2,Y2,Z2],[NewX1,NewY1,Z1,X2,Y2,Z2]) :-
  small(Small), B>Small, C>Small, !,
  NewX1 is max(X1,-(A+C*Y2)/B), NewX1=<X2,
  NewY1 is max(Y1,-(A+B*X2)/C), NewY1=<Y2.

f_truncates_box([A,B,C],[X1,Y1,Z1,X2,Y2,Z2],[NewX1,Y1,Z1,X2,NewY2,Z2]) :-
  small(Small), B>Small, C< -Small, !,
  NewX1 is max(X1,-(A+C*Y1)/B), NewX1=<X2,
  NewY2 is min(Y2,-(A+B*X2)/C), NewY2>=Y1.

f_truncates_box([A,B,_],[X1,Y1,Z1,X2,Y2,Z2],[NewX1,Y1,Z1,X2,Y2,Z2]) :-
  small(Small), B>Small, !, NewX1 is max(X1,-A/B), NewX1=<X2.

f_truncates_box([A,B,C],[X1,Y1,Z1,X2,Y2,Z2],[X1,NewY1,Z1,NewX2,Y2,Z2]) :-
  small(Small), B< -Small, C>Small, !,
  NewX2 is min(X2,-(A+C*Y2)/B), NewX2>=X1,
  NewY1 is max(Y1,-(A+B*X1)/C), NewY1=<Y2.

f_truncates_box([A,B,C],[X1,Y1,Z1,X2,Y2,Z2],[X1,Y1,Z1,NewX2,NewY2,Z2]) :-
  small(Small), B< -Small, C< -Small, !,
  NewX2 is min(X2,-(A+C*Y1)/B), NewX2>=X1,
  NewY2 is min(Y2,-(A+B*X1)/C), NewY2>=Y1.

f_truncates_box([A,B,_],[X1,Y1,Z1,X2,Y2,Z2],[X1,Y1,Z1,NewX2,Y2,Z2]) :-
  small(Small), B< -Small, !, NewX2 is min(X2,-A/B), NewX2>=X1.

f_truncates_box([A,_,C],[X1,Y1,Z1,X2,Y2,Z2],[X1,NewY1,Z1,X2,Y2,Z2]) :-
  small(Small), C>Small, !, NewY1 is max(Y1,-A/C), NewY1=<Y2.

f_truncates_box([A,_,C],[X1,Y1,Z1,X2,Y2,Z2],[X1,Y1,Z1,X2,NewY2,Z2]) :-
  small(Small), C< -Small, !, NewY2 is min(Y2,-A/C), NewY2>=Y1.

f_truncates_box(_,Box,Box).

/* ============= */
/* f_box_valid/1 */
/* f_solve_Z/3   */
/* f_box_Z/3     */
/* f_valid_Z/1   */
/* ============= */

f_box_valid([X1,_,_,X2,_,_]) :- X1>X2, !, fail.
f_box_valid([_,Y1,_,_,Y2,_]) :- Y1>Y2, !, fail.
f_box_valid(_).

f_solve_Z(Box,[],Box) :- !.
f_solve_Z(Box1,[C|C0],Box) :- f_box_Z(Box1,C,Box2), f_valid_Z(Box2), !, f_solve_Z(Box2,C0,Box).

f_box_Z([X1,Y1,Z1,X2,Y2,Z2],[A,B,C,D],[X1,Y1,NewZ1,X2,Y2,Z2]) :-
  small(Small), D>Small, !, NewZ1 is max(Z1,-(A+0.5*(B*(X1+X2)+C*(Y1+Y2)))/D).

f_box_Z([X1,Y1,Z1,X2,Y2,Z2],[A,B,C,D],[X1,Y1,Z1,X2,Y2,NewZ2]) :-
  small(Small), D< -Small, !, NewZ2 is min(Z2,-(A+0.5*(B*(X1+X2)+C*(Y1+Y2)))/D).

f_box_Z(Box,_,Box).  

f_valid_Z([_,_,[],_,_,_]) :- !.
f_valid_Z([_,_,_,_,_,[]]) :- !.
f_valid_Z([_,_,Z1,_,_,Z2]) :- Z2>=Z1.

/* ================= */
/* f_tighten_box/3   */
/* f_tighten_box_1/4 */
/* f_tighten_box_B/4 */
/* f_tighten_box_C/4 */
/* ================= */

f_tighten_box(Box,[],Box) :- !.

f_tighten_box(Box,[[A,B,C]|Rest],Box0) :-
  f_tighten_box_1(Box,[A,B,C],Rest,Box1),
  !,
  f_box_valid(Box1),
  f_tighten_box(Box1,Rest,Box0).

f_tighten_box_1(Box,_,[],Box) :- !.

f_tighten_box_1(Box,[A,B,C],[[A1,B1,C1]|Rest],Box0) :-
  f_tighten_box_B(Box,[A,B,C],[A1,B1,C1],Box1),
  f_tighten_box_C(Box1,[A,B,C],[A1,B1,C1],Box2),
  !,
  f_box_valid(Box2),
  f_tighten_box_1(Box2,[A,B,C],Rest,Box0).
  
f_tighten_box_B([X1,Y1,Z1,X2,Y2,Z2],[A1,B1,C1],[A2,B2,C2],[X1,NewY1,Z1,X2,NewY2,Z2]) :-
  B1>0, B2<0, !,
  T1 is C1/B1-C2/B2, T2 is A2/B2-A1/B1,
  (T1>0 -> NewY1 is max(Y1,T2/T1); NewY1=Y1),
  (T1<0 -> NewY2 is min(Y2,T2/T1); NewY2=Y2).

f_tighten_box_B([X1,Y1,Z1,X2,Y2,Z2],[A1,B1,C1],[A2,B2,C2],[X1,NewY1,Z1,X2,NewY2,Z2]) :-
  B1<0, B2>0, !,
  T1 is C1/B1-C2/B2, T2 is A2/B2-A1/B1,
  (T1<0 -> NewY1 is max(Y1,T2/T1); NewY1=Y1),
  (T1>0 -> NewY2 is min(Y2,T2/T1); NewY2=Y2).

f_tighten_box_B(Box,_,_,Box).

f_tighten_box_C([X1,Y1,Z1,X2,Y2,Z2],[A1,B1,C1],[A2,B2,C2],[NewX1,Y1,Z1,NewX2,Y2,Z2]) :-
  C1>0, C2<0, !,
  T1 is B1/C1-B2/C2, T2 is A2/C2-A1/C1,
  (T1>0 -> NewX1 is max(X1,T2/T1); NewX1=X1),
  (T1<0 -> NewX2 is min(X2,T2/T1); NewX2=X2).

f_tighten_box_C([X1,Y1,Z1,X2,Y2,Z2],[A1,B1,C1],[A2,B2,C2],[NewX1,Y1,Z1,NewX2,Y2,Z2]) :-
  C1<0, C2>0, !,
  T1 is B1/C1-B2/C2, T2 is A2/C2-A1/C1,
  (T1<0 -> NewX1 is max(X1,T2/T1); NewX1=X1),
  (T1>0 -> NewX2 is min(X2,T2/T1); NewX2=X2).

f_tighten_box_C(Box,_,_,Box).

/* =========== */
/* f_repivot/5 */
/* =========== */

f_repivot(File,Model,NodeName,NodeRef,[X,Y,Z]) :-
  (gotdata(File,Model,NodeName,NodeRef,verts(NVerts)) ->
     f_subtract_pivot_from_vertices(File,Model,NodeName,NodeRef,NVerts,0,[X,Y,Z]) ; true),
  f_subtract_pivot_from_children(File,Model,NodeName,NodeRef,[X,Y,Z]),
  (gotdata(File,Model,NodeName,NodeRef,orientation(U,V,W,A)) ->
     f_rotate_vector([X,Y,Z],[U,V,W,A],[X1,Y1,Z1]);
     X1=X, Y1=Y, Z1=Z),
  ( abs(X1)<1.0E-05, abs(Y1)<1.0E-05, abs(Z1)<1.0E-05 -> true
    ;
    retract(gotdata(File,Model,NodeName,NodeRef,position(X0,Y0,Z0))),
    X2 is X0+X1, (abs(X2)<1.0E-6 -> X3=0 ; X3=X2),
    Y2 is Y0+Y1, (abs(Y2)<1.0E-6 -> Y3=0 ; Y3=Y2),
    Z2 is Z0+Z1, (abs(Z2)<1.0E-6 -> Z3=0 ; Z3=Z2),
    asserta(gotdata(File,Model,NodeName,NodeRef,position(X3,Y3,Z3))),
    increment_bug_count(File)
  ).

/* ================================ */
/* f_subtract_pivot_from_vertices/7 */
/* ================================ */

f_subtract_pivot_from_vertices(_,_,_,_,NVerts,NVerts,_) :- !.

f_subtract_pivot_from_vertices(File,Model,NodeName,NodeRef,NVerts,ThisVert,[X,Y,Z]) :-
  ( abs(X)<1.0E-05, abs(Y)<1.0E-05, abs(Z)<1.0E-05 -> true
    ;
    retract(gotdata(File,Model,NodeName,NodeRef,verts(ThisVert,X0,Y0,Z0))),
    X1 is X0-X, (abs(X1)<1.0E-6 -> X2=0 ; X2=X1),
    Y1 is Y0-Y, (abs(Y1)<1.0E-6 -> Y2=0 ; Y2=Y1),
    Z1 is Z0-Z, (abs(Z1)<1.0E-6 -> Z2=0 ; Z2=Z1),
    asserta(gotdata(File,Model,NodeName,NodeRef,verts(ThisVert,X2,Y2,Z2))),
    increment_bug_count(File)
  ),
  NextVert is ThisVert+1, !,
  f_subtract_pivot_from_vertices(File,Model,NodeName,NodeRef,NVerts,NextVert,[X,Y,Z]).

/* ================================ */
/* f_subtract_pivot_from_children/5 */
/* f_subtract_pivot_from_child/5    */
/* ================================ */

f_subtract_pivot_from_children(File,Model,NodeName,NodeRef,Pivot) :-
  gotdata(File,Model,Child,ChildRef,parent(NodeName/NodeRef)),
  f_subtract_pivot_from_child(File,Model,Child,ChildRef,Pivot),
  fail.

f_subtract_pivot_from_children(_,_,_,_,_) :- !.

f_subtract_pivot_from_child(File,Model,Child,ChildRef,[X,Y,Z]) :-
  ( abs(X)<1.0E-05, abs(Y)<1.0E-05, abs(Z)<1.0E-05 -> true
    ;
    retract(gotdata(File,Model,Child,ChildRef,position(X0,Y0,Z0))),
    X1 is X0-X, (abs(X1)<1.0E-6 -> X2=0 ; X2=X1),
    Y1 is Y0-Y, (abs(Y1)<1.0E-6 -> Y2=0 ; Y2=Y1),
    Z1 is Z0-Z, (abs(Z1)<1.0E-6 -> Z2=0 ; Z2=Z1),
    asserta(gotdata(File,Model,Child,ChildRef,position(X2,Y2,Z2))),
    increment_bug_count(File)
  ), !.

/* ================= */
/* f_rotate_vector/3 */
/* ================= */

f_rotate_vector(V,[_,_,_,A],V) :- A=:=0, !.

f_rotate_vector(V,[X,Y,Z,_],V) :- X=:=0, Y=:=0, Z=:=0, !.

f_rotate_vector([Ux,Uy,Uz],[X0,Y0,Z0,Angle],[Vx,Vy,Vz]) :-
  R is sqrt(X0*X0+Y0*Y0+Z0*Z0),
  X is X0/R, Y is Y0/R, Z is Z0/R,
  C is cos(Angle), S is sin(Angle), T is 1.0-C,
  Vx is (T*X*X+C)*Ux + (T*X*Y-Z*S)*Uy + (T*X*Z+Y*S)*Uz,
  Vy is (T*X*Y+Z*S)*Ux + (T*Y*Y+C)*Uy + (T*Y*Z-X*S)*Uz,
  Vz is (T*X*Z-Y*S)*Ux + (T*Y*Z+X*S)*Uy + (T*Z*Z+C)*Uz.

/* ============================== */
/* f_rotate_rotation/3            */
/* f_convert_rotation_to_matrix/2 */
/* f_multiply_matrices/3          */
/* f_convert_matrix_to_rotation/2 */
/* ============================== */

f_rotate_rotation(R0,R,R1) :-
  f_convert_rotation_to_matrix(R0,M0),
  f_convert_rotation_to_matrix(R,M),
  f_multiply_matrices(M,M0,M1),
  f_convert_matrix_to_rotation(M1,R1), !.

f_convert_rotation_to_matrix([_,_,_,A],[[1,0,0],[0,1,0],[0,0,1]]) :- A=:=0.0, !.

f_convert_rotation_to_matrix([X,Y,Z,_],[[1,0,0],[0,1,0],[0,0,1]]) :- X=:=0.0, Y=:=0.0, Z=:=0.0, !.

f_convert_rotation_to_matrix([Xa,Ya,Za,A],[[A11,A12,A13],[A21,A22,A23],[A31,A32,A33]]) :-
  C is cos(A), S is sin(A), T is 1.0-C,
  R is sqrt(Xa*Xa+Ya*Ya+Za*Za),
  X is Xa/R, Y is Ya/R, Z is Za/R,
  A11 is T*X*X+C, A12 is T*X*Y-Z*S, A13 is T*X*Z+Y*S,
  A21 is T*X*Y+Z*S, A22 is T*Y*Y+C, A23 is T*Y*Z-X*S,
  A31 is T*X*Z-Y*S, A32 is T*Y*Z+X*S, A33 is T*Z*Z+C, !.

f_multiply_matrices([[A11,A12,A13],[A21,A22,A23],[A31,A32,A33]],
                    [[B11,B12,B13],[B21,B22,B23],[B31,B32,B33]],
                    [[C11,C12,C13],[C21,C22,C23],[C31,C32,C33]]) :-
  C11 is A11*B11+A12*B21+A13*B31,
  C12 is A11*B12+A12*B22+A13*B32,
  C13 is A11*B13+A12*B23+A13*B33,
  C21 is A21*B11+A22*B21+A23*B31,
  C22 is A21*B12+A22*B22+A23*B32,
  C23 is A21*B13+A22*B23+A23*B33,
  C31 is A31*B11+A32*B21+A33*B31,
  C32 is A31*B12+A32*B22+A33*B32,
  C33 is A31*B13+A32*B23+A33*B33, !.

f_convert_matrix_to_rotation([[A11,A12,A13],[A21,A22,A23],[A31,A32,A33]],[0,0,0,0]) :-
  A11=:=1, A12=:=0, A13=:=0,
  A21=:=0, A22=:=1, A23=:=0,
  A31=:=0, A32=:=0, A33=:=1, !.

f_convert_matrix_to_rotation([[A11,A12,A13],[A21,A22,A23],[A31,A32,A33]],[X,Y,Z,A]) :-
  ZS is (A21-A12)/2, YS is (A13-A31)/2, XS is (A32-A23)/2,
  S is sqrt(ZS*ZS+YS*YS+XS*XS),
  (S=:=0 -> [X,Y,Z,A] = [0,0,0,0] ;
   X is XS/S, Y is YS/S, Z is ZS/S,
   C is (A11+A22+A33-1)/2,
   A is atan(S,C)
  ), !.
  
/* ========================= */
/* f_satisfies_constraints/2 */
/* f_satisfies_constraint1/2 */
/* ========================= */

f_satisfies_constraints(_,[]) :- !.

f_satisfies_constraints(Pivot,[C|C0]) :-
  f_satisfies_constraint1(Pivot,C), !,
  f_satisfies_constraints(Pivot,C0).

f_satisfies_constraint1([X,Y,Z],[A,B,C,D]) :- A+B*X+C*Y+D*Z >= 0.0 .

/* =========================== */
/* f_split_node_by_face_list/9 */
/* =========================== */

/* Bugfix for CM312 2008-02-05 */
f_split_node_by_face_list(File,Model,NodeName,NodeRef,FaceList,NewName,NewRef,Criterion,SmallLogStream) :-
  f_copy_trimesh(File,Model,NodeName,NodeRef,NewName,NewRef,Criterion,FaceList),
  f_delete_faces_in_list(File,Model,NodeName,NodeRef,FaceList),
  (gotdata(File,Model,NewName,NewRef,tverts(NewNT)) -> renormalize_tverts(File,Model,NewName,NewRef,NewNT,_) ; true),
  prepare_face_normals(File,Model,NewName,NewRef),
  attempt_to_fix_pivots(File,Model,NewName,NewRef,SmallLogStream), !.

/* ================ */
/* f_copy_trimesh/8 */
/* ================ */

f_copy_trimesh(File,Model,NodeName,NodeRef,_,_,_,_) :-
  gotdata(File,Model,NodeName,NodeRef,_,_), !, fail.

f_copy_trimesh(File,Model,NodeName,NodeRef,NewName,NewRef,Criterion,FaceList) :-
  clause(gotdata(File,Model,node(trimesh,NodeName)),true,NodeRef),
  f_newname(NodeName,NewName),
  asserta(gotdata(File,Model,node(trimesh,NewName)),NewRef),
  once((
    gotdata(File,Model,NodeName,NodeRef,Q),
    Q=..[Q0|_], \+ member(Q0,[verts,tverts,faces,wirecolor]),
    asserta(gotdata(File,Model,NewName,NewRef,Q)),
    increment_bug_count(File),
    fail ; true
      )),
  Red is random(256)/256, Green is random(256)/256, Blue is random(256)/256,
  asserta(gotdata(File,Model,NewName,NewRef,wirecolor(Red,Green,Blue))),
  f_copy_geometry(File,Model,NodeName,NodeRef,NewName,NewRef,FaceList),
  length(FaceList,NFaces),
  tab(2), write(Criterion), write(' sub-object '), write(NewName), write(' detached with '), write(NFaces), write(' faces'), nl.

/* ================= */
/* f_copy_geometry/7 */
/* f_copy_verts/8    */
/* f_copy_tverts/8   */
/* f_copy_faces/9    */
/* ================= */

f_copy_geometry(File,Model,NodeName,NodeRef,NewName,NewRef,FaceList) :-
  f_copy_verts(File,Model,NodeName,NodeRef,NewName,NewRef,FaceList,VertsAssoc),
  f_copy_tverts(File,Model,NodeName,NodeRef,NewName,NewRef,FaceList,TVertsAssoc),
  f_copy_faces(File,Model,NodeName,NodeRef,NewName,NewRef,FaceList,VertsAssoc,TVertsAssoc), !.

f_copy_verts(File,Model,NodeName,NodeRef,NewName,NewRef,FaceList,VertsAssoc) :-
  f_make_verts_list(File,Model,NodeName,NodeRef,FaceList,VertsList),
  f_make_ordinal_assoc(VertsList,VertsAssoc),
  (member(V0,VertsList),
   gotdata(File,Model,NodeName,NodeRef,verts(V0,X,Y,Z)),
   get_assoc(V0,VertsAssoc,V1),
   asserta(gotdata(File,Model,NewName,NewRef,verts(V1,X,Y,Z))),
   (gotdata(File,Model,NodeName,NodeRef,weights(V0,W)) -> asserta(gotdata(File,Model,NodeName,NodeRef,weights(V1,W))) ; true),
   (gotdata(File,Model,NodeName,NodeRef,constraints(V0,C)) -> asserta(gotdata(File,Model,NodeName,NodeRef,constraints(V1,C))) ; true),
   (gotdata(File,Model,NodeName,NodeRef,colors(V0,R,G,B)) -> asserta(gotdata(File,Model,NodeName,NodeRef,colors(V1,R,G,B))) ; true),
   increment_bug_count(File),
   fail ; true),
  length(VertsList,NVerts),
  asserta(gotdata(File,Model,NewName,NewRef,verts(NVerts))),
  (gotdata(File,Model,NodeName,NodeRef,weights(_)) -> asserta(gotdata(File,Model,NodeName,NodeRef,weights(NVerts))) ; true),
  (gotdata(File,Model,NodeName,NodeRef,constraints(_)) -> asserta(gotdata(File,Model,NodeName,NodeRef,constraints(NVerts))) ; true),
  (gotdata(File,Model,NodeName,NodeRef,colors(_)) -> asserta(gotdata(File,Model,NodeName,NodeRef,colors(NVerts))) ; true),
  !.

f_copy_tverts(File,Model,NodeName,NodeRef,NewName,NewRef,FaceList,TVertsAssoc) :-
  f_make_tverts_list(File,Model,NodeName,NodeRef,FaceList,TVertsList),
  f_make_ordinal_assoc(TVertsList,TVertsAssoc),
  (member(T0,TVertsList),
   gotdata(File,Model,NodeName,NodeRef,tverts(T0,U,V,W)),
   get_assoc(T0,TVertsAssoc,T1),
   asserta(gotdata(File,Model,NewName,NewRef,tverts(T1,U,V,W))),
   increment_bug_count(File),
   fail ; true),
  length(TVertsList,NTVerts),
  ((NTVerts>1) -> asserta(gotdata(File,Model,NewName,NewRef,tverts(NTVerts))) ; true), !.

f_copy_faces(File,Model,NodeName,NodeRef,NewName,NewRef,FaceList,VertsAssoc,TVertsAssoc) :-
  f_make_ordinal_assoc(FaceList,FacesAssoc),
  (member(F0,FaceList),
   gotdata(File,Model,NodeName,NodeRef,faces(F0,V1,V2,V3,S,T1,T2,T3,M)),
   get_assoc(F0,FacesAssoc,F1),
   get_assoc(V1,VertsAssoc,NewV1),
   get_assoc(V2,VertsAssoc,NewV2),
   get_assoc(V3,VertsAssoc,NewV3),
   get_assoc(T1,TVertsAssoc,NewT1),
   get_assoc(T2,TVertsAssoc,NewT2),
   get_assoc(T3,TVertsAssoc,NewT3),
   asserta(gotdata(File,Model,NewName,NewRef,faces(F1,NewV1,NewV2,NewV3,S,NewT1,NewT2,NewT3,M))),
   increment_bug_count(File),
   fail ; true),
  length(FaceList,NFaces),
  asserta(gotdata(File,Model,NewName,NewRef,faces(NFaces))), !.

/* ========================= */
/* f_make_verts_list/6       */
/* f_make_tverts_list/6      */
/* f_make_verts_list_elem/6  */
/* f_make_tverts_list_elem/6 */
/* ========================= */

f_make_verts_list(File,Model,NodeName,NodeRef,FaceList,VertsList) :-
  setof(V,f_make_verts_list_elem(File,Model,NodeName,NodeRef,FaceList,V),VertsList).
  
f_make_tverts_list(File,Model,NodeName,NodeRef,FaceList,TVertsList) :-  
  setof(T,f_make_tverts_list_elem(File,Model,NodeName,NodeRef,FaceList,T),TVertsList).

f_make_verts_list_elem(File,Model,NodeName,NodeRef,FaceList,V) :-
  member(F,FaceList),
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,_,_,_,_)),
  (V=V1;V=V2;V=V3).

f_make_tverts_list_elem(File,Model,NodeName,NodeRef,FaceList,T) :-
  member(F,FaceList),
  gotdata(File,Model,NodeName,NodeRef,faces(F,_,_,_,_,T1,T2,T3,_)),
  (T=T1;T=T2;T=T3).

/* ====================== */
/* f_make_ordinal_assoc/2 */
/* ====================== */

f_make_ordinal_assoc(List,Assoc) :-
  f_make_ordinal_list(0,List,OrdList),
  list_to_assoc(OrdList,Assoc).

f_make_ordinal_list(_,[],[]) :- !.
f_make_ordinal_list(I,[X|X0],[X-I|X1]) :- I1 is I+1, !, f_make_ordinal_list(I1,X0,X1).


/* =========== */
/* f_newname/2 */
/* =========== */

f_newname(OldName,NewName) :-
  NewName==OldName, !, fail.

f_newname(_,NewName) :-
  nonvar(NewName), !,
  \+ gotdata(_,_,node(_,NewName)).

f_newname(OldName,NewName) :- 
  var(NewName),
  atom_concat(OldName,'_',Prefix),
  between(1,inf,N),
  atom_concat(Prefix,N,NewName),
  \+ gotdata(_,_,node(_,NewName)),
  !.

/* ======================== */
/* f_delete_faces_in_list/5 */
/* f_delete_faces_in_list/8 */
/* ======================== */

f_delete_faces_in_list(_,_,_,_,[]) :- !.

f_delete_faces_in_list(File,Model,NodeName,NodeRef,List) :-
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)),
  f_delete_faces_in_list(File,Model,NodeName,NodeRef,List,NFaces,0,0).

f_delete_faces_in_list(File,Model,NodeName,NodeRef,_,NFaces,NFaces,NewNFaces) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  (NewNFaces>0 -> asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))); true), !.

f_delete_faces_in_list(File,Model,NodeName,NodeRef,List,NFaces,ThisFace,NewNFaces) :-
  member(ThisFace,List),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(ThisFace,_,_,_,_,_,_,_,_))),
  NextFace is ThisFace+1, !,
  f_delete_faces_in_list(File,Model,NodeName,NodeRef,List,NFaces,NextFace,NewNFaces).

f_delete_faces_in_list(File,Model,NodeName,NodeRef,List,NFaces,ThisFace,NewNFaces) :-
  (ThisFace\=NewNFaces ->
    retract(gotdata(File,Model,NodeName,NodeRef,faces(ThisFace,V1,V2,V3,G,T1,T2,T3,M))),
    asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces,V1,V2,V3,G,T1,T2,T3,M)));
    increment_bug_count(File),
    true),
  NextFace is ThisFace+1, NextNewNFaces is NewNFaces+1, !,
  f_delete_faces_in_list(File,Model,NodeName,NodeRef,List,NFaces,NextFace,NextNewNFaces).

/* ============================ */
/* f_keep_only_faces_in_list/5 */
/* f_keep_only_faces_in_list/8 */
/* ============================ */

f_keep_only_faces_in_list(_,_,_,_,[]) :- !, fail.

f_keep_only_faces_in_list(File,Model,NodeName,NodeRef,List) :-
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)),
  f_keep_only_faces_in_list(File,Model,NodeName,NodeRef,List,NFaces,0,0).

f_keep_only_faces_in_list(File,Model,NodeName,NodeRef,_,NFaces,NFaces,NewNFaces) :-
  retract(gotdata(File,Model,NodeName,NodeRef,faces(NFaces))),
  (NewNFaces>0 -> asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces))); true), !.

f_keep_only_faces_in_list(File,Model,NodeName,NodeRef,List,NFaces,ThisFace,NewNFaces) :-
  \+ member(ThisFace,List),
  retract(gotdata(File,Model,NodeName,NodeRef,faces(ThisFace,_,_,_,_,_,_,_,_))),
  NextFace is ThisFace+1, !,
  f_keep_only_faces_in_list(File,Model,NodeName,NodeRef,List,NFaces,NextFace,NewNFaces).

f_keep_only_faces_in_list(File,Model,NodeName,NodeRef,List,NFaces,ThisFace,NewNFaces) :-
  (ThisFace\=NewNFaces ->
    retract(gotdata(File,Model,NodeName,NodeRef,faces(ThisFace,V1,V2,V3,G,T1,T2,T3,M))),
    asserta(gotdata(File,Model,NodeName,NodeRef,faces(NewNFaces,V1,V2,V3,G,T1,T2,T3,M))),
    increment_bug_count(File);
    true),
  NextFace is ThisFace+1, NextNewNFaces is NewNFaces+1, !,
  f_keep_only_faces_in_list(File,Model,NodeName,NodeRef,List,NFaces,NextFace,NextNewNFaces).

/* ============================== */
/* f_detach_subobjects/7 */
/* ============================== */

f_detach_subobjects(File,Model,NodeName,NodeRef,Criterion,[[NewName,NewRef]|Rest],SmallLogStream) :-
  once((gotdata(File,Model,NodeName,NodeRef,render(0)), MinFaces=2; g_user_option(min_Size,MinFaces); MinFaces=1)),
  once((gotdata(File,Model,NodeName,NodeRef,render(0)), Smoothed=ignore; g_user_option(use_Smoothed,Smoothed); Smoothed=ignore)),
  f_prepare_neighbourhood(File,Model,NodeName,NodeRef),
  predicate_property(neighbourhood(_,_,_,_,_,_,_),number_of_clauses(NFaces)), NFaces>=2*MinFaces,
  f_identify_face_group(File,Model,NodeName,NodeRef,NFaces,MinFaces,Criterion,FaceList,Smoothed),
  f_split_node_by_face_list(File,Model,NodeName,NodeRef,FaceList,NewName,NewRef,Criterion,SmallLogStream), !,
  f_detach_subobjects(File,Model,NodeName,NodeRef,Criterion,Rest,SmallLogStream),
  (Rest=[] ->
     delete_unused_vertices(File,Model,NodeName,NodeRef),
     delete_unused_tverts(File,Model,NodeName,NodeRef),
     gotdata(File,Model,NodeName,NodeRef,tverts(NT)),
     renormalize_tverts(File,Model,NodeName,NodeRef,NT,_)
     ;
     true
  ).

f_detach_subobjects(_,_,_,_,_,[],_).

/* ======================= */
/* f_identify_face_group/9 */
/* ======================= */

f_identify_face_group(File,Model,NodeName,NodeRef,NFaces,MinFaces,isolated,FaceList,_) :-
  retractall(face_group(_,_)),
  f_assign_face_group(File,Model,NodeName,NodeRef,0,0,neighbour,_),
  setof(X,face_group(0,X),FaceList),
  length(FaceList,L), L>=MinFaces, L=<NFaces-MinFaces.

f_identify_face_group(File,Model,NodeName,NodeRef,NFaces,MinFaces,smoothed,FaceList,Smoothed) :-
  Smoothed\=ignore,
  once((
    gotdata(File,Model,NodeName,NodeRef,faces(StartFace,_,_,_,S0,_,_,_,_)), S0\=0,
    gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,_,S1,_,_,_,_)), S1\=S0, 0 is S1/\S0
      )),
  retractall(face_group(_,_)),
  f_assign_face_group(File,Model,NodeName,NodeRef,0,StartFace,smoothed,_),
  setof(X,face_group(0,X),FaceList),
  length(FaceList,L), L>=MinFaces, L=<NFaces-MinFaces.

f_identify_face_group(File,Model,NodeName,NodeRef,NFaces,MinFaces,ground,FaceList,_) :-
  retractall(face_group(_,_)),
  (ground_face(StartFace), \+face_group(_,StartFace),
   f_assign_face_group(File,Model,NodeName,NodeRef,ground,StartFace,ground,_),
   fail; true),
  setof(X,face_group(ground,X),FaceList),
  length(FaceList,L), L>=MinFaces, L=<NFaces-MinFaces.

f_identify_face_group(File,Model,NodeName,NodeRef,NFaces,MinFaces,concave,FaceList,Smoothed) :-
  once(g_user_option(split_Priority,Priority)), Priority=concave,
  retractall(face_group(_,_)),
  neighbourhood(StartFace,_,_,N,_,_,_), N>0, (Smoothed==protect -> gotdata(File,Model,NodeName,NodeRef,faces(StartFace,_,_,_,0,_,_,_,_)); true),
  f_assign_face_group(File,Model,NodeName,NodeRef,0,StartFace,concave,Smoothed),
  setof(X,face_group(0,X),FaceList),
  length(FaceList,L), L>=MinFaces, L=<NFaces-MinFaces.

f_identify_face_group(File,Model,NodeName,NodeRef,NFaces,MinFaces,convex,FaceList,Smoothed) :-
  retractall(face_group(_,_)),
  neighbourhood(StartFace,_,_,_,_,N,_), N>0, (Smoothed==protect -> gotdata(File,Model,NodeName,NodeRef,faces(StartFace,_,_,_,0,_,_,_,_)); true),
  f_assign_face_group(File,Model,NodeName,NodeRef,0,StartFace,convex,Smoothed),
  setof(X,face_group(0,X),FaceList),
  length(FaceList,L), L>=MinFaces, L=<NFaces-MinFaces.

f_identify_face_group(File,Model,NodeName,NodeRef,NFaces,MinFaces,concave,FaceList,Smoothed) :-
  once(g_user_option(split_Priority,Priority)), Priority\=concave,
  retractall(face_group(_,_)),
  neighbourhood(StartFace,_,_,N,_,_,_), N>0, (Smoothed==protect -> gotdata(File,Model,NodeName,NodeRef,faces(StartFace,_,_,_,0,_,_,_,_)); true),
  f_assign_face_group(File,Model,NodeName,NodeRef,0,StartFace,concave,Smoothed),
  setof(X,face_group(0,X),FaceList),
  length(FaceList,L), L>=MinFaces, L=<NFaces-MinFaces.

f_identify_face_group(File,Model,NodeName,NodeRef,NFaces,MinFaces,plane,FaceList,Smoothed) :-
  retractall(face_group(_,_)),
  neighbourhood(StartFace,N,_,_,_,_,_), N>0, (Smoothed==protect -> gotdata(File,Model,NodeName,NodeRef,faces(StartFace,_,_,_,0,_,_,_,_)); true),
  f_assign_face_group(File,Model,NodeName,NodeRef,0,StartFace,plane,Smoothed),
  setof(X,face_group(0,X),FaceList),
  length(FaceList,L), L>=MinFaces, L=<NFaces-MinFaces.

/* ================================ */
/* f_assign_face_group/8            */
/* ================================ */

f_assign_face_group(_,_,_,_,_,Face,_,_) :-
  face_group(_,Face), !.

f_assign_face_group(File,Model,NodeName,NodeRef,Group,Face,ground,_) :-
  asserta(face_group(Group,Face)),
  (f_ground_neighbour(Face,F),
   f_assign_face_group(File,Model,NodeName,NodeRef,Group,F,ground,_),
   fail; true), !.

f_assign_face_group(File,Model,NodeName,NodeRef,Group,Face,neighbour,_) :-
  asserta(face_group(Group,Face)),
  (f_neighbour(Face,F),
   f_assign_face_group(File,Model,NodeName,NodeRef,Group,F,neighbour,_),
   fail ; true), !.

f_assign_face_group(File,Model,NodeName,NodeRef,Group,Face,smoothed,_) :-
  asserta(face_group(Group,Face)),
  (f_neighbour(Face,F),
   f_same_smoothing_group(File,Model,NodeName,NodeRef,Face,F),
   f_assign_face_group(File,Model,NodeName,NodeRef,Group,F,smoothed,_),
   fail ; true), !.

f_assign_face_group(File,Model,NodeName,NodeRef,Group,Face,concave,Smoothed) :-
  asserta(face_group(Group,Face)),
  (f_plano_concave_neighbour(Face,F),
   (Smoothed==protect -> f_same_smoothing_group(File,Model,NodeName,NodeRef,Face,F); true),
   f_assign_face_group(File,Model,NodeName,NodeRef,Group,F,concave,Smoothed),
   fail; true), !.

f_assign_face_group(File,Model,NodeName,NodeRef,Group,Face,convex,Smoothed) :-
  asserta(face_group(Group,Face)),
  (f_plano_convex_neighbour(Face,F),
   (Smoothed==protect -> f_same_smoothing_group(File,Model,NodeName,NodeRef,Face,F); true),
   f_assign_face_group(File,Model,NodeName,NodeRef,Group,F,convex,Smoothed),
   fail; true), !.

f_assign_face_group(File,Model,NodeName,NodeRef,Group,Face,plane,Smoothed) :-
  asserta(face_group(Group,Face)),
  (f_plane_neighbour(Face,F),
   (Smoothed==protect -> f_same_smoothing_group(File,Model,NodeName,NodeRef,Face,F); true),
   f_assign_face_group(File,Model,NodeName,NodeRef,Group,F,plane,Smoothed),
   fail; true), !.

/* ============================== */
/* f_neighbour/2                  */
/* f_plane_neighbour/2            */
/* f_plano_convex_neighbour/2     */
/* f_plano_conconcave_neighbour/2 */
/* f_ground_neighbour/2           */
/* ============================== */

f_neighbour(Face1,Face2) :-
  neighbourhood(Face1,_,L1,_,L2,_,L3),
  flatten([L1,L2,L3],L),
  member(Face2,L).

f_plane_neighbour(Face1,Face2) :-
  neighbourhood(Face1,_,L,_,_,_,_),
  member(Face2,L).

f_plano_concave_neighbour(Face1,Face2) :-
  neighbourhood(Face1,_,L1,_,L2,_,_),
  flatten([L1,L2],L),
  member(Face2,L).

f_plano_convex_neighbour(Face1,Face2) :-
  neighbourhood(Face1,_,L1,_,_,_,L2),
  flatten([L1,L2],L),
  member(Face2,L).

f_ground_neighbour(Face1,Face2) :-
   f_neighbour(Face1,Face2),
   ground_face(Face2).

/* ========================= */
/* f_prepare_neighbourhood/4 */
/* f_neighbour_type/8        */
/* f_neighbour_type/11       */
/* f_neighbour_type/3        */
/* f_restructure/4           */
/* ========================= */

f_prepare_neighbourhood(File,Model,NodeName,NodeRef) :-
  retractall(neighbourhood(_,_,_,_,_,_,_)),
  retractall(ground_face(_)),
  /*  abs_position(File,Model,NodeName,NodeRef,_,_,Z0), */
  relate_to_tile(File,Model,NodeName,NodeRef,[0,0,0],[_,_,Z0]),
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)), LastFace is NFaces-1,
  between(0,LastFace,F1),
  setof([F2,T],f_neighbour_type(File,Model,NodeName,NodeRef,Z0,F1,F2,T),S),
  f_restructure(S,Planar,Concave,Convex),
  length(Planar,LPlanar), length(Concave,LConcave), length(Convex,LConvex),
  asserta(neighbourhood(F1,LPlanar,Planar,LConcave,Concave,LConvex,Convex)),
  fail ; true.

f_neighbour_type(File,Model,NodeName,NodeRef,Z0,Face1,Face2,Type) :-
  gotdata(File,Model,NodeName,NodeRef,faces(Face1,V1,V2,V3,_,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,X1,Y1,Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,X2,Y2,Z2)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,X3,Y3,Z3)),
  (Z1+Z0=<0.0, Z2+Z0=<0.0, Z3+Z0=<0.0 -> asserta(ground_face(Face1)); true),
  vector_subtract([X3,Y3,Z3],[X1,Y1,Z1],A13),
  vector_subtract([X1,Y1,Z1],[X2,Y2,Z2],A21),
  vector_subtract([X2,Y2,Z2],[X3,Y3,Z3],A32),
  vector_subtract([X2,Y2,Z2],[X1,Y1,Z1],A12),
  vector_cross_product(A12,A13,A123),
  vector_normalise(A123,N123),
  (
   f_neighbour_type(File,Model,NodeName,NodeRef,V1,V2,[X1,Y1,Z1],N123,A21,Face2,Type);
   f_neighbour_type(File,Model,NodeName,NodeRef,V2,V3,[X2,Y2,Z2],N123,A32,Face2,Type);
   f_neighbour_type(File,Model,NodeName,NodeRef,V3,V1,[X3,Y3,Z3],N123,A13,Face2,Type)
  ).

f_neighbour_type(File,Model,NodeName,NodeRef,V1,V2,[X1,Y1,Z1],N123,A21,Face2,Type) :- 
  (
   gotdata(File,Model,NodeName,NodeRef,faces(Face2,V2,V1,V4,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(Face2,V1,V4,V2,_,_,_,_,_));
   gotdata(File,Model,NodeName,NodeRef,faces(Face2,V4,V2,V1,_,_,_,_,_))
  ),
  gotdata(File,Model,NodeName,NodeRef,verts(V4,X4,Y4,Z4)),
  vector_subtract([X4,Y4,Z4],[X1,Y1,Z1],A14),
  vector_normalise(A14,N14),
  vector_cross_product(A21,A14,A214),
  vector_normalise(A214,N214),
  vector_dot_product(N123,N14,D),
  vector_dot_product(N123,N214,P),
  f_neighbour_type(P,D,Type).

f_neighbour_type(P,D,planar)  :- abs(D)<0.1, P > 0.9, !.
f_neighbour_type(_,D,convex)  :- D<0, !.
f_neighbour_type(_,D,concave) :- D>0, !.

f_restructure([],[],[],[]) :- !.
f_restructure([[F,planar]|Rest],[F|L1],L2,L3)  :- !, f_restructure(Rest,L1,L2,L3).
f_restructure([[F,concave]|Rest],L1,[F|L2],L3) :- !, f_restructure(Rest,L1,L2,L3).
f_restructure([[F,convex]|Rest],L1,L2,[F|L3])  :- !, f_restructure(Rest,L1,L2,L3).

f_same_smoothing_group(File,Model,NodeName,NodeRef,F1,F2) :-
  gotdata(File,Model,NodeName,NodeRef,faces(F1,_,_,_,G1,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,faces(F2,_,_,_,G2,_,_,_,_)),
  (G1=G2 ; G1/\G2 =\= 0), !.
