/* ==================================================== */
/*                                                      */
/* Predicates for generating skinmesh bodies from parts */
/* Part of the CleanModels 3 suite by OldMansBeard      */
/*                                                      */
/* This version dated 2014-01-16                        */
/*                                                      */
/* ==================================================== */

/* ==================== */
/* make_skinmesh_body/3 */
/* ==================== */

make_skinmesh_body(File,Model,_) :-
  gotdata(File,Model,newmodel(Model)),

  LCollarBone    = omb_bone_body_lcollar,
  RCollarBone    = omb_bone_body_rcollar,
  LBreastBone    = omb_bone_body_lbreast,
  RBreastBone    = omb_bone_body_rbreast,
  Pendulum       = chest_g,
  Pendulum_Front = omb_bone_body_pendulum_front,
  Pendulum_Back  = omb_bone_body_pendulum_back,
  Pendulum_Left  = omb_bone_body_pendulum_left,
  Pendulum_Right = omb_bone_body_pendulum_right,
  PendulumList   = [Pendulum, Pendulum_Front, Pendulum_Back, Pendulum_Left, Pendulum_Right],
  Shaker         = omb_bone_body_shaker,
  TorsoSkin      = omb_skin_body,

  force_zero_orientations(File,Model),

  make_shake_keys(File,Model,rootdummy,Shaker),
  make_kilt_bones(File,Model,KiltBones),

  create_limb_skin(File,Model,l,arm,left,Shaker,SmallLogStream),
  create_limb_skin(File,Model,r,arm,right,Shaker,SmallLogStream),
  create_limb_skin(File,Model,l,leg,knee,Shaker,SmallLogStream),
  create_limb_skin(File,Model,r,leg,knee,Shaker,SmallLogStream),

  create_torso_skin(File,Model,LCollarBone,RCollarBone,LBreastBone,RBreastBone,PendulumList,KiltBones,TorsoSkin),

  reparent_bicep_to_collarbone(File,Model,torso_g,LCollarBone,lbicep_g),
  reparent_shoulder_to_collarbone(File,Model,lbicep_g,LCollarBone,lshoulder_g),
  reparent_bicep_to_collarbone(File,Model,torso_g,RCollarBone,rbicep_g),
  reparent_shoulder_to_collarbone(File,Model,rbicep_g,RCollarBone,rshoulder_g),
  apply_shoulder_heave(File,Model,lbicep_g,LCollarBone),
  apply_shoulder_heave(File,Model,rbicep_g,RCollarBone),
  make_jiggle_keys(File,Model,torso_g,LBreastBone,RBreastBone),
  set_pendulum_keys(File,Model,PendulumList),

  /* Added in CM362m */
  reparent_rotator_to_body(File,Model,omb_bone_larm_a_1,lbicep_g),
  reparent_rotator_to_body(File,Model,omb_bone_rarm_a_1,rbicep_g),
  reparent_rotator_to_body(File,Model,omb_bone_lleg_a_1,lthigh_g),
  reparent_rotator_to_body(File,Model,omb_bone_rleg_a_1,rthigh_g),

  !.

/* =================== */
/* create_limb_skin/7  */
/* create_limb_skin/11 */
/* =================== */

create_limb_skin(File,Model,LR,ArmOrLeg,JointType,Shaker,SmallLogStream) :-
  (ArmOrLeg==arm ->
     atom_concat(LR,'bicep_g',Limb1),
     atom_concat(LR,'forearm_g',Limb2),
     atom_concat(LR,'hand_g',Limb3)
   ;
   ArmOrLeg==leg ->
     atom_concat(LR,'thigh_g',Limb1),
     atom_concat(LR,'shin_g',Limb2),
     atom_concat(LR,'foot_g',Limb3)
   ;
   tab(2), write('*** CM3 internal error: '), write(create_limb_skin(File,Model,LR,ArmOrLeg,left,SmallLogStream)), nl, fail
  ),
  atom_concat(LR,ArmOrLeg,Limb),
  concat_atom([omb,bone,Limb,a,1],'_',TopRotator),
  concat_atom([omb,bone,Limb,b,1],'_',Dummy1),
  concat_atom([omb,bone,Limb,b,2],'_',Pendulum1),
  concat_atom([omb,bone,Limb,c,1],'_',MidDummy1),
  concat_atom([omb,bone,Limb,d,1],'_',Dummy2),
  concat_atom([omb,bone,Limb,d,2],'_',Sleeve1),
  concat_atom([omb,bone,Limb,f,1],'_',Rotator),
  concat_atom([omb,bone,Limb,g,1],'_',Dummy3),
  concat_atom([omb,bone,Limb,g,2],'_',Pendulum2),
  concat_atom([omb,bone,Limb,h,1],'_',MidDummy2),
  concat_atom([omb,bone,Limb,k,1],'_',Dummy4),
  concat_atom([omb,bone,Limb,k,2],'_',Sleeve2),
  DummyList = [TopRotator,Dummy1,Pendulum1,MidDummy1,Dummy2,Rotator,Dummy3,Pendulum2,MidDummy2,Dummy4,Sleeve1,Sleeve2,Shaker],

  concat_atom([omb,skin,Limb1],'_',Skin1),
  concat_atom([omb,skin,Limb2],'_',Skin2),
  concat_atom([omb,skin,Limb3],'_',Skin3),
  concat_atom([omb,skin,Limb,1],'_',Skin),
  create_limb_skin(File,Model,Limb1,Limb2,Limb3,DummyList,Skin1,Skin2,Skin3,JointType,Skin),
  skin_join_skins(File,Model,Skin1,Skin2,Skin3,Skin),
  split_skin_by_smoothing_groups(File,Model,Skin,_).

/* CM362g: the next three clauses had the wrong number of arguments, so the tests were not being made */

create_limb_skin(File,Model,Limb1,_,_,_,_,_,_,_,_) :-
  \+gotdata(File,Model,node(_,Limb1)),
  !.

create_limb_skin(File,Model,_,Limb2,_,_,_,_,_,_,_) :-
  \+gotdata(File,Model,node(_,Limb2)),
  !.

create_limb_skin(File,Model,_,_,Limb3,_,_,_,_,_,_) :-
  \+gotdata(File,Model,node(_,Limb3)),
  !.

create_limb_skin(File,Model,Limb1,Limb2,Limb3,DummyList,Skin1,Skin2,Skin3,JointType,Skin) :-
  /* Numbering runs from 1 at the shoulder/hip to 4 at the hand/foot */
  DummyList = [TopRotator,Dummy1,Pendulum1,MidDummy1,Dummy2,Rotator,Dummy3,Pendulum2,MidDummy2,Dummy4,Sleeve1,Sleeve2,Shaker],
  create_proximal_dummy(File,Model,Limb1,TopRotator,C,C1),
  create_proximal_dummy(File,Model,Limb1,Dummy1,C1,C2),
  create_proximal_dummy(File,Model,Limb1,Pendulum1,C2,C3),
  create_distal_dummy(File,Model,Limb1,Limb2,Dummy2,C3,C4),
  create_proximal_dummy(File,Model,Limb2,Dummy3,C4,C5),
  create_proximal_dummy(File,Model,Limb2,Pendulum2,C5,C6),
  create_distal_dummy(File,Model,Limb2,Limb3,Dummy4,C6,C7),
  create_midpoint_dummy(File,Model,Limb1,Limb2,MidDummy1,C7,C8),
  create_midpoint_dummy(File,Model,Limb2,Limb3,MidDummy2,C8,C9),
  create_distal_dummy(File,Model,Limb1,Limb2,Rotator,C9,C10),
  create_referred_dummy(File,Model,Pendulum1,Dummy2,Sleeve1,C10,C11),
  create_referred_dummy(File,Model,Pendulum2,Dummy4,Sleeve2,C11,[]),
  (C\=[] -> length(C,CLen),tab(2), write('created '), write(CLen), write(' bones for '), write(Skin), nl ; true),

  set_rotation_type4(File,Model,Limb3,Dummy4),
  set_rotation_type1(File,Model,Limb1,Dummy1),
  /* set_rotation_type_hinge(File,Model,Limb2,Dummy1,Dummy2,Dummy3,Dummy4,JointType), */
  set_rotation_type4(File,Model,Limb2,Dummy2),
  set_rotation_type1(File,Model,Limb2,Dummy3),
  set_midpoint_rotations(File,Model,Dummy1,Dummy2,MidDummy1),
  set_midpoint_rotations(File,Model,Dummy3,Dummy4,MidDummy2),
  set_rotator_rotations(File,Model,Dummy2,Limb2,Dummy3,Rotator),
  set_half_rotations(File,Model,Limb1,TopRotator),
  set_limb_pendulum_keys(File,Model,Pendulum1,Pendulum2,Sleeve1,Sleeve2,Shaker),

  create_skin(File,Model,Limb1,Dummy1,Dummy2,MidDummy1,Sleeve1,Skin1,L,L1,S,S1),
  create_skin(File,Model,Limb2,Dummy3,Dummy4,MidDummy2,Sleeve2,Skin2,L1,L2,S1,S2),
  create_skin(File,Model,Limb3,_,_,_,_,Skin3,L2,[],S2,[]),
  (L\=[] -> tab(2), write('created skinmesh:  '), write(L), nl ; true),
  (S\=[] -> tab(2), write('converted to shadow mesh: '), write(S), nl ; true),

  (L=[Skin1,Skin2|_] -> make_joint(File,Model,Skin1,Skin2,Rotator,Rotator,JointType) ; true),
  (JointType==knee -> JointType2 = ankle ; JointType2 = wrist),
  (append(_,[Skin2,Skin3],L) -> make_joint(File,Model,Skin2,Skin3,Dummy4,Limb3,JointType2) ; true),
  !.

/* ============================== */
/* create_torso_skin/9            */
/* compute_pendulum_coordinates/8 */
/* bounding_box/10                */
/* ============================== */

create_torso_skin(File,Model,_,_,_,_,_,_,_) :-
 \+gotdata(File,Model,node(_,torso_g)),
 !.

/* Taken out in CM362s */
/*
create_torso_skin(File,Model,_,_,_,_,_,_,_) :-
 \+gotdata(File,Model,node(_,pelvis_g)),
 !.
*/

create_torso_skin(File,Model,_,_,_,_,_,_,_) :-
  /* CM362h: catch if this is a robe with bicep nodes but no neck node.       */
  /* Create a neck_g placeholder so that biceps can be re-parented correctly. */
  \+gotdata(File,Model,node(_,neck_g)),
  gotdata(File,Model,lbicep_g,LRef,parent(torso_g/TRef)),
  gotdata(File,Model,rbicep_g,RRef,parent(torso_g/TRef)),
  abs_position(File,Model,torso_g,TRef,X0,Y0,Z0),
  abs_position(File,Model,lbicep_g,LRef,X1,Y1,Z1),
  abs_position(File,Model,rbicep_g,RRef,X2,Y2,Z2),
  X is (X1+X2)*0.5-X0, Y is (Y1+Y2)*0.5-Y0, Z is (Z1+Z2)*0.5-Z0,
  create_offset_dummy(File,Model,torso_g,X,Y,Z,neck_g,_,_),
  tab(2), write('placeholder neck_g dummy created'), nl,
  fail.

create_torso_skin(File,Model,_,_,_,_,_,_,_) :-
 \+gotdata(File,Model,node(_,neck_g)),
 !.

create_torso_skin(File,Model,LCollar,RCollar,LBreast,RBreast,PendulumList,KiltBones,Skin) :-

  Torso = torso_g, Pelvis = pelvis_g, Neck = neck_g, Belt = belt_g,
  PendulumList = [Pendulum, Pendulum_Front, Pendulum_Back, Pendulum_Left, Pendulum_Right],

  create_distal_dummy(File,Model,Torso,Neck,LCollar,C,C1),
  create_distal_dummy(File,Model,Torso,Neck,RCollar,C1,C2),
  atom_concat(RBreast,'_1',RGimbal),
  atom_concat(LBreast,'_1',LGimbal),
  atom_concat(Pendulum,'_1',ShakeNode),
  create_proximal_dummy(File,Model,Torso,ShakeNode,C2,C3),
  create_referred_dummy(File,Model,ShakeNode,Neck,Pendulum,C3,C4),
  compute_pendulum_coordinates(File,Model,Torso,Yfront,Yback,Xleft,Xright,Z),
  create_offset_dummy(File,Model,ShakeNode,0,Yfront,Z,Pendulum_Front,C4,C5),
  create_offset_dummy(File,Model,ShakeNode,0,Yback,Z,Pendulum_Back,C5,C6),
  create_offset_dummy(File,Model,ShakeNode,Xleft,0,Z,Pendulum_Left,C6,C7),
  create_offset_dummy(File,Model,ShakeNode,Xright,0,Z,Pendulum_Right,C7,C8),
  create_proximal_dummy(File,Model,LCollar,LGimbal,C8,C9),
  create_proximal_dummy(File,Model,LGimbal,LBreast,C9,C10),
  create_proximal_dummy(File,Model,RCollar,RGimbal,C10,C11),
  create_proximal_dummy(File,Model,RGimbal,RBreast,C11,[]),
  (C\=[] -> length(C,CLen), tab(2), write('created '), write(CLen), write(' bones for '), write(Skin), nl ; true),

  create_torso_skin(File,Model,Torso,Pelvis,LCollar,RCollar,LBreast,RBreast,PendulumList,Skin,L,L1,S,S1),

  PelvisSkin = omb_skin_pelvis,
  NeckSkin   = omb_skin_neck,
  KiltSkin   = omb_skin_kilt,

  create_skin(File,Model,Pelvis,_,_,_,_,PelvisSkin,L1,L2,S1,S2),
  create_skin(File,Model,Neck,_,_,_,_,NeckSkin,L2,L3,S2,S3),
  create_kilt_skin(File,Model,Belt,Pelvis,KiltBones,KiltSkin,L3,[],S3,[]),

  (L\=[] -> tab(2), write('created skinmesh:  '), write(L), nl ; true),
  (S\=[] -> tab(2), write('converted to shadow mesh: '), write(S), nl ; true),

  (member(Skin,L), member(KiltSkin,L) ->
    make_joint(File,Model,Skin,KiltSkin,Pelvis,Pelvis,waist),
    append_mesh(File,Model,Skin,_,KiltSkin,_),
    M=[KiltSkin|M1] ; M=M1),
  (member(Skin,L), member(PelvisSkin,L), \+member(KiltSkin,L) ->
    make_joint(File,Model,Skin,PelvisSkin,Pelvis,Pelvis,waist),
    append_mesh(File,Model,Skin,_,PelvisSkin,_),
    M1=[PelvisSkin|M2] ; M1=M2),
  (member(Skin,L), member(NeckSkin,L) ->
    make_joint(File,Model,NeckSkin,Skin,Neck,Torso,neck),
    append_mesh(File,Model,Skin,_,NeckSkin,_),
    M2=[NeckSkin]; M2=[]),
  (M\=[] ->
    gotdata(File,Model,Skin,SkinRef,verts(NVerts)),
    hash_verts(File,Model,Skin,SkinRef),
    has_unwelded_verts(File,Model,Skin,SkinRef),
    weld_vertices(File,Model,Skin,SkinRef,NVerts,0,0),
    tab(2), write(M), write(' merged into '), write(Skin),
    (cm3_verbose -> gotdata(File,Model,Skin,SkinRef,verts(NewNVerts)), tab(1), write(verts(NVerts>NewNVerts)) ; true),
    nl
    ;
    true),
  split_skin_by_smoothing_groups(File,Model,Skin,SkinRef),
  !.

compute_pendulum_coordinates(File,Model,Torso,Yfront,Yback,Xleft,Xright,Z) :-
  bounding_box(File,Model,Torso,_,Xleft,Yback,_,Xright,Yfront,Zmax),
  !,
  Z is 0.7*Zmax.

compute_pendulum_coordinates(File,Model,Torso,Yfront,Yback,Xleft,Xright,Z) :-
  /* Kludge in CM362g - default if this is a supermodel with no torso mesh */
  abs_position(File,Model,Torso,_,X0,Y0,Z0),
  abs_position(File,Model,lbicep_g,_,X1,Y1,Z1),
  abs_position(File,Model,rbicep_g,_,X2,Y2,Z2),
  Xleft is X1-X0, Xright is X2-X0,
  Yfront is 0.5*(Y1+Y2)-Y0+0.25*(X2-X1), Yback is 0.5*(Y1+Y2)-Y0-0.25*(X2-X1),
  Z is 0.7*((Z1+Z2)*0.5 - Z0).


bounding_box(File,Model,NodeName,NodeRef,X1,Y1,Z1,X2,Y2,Z2) :-
  gotdata(File,Model,NodeName,NodeRef,verts(_,X1,_,_)), \+((gotdata(File,Model,NodeName,NodeRef,verts(_,X,_,_)), X<X1)),
  gotdata(File,Model,NodeName,NodeRef,verts(_,X2,_,_)), \+((gotdata(File,Model,NodeName,NodeRef,verts(_,X,_,_)), X>X2)),
  gotdata(File,Model,NodeName,NodeRef,verts(_,_,Y1,_)), \+((gotdata(File,Model,NodeName,NodeRef,verts(_,_,Y,_)), Y<Y1)),
  gotdata(File,Model,NodeName,NodeRef,verts(_,_,Y2,_)), \+((gotdata(File,Model,NodeName,NodeRef,verts(_,_,Y,_)), Y>Y2)),
  gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z1)), \+((gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z)), Z<Z1)),
  gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z2)), \+((gotdata(File,Model,NodeName,NodeRef,verts(_,_,_,Z)), Z>Z2)),
  (cm3_verbose -> tab(2), write('bounding box of '), write(NodeName), tab(1), write([X1,Y1,Z1,X2,Y2,Z2]), nl ; true),
  !.

/* =================== */
/* create_kilt_skin/10 */
/* =================== */

create_kilt_skin(File,Model,_,_,_,Skin,C,C,S,S) :-
  gotdata(File,Model,node(skin,Skin)),
  !.

create_kilt_skin(File,Model,Belt,_,_,_,C,C,S,S) :-
  \+((
    gotdata(File,Model,node(NodeType,Belt)),
    member(NodeType,[trimesh,danglymesh]),
    gotdata(File,Model,Belt,_,faces(_))
    )),
  !.

create_kilt_skin(File,Model,Belt,_,_,_,C,C,S,S) :-
  gotdata(File,Model,Belt,_,render(0)),
  !.

create_kilt_skin(File,Model,Belt,Pelvis,KiltBones,Skin,[Skin|C0],C0,S,S0) :-
  clause(gotdata(File,Model,node(_,Belt)),true,BeltRef),
  assertz(gotdata(File,Model,node(skin,Skin)),SkinRef),
  assertz(gotdata(File,Model,Skin,SkinRef,shadow(0))),  /* changed in CM362n */
  (gotdata(File,Model,Belt,BeltRef,Q),
   functor(Q,Q0,_), Q0\=='#part-number', Q0\=='shadow', \+clause(paramtype(danglymesh,_,Q0),true),
   assertz(gotdata(File,Model,Skin,SkinRef,Q)),
   fail ; true),

  gotdata(File,Model,Skin,SkinRef,verts(N)),
  assertz(gotdata(File,Model,Skin,SkinRef,weights(N))),
  gotdata(File,Model,rthigh_g,_,position(X0,Y0,Z0)),
  bottom_line(File,Model,Skin,SkinRef,Hem),
  (gotdata(File,Model,Skin,SkinRef,verts(V,X,Y,Z)),
   kilt_skin_weights(X,Y,Z,X0,Y0,Z0,Hem,Pelvis,KiltBones,WList),
   assertz(gotdata(File,Model,Skin,SkinRef,weights(V,WList))),
   fail ; true
  ),
  raise_to_tile(File,Model,Skin,SkinRef),
  set_zero_orientation(File,Model,Skin,SkinRef),
  set_zero_position(File,Model,Skin,SkinRef),
  apply_snap(File,Model,Skin,SkinRef),
  (gotdata(File,Model,Belt,BeltRef,shadow(0)) ->
   reclassify_node(File,Model,Belt,BeltRef,dummy,_),
   S=S0
   ;
   retractall(gotdata(File,Model,Belt,BeltRef,render(_))),
   assertz(gotdata(File,Model,Belt,BeltRef,render(0))),
   retractall(gotdata(File,Model,Belt,BeltRef,bitmap(_))),
   assertz(gotdata(File,Model,Belt,BeltRef,bitmap(black))),
   S=[Belt|S0]
  ),
  !.

create_kilt_skin(_,_,_,_,_,Skin,C,C,S,S) :-
  tab(2), write('*** failed to make skinmesh '), write(Skin), nl.

bottom_line(File,Model,Node,NodeRef,Zbottom) :-
  setof(Z,V^X^Y^gotdata(File,Model,Node,NodeRef,verts(V,X,Y,Z)),[Zbottom|_]).


/* ========================= */
/* create_proximal_dummy/6   */
/* create_distal_dummy/6     */
/* create_midpoint_dummy/7   */
/* create_referred_dummy/7   */
/* create_offset_dummy/9     */
/* ========================= */

create_proximal_dummy(File,Model,_,Dummy,C,C) :-
  gotdata(File,Model,node(dummy,Dummy)), !.

create_proximal_dummy(File,Model,Limb,Dummy,[Dummy|C0],C0) :-
  clause(gotdata(File,Model,node(_,Limb)),true,LimbRef),
  assertz(gotdata(File,Model,node(dummy,Dummy)),Dref),
  assertz(gotdata(File,Model,Dummy,Dref,parent(Limb/LimbRef))),
  assertz(gotdata(File,Model,Dummy,Dref,position(0,0,0))),
  assertz(gotdata(File,Model,Dummy,Dref,orientation(0,0,0,0))).

create_distal_dummy(File,Model,_,_,Dummy,C,C) :-
  gotdata(File,Model,node(dummy,Dummy)), !.

create_distal_dummy(File,Model,Limb1,Limb2,Dummy,[Dummy|C0],C0) :-
  clause(gotdata(File,Model,node(_,Limb1)),true,LimbRef1),
  assertz(gotdata(File,Model,node(dummy,Dummy)),Dref),
  assertz(gotdata(File,Model,Dummy,Dref,parent(Limb1/LimbRef1))),
  gotdata(File,Model,Limb2,LimbRef2,position(X0,Y0,Z0)),
  assertz(gotdata(File,Model,Dummy,Dref,position(X0,Y0,Z0))),
  assertz(gotdata(File,Model,Dummy,Dref,orientation(0,0,0,0))),
  (gotdata(File,Model,Limb2,LimbRef2,AnimName,positionkey(K)),
   assertz(gotdata(File,Model,Dummy,Dref,AnimName,positionkey(K))),
   fail ; true),
  (gotdata(File,Model,Limb2,LimbRef2,AnimName,positionkey(N,T,X,Y,Z)),
   assertz(gotdata(File,Model,Dummy,Dref,AnimName,positionkey(N,T,X,Y,Z))),
   fail ; true).

create_midpoint_dummy(File,Model,_,_,MidDummy,C,C) :-
  gotdata(File,Model,node(dummy,MidDummy)), !.

create_midpoint_dummy(File,Model,Limb1,Limb2,MidDummy,[MidDummy|C],C) :-
  clause(gotdata(File,Model,node(_,Limb1)),true,LimbRef1),
  assertz(gotdata(File,Model,node(dummy,MidDummy)),Dref),
  assertz(gotdata(File,Model,MidDummy,Dref,parent(Limb1/LimbRef1))),
  gotdata(File,Model,Limb2,LimbRef2,position(X2,Y2,Z2)),
  X0 is X2/2, Y0 is Y2/2, Z0 is Z2/2,
  assertz(gotdata(File,Model,MidDummy,Dref,position(X0,Y0,Z0))),
  assertz(gotdata(File,Model,MidDummy,Dref,orientation(0,0,0,0))),
  (gotdata(File,Model,Limb2,LimbRef2,AnimName,positionkey(K)),
   assertz(gotdata(File,Model,MidDummy,Dref,AnimName,positionkey(K))),
   fail ; true),
  (gotdata(File,Model,Limb2,LimbRef2,AnimName,positionkey(N,T,X,Y,Z)),
   Xa is X/2, Ya is Y/2, Za is Z/2,
   assertz(gotdata(File,Model,MidDummy,Dref,AnimName,positionkey(N,T,Xa,Ya,Za))),
   fail ; true).

create_referred_dummy(File,Model,_,_,Dummy,C,C) :-
  gotdata(File,Model,node(dummy,Dummy)), !.

create_referred_dummy(File,Model,Parent,Reference,Dummy,[Dummy|C0],C0) :-
  clause(gotdata(File,Model,node(_,Parent)),true,ParentRef),
  assertz(gotdata(File,Model,node(dummy,Dummy)),Dref),
  assertz(gotdata(File,Model,Dummy,Dref,parent(Parent/ParentRef))),
  gotdata(File,Model,Reference,RefRef,position(X0,Y0,Z0)),
  assertz(gotdata(File,Model,Dummy,Dref,position(X0,Y0,Z0))),
  assertz(gotdata(File,Model,Dummy,Dref,orientation(0,0,0,0))),
  (gotdata(File,Model,Reference,RefRef,AnimName,positionkey(K)),
   assertz(gotdata(File,Model,Dummy,Dref,AnimName,positionkey(K))),
   fail ; true),
  (gotdata(File,Model,Reference,RefRef,AnimName,positionkey(N,T,X,Y,Z)),
   assertz(gotdata(File,Model,Dummy,Dref,AnimName,positionkey(N,T,X,Y,Z))),
   fail ; true).

create_offset_dummy(File,Model,Parent,_,_,_,Dummy,C,C) :-
  clause(gotdata(File,Model,node(dummy,Dummy)),true,Dref),
  gotdata(File,Model,Dummy,Dref,parent(Parent/_)),
  !.

create_offset_dummy(File,Model,Parent,X,Y,Z,Dummy,[Dummy|C0],C0) :-
  clause(gotdata(File,Model,node(_,Parent)),true,PRef),
  assertz(gotdata(File,Model,node(dummy,Dummy)),Dref),
  assertz(gotdata(File,Model,Dummy,Dref,parent(Parent/PRef))),
  assertz(gotdata(File,Model,Dummy,Dref,position(X,Y,Z))),
  assertz(gotdata(File,Model,Dummy,Dref,orientation(0,0,0,0))).

/* ========================= */
/* set_rotation_type1/4      */
/* set_rotation_type4/4      */
/* set_rotation_type_hinge/8 */
/* correct_possible_flip/19  */
/* interpolate_rotationkey/6 */
/* set_midpoint_rotations/5  */
/* set_rotator_rotations/6   */
/* set_half_rotations/4      */
/* ========================= */

set_rotation_type1(File,Model,Limb,Dummy) :-
  clause(gotdata(File,Model,node(dummy,Dummy)),true,Dref),
  \+gotdata(File,Model,Dummy,Dref,_,orientationkey(_)),
  gotdata(File,Model,Limb,LimbRef,orientation(X0,Y0,Z0,A0)),
  two_pi_fix(A0,A0f),
  rotation_type1(X0,Y0,Z0,A0f,Theta0),
  (Theta0=:=0 -> Z1=0 ; Z1=1),
  retractall(gotdata(File,Model,Dummy,Dref,orientation(_,_,_,_))),
  assertz(gotdata(File,Model,Dummy,Dref,orientation(0,0,Z1,Theta0))),
  (gotdata(File,Model,Limb,LimbRef,AnimName,orientationkey(K)),
   retractall(gotdata(File,Model,Dummy,Dref,AnimName,orientationkey(_))),
   assertz(gotdata(File,Model,Dummy,Dref,AnimName,orientationkey(K))),
   fail ; true),
  (gotdata(File,Model,Limb,LimbRef,AnimName,orientationkey(N,T,X,Y,Z,A)),
   two_pi_fix(A,Af),
   rotation_type1(X,Y,Z,Af,Theta),
   (Theta=:=0 -> Z2=0 ; Z2=1),
   retractall(gotdata(File,Model,Dummy,Dref,AnimName,orientationkey(N,T,_,_,_,_))),
   assertz(gotdata(File,Model,Dummy,Dref,AnimName,orientationkey(N,T,0,0,Z2,Theta))),
   fail ; true),
  !.

set_rotation_type1(_,_,_,_).

set_rotation_type4(File,Model,Limb,Dummy) :-
  clause(gotdata(File,Model,node(dummy,Dummy)),true,Dref),
  \+gotdata(File,Model,Dummy,Dref,_,orientationkey(_)),
  gotdata(File,Model,Limb,LimbRef,orientation(X0,Y0,Z0,A0)),
  two_pi_fix(A0,A0f),
  rotation_type4(X0,Y0,Z0,A0f,Theta0),
  (Theta0=:=0 -> Z1=0 ; Z1=1),
  retractall(gotdata(File,Model,Dummy,Dref,orientation(_,_,_,_))),
  assertz(gotdata(File,Model,Dummy,Dref,orientation(0,0,Z1,Theta0))),
  (gotdata(File,Model,Limb,LimbRef,AnimName,orientationkey(K)),
   retractall(gotdata(File,Model,Dummy,Dref,AnimName,orientationkey(_))),
   assertz(gotdata(File,Model,Dummy,Dref,AnimName,orientationkey(K))),
   fail ; true),
  (gotdata(File,Model,Limb,LimbRef,AnimName,orientationkey(N,T,X,Y,Z,A)),
   two_pi_fix(A,Af),
   rotation_type4(X,Y,Z,Af,Theta),
   (Theta=:=0 -> Z2=0 ; Z2=1),
   retractall(gotdata(File,Model,Dummy,Dref,AnimName,orientationkey(N,T,_,_,_,_))),
   assertz(gotdata(File,Model,Dummy,Dref,AnimName,orientationkey(N,T,0,0,Z2,Theta))),
   fail ; true),
  !.

set_rotation_type4(_,_,_,_).

set_rotation_type_hinge(File,Model,Limb,Dummy1,Dummy2,Dummy3,Dummy4,JointType) :-
  retractall(fixlist(_)),
  clause(gotdata(File,Model,node(dummy,Dummy2)),true,Dref2),
  clause(gotdata(File,Model,node(dummy,Dummy3)),true,Dref3),
  \+gotdata(File,Model,Dummy2,Dref2,_,orientationkey(_)),
  \+gotdata(File,Model,Dummy3,Dref3,_,orientationkey(_)),
  gotdata(File,Model,Limb,LimbRef,orientation(X0,Y0,Z0,A0)),
  rotation_type_hinge(X0,Y0,Z0,A0,JointType,Theta2,Theta3),
  (Theta2=:=0 -> Z2=0 ; Z2=1),
  retractall(gotdata(File,Model,Dummy2,Dref2,orientation(_,_,_,_))),
  assertz(gotdata(File,Model,Dummy2,Dref2,orientation(0,0,Z2,Theta2))),
  (Theta3=:=0 -> Z3=0 ; Z3=1),
  retractall(gotdata(File,Model,Dummy3,Dref3,orientation(_,_,_,_))),
  assertz(gotdata(File,Model,Dummy3,Dref3,orientation(0,0,Z3,Theta3))),
  (gotdata(File,Model,newanim(AnimName,Model)),
   gotdata(File,Model,Limb,LimbRef,AnimName,orientationkey(K)),
   retractall(gotdata(File,Model,Dummy2,Dref2,AnimName,orientationkey(_))),
   assertz(gotdata(File,Model,Dummy2,Dref2,AnimName,orientationkey(K))),
   retractall(gotdata(File,Model,Dummy3,Dref3,AnimName,orientationkey(_))),
   assertz(gotdata(File,Model,Dummy3,Dref3,AnimName,orientationkey(K))),
   K1 is K-1, between(0,K1,N),
   gotdata(File,Model,Limb,LimbRef,AnimName,orientationkey(N,T,X,Y,Z,A)),
   rotation_type_hinge(X,Y,Z,A,JointType,Theta_2a,Theta_3a),
   correct_possible_flip(File,Model,Dummy1,Dummy2,Dref2,Dummy3,Dref3,Dummy4,AnimName,N,T,X,Y,Z,A,Theta_2a,Theta_3a,Theta_2,Theta_3),
   (Theta_2=:=0 -> Z_2=0 ; Z_2=1),
   retractall(gotdata(File,Model,Dummy2,Dref2,AnimName,orientationkey(N,T,_,_,_,_))),
   assertz(gotdata(File,Model,Dummy2,Dref2,AnimName,orientationkey(N,T,0,0,Z_2,Theta_2))),
   (Theta_3=:=0 -> Z_3=0 ; Z_3=1),
   retractall(gotdata(File,Model,Dummy3,Dref3,AnimName,orientationkey(N,T,_,_,_,_))),
   assertz(gotdata(File,Model,Dummy3,Dref3,AnimName,orientationkey(N,T,0,0,Z_3,Theta_3))),
   fail),
  fail.

set_rotation_type_hinge(_,_,_,Dummy,_,_,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>0, N=<5,
  setof(X,fixlist(X),List),
  concat_atom([omb,bone,JointName|_],'_',Dummy),
  tab(2), write(JointName), write(' joint-flips detected in '), write(List), nl,
  fail.

set_rotation_type_hinge(_,_,_,Dummy,_,_,_,_) :-
  predicate_property(fixlist(_),number_of_clauses(N)), N>5,
  concat_atom([omb,bone,JointName|_],'_',Dummy),
  tab(2), write(JointName), write(' joint-flips detected in '), write(N), write(' animations'), nl,
  fail.

set_rotation_type_hinge(_,_,_,_,_,_,_,_).

correct_possible_flip(File,Model,Dummy1,_,_,_,_,Dummy4,AnimName,_,T,_,_,Z,A,Theta_2a,Theta_3a,Theta_2,Theta_3) :-
  /* Anomalies can occur if:                                                                                    */
  /* the hinge calculation returns a result that flips by +-pi as a result of overstraightening the joint;      */
  /* the joint is subjected to a significant Y-axis dislocation and the hinge rotates to try to accommodate it; */
  interpolate_rotationkey(File,Model,Dummy1,AnimName,T,A1),
  interpolate_rotationkey(File,Model,Dummy4,AnimName,T,A4),
  Test1 is A4-Theta_3a, Test2 is Theta_2a-A1, Test is Test1-Test2, DTest is Test1+Test2, Zdot is (1-cos(A))*Z*Z+cos(A),
  (max(abs(Test1),abs(Test2)) > 2.4, Test > 1.1*pi, Zdot > 0 ->
     two_pi_fix(Theta_2a-pi,Theta_2),
     two_pi_fix(Theta_3a-pi,Theta_3),
     ( \+ fixlist(AnimName) -> assertz(fixlist(AnimName)) ; true)
   ;
   max(abs(Test1),abs(Test2)) > 2.4, Test < -1.1*pi, Zdot > 0 ->
     two_pi_fix(Theta_2a+pi,Theta_2),
     two_pi_fix(Theta_3a+pi,Theta_3),
     ( \+ fixlist(AnimName) -> assertz(fixlist(AnimName)) ; true)
   ;
   Zdot > 0.9, abs(DTest) > 2.4 ->
     two_pi_fix((Theta_2a-DTest/2),Theta_2),
     two_pi_fix((Theta_3a+DTest/2),Theta_3),
     ( \+ fixlist(AnimName) -> assertz(fixlist(AnimName)) ; true)
   ;
   Zdot > 0 ->
     two_pi_fix((Theta_2a+Test/2),Theta_2),
     two_pi_fix((Theta_3a+Test/2),Theta_3),
     ( \+ fixlist(AnimName) -> assertz(fixlist(AnimName)) ; true)
   ;
     Theta_2=Theta_2a, Theta_3=Theta_3a
  ),
  !.

correct_possible_flip(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,Theta_2,Theta_3,Theta_2,Theta_3).


interpolate_rotationkey(File,Model,Dummy,AnimName,T,A) :-
  gotdata(File,Model,Dummy,_,AnimName,orientationkey(_,T,_,_,_,A)),
  !.

interpolate_rotationkey(File,Model,Dummy,AnimName,T,A) :-
  gotdata(File,Model,Dummy,Dref,AnimName,orientationkey(N1,T1,_,_,_,A1)),
  T1<T, N2 is N1+1,
  gotdata(File,Model,Dummy,Dref,AnimName,orientationkey(N2,T2,_,_,_,A2)),
  T2>T,
  A is A1+(A2-A1)*(T-T1)/(T2-T1),
  !.

two_pi_fix(A,A1) :-
  A > pi -> two_pi_fix(A-2*pi,A1) ;
  A < -pi -> two_pi_fix(A+2*pi,A1) ;
  A1 is A.

set_midpoint_rotations(File,Model,Dummy1,Dummy2,MidDummy) :-
  clause(gotdata(File,Model,node(dummy,Dummy1)),true,Dummy1Ref),
  clause(gotdata(File,Model,node(dummy,MidDummy)),true,MidDummyRef),
  gotdata(File,Model,node(dummy,Dummy2)),
  \+gotdata(File,Model,MidDummy,MidDummyRef,_,orientationkey(_)),
  gotdata(File,Model,newanim(AnimName,Model)),
  (gotdata(File,Model,Dummy1,Dummy1Ref,AnimName,orientationkey(K)),
   gotdata(File,Model,Dummy2,_,AnimName,orientationkey(_)),
   assertz(gotdata(File,Model,MidDummy,MidDummyRef,AnimName,orientationkey(K))),
   fail ; true),
  (gotdata(File,Model,Dummy1,Dummy1Ref,AnimName,orientationkey(N,T,_,_,_,A1)),
   interpolate_rotationkey(File,Model,Dummy2,AnimName,T,A2),
   two_pi_fix(A1,B1), two_pi_fix(A2,B2), two_pi_fix((B1+B2)/2,A), (A=:=0 -> Z1=0 ; Z1=1),
   assertz(gotdata(File,Model,MidDummy,MidDummyRef,AnimName,orientationkey(N,T,0,0,Z1,A))),
   fail ; true),
  fail ; true.

set_rotator_rotations(File,Model,Dummy2,Limb2,Dummy3,Rotator) :-
  clause(gotdata(File,Model,node(dummy,Rotator)),true,RotatorRef),
  \+gotdata(File,Model,Rotator,RotatorRef,_,orientationkey(_)),
  (gotdata(File,Model,Limb2,Limb2Ref,AnimName,orientationkey(K)),
   assertz(gotdata(File,Model,Rotator,RotatorRef,AnimName,orientationkey(K))),
   fail ; true),
  (gotdata(File,Model,Limb2,Limb2Ref,AnimName1,orientationkey(N,T,X1,Y1,Z1,A1)),
   gotdata(File,Model,Dummy2,_,AnimName1,orientationkey(N,T,X2,Y2,Z2,A2)),
   gotdata(File,Model,Dummy3,_,AnimName1,orientationkey(N,T,X3,Y3,Z3,A3)),
   two_pi_fix(A1,B1), axisangle2quaternion([X1,Y1,Z1,B1],Q1),
   two_pi_fix(A2,B2), axisangle2quaternion([X2,Y2,Z2,B2],Q2),
   two_pi_fix(A3,B3), axisangle2quaternion([X3,Y3,Z3,B3],Q3),
   multiply_quaternions(Q1,Q3,Q13),
   interpolate_quaternions(Q2,Q13,0.5,Q),
   quaternion2axisangle(Q,[X,Y,Z,A]), two_pi_fix(A,B),
   assertz(gotdata(File,Model,Rotator,RotatorRef,AnimName1,orientationkey(N,T,X,Y,Z,B))),
   fail ; true),
  fail ; true.

/* ==========================================================================*/
/* Modified in CM362m 2013-08-13                                             */
/* ==========================================================================*/
/* Preset a relaxed position with the limb 45deg forward then outward.       */
/* Set orientationkeys by interpolating 0.5 between the Limb and the relaxed */
/* position, discarding any existing keys from stale animations.             */
/* ==========================================================================*/

set_half_rotations(File,Model,Limb,TopRotator) :-
  clause(gotdata(File,Model,node(_,Limb)),true,LimbRef),
  clause(gotdata(File,Model,node(dummy,TopRotator)),true,TopRef),
  sub_atom(Limb,0,1,_,LR),
  axisangle2quaternion([1,0,0,pi/4],Q1),
  (LR=='l' -> axisangle2quaternion([0,0,1,pi/4],Q2) ; axisangle2quaternion([0,0,-1,pi/4],Q2)),
  multiply_quaternions(Q2,Q1,Q0),
  (gotdata(File,Model,Limb,LimbRef,AnimName,orientationkey(K)),
   retractall(gotdata(File,Model,TopRotator,TopRef,AnimName,orientationkey(_))),
   assertz(gotdata(File,Model,TopRotator,TopRef,AnimName,orientationkey(K))),
   fail; true),
  (gotdata(File,Model,Limb,LimbRef,AnimName1,orientationkey(N,T,X,Y,Z,A)),
   axisangle2quaternion([X,Y,Z,A],Qr),
   interpolate_quaternions(Qr,Q0,0.5,Q),
   quaternion2axisangle(Q,[X1,Y1,Z1,Aq]), two_pi_fix(Aq,A1),
   retractall(gotdata(File,Model,TopRotator,TopRef,AnimName1,orientationkey(N,T,_,_,_,_))),
   assertz(gotdata(File,Model,TopRotator,TopRef,AnimName1,orientationkey(N,T,X1,Y1,Z1,A1))),
   fail; true),
  (gotdata(File,Model,Limb,LimbRef,_,orientationkey(_)) -> tab(2), write('half-rotations set on '), write(TopRotator), nl ; true),
  fail ; true.

/* ===================== */
/* rotation_type1/5      */
/* rotation_type4/5      */
/* rotation_type_hinge/7 */
/* ===================== */

rotation_type1(_,_,_,A,Theta) :- A=:=0, !, Theta=0.

rotation_type1(X0,Y0,Z0,A,Theta) :-
  R is sqrt(X0*X0+Y0*Y0+Z0*Z0),
  /*
  X is X0/R, Y is Y0/R, Z is Z0/R,
  C is cos(A), S is sin(A), T is 1.0-C,
  K is C+T*Z*Z,
  Theta is atan(-K*(T*X*Y+Z*S),K*(T*Y*Y+C)).
  */
  Theta is -A*Z0/R. /* Bodge */

rotation_type4(_,_,_,A,Theta) :- A=:=0, !, Theta=0.

rotation_type4(X0,Y0,Z0,A,Theta) :-
  R is sqrt(X0*X0+Y0*Y0+Z0*Z0), Z is Z0/R,
  S is sin(A), C is cos(A), T is 1.0-C,
  Theta is atan(2*Z*S,2.0-T*(1.0+Z*Z)).

rotation_type_hinge(_,_,_,A,_,0,0) :- A=:=0, !.

rotation_type_hinge(X0,Y0,Z0,A,JointType,Theta1,Theta2) :-
  R is sqrt(X0*X0+Y0*Y0+Z0*Z0),
  X is X0/R, Y is Y0/R, Z is Z0/R,
  S is sin(A), C is cos(A), T is 1.0-C,
  A31 is T*X*Z-Y*S, A13 is T*X*Z+Y*S,
  A32 is T*Y*Z+X*S, A23 is T*Y*Z-X*S,
  (JointType==knee ->
     Sin1 is -A13, Cos1 is  A23, Sin2 is -A31, Cos2 is -A32,
     (abs(X)>0.9, X*S>0 -> K = -1 ; K = 1) /* Checks for overstraightening */
   ;
     Sin1 is A23, Cos1 is A13, Sin2 is -A32, Cos2 is -A31,
     (JointType==left -> K = -1 ;  K = 1)
  ),
  Theta1 is atan(K*Sin1,K*Cos1),
  Theta2 is atan(K*Sin2,K*Cos2).

/* ============== */
/* create_skin/12 */
/* weight_list/7  */
/* ============== */

create_skin(File,Model,_,_,_,_,_,Skin,C,C,S,S) :-
  gotdata(File,Model,node(skin,Skin)),
  !.

create_skin(File,Model,Limb,_,_,_,_,_,C,C,S,S) :-
  \+((
    gotdata(File,Model,node(NodeType,Limb)),
    member(NodeType,[trimesh,danglymesh]),
    gotdata(File,Model,Limb,_,faces(_))
    )),
  !.

create_skin(File,Model,Limb,_,_,_,_,_,C,C,S,S) :-
  gotdata(File,Model,Limb,_,render(0)),
  !.

create_skin(File,Model,Limb,Dummy1,Dummy2,_,_,Skin,[Skin|C0],C0,S,S0) :-
  var(Dummy1), var(Dummy2),
  clause(gotdata(File,Model,node(_,Limb)),true,LimbRef),
  assertz(gotdata(File,Model,node(skin,Skin)),SkinRef),
  assertz(gotdata(File,Model,Skin,SkinRef,shadow(0))),  /* changed in CM362n */
  (gotdata(File,Model,Limb,LimbRef,Q),
   functor(Q,Q0,_), Q0\=='#part-number', Q0\=='shadow', \+clause(paramtype(danglymesh,_,Q0),true),
   assertz(gotdata(File,Model,Skin,SkinRef,Q)),
   fail ; true),
  gotdata(File,Model,Skin,SkinRef,verts(N)),
  assertz(gotdata(File,Model,Skin,SkinRef,weights(N))),
  Last is N-1,
  (between(0,Last,V), assertz(gotdata(File,Model,Skin,SkinRef,weights(V,[Limb,1.0]))), fail ; true),
  raise_to_tile(File,Model,Skin,SkinRef),
  set_zero_orientation(File,Model,Skin,SkinRef),
  set_zero_position(File,Model,Skin,SkinRef),
  apply_snap(File,Model,Skin,SkinRef),
  (gotdata(File,Model,Limb,LimbRef,shadow(0)) ->
   reclassify_node(File,Model,Limb,LimbRef,dummy,_),
   S=S0
   ;
   retractall(gotdata(File,Model,Limb,LimbRef,render(_))),
   assertz(gotdata(File,Model,Limb,LimbRef,render(0))),
   retractall(gotdata(File,Model,Limb,LimbRef,bitmap(_))),
   assertz(gotdata(File,Model,Limb,LimbRef,bitmap(black))),
   S=[Limb|S0]
  ),
  !.

create_skin(File,Model,Limb,Dummy1,Dummy2,MidDummy,Pendulum,Skin,[Skin|C0],C0,S,S0) :-
  nonvar(Dummy1), nonvar(Dummy2),
  clause(gotdata(File,Model,node(_,Limb)),true,LimbRef),
  gotdata(File,Model,Dummy2,_,position(_,_,Zbase)),
  assertz(gotdata(File,Model,node(skin,Skin)),SkinRef),
  assertz(gotdata(File,Model,Skin,SkinRef,shadow(0))),  /* changed in CM362n */
  (gotdata(File,Model,Limb,LimbRef,Q),
   functor(Q,Q0,_), Q0\=='#part-number', Q0\=='shadow', \+clause(paramtype(danglymesh,_,Q0),true),
   assertz(gotdata(File,Model,Skin,SkinRef,Q)),
   fail ; true),
  gotdata(File,Model,Skin,SkinRef,verts(N)),
  assertz(gotdata(File,Model,Skin,SkinRef,weights(N))),
  (gotdata(File,Model,Skin,SkinRef,verts(V,_,_,Z)), T is Z/Zbase,
   get_vertex_material(File,Model,Skin,SkinRef,V,Material), M is max(0,(Material-1)/9),
   weight_list(Dummy1,Dummy2,MidDummy,Pendulum,T,M,WList),
   assertz(gotdata(File,Model,Skin,SkinRef,weights(V,WList))),
   fail ; true
  ),
  raise_to_tile(File,Model,Skin,SkinRef),
  set_zero_orientation(File,Model,Skin,SkinRef),
  set_zero_position(File,Model,Skin,SkinRef),
  apply_snap(File,Model,Skin,SkinRef),
  (gotdata(File,Model,Limb,LimbRef,shadow(0)) ->
   reclassify_node(File,Model,Limb,LimbRef,dummy,_),
   S=S0
   ;
   retractall(gotdata(File,Model,Limb,LimbRef,render(_))),
   assertz(gotdata(File,Model,Limb,LimbRef,render(0))),
   retractall(gotdata(File,Model,Limb,LimbRef,bitmap(_))),
   assertz(gotdata(File,Model,Limb,LimbRef,bitmap(black))),
   S=[Limb|S0]
  ),
  !.

create_skin(_,_,_,_,_,_,_,Skin,C,C,S,S) :-
  tab(2), write('*** failed to make skinmesh '), write(Skin), nl.

weight_list(Dummy1,_,_,_,T,_,WList)        :- T=<0, WList=[Dummy1,1.0], !.
weight_list(_,Dummy2,_,_,T,M,WList)        :- T>=1, M=<0, WList=[Dummy2,1.0], !.
weight_list(_,_,_,Pendulum,T,M,WList)      :- T>=1, M>=1, WList=[Pendulum,1.0], !.
weight_list(_,Dummy2,_,Pendulum,T,M,WList) :- T>=1, M1 is 1.0*M, M2 is 1.0-M1, WList=[Pendulum,M1,Dummy2,M2], !.

/* Modified in CM362p to catch cases leading to zero weights */

weight_list(Dummy1,Dummy2,MidDummy,_,T,M,[Dummy1,W1,Dummy2,T]) :-
  var(MidDummy), M=<0, !, W1 is (1.0-T).

weight_list(Dummy1,_,MidDummy,Pendulum,T,M,[Dummy1,W1,Pendulum,T]) :-
  var(MidDummy), M>=1, !, W1 is (1.0-T).

weight_list(Dummy1,Dummy2,MidDummy,Pendulum,T,M,WList) :-
  var(MidDummy),
  W1 is (1.0-T), W2 is (1.0-M)*T, W3 is 1.0*M*T,
  WList=[Dummy1,W1,Dummy2,W2,Pendulum,W3],
  !.

weight_list(Dummy1,_,MidDummy,_,T,M,[Dummy1,W1,MidDummy,W2]) :-
  T<0.5, nonvar(MidDummy), M=<0, !, W1 is (1.0-2.0*T), W2 is 2.0*T.

weight_list(Dummy1,_,MidDummy,Pendulum,T,M,[Dummy1,W1,Pendulum,W3]) :-
  T<0.5, nonvar(MidDummy), M>=1, !, W1 is (1.0-2.0*T), W3 is 2.0*T.

weight_list(Dummy1,_,MidDummy,Pendulum,T,M,WList) :-
  T<0.5, nonvar(MidDummy),
  W1 is (1.0-2.0*T), W2 is 2.0*T*(1.0-M), W3 is 2.0*T*M,
  WList=[Dummy1,W1,MidDummy,W2,Pendulum,W3],
  !.

weight_list(_,_,MidDummy,_,T,M,[MidDummy,1]) :-
  T=:=0.5, nonvar(MidDummy), M=<0, !.

weight_list(_,_,MidDummy,Pendulum,T,M,[Pendulum,1]) :-
  T=:=0.5, nonvar(MidDummy), M>=1, !.

weight_list(_,_,MidDummy,Pendulum,T,M,[MidDummy,W2,Pendulum,M]) :-
  T=:=0.5, nonvar(MidDummy), !, W2 is (1.0-M).

weight_list(_,Dummy2,MidDummy,_,T,M,WList) :-
  T>0.5, nonvar(MidDummy), M=<0, !,
  W1 is 2.0*(1.0-T), W2 is (2.0*T-1.0),
  WList=[MidDummy,W1,Dummy2,W2],
  !.

weight_list(_,_,MidDummy,Pendulum,T,M,[Pendulum,1]) :-
  T>0.5, nonvar(MidDummy), M>=1, !.

weight_list(_,Dummy2,MidDummy,Pendulum,T,M,WList) :-
  T>0.5, nonvar(MidDummy),
  W1 is 2.0*(1.0-T)*(1.0-M), W2 is (2.0*T-1.0)*(1.0-M), W3 is 1.0*M,
  WList=[MidDummy,W1,Dummy2,W2,Pendulum,W3],
  !.

/* ==================== */
/* create_torso_skin/14 */
/* torso_weight/18      */
/* ==================== */

create_torso_skin(File,Model,_,_,_,_,_,_,_,Skin,C,C,S,S) :-
  gotdata(File,Model,node(skin,Skin)),
  !.

create_torso_skin(File,Model,Torso,_,_,_,_,_,_,_,C,C,S,S) :-
  \+((
    gotdata(File,Model,node(NodeType,Torso)),
    member(NodeType,[trimesh,danglymesh]),
    gotdata(File,Model,Torso,_,faces(_))
    )),
  !.

create_torso_skin(File,Model,Torso,_,_,_,_,_,_,_,C,C,S,S) :-
  gotdata(File,Model,Torso,_,render(0)),
  !.

create_torso_skin(File,Model,Torso,Pelvis,LCollar,RCollar,LBreast,RBreast,PendulumList,Skin,[Skin|C0],C0,S,S0) :-
  clause(gotdata(File,Model,node(_,Torso)),true,TorsoRef),
  gotdata(File,Model,Torso,TorsoRef,verts(_,_,Ybase,_)),
  \+((gotdata(File,Model,Torso,TorsoRef,verts(_,_,Y1,_)), Y1>Ybase)),
  gotdata(File,Model,LCollar,_,position(_,_,Zbase)),
  gotdata(File,Model,LCollar,_,position(_,_,Zbase)),
  assertz(gotdata(File,Model,node(skin,Skin)),SkinRef),
  assertz(gotdata(File,Model,Skin,SkinRef,shadow(0))),  /* changed in CM362n */
  (gotdata(File,Model,Torso,TorsoRef,Q),
   functor(Q,Q0,_), Q0\=='#part-number', Q0\=='shadow', \+clause(paramtype(danglymesh,_,Q0),true),
   assertz(gotdata(File,Model,Skin,SkinRef,Q)),
   fail ; true),
  gotdata(File,Model,Skin,SkinRef,verts(N)),
  assertz(gotdata(File,Model,Skin,SkinRef,weights(N))),
  (gotdata(File,Model,Skin,SkinRef,verts(V,X,Y,Z)),
   torso_weight(File,Model,Torso,TorsoRef,Pelvis,LCollar,RCollar,LBreast,RBreast,PendulumList,Skin,SkinRef,V,X,Y,Z,Zbase,WHL),
   assertz(gotdata(File,Model,Skin,SkinRef,weights(V,WHL))),
   fail ; true),
  raise_to_tile(File,Model,Skin,SkinRef),
  set_zero_orientation(File,Model,Skin,SkinRef),
  set_zero_position(File,Model,Skin,SkinRef),
  (clause(gotdata(File,Model,node(danglymesh,Torso)),true,TorsoRef) -> true ; apply_snap(File,Model,Skin,SkinRef)),
  (gotdata(File,Model,Torso,TorsoRef,shadow(0)) ->
    reclassify_node(File,Model,Torso,TorsoRef,dummy,_),
    S=S0
    ;
    retractall(gotdata(File,Model,Torso,TorsoRef,render(_))),
    assertz(gotdata(File,Model,Torso,TorsoRef,render(0))),
    retractall(gotdata(File,Model,Torso,TorsoRef,bitmap(_))),
    assertz(gotdata(File,Model,Torso,TorsoRef,bitmap(black))),
    S=[Torso|S0]
  ),
  !.

create_torso_skin(_,_,_,_,_,_,_,_,_,Skin,C,C,S,S) :-
  tab(2), write('*** failed to make skinmesh '), write(Skin), nl.

torso_weight(File,Model,Torso,TorsoRef,Pelvis,LCollar,RCollar,LBreast,RBreast,PendulumList,Skin,SkinRef,V,X,Y,Z,Zbase,WHL) :-
  PendulumList   = [Pendulum, Pendulum_Front, Pendulum_Back, Pendulum_Left, Pendulum_Right],
  (secret(jiggly_boobs), gotdata(File,Model,Torso,TorsoRef,constraints(V,C)) -> Dangle = C ;
   secret(jiggly_boobs), gotdata(File,Model,Torso,TorsoRef,colors(V,C,_,_)) -> Dangle = C ;
   Dangle = 0),
  get_vertex_material(File,Model,Skin,SkinRef,V,Material),
  once((gotdata(File,Model,Torso,TorsoRef,colors(V,_,_,Blue)) ; Blue = 0)),
  Margin = 0.1, Zw is (Z/Zbase-Margin)/(1-Margin),
  (
    Zw =< 0 -> W2=0; Zw >= 1 -> W2=1;
    /* next line changed in CM362m to faster fall-off */
    /* Y >=0 -> W2 is min(1,2*Zw); Y =< -abs(X) -> W2 is Zw; */
    Y >=0 -> W2 is min(1,3*Zw); Y =< -abs(X) -> W2 is Zw;
    W2 is  min(1,(2+Y/sqrt((X*X+Y*Y)/2))*Zw)
  ),
  W1 is 1.0-W2,
  Mfactor is min(1.0,max(0.0,1.0-0.1*Material)),
  W2b is W2*Dangle/255*Mfactor, W2c is W2-W2b, W2b2 is 0.5*W2b,
  (Dangle>0 -> (X>0 -> WL0=[RBreast,W2b]; X<0 -> WL0=[LBreast,W2b] ; WL0=[LBreast,W2b2,RBreast,W2b2]) ; WL0=[]),
  (
    Z<0.9*Zbase -> WL=[Torso,W2c|WL0] ;
    X>0 -> WL=[RCollar,W2c|WL0] ;
    X<0 -> WL=[LCollar,W2c|WL0] ;
    W22 is 0.5*W2, WL=[LCollar,W22,RCollar,W22]
  ),
  (Blue < 10 ->
    (Material>=10 -> Hem=[Pendulum,W1] ; Material=<1 -> Hem=[Pelvis,W1] ;
     W1m is W1*(Material-1.0)/9, W1p is W1-W1m, Hem=[Pelvis,W1p,Pendulum,W1m])
   ;
    W1m is W1*Blue/255, W1p is W1-W1m,
    (Y >=0 -> YBone = Pendulum_Front ; YBone = Pendulum_Back), W1my is W1m*abs(Y)/(abs(X)+abs(Y)),
    (X >=0 -> XBone = Pendulum_Right ; XBone = Pendulum_Left), W1mx is W1m*abs(X)/(abs(X)+abs(Y)),
    Hem = [Pelvis,W1p,XBone,W1mx,YBone,W1my]
  ),
  (W1=:=0 -> WHL0=WL ; W2=:=0 -> WHL0=Hem ; append(Hem,WL,WHL0)),
  enforce4bpv(WHL0,WHL),
  !.

/* ================================= */
/* reparent_bicep_to_collarbone/5    */
/* reparent_shoulder_to_collarbone/5 */
/* reparent_rotator_to_body/4        */
/* ================================= */

reparent_bicep_to_collarbone(File,Model,Torso,CollarBone,Bicep) :-
  clause(gotdata(File,Model,node(BicepType,Bicep)),true,BicepRef),
  gotdata(File,Model,Bicep,BicepRef,parent(Torso/TorsoRef)),
  gotdata(File,Model,CollarBone,CollarRef,parent(Torso/TorsoRef)),
  gotdata(File,Model,CollarBone,CollarRef,position(Xc,Yc,Zc)),
  assertz(gotdata(File,Model,node(BicepType,Bicep)),NewRef),
  assertz(gotdata(File,Model,Bicep,NewRef,parent(CollarBone/CollarRef))),
  gotdata(File,Model,Bicep,BicepRef,position(Xb,Yb,Zb)),
  X0 is Xb-Xc, Y0 is Yb-Yc, Z0 is Zb-Zc,
  assertz(gotdata(File,Model,Bicep,NewRef,position(X0,Y0,Z0))),
  (gotdata(File,Model,Bicep,BicepRef,Q), functor(Q,Q0,_), Q0\==position, Q0\==parent,
   assertz(gotdata(File,Model,Bicep,NewRef,Q)),
   fail ; true),
  retractall(gotdata(File,Model,Bicep,BicepRef,_)),
  (gotdata(File,Model,Bicep,BicepRef,AnimName,positionkey(N,T,Xb1,Yb1,Zb1)),
   X1 is Xb1-Xc, Y1 is Yb1-Yc, Z1 is Zb1-Zc,
   assertz(gotdata(File,Model,Bicep,NewRef,AnimName,positionkey(N,T,X1,Y1,Z1))),
   fail ; true),
  (gotdata(File,Model,Bicep,BicepRef,AnimName1,Q1), functor(Q1,Q00,_), Q00\==positionkey,
   assertz(gotdata(File,Model,Bicep,NewRef,AnimName1,Q1)),
   fail ; true),
  retractall(gotdata(File,Model,Bicep,BicepRef,_,_)),
  (gotdata(File,Model,Child,ChildRef,parent(Bicep/BicepRef)),
   assertz(gotdata(File,Model,Child,ChildRef,parent(Bicep/NewRef))),
   fail ; true),
  retractall(gotdata(File,Model,_,_,parent(Bicep/BicepRef))),
  erase(BicepRef),
  !,
  tab(2), write('reparented '), write(Bicep), write(' to '), write(CollarBone), nl.

reparent_bicep_to_collarbone(_,_,_,_,_).

reparent_shoulder_to_collarbone(File,Model,Bicep,CollarBone,Shoulder) :-
  clause(gotdata(File,Model,node(_,Shoulder)),true,ShRef),
  clause(gotdata(File,Model,Shoulder,ShRef,parent(Bicep/BicepRef)),true,Eref),
  gotdata(File,Model,Bicep,BicepRef,parent(CollarBone/CollarRef)),
  gotdata(File,Model,Bicep,BicepRef,position(X,Y,Z)),
  erase(Eref),
  assertz(gotdata(File,Model,Shoulder,ShRef,parent(CollarBone/CollarRef))),
  retractall(gotdata(File,Model,Shoulder,ShRef,position(_,_,_))),
  assertz(gotdata(File,Model,Shoulder,ShRef,position(X,Y,Z))),
  !,
  tab(2), write('reparented '), write(Shoulder), write(' to '), write(CollarBone), nl.

reparent_shoulder_to_collarbone(_,_,_,_,_).

reparent_rotator_to_body(File,Model,TopRotator,Limb) :-
  gotdata(File,Model,TopRotator,TopRef,parent(Limb/LimbRef)),
  gotdata(File,Model,Limb,LimbRef,parent(Parent/ParentRef)),
  gotdata(File,Model,Limb,LimbRef,position(X,Y,Z)),
  retractall(gotdata(File,Model,TopRotator,TopRef,parent(_))),
  asserta(gotdata(File,Model,TopRotator,TopRef,parent(Parent/ParentRef))),
  retractall(gotdata(File,Model,TopRotator,TopRef,position(_,_,_))),
  asserta(gotdata(File,Model,TopRotator,TopRef,position(X,Y,Z))),
  !,
  tab(2), write(TopRotator), write(' parented to '), write(Parent), nl.

reparent_rotator_to_body(_,_,_,_).

/* ====================== */
/* apply_shoulder_heave/4 */
/* compute_heave/5        */
/* ====================== */

apply_shoulder_heave(File,Model,Bicep,CollarBone) :-
  gotdata(File,Model,Bicep,BicepRef,parent(CollarBone/CollarRef)),
  \+gotdata(File,Model,CollarBone,CollarRef,_,orientationkey(_)),
  (gotdata(File,Model,Bicep,BicepRef,AnimName,orientationkey(K)),
   retractall(gotdata(File,Model,CollarBone,CollarRef,AnimName,orientationkey(_))),
   assertz(gotdata(File,Model,CollarBone,CollarRef,AnimName,orientationkey(K))),
   fail ; true),
  (gotdata(File,Model,Bicep,BicepRef,AnimName1,orientationkey(N,T,X,Y,Z,A)),
   compute_heave(X,Y,Z,A,Theta),
   retractall(gotdata(File,Model,CollarBone,CollarRef,AnimName1,orientationkey(N,T,_,_,_,_))),
   assertz(gotdata(File,Model,CollarBone,CollarRef,AnimName1,orientationkey(N,T,0,1,0,Theta))),
   fail ; true),
  fail.

apply_shoulder_heave(_,_,_,_).

compute_heave(_,_,_,A,0) :- A=:=0, !.

compute_heave(X0,Y0,Z0,A,Theta) :-
  R is sqrt(X0*X0+Y0*Y0+Z0*Z0),
  X is X0/R, Y is Y0/R, Z is Z0/R,
  C is cos(A), S is sin(A), T is 1.0-C,
  W1 is T*X*Z+Y*S, W2 is T*Z*Z+C, Theta0 is atan(W1,W2), Pi2 is pi/2,
  (
   W2<0, W1>  0.7 -> Theta is 0.3*(Theta0-Pi2);
   W2<0, W1< -0.7 -> Theta is 0.3*(Theta0+Pi2);
   Theta = 0
  ).

/* ======================== */
/* make_shake_keys/4        */
/* make_shake_keys/7        */
/* make_jiggle_keys/5       */
/* safe_make_jiggle_keys/14 */
/* gravity_keys/2           */
/* limit/3                  */
/* ======================== */

make_shake_keys(File,Model,Rootdummy,Shakedummy) :-
  gotdata(File,Model,node(dummy,Rootdummy)),
  \+gotdata(File,Model,node(dummy,Shakedummy)),
  create_proximal_dummy(File,Model,Rootdummy,Shakedummy,_,[]),
  fail.

make_shake_keys(File,Model,Rootdummy,Shakedummy) :-
  clause(gotdata(File,Model,node(dummy,Rootdummy)),true,RootRef),
  clause(gotdata(File,Model,node(dummy,Shakedummy)),true,ShakeRef),
  \+ gotdata(File,Model,Shakedummy,ShakeRef,_,_),
  gotdata(File,Model,Rootdummy,RootRef,AnimName,positionkey(Nk)),
  Nk>=3, Nk1 is Nk-1, Nk2 is Nk-2,
  assertz(gotdata(File,Model,Shakedummy,ShakeRef,AnimName,positionkey(Nk))),
  make_shake_keys(File,Model,Rootdummy,RootRef,Shakedummy,ShakeRef,AnimName,Nk1,Nk2),
  fail
  ;
  tab(2), write('created '), write(Shakedummy), write(' keys'), nl,
  !.

make_shake_keys(_,_,_,_).

make_shake_keys(File,Model,Rootdummy,RootRef,Shakedummy,ShakeRef,AnimName,Nk1,Nk2) :-
  gotdata(File,Model,Rootdummy,RootRef,AnimName,positionkey(0,T00,_,_,Z00)),
  gotdata(File,Model,Rootdummy,RootRef,AnimName,positionkey(1,T01,_,_,Z01)),
  gotdata(File,Model,Rootdummy,RootRef,AnimName,positionkey(Nk2,Tn2,_,_,Zn2)),
  gotdata(File,Model,Rootdummy,RootRef,AnimName,positionkey(Nk1,Tn1,_,_,Zn1)),
  Dz01 is (Z01-Z00)/(T01-T00), Dzn is (Zn1-Zn2)/(Tn1-Tn2), Dz0 is -0.01*(Dz01-Dzn)/(Tn1-Tn2+T01-T00),
  limit(Dz0,0.04,Dz0t),
  assertz(gotdata(File,Model,Shakedummy,ShakeRef,AnimName,positionkey(0,T00,0,0,Dz0t))),
  assertz(gotdata(File,Model,Shakedummy,ShakeRef,AnimName,positionkey(Nk1,Tn1,0,0,Dz0t))),
  (between(1,Nk2,N2), N1 is N2-1, N3 is N2+1,
    gotdata(File,Model,Rootdummy,RootRef,AnimName,positionkey(N1,T1,_,_,Z1)),
    gotdata(File,Model,Rootdummy,RootRef,AnimName,positionkey(N2,T2,_,_,Z2)),
    gotdata(File,Model,Rootdummy,RootRef,AnimName,positionkey(N3,T3,_,_,Z3)),
    Dz12 is (Z2-Z1)/(T2-T1), Dz23 is (Z3-Z2)/(T3-T2), Dz is -0.01*(Dz23-Dz12)/(T3-T1),
    limit(Dz,0.04,Dz1),
    assertz(gotdata(File,Model,Shakedummy,ShakeRef,AnimName,positionkey(N2,T2,0,0,Dz1))),
    fail ; true).

make_jiggle_keys(File,Model,Torso,LBreast,RBreast) :-
  /* Validate the heirarchy */
  clause(gotdata(File,Model,node(_,Torso)),true,TorsoRef),
  gotdata(File,Model,Torso,TorsoRef,parent(Rootdummy/RootRef)),
  gotdata(File,Model,Rootdummy,RootRef,parent(Model/_)),
  clause(gotdata(File,Model,node(dummy,LBreast)),true,LBreastRef),
  clause(gotdata(File,Model,node(dummy,RBreast)),true,RBreastRef),
  atom_concat(RBreast,'_1',RGimbal), atom_concat(LBreast,'_1',LGimbal),
  clause(gotdata(File,Model,node(dummy,LGimbal)),true,LGimbalRef),
  clause(gotdata(File,Model,node(dummy,RGimbal)),true,RGimbalRef),
  safe_make_jiggle_keys(File,Model,Torso,TorsoRef,Rootdummy,RootRef,LBreast,LBreastRef,RBreast,RBreastRef,LGimbal,LGimbalRef,RGimbal,RGimbalRef),
  !.

make_jiggle_keys(_,_,_,_,_).

safe_make_jiggle_keys(File,Model,Torso,TorsoRef,Rootdummy,RootRef,LBreast,LBreastRef,RBreast,RBreastRef,LGimbal,LGimbalRef,RGimbal,RGimbalRef) :-
  /* Make or remake static corrections */
  gotdata(File,Model,Torso,TorsoRef,orientation(Xt,Yt,Zt,At)),
  gravity_keys([Xt,Yt,Zt,At],[Xt1,Yt1,Zt1,At1]),
  retractall(gotdata(File,Model,LGimbal,LGimbalRef,orientation(_,_,_,_))),
  assertz(gotdata(File,Model,LGimbal,LGimbalRef,orientation(Xt1,Yt1,Zt1,At1))),
  retractall(gotdata(File,Model,RGimbal,RGimbalRef,orientation(_,_,_,_))),
  assertz(gotdata(File,Model,RGimbal,RGimbalRef,orientation(Xt1,Yt1,Zt1,At1))),
  gotdata(File,Model,Rootdummy,RootRef,orientation(Xr,Yr,Zr,Ar)),
  gravity_keys([Xr,Yr,Zr,Ar],[Xr1,Yr1,Zr1,Ar1]),
  retractall(gotdata(File,Model,LBreast,LBreastRef,orientation(_,_,_,_))),
  assertz(gotdata(File,Model,LBreast,LBreastRef,orientation(Xr1,Yr1,Zr1,Ar1))),
  retractall(gotdata(File,Model,RBreast,RBreastRef,orientation(_,_,_,_))),
  assertz(gotdata(File,Model,RBreast,RBreastRef,orientation(Xr1,Yr1,Zr1,Ar1))),
  fail.

safe_make_jiggle_keys(File,Model,_,_,_,_,_,_,_,_,_,_,_,_) :-
  \+gotdata(File,Model,newanim(_,Model)),
  !.

safe_make_jiggle_keys(File,Model,_,_,_,_,LBreast,LBreastRef,RBreast,RBreastRef,LGimbal,LGimbalRef,RGimbal,RGimbalRef) :-
  /* Check that this is not an existing model being run through CM3 again */
  gotdata(File,Model,LGimbal,LGimbalRef,_,orientationkey(_)),
  gotdata(File,Model,RGimbal,RGimbalRef,_,orientationkey(_)),
  gotdata(File,Model,LBreast,LBreastRef,_,orientationkey(_)),
  gotdata(File,Model,RBreast,RBreastRef,_,orientationkey(_)),
  gotdata(File,Model,LBreast,LBreastRef,_,positionkey(_)),
  gotdata(File,Model,RBreast,RBreastRef,_,positionkey(_)),
  !.

safe_make_jiggle_keys(File,Model,Torso,TorsoRef,Rootdummy,RootRef,LBreast,LBreastRef,RBreast,RBreastRef,LGimbal,LGimbalRef,RGimbal,RGimbalRef) :-
  /* Do keys for doable animations */
  gotdata(File,Model,newanim(AnimName,Model)),
  gotdata(File,Model,Torso,TorsoRef,AnimName,orientationkey(Kt)),
  gotdata(File,Model,Rootdummy,RootRef,AnimName,orientationkey(Kr)),
  retractall(gotdata(File,Model,LGimbal,LGimbalRef,AnimName,orientationkey(_))),
  assertz(gotdata(File,Model,LGimbal,LGimbalRef,AnimName,orientationkey(Kt))),
  retractall(gotdata(File,Model,RGimbal,RGimbalRef,AnimName,orientationkey(_))),
  assertz(gotdata(File,Model,RGimbal,RGimbalRef,AnimName,orientationkey(Kt))),
  retractall(gotdata(File,Model,LBreast,LBreastRef,AnimName,orientationkey(_))),
  assertz(gotdata(File,Model,LBreast,LBreastRef,AnimName,orientationkey(Kr))),
  retractall(gotdata(File,Model,RBreast,RBreastRef,AnimName,orientationkey(_))),
  assertz(gotdata(File,Model,RBreast,RBreastRef,AnimName,orientationkey(Kr))),

  (gotdata(File,Model,Torso,TorsoRef,AnimName,orientationkey(Na,Ta,Xa,Ya,Za,Aa)),
   gravity_keys([Xa,Ya,Za,Aa],[Xa1,Ya1,Za1,Aa1]),
   retractall(gotdata(File,Model,LGimbal,LGimbalRef,AnimName,orientationkey(Na,_,_,_,_,_))),
   assertz(gotdata(File,Model,LGimbal,LGimbalRef,AnimName,orientationkey(Na,Ta,Xa1,Ya1,Za1,Aa1))),
   retractall(gotdata(File,Model,RGimbal,RGimbalRef,AnimName,orientationkey(Na,_,_,_,_,_))),
   assertz(gotdata(File,Model,RGimbal,RGimbalRef,AnimName,orientationkey(Na,Ta,Xa1,Ya1,Za1,Aa1))),
   fail ; true),

  (gotdata(File,Model,Rootdummy,RootRef,AnimName,orientationkey(Nb,Tb,Xb,Yb,Zb,Ab)),
   gravity_keys([Xb,Yb,Zb,Ab],[Xb1,Yb1,Zb1,Ab1]),
   retractall(gotdata(File,Model,LBreast,LBreastRef,AnimName,orientationkey(Nb,_,_,_,_,_))),
   assertz(gotdata(File,Model,LBreast,LBreastRef,AnimName,orientationkey(Nb,Tb,Xb1,Yb1,Zb1,Ab1))),
   retractall(gotdata(File,Model,RBreast,RBreastRef,AnimName,orientationkey(Nb,_,_,_,_,_))),
   assertz(gotdata(File,Model,RBreast,RBreastRef,AnimName,orientationkey(Nb,Tb,Xb1,Yb1,Zb1,Ab1))),
   fail ; true),

  fail.

safe_make_jiggle_keys(File,Model,_,_,Rootdummy,RootRef,LBreast,LBreastRef,RBreast,RBreastRef,_,_,_,_) :-
  /* Make positionkeys on breast dummies from 2nd derivative of positionkeys on rootdummy */
  gotdata(File,Model,Rootdummy,RootRef,AnimName2,positionkey(Nk)),
  Nk>=3, Nk1 is Nk-1, Nk2 is Nk-2,
  assertz(gotdata(File,Model,LBreast,LBreastRef,AnimName2,positionkey(Nk))),
  assertz(gotdata(File,Model,RBreast,RBreastRef,AnimName2,positionkey(Nk))),
  gotdata(File,Model,Rootdummy,RootRef,AnimName2,positionkey(0,T00,_,_,Z00)),
  gotdata(File,Model,Rootdummy,RootRef,AnimName2,positionkey(1,T01,_,_,Z01)),
  gotdata(File,Model,Rootdummy,RootRef,AnimName2,positionkey(Nk2,Tn2,_,_,Zn2)),
  gotdata(File,Model,Rootdummy,RootRef,AnimName2,positionkey(Nk1,Tn1,_,_,Zn1)),
  Dz01 is (Z01-Z00)/(T01-T00), Dzn is (Zn1-Zn2)/(Tn1-Tn2), Dz0 is -0.01*(Dz01-Dzn)/(Tn1-Tn2+T01-T00),
  limit(Dz0,0.04,Dz0t),
  assertz(gotdata(File,Model,LBreast,LBreastRef,AnimName2,positionkey(0,T00,0,0,Dz0t))),
  assertz(gotdata(File,Model,RBreast,RBreastRef,AnimName2,positionkey(0,T00,0,0,Dz0t))),
  assertz(gotdata(File,Model,LBreast,LBreastRef,AnimName2,positionkey(Nk1,Tn1,0,0,Dz0t))),
  assertz(gotdata(File,Model,RBreast,RBreastRef,AnimName2,positionkey(Nk1,Tn1,0,0,Dz0t))),
  (between(1,Nk2,N2), N1 is N2-1, N3 is N2+1,
   gotdata(File,Model,Rootdummy,RootRef,AnimName2,positionkey(N1,T1,_,_,Z1)),
   gotdata(File,Model,Rootdummy,RootRef,AnimName2,positionkey(N2,T2,_,_,Z2)),
   gotdata(File,Model,Rootdummy,RootRef,AnimName2,positionkey(N3,T3,_,_,Z3)),
   Dz12 is (Z2-Z1)/(T2-T1), Dz23 is (Z3-Z2)/(T3-T2), Dz is -0.01*(Dz23-Dz12)/(T3-T1),
   limit(Dz,0.04,Dz1),
   assertz(gotdata(File,Model,LBreast,LBreastRef,AnimName2,positionkey(N2,T2,0,0,Dz1))),
   assertz(gotdata(File,Model,RBreast,RBreastRef,AnimName2,positionkey(N2,T2,0,0,Dz1))),
   fail ; true),
  fail.

safe_make_jiggle_keys(_,_,_,_,_,_,LBreast,_,RBreast,_,LGimbal,_,RGimbal,_) :-
  tab(2), write('jiggly adjustments computed for '), write([LBreast,LGimbal,RBreast,RGimbal]), nl,
  !.

gravity_keys([_,_,_,A],[0,0,0,0]) :- A=:=0, !.

gravity_keys([X,Y,_,_],[0,0,0,0]) :- X=:=0, Y=:=0, !.

gravity_keys([X0,Y0,Z0,A],[X1,Y1,0,A1]) :-
  R is sqrt(X0*X0+Y0*Y0+Z0*Z0),
  X is X0/R, Y is Y0/R, Z is Z0/R,
  C is cos(A), S is sin(A), T is 1.0-C,
  Cxy is T*Z*Z+C, Sxy is sqrt(1.0-Cxy*Cxy),
  X1 is -(T*Y*Z-X*S)/Sxy, Y1 is (T*X*Z+Y*S)/Sxy,
  A1 is -0.1*acos(Cxy),
  !.

limit(X,L,X1) :- X1 is min(L,max(X,-L)).

/* ============ */
/* make_joint/7 */
/* ============ */

make_joint(File,Model,Skin1,Skin2,Dummy1,Dummy2,JointType) :-
  clause(gotdata(File,Model,node(skin,Skin1)),true,SkinRef1),
  clause(gotdata(File,Model,node(skin,Skin2)),true,SkinRef2),
  (member(JointType,[knee,left,right]) -> Band = 0.03 ; JointType==waist -> Band = 0.05 ; JointType==neck -> Band = 0.01 ; Band = 0.02),
  relate_to_tile(File,Model,Dummy1,_,[0,0,0],[_,_,Z1]),
  (JointType==neck ->
     once((
             gotdata(File,Model,Skin2,Sref2,verts(_,_,_,Z2)),
             \+((gotdata(File,Model,Skin2,Sref2,verts(_,_,_,Zr)), Zr>Z2))
         )),
     Z is (Z1+Z2)/2+Band
   ;
     Z=Z1
  ),
  snap(Z,Zs),
  skin_prepare_end(File,Model,Skin1,SkinRef1,Zs,removeBottom,Band),
  skin_prepare_end(File,Model,Skin2,SkinRef2,Zs,removeTop,Band),
  match_end_verts(File,Model,Skin1,SkinRef1,Skin2,SkinRef2,Zs,JointType),
  skin_match_weights(File,Model,Skin1,SkinRef1,Zs,Dummy1,Dummy2,down),
  skin_match_weights(File,Model,Skin2,SkinRef2,Zs,Dummy1,Dummy2,up),
  !,
  tab(2), write('prepared '), write(JointType), write(' joint between '), write(Skin1), write(' and '), write(Skin2), nl.

make_joint(_,_,_,_,_,_,_).


/* ================== */
/* skin_prepare_end/7 */
/* black_or_white/3   */
/* ================== */

skin_prepare_end(File,Model,Skin,SkinRef,Zs,WhichSide,Band) :-
  clause(gotdata(File,Model,Skin,SkinRef,verts(V,X,Y,Z)),true,Eref),
  (gotdata(File,Model,Skin,SkinRef,constraints(V,C)) -> C=:=0 ; true),
  (gotdata(File,Model,Skin,SkinRef,colors(V,R,G,B)) -> black_or_white(R,G,B) ; true),
  (WhichSide==removeTop -> Z>Zs-Band ; WhichSide=removeBottom -> Z<Zs+Band ; fail),
  /* once(get_vertex_material(File,Model,Skin,SkinRef,V,M)), M==1, */
  erase(Eref),
  assertz(gotdata(File,Model,Skin,SkinRef,verts(V,X,Y,Zs))),
  fail.

skin_prepare_end(File,Model,Skin,SkinRef,Zs,WhichSide,_) :-
  gotdata(File,Model,Skin,SkinRef,faces(NFaces)),
  setof(F,skin_face_unwanted(File,Model,Skin,SkinRef,Zs,WhichSide,F),FacesToRemove),
  length(FacesToRemove,N), N>0, N<NFaces,
  f_delete_faces_in_list(File,Model,Skin,SkinRef,FacesToRemove),
  delete_unused_vertices(File,Model,Skin,SkinRef),
  delete_unused_tverts(File,Model,Skin,SkinRef),
  !.

skin_prepare_end(_,_,_,_,_,_,_).

black_or_white(0,0,0) :- !.
black_or_white(255,255,255) :- !.

/* ==================== */
/* skin_face_unwanted/7 */
/* ==================== */

skin_face_unwanted(File,Model,NodeName,NodeRef,Z,removeTop,F) :-
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,_,_,Zv1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,_,_,Zv2)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,_,_,Zv3)),
  once(( Zv1>Z ; Zv2>Z ; Zv3>Z )).

skin_face_unwanted(File,Model,NodeName,NodeRef,Z,removeBottom,F) :-
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,_,_,Zv1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,_,_,Zv2)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,_,_,Zv3)),
  once(( Zv1<Z ; Zv2<Z ; Zv3<Z )).

skin_face_unwanted(File,Model,NodeName,NodeRef,Z,_,F) :-
  gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,_,_,_,_)),
  gotdata(File,Model,NodeName,NodeRef,verts(V1,_,_,Zv1)),
  gotdata(File,Model,NodeName,NodeRef,verts(V2,_,_,Zv2)),
  gotdata(File,Model,NodeName,NodeRef,verts(V3,_,_,Zv3)),
  snap_equal(Zv1,Z), snap_equal(Zv2,Z), snap_equal(Zv3,Z).

/* ==================== */
/* skin_match_weights/8 */
/* ==================== */

skin_match_weights(File,Model,Skin,SkinRef,Zc,Dummy1,Dummy2,UpOrDown) :-
  gotdata(File,Model,Skin,SkinRef,verts(V,_,_,Z)),
  /* once(get_vertex_material(File,Model,Skin,SkinRef,V,M)), M==1, */
  (snap_equal(Z,Zc), Dummy1==Dummy2 ->
     retractall(gotdata(File,Model,Skin,SkinRef,weights(V,_))),
     assertz(gotdata(File,Model,Skin,SkinRef,weights(V,[Dummy1,1.0])))
   ;
   snap_equal(Z,Zc) ->
     retractall(gotdata(File,Model,Skin,SkinRef,weights(V,_))),
     assertz(gotdata(File,Model,Skin,SkinRef,weights(V,[Dummy1,0.5,Dummy2,0.5])))
   ;
   UpOrDown=='up', Z>Zc ->
     retractall(gotdata(File,Model,Skin,SkinRef,weights(V,_))),
     assertz(gotdata(File,Model,Skin,SkinRef,weights(V,[Dummy1,1.0])))
   ;
   UpOrDown=='down', Z<Zc ->
     retractall(gotdata(File,Model,Skin,SkinRef,weights(V,_))),
     assertz(gotdata(File,Model,Skin,SkinRef,weights(V,[Dummy2,1.0])))
   ;
     true
  ),
  fail ; true.

/* ================== */
/* match_end_verts/8  */
/* end_vertex/6       */
/* centroid_both/4    */
/* total_xy/3         */
/* mean_radius_both/5 */
/* total_radius/4     */
/* check_convex/9     */
/* adjust_to_circle/8 */
/* ================== */

match_end_verts(File,Model,Skin1,SkinRef1,Skin2,SkinRef2,Zc,waist) :-
  setof(R1,end_vertex(File,Model,Skin1,SkinRef1,Zc,R1),Ends1),
  setof(R2,end_vertex(File,Model,Skin2,SkinRef2,Zc,R2),Ends2),
  centroid_both(Ends1,Ends2,Xc,Yc),
  check_convex(File,Model,Skin1,SkinRef1,Xc,Yc,Zc,Ends1,up),
  check_convex(File,Model,Skin2,SkinRef2,Xc,Yc,Zc,Ends2,down),
  best_fit_trilobe(Ends1,Ends2,Xc,Yc,Params),
  (cm3_verbose -> tab(2), write('best fit shape '), write(Params), nl ; true),
  adjust_to_trilobe(File,Model,Skin1,SkinRef1,Xc,Yc,Zc,Params),
  adjust_to_trilobe(File,Model,Skin2,SkinRef2,Xc,Yc,Zc,Params),
  skin_match_verts(File,Model,Skin1,SkinRef1,Skin2,SkinRef2,Xc,Yc,Zc),
  skin_match_tverts(File,Model,Skin1,SkinRef1,Xc,Yc,Zc,waist),
  skin_match_tverts(File,Model,Skin2,SkinRef2,Xc,Yc,Zc,waist),
  (cm3_verbose -> tab(2), write('match_end_verts succeeded on '), write(Skin1/Skin2), nl ; true),
  !.

match_end_verts(File,Model,Skin1,SkinRef1,Skin2,SkinRef2,Zc,JointType) :-
  bagof(R1,end_vertex(File,Model,Skin1,SkinRef1,Zc,R1),Ends1),
  bagof(R2,end_vertex(File,Model,Skin2,SkinRef2,Zc,R2),Ends2),
  centroid_both(Ends1,Ends2,Xc,Yc),
  check_convex(File,Model,Skin1,SkinRef1,Xc,Yc,Zc,Ends1,up),
  check_convex(File,Model,Skin2,SkinRef2,Xc,Yc,Zc,Ends2,down),
  best_fit_ellipse(Ends1,Ends2,Xc,Yc,Params),
  (cm3_verbose -> tab(2), write('best fit ellipse '), write(Params), nl ; true),
  adjust_to_ellipse(File,Model,Skin1,SkinRef1,Xc,Yc,Zc,Params),
  adjust_to_ellipse(File,Model,Skin2,SkinRef2,Xc,Yc,Zc,Params),
  skin_match_verts(File,Model,Skin1,SkinRef1,Skin2,SkinRef2,Xc,Yc,Zc),
  skin_match_tverts(File,Model,Skin1,SkinRef1,Xc,Yc,Zc,JointType),
  skin_match_tverts(File,Model,Skin2,SkinRef2,Xc,Yc,Zc,JointType),
  (cm3_verbose -> tab(2), write('match_end_verts succeeded on '), write(Skin1/Skin2), nl ; true),
  !.

match_end_verts(_,_,Skin1,_,Skin2,_,_,_) :-
  (cm3_verbose -> tab(2), write('*** match_end_verts failed on '), write(Skin1/Skin2), nl ; true),
  !, fail.

end_vertex(File,Model,Node,NodeRef,Z0,[V,X,Y]) :-
  gotdata(File,Model,Node,NodeRef,verts(V,X,Y,Z)),
  /* once(get_vertex_material(File,Model,Node,NodeRef,V,M)), M==1, */
  snap_equal(Z0,Z).

centroid_both(Ends1,Ends2,Xc,Yc) :-
  length(Ends1,L1), length(Ends2,L2),
  total_xy(Ends1,Tx1,Ty1),
  total_xy(Ends2,Tx2,Ty2),
  Xc is (Tx1+Tx2)/(L1+L2), Yc is (Ty1+Ty2)/(L1+L2).

total_xy([],0,0) :- !.
total_xy([[_,X,Y]|T],Tx,Ty) :- total_xy(T,Tx1,Ty1), !, Tx is Tx1+X, Ty is Ty1+Y.

mean_radius_both(Ends1,Ends2,Xc,Yc,R0) :-
  length(Ends1,L1), length(Ends2,L2),
  total_radius(Ends1,Xc,Yc,Tot1),
  total_radius(Ends2,Xc,Yc,Tot2),
  R0 is (Tot1+Tot2)/(L1+L2).

total_radius([],_,_,0) :- !.
total_radius([[_,X,Y]|T],Xc,Yc,Rtot) :- total_radius(T,Xc,Yc,R1), !, Rtot is R1+sqrt((X-Xc)*(X-Xc)+(Y-Yc)*(Y-Yc)).

check_convex(_,_,_,_,_,_,_,[],_) :- !.

check_convex(File,Model,Skin,SkinRef,Xc,Yc,Zc,[[V1,X1,Y1]|L],UpOrDown) :-
  clockwise_edge(File,Model,Skin,SkinRef,V1,V2),
  end_vertex(File,Model,Skin,SkinRef,Zc,[V2,X2,Y2]),
  T is (X2-X1)*(Yc-Y1)-(Y2-Y1)*(Xc-X1),
  (UpOrDown=='up' -> T1=T ; T1 is -T),
  (T1=<0 ->
     (cm3_verbose -> tab(2), write(transposing(V1,V2)), write(' in '), write(Skin), nl ; true),
     retract(gotdata(File,Model,Skin,SkinRef,verts(V1,X1,Y1,_))),
     retract(gotdata(File,Model,Skin,SkinRef,verts(V2,X2,Y2,_))),
     assertz(gotdata(File,Model,Skin,SkinRef,verts(V1,X2,Y2,Zc))),
     assertz(gotdata(File,Model,Skin,SkinRef,verts(V2,X1,Y1,Zc))),
     replace_in_list(L,[V2,X2,Y2],[V2,X1,Y1],L1)
     ;
     L1=L
  ),
  !,
  check_convex(File,Model,Skin,SkinRef,Xc,Yc,Zc,L1,UpOrDown).

check_convex(File,Model,Skin,SkinRef,Xc,Yc,Zc,[[V1,X1,Y1]|L],UpOrDown) :-
  tab(2), write('*** CM3 error: '), write(check_convex(File,Model,Skin,SkinRef,Xc,Yc,Zc,[[V1,X1,Y1]|L],UpOrDown)), nl.

adjust_to_circle(File,Model,NodeName,NodeRef,Xc,Yc,Zc,Rc) :-
  end_vertex(File,Model,NodeName,NodeRef,Zc,[V,X,Y]),
  Factor is Rc/sqrt((X-Xc)*(X-Xc)+(Y-Yc)*(Y-Yc)),
  X1 is Xc+Factor*(X-Xc), Y1 is Yc+Factor*(Y-Yc),
  retract(gotdata(File,Model,NodeName,NodeRef,verts(V,_,_,_))),
  assertz(gotdata(File,Model,NodeName,NodeRef,verts(V,X1,Y1,Zc))),
  fail ; true.

replace_in_list([],_,_,[]) :- !.
replace_in_list([T1|L],T1,T2,[T2|L]) :- !.
replace_in_list([T|L],T1,T2,[T|L1]) :- replace_in_list(L,T1,T2,L1).

/* =================== */
/* adjust_to_ellipse/8 */
/* adjust_to_trilobe/8 */
/* best_fit_ellipse/5  */
/* best_fit_trilobe/5  */
/* do_regression/2     */
/* ellipse_totals/4    */
/* trilobe_totals/4    */
/* list_add/3          */
/* f_waist/3           */
/* f_waist_centre/2    */
/* =================== */

adjust_to_ellipse(File,Model,Skin,SkinRef,Xc,Yc,Zc,[A,B,C]) :-
  end_vertex(File,Model,Skin,SkinRef,Zc,[V,X,Y]),
  Theta is atan((Y-Yc),(X-Xc)),
  ThetaS is round(Theta*8/pi)*pi/8,
  Cos is cos(ThetaS), Sin is sin(ThetaS),
  Factor is A + B*(Cos*Cos-Sin*Sin) + C*2*Sin*Cos,
  X1 is Xc+Factor*Cos, Y1 is Yc+Factor*Sin,
  retract(gotdata(File,Model,Skin,SkinRef,verts(V,_,_,_))),
  assertz(gotdata(File,Model,Skin,SkinRef,verts(V,X1,Y1,Zc))),
  fail.

adjust_to_ellipse(File,Model,Skin,SkinRef,_,_,_,_) :- skin_repair_mesh(File,Model,Skin,SkinRef).

/*
adjust_to_trilobe(File,Model,Skin,SkinRef,_,_,Zc,_) :-
  atom_concat('pfh0_robe',_,Model),
  tab(2), write('standard pfh0_robe waist profile used for '), write(Skin), nl,
  f_waist_centre(Xc,Yc),
  end_vertex(File,Model,Skin,SkinRef,Zc,[V,X,Y]),
  Theta is atan((Y-Yc),(X-Xc)),
  NTheta is round(Theta*16/pi),
  f_waist(NTheta,X1,Y1),
  retract(gotdata(File,Model,Skin,SkinRef,verts(V,_,_,_))),
  assertz(gotdata(File,Model,Skin,SkinRef,verts(V,X1,Y1,Zc))),
  fail.
*/

adjust_to_trilobe(File,Model,Skin,SkinRef,Xc,Yc,Zc,[A,B,C]) :-
  /* \+atom_concat('pfh0_robe',_,Model), */
  end_vertex(File,Model,Skin,SkinRef,Zc,[V,X,Y]),
  Theta is atan((Y-Yc),(X-Xc)),
  ThetaS is round(Theta*16/pi)*pi/16,
  Cos is cos(ThetaS), Sin is sin(ThetaS),
  Factor is A + B*(Cos*Cos-Sin*Sin) + C*Sin*(4*Cos*Cos-1),
  X1 is Xc+Factor*Cos, Y1 is Yc+Factor*Sin,
  retract(gotdata(File,Model,Skin,SkinRef,verts(V,_,_,_))),
  assertz(gotdata(File,Model,Skin,SkinRef,verts(V,X1,Y1,Zc))),
  fail.

adjust_to_trilobe(File,Model,Skin,SkinRef,_,_,_,_) :- skin_repair_mesh(File,Model,Skin,SkinRef).

best_fit_ellipse(Ends1,Ends2,Xc,Yc,Params) :-
  ellipse_totals(Ends1,Xc,Yc,Totals1),
  ellipse_totals(Ends2,Xc,Yc,Totals2),
  list_add(Totals1,Totals2,Totals),
  do_regression(Totals,Params).

best_fit_trilobe(Ends1,Ends2,Xc,Yc,Params) :-
  trilobe_totals(Ends1,Xc,Yc,Totals1),
  trilobe_totals(Ends2,Xc,Yc,Totals2),
  list_add(Totals1,Totals2,Totals),
  do_regression(Totals,Params).

do_regression([N,R,S,C,RS,RC,SS,SC,CC],[A0,B0,C0]) :-
  T11 is C*C-N*CC, T12 is S*C-N*SC, T13 is R*C-N*RC,
  T21 is S*C-N*SC, T22 is S*S-N*SS, T23 is R*S-N*RS,
  B0 is (T13*T22-T23*T12)/(T11*T22-T21*T12),
  C0 is (T11*T23-T21*T13)/(T11*T22-T21*T12),
  A0 is (R-B0*C-C0*S)/N.

ellipse_totals([],_,_,[0,0,0,0,0,0,0,0,0]) :- !.

ellipse_totals([[_,X,Y]|Ends],Xc,Yc,Totals) :-
  ellipse_totals(Ends,Xc,Yc,Part),
  R is sqrt((X-Xc)*(X-Xc)+(Y-Yc)*(Y-Yc)),
  Cos is (X-Xc)/R, Sin is (Y-Yc)/R,
  C is Cos*Cos-Sin*Sin, S is 2*Sin*Cos,
  RS is R*S, RC is R*C, SS is S*S, SC is S*C, CC is C*C,
  This = [1,R,S,C,RS,RC,SS,SC,CC],
  list_add(This,Part,Totals).

trilobe_totals([],_,_,[0,0,0,0,0,0,0,0,0]) :- !.

trilobe_totals([[_,X,Y]|Ends],Xc,Yc,Totals) :-
  trilobe_totals(Ends,Xc,Yc,Part),
  R is sqrt((X-Xc)*(X-Xc)+(Y-Yc)*(Y-Yc)),
  Cos is (X-Xc)/R, Sin is (Y-Yc)/R,
  C is Cos*Cos-Sin*Sin, S is Sin*(4*Cos*Cos-1),
  RS is R*S, RC is R*C, SS is S*S, SC is S*C, CC is C*C,
  This = [1,R,S,C,RS,RC,SS,SC,CC],
  list_add(This,Part,Totals).

list_add([],[],[]).

list_add([H1|T1],[H2|T2],[H|T]) :- H is H1+H2, list_add(T1,T2,T).

/* f_waist_centre(0.071,-0.067). */

f_waist_centre(X,Y) :- gotdata(_,_,rootdummy,_,position(X,Y,_)), !.
f_waist_centre(0,0).

f_waist(0,0.210,-0.070) :- !.
f_waist(1,0.206,-0.041) :- !.
f_waist(2,0.194,-0.019) :- !.
f_waist(3,0.183,-0.012) :- !.
f_waist(4,0.169,-0.005) :- !.
f_waist(5,0.152,0.006) :- !.
f_waist(6,0.1265,0.009) :- !.
f_waist(7,0.099,0.011) :- !.
f_waist(8,0.071,0.012) :- !.
f_waist(9,0.043,0.011) :- !.
f_waist(10,0.0155,0.009) :- !.
f_waist(11,-0.010,0.006) :- !.
f_waist(12,-0.027,-0.005) :- !.
f_waist(13,-0.041,-0.012) :- !.
f_waist(14,-0.052,-0.019) :- !.
f_waist(15,-0.064,-0.041) :- !.
f_waist(16,-0.068,-0.070) :- !.
f_waist(-1,0.206,-0.088) :- !.
f_waist(-2,0.194,-0.107) :- !.
f_waist(-3,0.183,-0.119) :- !.
f_waist(-4,0.169,-0.133) :- !.
f_waist(-5,0.152,-0.140) :- !.
f_waist(-6,0.1265,-0.148) :- !.
f_waist(-7,0.099,-0.150) :- !.
f_waist(-8,0.071,-0.150) :- !.
f_waist(-9,0.043,-0.150) :- !.
f_waist(-10,0.0155,-0.148) :- !.
f_waist(-11,-0.010,-0.140) :- !.
f_waist(-12,-0.027,-0.133) :- !.
f_waist(-13,-0.041,-0.119) :- !.
f_waist(-14,-0.052,-0.107) :- !.
f_waist(-15,-0.063,-0.088) :- !.
f_waist(-16,-0.068,-0.070) :- !.

/* ================== */
/* skin_repair_mesh/4 */
/* ================== */

skin_repair_mesh(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,verts(NVerts)),
  hash_verts(File,Model,NodeName,NodeRef),
  has_unwelded_verts(File,Model,NodeName,NodeRef),
  weld_vertices(File,Model,NodeName,NodeRef,NVerts,0,0),
  (cm3_verbose -> tab(2), write('welded verts in '), write(NodeName), nl ; true),
  fail.

skin_repair_mesh(File,Model,NodeName,NodeRef) :-
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)),
  once((gotdata(File,Model,NodeName,NodeRef,faces(_,V1,V2,V3,_,_,_,_,_)),(V1=V2; V2=V3; V3=V1))),
  delete_null_faces(File,Model,NodeName,NodeRef,NFaces,0,0),
  (cm3_verbose -> tab(2), write('null face(s) deleted from '), write(NodeName), nl ; true),
  fail.

skin_repair_mesh(File,Model,NodeName,NodeRef) :-
  setof(F,is_degenerate_face(File,Model,NodeName,NodeRef,F),Flist),
  length(Flist,L0), L0>0,
  fix_degenerate_faces(File,Model,NodeName,NodeRef,Flist),
  gotdata(File,Model,NodeName,NodeRef,faces(NFaces)),
  delete_null_faces(File,Model,NodeName,NodeRef,NFaces,0,0),
  (cm3_verbose -> tab(2), write('repaired '), write(L0), write(' degenerate face(s) in '), write(NodeName), nl ; true),
  fail.

skin_repair_mesh(File,Model,NodeName,NodeRef) :-
  setof(T,is_tear_in_mesh(File,Model,NodeName,NodeRef,T),Tlist),
  length(Tlist,L0), L0>0,
  fix_tears_in_mesh(File,Model,NodeName,NodeRef,Tlist),
  (cm3_verbose -> tab(2), write('stitched '), write(L0), write(' tear(s) in '), write(NodeName), nl ; true),
  fail.

skin_repair_mesh(File,Model,NodeName,NodeRef) :-
  setof(Pair,is_overlapping_face_pair(File,Model,NodeName,NodeRef,Pair),PairList),
  length(PairList,L),
  tab(2), write('warning - '), write(L), write(' overlapping faces in '), write(NodeName), nl,
  fail.

skin_repair_mesh(_,_,_,_).

/* =================== */
/* skin_match_verts/9  */
/* skin_match_verts/7  */
/* polar_end_vert/8    */
/* ordered_end_edge/   */
/* skin_new_vert/9     */
/* skin_new_tvert/8    */
/* skin_divide_face/15 */
/* =================== */

skin_match_verts(File,Model,Skin1,SkinRef1,Skin2,SkinRef2,Xc,Yc,Zc) :-
  setof(V1,polar_end_vert(File,Model,Skin1,SkinRef1,Xc,Yc,Zc,V1),Verts1),
  setof(V2,polar_end_vert(File,Model,Skin2,SkinRef2,Xc,Yc,Zc,V2),Verts2),
  bagof(E1,ordered_end_edge(File,Model,Skin1,SkinRef1,Verts1,E1),Edges1),
  bagof(E2,ordered_end_edge(File,Model,Skin2,SkinRef2,Verts2,E2),Edges2),
  skin_match_verts(File,Model,Skin1,SkinRef1,Zc,Edges1,Verts2),
  skin_match_verts(File,Model,Skin2,SkinRef2,Zc,Edges2,Verts1).

skin_match_verts(_,_,_,_,_,[],_) :- !.

skin_match_verts(File,Model,Skin,SkinRef,Zc,[[T1,V1,T2,V2,V3,F,Sense,TV1,TV2,TV3]|Edges],Verts) :-
  abs(T2-T1)<pi, /* Normal case, not the closing edge that crosses +-pi */
  member([Ti,_,_,Xi,Yi],Verts),
  Ti>min(T1,T2), Ti<max(T1,T2),
  Lambda is (Ti-T1)/(T2-T1),
  gotdata(File,Model,Skin,SkinRef,weights(V1,W)),
  (gotdata(File,Model,Skin,SkinRef,colors(_)) ->
     gotdata(File,Model,Skin,SkinRef,colors(V1,R1,G1,B1)),
     gotdata(File,Model,Skin,SkinRef,colors(V2,R2,G2,B2)),
     Ri is round(R1+Lambda*(R2-R1)),
     Gi is round(G1+Lambda*(G2-G1)),
     Bi is round(B1+Lambda*(B2-B1)),
     Ci = [Ri,Gi,Bi] ; Ci=[]),
  skin_new_vert(File,Model,Skin,SkinRef,Xi,Yi,Zc,Vi,W,Ci),
  skin_new_tvert(File,Model,Skin,SkinRef,TV1,TV2,Lambda,TVi),
  skin_divide_face(File,Model,Skin,SkinRef,F,V1,V2,V3,Vi,TV1,TV2,TV3,TVi,Sense,Fnew),
  Left  =[T1,V1,Ti,Vi,V3,F,Sense,TV1,TVi,TV3],
  Right =[Ti,Vi,T2,V2,V3,Fnew,Sense,TVi,TV2,TV3],
  !,
  skin_match_verts(File,Model,Skin,SkinRef,Zc,[Left,Right|Edges],Verts).

skin_match_verts(File,Model,Skin,SkinRef,Zc,[[T1,V1,T2,V2,V3,F,Sense,TV1,TV2,TV3]|Edges],Verts) :-
  abs(T2-T1)>pi, /* Exceptional case, the closing edge that crosses +-pi */
  (T1<0 -> T1p is T1+2*pi ; T1p = T1),
  (T2<0 -> T2p is T2+2*pi ; T2p = T2),
  member([Ti,_,_,Xi,Yi],Verts),
  (Ti<0 -> Tip is Ti+2*pi ; Tip = Ti),
  Tip > min(T1p,T2p), Tip < max(T1p,T2p),
  Lambda is (Tip-T1p)/(T2p-T1p),
  gotdata(File,Model,Skin,SkinRef,weights(V1,W)),
  (gotdata(File,Model,Skin,SkinRef,colors(_)) ->
     gotdata(File,Model,Skin,SkinRef,colors(V1,R1,G1,B1)),
     gotdata(File,Model,Skin,SkinRef,colors(V2,R2,G2,B2)),
     Ri is round(R1+Lambda*(R2-R1)),
     Gi is round(G1+Lambda*(G2-G1)),
     Bi is round(B1+Lambda*(B2-B1)),
     Ci = [Ri,Gi,Bi] ; Ci=[]),
  skin_new_vert(File,Model,Skin,SkinRef,Xi,Yi,Zc,Vi,W,Ci),
  skin_new_tvert(File,Model,Skin,SkinRef,TV1,TV2,Lambda,TVi),
  skin_divide_face(File,Model,Skin,SkinRef,F,V1,V2,V3,Vi,TV1,TV2,TV3,TVi,Sense,Fnew),
  Left  =[T1,V1,Ti,Vi,V3,F,Sense,TV1,TVi,TV3],
  Right =[Ti,Vi,T2,V2,V3,Fnew,Sense,TVi,TV2,TV3],
  !,
  skin_match_verts(File,Model,Skin,SkinRef,Zc,[Left,Right|Edges],Verts).

skin_match_verts(File,Model,Skin,SkinRef,Zc,[_|Edges],Verts) :-
  skin_match_verts(File,Model,Skin,SkinRef,Zc,Edges,Verts).

polar_end_vert(File,Model,Skin,SkinRef,Xc,Yc,Zc,[T,R,V,X,Y]) :-
  gotdata(File,Model,Skin,SkinRef,verts(V,X,Y,Z)),
  snap_equal(Z,Zc),
  R is sqrt((X-Xc)*(X-Xc)+(Y-Yc)*(Y-Yc)),
  T is atan((Y-Yc),(X-Xc)).

ordered_end_edge(File,Model,Skin,SkinRef,Verts,[T1,V1,T2,V2,V3,F,Sense,TV1,TV2,TV3]) :-
  member([T1,_,V1,_,_],Verts), member([T2,_,V2,_,_],Verts), T2>T1,
  (gotdata(File,Model,Skin,SkinRef,faces(F,V1,V2,V3,_,TV1,TV2,TV3,_)) -> Sense = 1-2;
   gotdata(File,Model,Skin,SkinRef,faces(F,V3,V1,V2,_,TV3,TV1,TV2,_)) -> Sense = 2-3;
   gotdata(File,Model,Skin,SkinRef,faces(F,V2,V3,V1,_,TV2,TV3,TV1,_)) -> Sense = 3-1;
   gotdata(File,Model,Skin,SkinRef,faces(F,V2,V1,V3,_,TV2,TV1,TV3,_)) -> Sense = 2-1;
   gotdata(File,Model,Skin,SkinRef,faces(F,V3,V2,V1,_,TV3,TV2,TV1,_)) -> Sense = 3-2;
   gotdata(File,Model,Skin,SkinRef,faces(F,V1,V3,V2,_,TV1,TV3,TV2,_)) -> Sense = 1-3;
   fail).

skin_new_vert(File,Model,Skin,SkinRef,Xi,Yi,Zc,Vi,W,Ci) :-
  clause(gotdata(File,Model,Skin,SkinRef,verts(N)),true,Eref),
  N1 is N+1, Vi=N,
  erase(Eref),
  assertz(gotdata(File,Model,Skin,SkinRef,verts(N1))),
  assertz(gotdata(File,Model,Skin,SkinRef,verts(Vi,Xi,Yi,Zc))),
  clause(gotdata(File,Model,Skin,SkinRef,weights(N)),true,Eref1),
  erase(Eref1),
  assertz(gotdata(File,Model,Skin,SkinRef,weights(N1))),
  assertz(gotdata(File,Model,Skin,SkinRef,weights(Vi,W))),
  (Ci=[Ri,Gi,Bi] ->
    clause(gotdata(File,Model,Skin,SkinRef,colors(N)),true,Eref2),
    erase(Eref2),
    assertz(gotdata(File,Model,Skin,SkinRef,colors(N1))),
    assertz(gotdata(File,Model,Skin,SkinRef,colors(Vi,Ri,Gi,Bi)))
    ;
    true),
  !.

skin_new_tvert(File,Model,Skin,SkinRef,TV1,TV2,Lambda,TVi) :-
  clause(gotdata(File,Model,Skin,SkinRef,tverts(NTV)),true,Eref2),
  gotdata(File,Model,Skin,SkinRef,tverts(TV1,Tu1,Tv1,Tw1)),
  gotdata(File,Model,Skin,SkinRef,tverts(TV2,Tu2,Tv2,Tw2)),
  Tui is Tu1+Lambda*(Tu2-Tu1), Tvi is Tv1+Lambda*(Tv2-Tv1), Twi is Tw1+Lambda*(Tw2-Tw1),
  TVi = NTV, NTV1 is NTV+1,
  erase(Eref2),
  assertz(gotdata(File,Model,Skin,SkinRef,tverts(TVi,Tui,Tvi,Twi))),
  assertz(gotdata(File,Model,Skin,SkinRef,tverts(NTV1))),
  !.

skin_divide_face(File,Model,Skin,SkinRef,F,V1,V2,V3,Vi,TV1,TV2,TV3,TVi,Sense,Fnew) :-
  /* Convert face F in Skin to go V1,Vi,V3 with the correct sense    */
  /* Add a new face Fnew in Skin to go Vi,V2,V3 in the correct sense */
  clause(gotdata(File,Model,Skin,SkinRef,faces(NF)),true,Eref),
  Fnew = NF, NF1 is NF+1,
  erase(Eref),
  assertz(gotdata(File,Model,Skin,SkinRef,faces(NF1))),
  clause(gotdata(File,Model,Skin,SkinRef,faces(F,_,_,_,G,_,_,_,M)),true,Eref1),
  erase(Eref1),
  (
    Sense== 1-2 -> Left = faces(F,V1,Vi,V3,G,TV1,TVi,TV3,M), Right = faces(Fnew,Vi,V2,V3,G,TVi,TV2,TV3,M) ;
    Sense== 2-3 -> Left = faces(F,V3,V1,Vi,G,TV3,TV1,TVi,M), Right = faces(Fnew,V3,Vi,V2,G,TV3,TVi,TV2,M) ;
    Sense== 3-1 -> Left = faces(F,Vi,V3,V1,G,TVi,TV3,TV1,M), Right = faces(Fnew,V2,V3,Vi,G,TV2,TV3,TVi,M) ;
    Sense== 2-1 -> Left = faces(F,Vi,V1,V3,G,TVi,TV1,TV3,M), Right = faces(Fnew,V2,Vi,V3,G,TV2,TVi,TV3,M) ;
    Sense== 3-2 -> Left = faces(F,V3,Vi,V1,G,TV3,TVi,TV1,M), Right = faces(Fnew,V3,V2,Vi,G,TV3,TV2,TVi,M) ;
    Sense== 1-3 -> Left = faces(F,V1,V3,Vi,G,TV1,TV3,TVi,M), Right = faces(Fnew,Vi,V3,V2,G,TVi,TV3,TV2,M)
  ),
  assertz(gotdata(File,Model,Skin,SkinRef,Left)),
  assertz(gotdata(File,Model,Skin,SkinRef,Right)),
  !.

/* ==================== */
/* skin_match_tverts/8  */
/* snap_tverts/7        */
/* ==================== */

skin_match_tverts(_,_,_,_,_,_,_,_) :-
  once(secret(skin_snap_tverts(G))),
  G==no,
  !.

skin_match_tverts(File,Model,Skin,SkinRef,_,_,Zc,JointType) :-
  setof(T,end_vert_tvert(File,Model,Skin,SkinRef,Zc,T),Tset),
  (JointType==waist -> N=32 ; JointType=neck -> N=53 ; N=64),
  (JointType=neck -> V=0.8;
   JointType=waist -> once((secret(skin_tvert_snap(waist,TVW)) -> V=TVW ; V=0.4));
   member(JointType,[left,right,knee]) -> V=0.6;
   member(JointType,[wrist,ankle]) -> V=0.2;
   fail),
  snap_tverts(File,Model,Skin,SkinRef,Tset,N,V),
  fail.

skin_match_tverts(_,_,_,_,_,_,_,_).

end_vert_tvert(File,Model,Skin,SkinRef,Zc,T) :-
  gotdata(File,Model,Skin,SkinRef,verts(V,_,_,Z)), snap_equal(Z,Zc),
  (gotdata(File,Model,Skin,SkinRef,faces(_,V,_,_,_,T,_,_,_)) ;
   gotdata(File,Model,Skin,SkinRef,faces(_,_,V,_,_,_,T,_,_)) ;
   gotdata(File,Model,Skin,SkinRef,faces(_,_,_,V,_,_,_,T,_))).

snap_tverts(_,_,_,_,[],_,_) :- !.

snap_tverts(File,Model,Skin,SkinRef,[T|T0],Nu,V1) :-
  clause(gotdata(File,Model,Skin,SkinRef,tverts(T,U,_,_)),true,Eref),
  U1 is round(U*Nu)/Nu,
  erase(Eref),
  assertz(gotdata(File,Model,Skin,SkinRef,tverts(T,U1,V1,0))),
  !,
  snap_tverts(File,Model,Skin,SkinRef,T0,Nu,V1).

/* ================= */
/* skin_join_skins/6 */
/* ================= */

skin_join_skins(File,Model,Skin1,Skin2,Skin3,NewSkin) :-
  (clause(gotdata(File,Model,node(skin,Skin1)),true,SkinRef1),
   gotdata(File,Model,Skin1,SkinRef1,parent(Model/ModelRef)) -> N1=1 ; N1=0),
  (clause(gotdata(File,Model,node(skin,Skin2)),true,SkinRef2),
   gotdata(File,Model,Skin2,SkinRef2,parent(Model/ModelRef)) -> N2=1 ; N2=0),
  (clause(gotdata(File,Model,node(skin,Skin3)),true,SkinRef3),
   gotdata(File,Model,Skin3,SkinRef3,parent(Model/ModelRef)) -> N3=1 ; N3=0),
  N1+N2+N3>=2,
  \+gotdata(File,Model,node(_,NewSkin)),
  !,
  assertz(gotdata(File,Model,node(skin,NewSkin)),NewSkinRef),
  assertz(gotdata(File,Model,NewSkin,NewSkinRef,parent(Model/ModelRef))),
  assertz(gotdata(File,Model,NewSkin,NewSkinRef,position(0,0,0))),
  assertz(gotdata(File,Model,NewSkin,NewSkinRef,orientation(0,0,0,0))),
  (paramtype(trimesh,P,Q0), functor(P,P,0), \+member(Q0,[parent,position,orientation,center]),
   (P=mn3 -> A=3 ; A=1), functor(Q1,Q0,A), functor(Q2,Q0,A), functor(Q3,Q0,A),
   once((
   (N1=1, gotdata(File,Model,Skin1,SkinRef1,Q1) -> T=1 ; true),
   (N2=1, gotdata(File,Model,Skin2,SkinRef2,Q2) -> T=1 ; true),
   (N3=1, gotdata(File,Model,Skin3,SkinRef3,Q3) -> T=1 ; true),
   T==1, Q1=Q2, Q2=Q3, Q3=Q1,
   assertz(gotdata(File,Model,NewSkin,NewSkinRef,Q1))
       )),
   fail ; true),
  (gotdata(File,Model,NewSkin,NewSkinRef,bitmap(_)) -> true ; assertz(gotdata(File,Model,NewSkin,NewSkinRef,bitmap(Model)))),
  (N1=1 -> append_mesh(File,Model,NewSkin,NewSkinRef,Skin1,SkinRef1) ; true),
  (N2=1 -> append_mesh(File,Model,NewSkin,NewSkinRef,Skin2,SkinRef2) ; true),
  (N3=1 -> append_mesh(File,Model,NewSkin,NewSkinRef,Skin3,SkinRef3) ; true),
  gotdata(File,Model,NewSkin,NewSkinRef,verts(NVerts)),
  hash_verts(File,Model,NewSkin,NewSkinRef),
  (has_unwelded_verts(File,Model,NewSkin,NewSkinRef) -> weld_vertices(File,Model,NewSkin,NewSkinRef,NVerts,0,0) ; true),
  tab(2),
  (N1=1 -> write(Skin1), tab(1) ; true),
  (N2=1 -> write(Skin2), tab(1) ; true),
  (N3=1 -> write(Skin3), tab(1) ; true),
  write('merged into '), write(NewSkin),
  (cm3_verbose ->
    gotdata(File,Model,NewSkin,NewSkinRef,verts(NewNVerts)),
    tab(1), write(verts(NVerts>NewNVerts)) ; true),
  nl, !.

skin_join_skins(_,_,_,_,_,_).

/* ========================= */
/* axisangle2quaternion/2    */
/* quaternion2axisangle/2    */
/* interpolate_quaternions/4 */
/* interpolate_axisangle/4   */
/* multiply_quaternions/3    */
/* multiply_axisangle/3      */
/* ========================= */

axisangle2quaternion([0,0,0,0],[0,0,0,1]) :- !.

axisangle2quaternion([_,_,_,A],[0,0,0,1]) :- A=:=0, !.

axisangle2quaternion([X,Y,Z,A],[Qx,Qy,Qz,Qw]) :-
  R is sqrt(X*X+Y*Y+Z*Z),
  S is sin(A/2)/R,
  Qx is X*S, Qy is Y*S, Qz is Z*S,
  Qw is cos(A/2).

quaternion2axisangle([Qx,Qy,Qz,Qw],[X,Y,Z,A]) :-
  R1 is sqrt(Qx*Qx+Qy*Qy+Qz*Qz),
  (R1=:=0 -> [X,Y,Z,A]=[0,0,0,0] ;
    R2 is sqrt(Qx*Qx+Qy*Qy+Qz*Qz+Qw*Qw),
    (R2=:=0 -> [X,Y,Z,A]=[0,0,0,0] ;
      A is 2*acos(Qw/R2),
      X is Qx/R1, Y is Qy/R1, Z is Qz/R1
    )
  ).

interpolate_quaternions([Qx1,Qy1,Qz1,Qw1],[Qx2,Qy2,Qz2,Qw2],Lambda,[Qx,Qy,Qz,Qw]) :-
  Dot is Qx1*Qx2+Qy1*Qy2+Qz1*Qz2+Qw1*Qw2,
  (Dot >= 1 -> [Qx,Qy,Qz,Qw]=[Qx1,Qy1,Qz1,Qw1]
   ;
   Dot =< -1 -> fail
   ;
   Omega is acos(Dot),
   K1 is sin((1-Lambda)*Omega)/sin(Omega),
   K2 is sin(Lambda*Omega)/sin(Omega),
   Qx is K1*Qx1+K2*Qx2, Qy is K1*Qy1+K2*Qy2, Qz is K1*Qz1+K2*Qz2, Qw is K1*Qw1+K2*Qw2
  ),
  !.

interpolate_axisangle(R1,R2,Lambda,R) :-
  axisangle2quaternion(R1,Q1),
  axisangle2quaternion(R2,Q2),
  interpolate_quaternions(Q1,Q2,Lambda,Q),
  quaternion2axisangle(Q,R).

multiply_quaternions([X1,Y1,Z1,W1],[X2,Y2,Z2,W2],[X,Y,Z,W]) :-
  X is W1*X2+W2*X1+Y1*Z2-Z1*Y2,
  Y is W1*Y2+W2*Y1+Z1*X2-X1*Z2,
  Z is W1*Z2+W2*Z1+X1*Y2-Y1*X2,
  W is W1*W2-X1*X2-Y1*Y2-Z1*Z2.

multiply_axisangle(R1,R2,R) :-
  /* Apply R2 to the result of applying R1 */
  axisangle2quaternion(R1,Q1),
  axisangle2quaternion(R2,Q2),
  multiply_quaternions(Q1,Q2,Q),
  quaternion2axisangle(Q,R).

/* ========================= */
/* get_vertex_material/6     */
/* get_one_vertex_material/6 */
/* ========================= */

get_vertex_material(File,Model,NodeName,NodeRef,Vert,Material) :-
  bagof(M,get_one_vertex_material(File,Model,NodeName,NodeRef,Vert,M),Materials),
  my_sumlist(Materials,Total), length(Materials,L),
  Material is Total/L.

get_one_vertex_material(File,Model,NodeName,NodeRef,V,M) :- gotdata(File,Model,NodeName,NodeRef,faces(_,V,_,_,_,_,_,_,M)).
get_one_vertex_material(File,Model,NodeName,NodeRef,V,M) :- gotdata(File,Model,NodeName,NodeRef,faces(_,_,V,_,_,_,_,_,M)).
get_one_vertex_material(File,Model,NodeName,NodeRef,V,M) :- gotdata(File,Model,NodeName,NodeRef,faces(_,_,_,V,_,_,_,_,M)).

max_of_set([X],X) :- !.
max_of_set([X|L],Xm) :- max_of_set(L,X0), !, Xm is max(X,X0).
max_of_set([],-32767).

/* ======================================== */
/* set_pendulum_keys/3                      */
/* set_pendulum_keys/11                     */
/* interpolate_orientation_key_quaternion/7 */
/* ======================================== */

set_pendulum_keys(File,Model,[NodeName|_]) :-
  gotdata(File,Model,NodeName,_,_,_),
  !.

set_pendulum_keys(File,Model,NodeList) :-
  NodeList = [NodeName0,NodeName1,NodeName2,NodeName3,NodeName4],
  clause(gotdata(File,Model,node(dummy,NodeName0)),true,NodeRef0),
  \+gotdata(File,Model,NodeName0,NodeRef0,_,_),
  clause(gotdata(File,Model,node(dummy,NodeName1)),true,NodeRef1),
  clause(gotdata(File,Model,node(dummy,NodeName2)),true,NodeRef2),
  clause(gotdata(File,Model,node(dummy,NodeName3)),true,NodeRef3),
  clause(gotdata(File,Model,node(dummy,NodeName4)),true,NodeRef4),
  NodeRefList = [NodeRef0,NodeRef1,NodeRef2,NodeRef3,NodeRef4],
  gotdata(File,Model,NodeName0,NodeRef0,parent(ShakeNode/ShakeRef)),
  gotdata(File,Model,ShakeNode,ShakeRef,parent(Torso/TorsoRef)),
  gotdata(File,Model,Torso,TorsoRef,parent(RootDummy/RootDummyRef)),
  once((
    gotdata(File,Model,newanim(AnimName1,Model)),
    gotdata(File,Model,Torso,TorsoRef,AnimName1,orientationkey(_)),
    gotdata(File,Model,RootDummy,RootDummyRef,AnimName1,orientationkey(_)),
    tab(2), write('gravity keys set on '), write(NodeList), nl
      )),
  gotdata(File,Model,newanim(AnimName,Model)),
  gotdata(File,Model,Torso,TorsoRef,AnimName,orientationkey(_)),
  gotdata(File,Model,RootDummy,RootDummyRef,AnimName,orientationkey(_)),
  set_pendulum_keys(File,Model,NodeList,NodeRefList,AnimName,Torso,TorsoRef,RootDummy,RootDummyRef,ShakeNode,ShakeRef),
  fail.

set_pendulum_keys(_,_,_).

set_pendulum_keys(File,Model,NodeList,NodeRefList,AnimName,Torso,TorsoRef,_,_,_,_) :-
  gotdata(File,Model,Torso,TorsoRef,AnimName,orientationkey(K)),
  NodeList = [NodeName0,NodeName1,NodeName2,NodeName3,NodeName4],
  NodeRefList = [NodeRef0,NodeRef1,NodeRef2,NodeRef3,NodeRef4],
  assertz(gotdata(File,Model,NodeName0,NodeRef0,AnimName,orientationkey(K))),
  assertz(gotdata(File,Model,NodeName1,NodeRef1,AnimName,orientationkey(K))),
  assertz(gotdata(File,Model,NodeName2,NodeRef2,AnimName,orientationkey(K))),
  assertz(gotdata(File,Model,NodeName3,NodeRef3,AnimName,orientationkey(K))),
  assertz(gotdata(File,Model,NodeName4,NodeRef4,AnimName,orientationkey(K))),
  fail.

set_pendulum_keys(File,Model,NodeList,NodeRefList,AnimName,Torso,TorsoRef,RootDummy,RootDummyRef,_,_) :-
  once((secret(body_gravity(G)) ; G=0.025)),
  gotdata(File,Model,Torso,TorsoRef,AnimName,orientationkey(N,T,Xt,Yt,Zt,At)),
  axisangle2quaternion([Xt,Yt,Zt,At],Qtorso),
  interpolate_orientation_key_quaternion(File,Model,RootDummy,RootDummyRef,AnimName,T,Qrootdummy),
  multiply_quaternions(Qtorso,Qrootdummy,Qnode),
  quaternion2axisangle(Qnode,[Xg,Yg,Zg,Ag]),
  NodeList = [NodeName0,NodeName1,NodeName2,NodeName3,NodeName4],
  NodeRefList = [NodeRef0,NodeRef1,NodeRef2,NodeRef3,NodeRef4],
  A0 is -G*Ag,
  assertz(gotdata(File,Model,NodeName0,NodeRef0,AnimName,orientationkey(N,T,Xg,Yg,Zg,A0))),
  (Xg*Ag=<0 -> A1 is -0.5*Ag ; A1 is -0.1*Ag),
  assertz(gotdata(File,Model,NodeName1,NodeRef1,AnimName,orientationkey(N,T,Xg,Yg,Zg,A1))),
  (Xg*Ag>=0 -> A2 is -0.5*Ag ; A2 is -0.1*Ag),
  assertz(gotdata(File,Model,NodeName2,NodeRef2,AnimName,orientationkey(N,T,Xg,Yg,Zg,A2))),
  (Yg*Ag=<0 -> A3 is -0.5*Ag ; A3 is -0.1*Ag),
  assertz(gotdata(File,Model,NodeName3,NodeRef3,AnimName,orientationkey(N,T,Xg,Yg,Zg,A3))),
  (Yg*Ag>=0 -> A4 is -0.5*Ag ; A4 is -0.1*Ag),
  assertz(gotdata(File,Model,NodeName4,NodeRef4,AnimName,orientationkey(N,T,Xg,Yg,Zg,A4))),
  fail.

set_pendulum_keys(File,Model,_,_,AnimName,_,_,_,_,ShakeNode,ShakeRef) :-
  once((secret(chest_shake_factor(F)) ; F=0.1)),
  retractall(gotdata(File,Model,ShakeNode,ShakeRef,AnimName,positionkey(_))),
  retractall(gotdata(File,Model,ShakeNode,ShakeRef,AnimName,positionkey(_,_,_,_,_))),
  gotdata(File,Model,omb_bone_body_lbreast,Bref,AnimName,positionkey(K)),
  assertz(gotdata(File,Model,ShakeNode,ShakeRef,AnimName,positionkey(K))),
  (gotdata(File,Model,omb_bone_body_lbreast,Bref,AnimName,positionkey(N,T,0,0,Z)),
   Z1 is Z*F*(random(1000)+random(1000)+random(1000))/3000,
   assertz(gotdata(File,Model,ShakeNode,ShakeRef,AnimName,positionkey(N,T,0,0,Z1))),
   fail ; true),
  fail.

set_pendulum_keys(_,_,_,_,_,_,_,_,_,_,_).


interpolate_orientation_key_quaternion(File,Model,NodeName,NodeRef,AnimName,T,Qnode) :-
  gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(_,T1,X,Y,Z,A)), T1=:=T,
  (NodeName==rootdummy -> Af=A ; two_pi_fix(A,Af)),
  axisangle2quaternion([X,Y,Z,Af],Qnode), !.

interpolate_orientation_key_quaternion(File,Model,NodeName,NodeRef,AnimName,T,Qnode) :-
  gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N1,T1,X1,Y1,Z1,A1)), T1<T,
  N2 is N1+1,
  gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(N2,T2,X2,Y2,Z2,A2)), T2>T,
  !,
  (NodeName==rootdummy -> A1f=A1, A2f=A2 ; two_pi_fix(A1,A1f), two_pi_fix(A2,A2f)),
  Lambda is (T-T1)/(T2-T1),
  axisangle2quaternion([X1,Y1,Z1,A1f],Q1),
  axisangle2quaternion([X2,Y2,Z2,A2f],Q2),
  interpolate_quaternions(Q1,Q2,Lambda,Qnode).

/* ====================================== */
/* set_limb_pendulum_keys/7               */
/* set_limb_pendulum_keys1/7               */
/* parent_list_to_rootdummy/5             */
/* has_complete_animation_key_hierarchy/4 */
/* set_limb_pendulum_keys2/6              */
/* set_sleeve_keys/6                      */
/* ====================================== */

set_limb_pendulum_keys(File,Model,Pendulum1,_,_,_,_) :- gotdata(File,Model,Pendulum1,_,_,_), !.
set_limb_pendulum_keys(File,Model,_,Pendulum2,_,_,_) :- gotdata(File,Model,Pendulum2,_,_,_), !.

set_limb_pendulum_keys(File,Model,Pendulum1,Pendulum2,Sleeve1,Sleeve2,Shaker) :-
  gotdata(File,Model,Pendulum2,P2Ref,parent(Limb2/Limb2Ref)),
  gotdata(File,Model,Limb2,Limb2Ref,parent(Limb1/Limb1Ref)),
  gotdata(File,Model,Pendulum1,P1Ref,parent(Limb1/Limb1Ref)),
  parent_list_to_rootdummy(File,Model,Limb1,Limb1Ref,PList),
  RefList = [Limb2,Limb2Ref,Limb1,Limb1Ref|PList],
  !,
  set_limb_pendulum_keys1(File,Model,Pendulum1,P1Ref,Pendulum2,P2Ref,RefList),
  set_sleeve_keys(File,Model,Pendulum1,P1Ref,Sleeve1,Shaker),
  set_sleeve_keys(File,Model,Pendulum2,P2Ref,Sleeve2,Shaker),
  (gotdata(File,Model,newanim(_,Model)) -> tab(2), write('set sleeve keys on '), write([Sleeve1,Sleeve2]), nl ; true).

set_limb_pendulum_keys(_,_,_,_,_,_,_).

set_limb_pendulum_keys1(File,Model,Pendulum1,P1Ref,Pendulum2,P2Ref,RefList) :-
  RefList = [_,_|RefList1],
  gotdata(File,Model,newanim(AnimName,Model)),
  /* has_complete_animation_key_hierarchy(File,Model,AnimName,RefList), */
  set_limb_pendulum_keys2(File,Model,AnimName,Pendulum1,P1Ref,RefList1),
  set_limb_pendulum_keys2(File,Model,AnimName,Pendulum2,P2Ref,RefList),
  fail ; true.

parent_list_to_rootdummy(_,_,rootdummy,_,[]) :- !.

parent_list_to_rootdummy(File,Model,NodeName,NodeRef,[Parent,ParentRef|L]) :-
  gotdata(File,Model,NodeName,NodeRef,parent(Parent/ParentRef)),
  parent_list_to_rootdummy(File,Model,Parent,ParentRef,L).

has_complete_animation_key_hierarchy(_,_,_,[]) :- !.

has_complete_animation_key_hierarchy(File,Model,AnimName,[NodeName,NodeRef|List]) :-
  gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(_)),
  !,
  has_complete_animation_key_hierarchy(File,Model,AnimName,List).

set_limb_pendulum_keys2(File,Model,AnimName,Pendulum,PRef,RefList) :-
  RefList = [Limb,LimbRef|PList],
  once((secret(limb_gravity(G)) ; G=0.1)),
  gotdata(File,Model,Limb,LimbRef,AnimName,orientationkey(K)),
  assertz(gotdata(File,Model,Pendulum,PRef,AnimName,orientationkey(K))),
  gotdata(File,Model,Limb,LimbRef,AnimName,orientationkey(N,T,X,Y,Z,A)),
  two_pi_fix(A,Af), axisangle2quaternion([X,Y,Z,Af],Q),
  interpolate_orientation_key_quaternion_to_rootdummy(File,Model,PList,AnimName,T,Qi),
  multiply_quaternions(Q,Qi,Qnode),
  quaternion2axisangle(Qnode,[Xg,Yg,Zg,Ag]),
  Ag1 is -G*Ag,
  assertz(gotdata(File,Model,Pendulum,PRef,AnimName,orientationkey(N,T,Xg,Yg,Zg,Ag1))),
  fail.

set_limb_pendulum_keys2(_,_,_,_,_,_).

set_sleeve_keys(File,Model,Pendulum,Pref,Sleeve,Shaker) :-
  once((secret(limb_gravity(G)) ; G=0.1)),
  once((secret(limb_impulse(I)) ; I=1.0)),
  gotdata(File,Model,Sleeve,SleeveRef,parent(Pendulum/Pref)),
  clause(gotdata(File,Model,node(dummy,Shaker)),true,ShakeRef),
  gotdata(File,Model,Sleeve,SleeveRef,position(X0,Y0,Z0)), Zf is 0.2*Z0,
  gotdata(File,Model,Pendulum,Pref,AnimName,orientationkey(K)),
  retractall(gotdata(File,Model,Sleeve,SleeveRef,AnimName,positionkey(_))),
  assertz(gotdata(File,Model,Sleeve,SleeveRef,AnimName,positionkey(K))),
  retractall(gotdata(File,Model,Sleeve,SleeveRef,AnimName,positionkey(_,_,_,_,_))),
  (gotdata(File,Model,Pendulum,Pref,AnimName,orientationkey(N,T,_,_,Z,A)),
   C is cos(-A/G), F is (1.0-C)*Z*Z+C,
   once((interpolate_positionkey(File,Model,Shaker,ShakeRef,AnimName,T,_,_,Zs) ; Zs=0.0)),
   Z1 is Z0+F*Zf+I*Zs,
   assertz(gotdata(File,Model,Sleeve,SleeveRef,AnimName,positionkey(N,T,X0,Y0,Z1))),
   fail ; true),
  fail; true.

/* ========================= */
/* interpolate_positionkey/9 */
/* ========================= */

interpolate_positionkey(File,Model,NodeName,NodeRef,AnimName,T,X,Y,Z) :-
  gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(_,T1,X,Y,Z)), T1=:=T,
  !.

interpolate_positionkey(File,Model,NodeName,NodeRef,AnimName,T,X,Y,Z) :-
  gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(N1,T1,X1,Y1,Z1)), T1<T, N2 is N1+1,
  gotdata(File,Model,NodeName,NodeRef,AnimName,positionkey(N2,T2,X2,Y2,Z2)), T2>T,
  Lambda is (T-T1)/(T2-T1),
  X is X1+Lambda*(X2-X1), Y is Y1+Lambda*(Y2-Y1), Z is Z1+Lambda*(Z2-Z1),
  !.

/* ===================================================== */
/* interpolate_orientation_key_quaternion_to_rootdummy/6 */
/* ===================================================== */

interpolate_orientation_key_quaternion_to_rootdummy(File,Model,[rootdummy,RootRef],AnimName,T,Q) :-
  !,
  interpolate_orientation_key_quaternion(File,Model,rootdummy,RootRef,AnimName,T,Q).

interpolate_orientation_key_quaternion_to_rootdummy(File,Model,[NodeName,NodeRef|List],AnimName,T,Q) :-
  interpolate_orientation_key_quaternion_to_rootdummy(File,Model,List,AnimName,T,Qi),
  interpolate_orientation_key_quaternion(File,Model,NodeName,NodeRef,AnimName,T,Qnode),
  multiply_quaternions(Qnode,Qi,Q),
  !.

interpolate_orientation_key_quaternion_to_rootdummy(File,Model,[NodeName,NodeRef|_],AnimName,T,Q) :-
  interpolate_orientation_key_quaternion(File,Model,NodeName,NodeRef,AnimName,T,Q),
  !.

interpolate_orientation_key_quaternion_to_rootdummy(_,_,_,_,_,[0,0,0,1]).

/* ================================ */
/* split_skin_by_smoothing_groups/4 */
/* ================================ */

split_skin_by_smoothing_groups(File,Model,Skin,SkinRef) :-
  clause(gotdata(File,Model,node(skin,Skin)),true,SkinRef),
  setof(G,uses_smoothing_group(File,Model,Skin,SkinRef,G),Groups),
  length(Groups,L), L>1,
  !,
  split_skin_by_grouplist(File,Model,Skin,SkinRef,Groups),
  tab(2), write(Skin), write(' split by smoothing groups '), write(Groups), nl.

split_skin_by_smoothing_groups(_,_,_,_).

uses_smoothing_group(File,Model,Skin,SkinRef,G) :-
  gotdata(File,Model,Skin,SkinRef,faces(_,_,_,_,B,_,_,_,_)),
  has_bit(B,G).

has_smoothing_group(File,Model,Skin,SkinRef,G,F) :-
  gotdata(File,Model,Skin,SkinRef,faces(F,_,_,_,B,_,_,_,_)),
  has_bit(B,G).

has_bit(0,0) :- !.
has_bit(B,G) :- between(1,32,G), Mask is 1<<(G-1), B/\Mask =:= Mask.

split_skin_by_grouplist(File,Model,Skin,SkinRef,[]) :-
  retractall(gotdata(File,Model,Skin,SkinRef,_)),
  erase(SkinRef),
  !.

split_skin_by_grouplist(File,Model,Skin,SkinRef,[G|G0]) :-
  setof(F,has_smoothing_group(File,Model,Skin,SkinRef,G,F),Faces),
  Faces\=[],
  concat_atom([Skin,G],'_',Skin_G),
  assertz(gotdata(File,Model,node(skin,Skin_G)),SkinRef_G),
  (gotdata(File,Model,Skin,SkinRef,Q), assertz(gotdata(File,Model,Skin_G,SkinRef_G,Q)), fail ; true),
  f_delete_faces_in_list(File,Model,Skin,SkinRef,Faces),
  f_keep_only_faces_in_list(File,Model,Skin_G,SkinRef_G,Faces),
  delete_unused_vertices(File,Model,Skin_G,SkinRef_G),
  delete_unused_tverts(File,Model,Skin_G,SkinRef_G),
  !,
  split_skin_by_grouplist(File,Model,Skin,SkinRef,G0).

split_skin_by_grouplist(File,Model,Skin,SkinRef,[_|G0]) :- split_skin_by_grouplist(File,Model,Skin,SkinRef,G0).

/* ==================== */
/* make_kilt_bones/3    */
/* set_kilt_keys/4      */
/* set_kilt_keys_1/6    */
/* set_kilt_keys_2/11   */
/* ==================== */

/* CM362h: pass if pelvis_g, lthigh_g or rthigh_g is missing */

make_kilt_bones(File,Model,_) :-
  \+gotdata(File,Model,node(_,pelvis_g)),
  (cm3_verbose -> tab(2), write('skipping kilt bones - no pelvis_g'), nl ; true),
  !.

make_kilt_bones(File,Model,_) :-
  \+gotdata(File,Model,node(_,lthigh_g)),
  (cm3_verbose -> tab(2), write('skipping kilt bones - no lthigh_g'), nl ; true),
  !.

make_kilt_bones(File,Model,_) :-
  \+gotdata(File,Model,node(_,rthigh_g)),
  (cm3_verbose -> tab(2), write('skipping kilt bones - no rthigh_g'), nl ; true),
  !.

make_kilt_bones(File,Model,[MidBone,LeftBone,RightBone,FrontBone,BackBone]) :-

  /* Attach a proximal dummy to pelvis_g and orient it so that it is always vertical */
  /* Attach a proximal dummy to each thigh_g and orient it vertical unless the limb  */
  /* exceeds about 15deg outwards from vertical, in which case follow the limb.      */
  /* Attach Front and Back proximal dummies to pelvis_g which follow whichever leg   */
  /* is the most forward or backward respectively.                                   */

  MidBone   = omb_bone_kilt_m,
  LeftBone  = omb_bone_kilt_l,
  RightBone = omb_bone_kilt_r,
  FrontBone = omb_bone_kilt_f,
  BackBone  = omb_bone_kilt_b,

  gotdata(File,Model,rthigh_g,_,position(_,Y,Z)),
  create_offset_dummy(File,Model,pelvis_g,0,Y,Z,MidBone,C,C1),
  create_proximal_dummy(File,Model,lthigh_g,LeftBone,C1,C2),
  create_proximal_dummy(File,Model,rthigh_g,RightBone,C2,C3),
  create_offset_dummy(File,Model,pelvis_g,0,Y,Z,FrontBone,C3,C4),
  create_offset_dummy(File,Model,pelvis_g,0,Y,Z,BackBone,C4,[]),
  (C\=[] -> length(C,L), tab(2), write(L), write(' kilt bone(s) created'), nl ; true),

  set_kilt_keys(File,Model,MidBone,mid),
  set_kilt_keys(File,Model,LeftBone,left),
  set_kilt_keys(File,Model,RightBone,right),
  set_kilt_keys_1(File,Model,lthigh_g,rthigh_g,FrontBone,BackBone),
  !.

set_kilt_keys(File,Model,Bone,Way) :-
  once((secret(kilt_threshold(Th)) ; Th=0.2)),
  clause(gotdata(File,Model,node(dummy,Bone)),true,BoneRef),
  \+gotdata(File,Model,Bone,BoneRef,_,_),
  parent_list_to_rootdummy(File,Model,Bone,BoneRef,RefList),
  gotdata(File,Model,newanim(AnimName,Model)),
  has_complete_animation_key_hierarchy(File,Model,AnimName,RefList),
  merge_key_frames(File,Model,AnimName,RefList,Frames),
  length(Frames,K),
  assertz(gotdata(File,Model,Bone,BoneRef,AnimName,orientationkey(K))),
  K1 is K-1,
  between(0,K1,N),
    nth0(N,Frames,T),
    interpolate_orientation_key_quaternion_to_rootdummy(File,Model,RefList,AnimName,T,Q),
    quaternion2axisangle(Q,R),
    gimbal_key(R,[X,Y,Z,A]),
    (Way==mid -> sawtooth(A,A1); A1 is max(-Th,min(A,Th))),
    assertz(gotdata(File,Model,Bone,BoneRef,AnimName,orientationkey(N,T,X,Y,Z,A1))),
  fail.

set_kilt_keys(_,_,_,_).

set_kilt_keys_1(File,Model,Leg1,Leg2,FrontBone,BackBone) :-
  clause(gotdata(File,Model,node(dummy,FrontBone)),true,FrontBoneRef),
  \+gotdata(File,Model,FrontBone,FrontBoneRef,_,_),
  clause(gotdata(File,Model,node(dummy,BackBone)),true,BackBoneRef),
  \+gotdata(File,Model,BackBone,BackBoneRef,_,_),
  clause(gotdata(File,Model,node(_,Leg1)),true,Leg1Ref),
  once(gotdata(File,Model,Leg1,Leg1Ref,_,_)),
  clause(gotdata(File,Model,node(_,Leg2)),true,Leg2Ref),
  once(gotdata(File,Model,Leg2,Leg2Ref,_,_)),
  gotdata(File,Model,newanim(AnimName,Model)),
  set_kilt_keys_2(File,Model,Leg1,Leg1Ref,Leg2,Leg2Ref,AnimName,FrontBone,FrontBoneRef,BackBone,BackBoneRef),
  fail.

set_kilt_keys_1(_,_,_,_,_,_).

set_kilt_keys_2(File,Model,Leg1,Leg1Ref,Leg2,Leg2Ref,AnimName,FrontBone,FrontBoneRef,BackBone,BackBoneRef) :-
  merge_key_frames(File,Model,AnimName,[Leg1,Leg1Ref,Leg2,Leg2Ref],Frames),
  length(Frames,K),
  assertz(gotdata(File,Model,FrontBone,FrontBoneRef,AnimName,orientationkey(K))),
  assertz(gotdata(File,Model,BackBone,BackBoneRef,AnimName,orientationkey(K))),
  K1 is K-1,
  between(0,K1,N),
    nth0(N,Frames,T),
    interpolate_orientation_key_quaternion(File,Model,Leg1,Leg1Ref,AnimName,T,Q1),
    quaternion2axisangle(Q1,R1),
    interpolate_orientation_key_quaternion(File,Model,Leg2,Leg2Ref,AnimName,T,Q2),
    quaternion2axisangle(Q2,R2),
    scissors(R1,R2,RFront,RBack),
    RFront=[Xf,Yf,Zf,Af], RBack=[Xb,Yb,Zb,Ab],
    assertz(gotdata(File,Model,FrontBone,FrontBoneRef,AnimName,orientationkey(N,T,Xf,Yf,Zf,Af))),
    assertz(gotdata(File,Model,BackBone,BackBoneRef,AnimName,orientationkey(N,T,Xb,Yb,Zb,Ab))),
  fail.

set_kilt_keys_2(_,_,_,_,_,_,_,_,_,_,_).

/* ================== */
/* scissors/4         */
/* foward_swing/2     */
/* slack/3            */
/* sawtooth/2         */
/* gimbal_key/2       */
/* merge_key_frames/5 */
/* key_frame/6        */
/* ================== */

scissors(R1,R2,Rf,Rb) :-
  once((secret(kilt_threshold(Th)) ; Th=0.2)),
  forward_swing(R1,F1,_),
  forward_swing(R2,F2,_),
  MaxF is max(F1,F2), slack(MaxF,Th,Af), (Af=:=0 -> Rf=[0,0,0,0] ; Rf=[1,0,0,Af]),
  MinF is min(F1,F2), slack(MinF,Th,Ab), (Ab=:=0 -> Rb=[0,0,0,0] ; Rb=[1,0,0,Ab]).

forward_swing([_,_,_,A],0,0) :- A=:=0, !.

forward_swing([X0,Y0,Z0,A],Front,Side) :-
  R is sqrt(X0*X0+Y0*Y0+Z0*Z0),
  X is X0/R, Y is Y0/R, Z is Z0/R,
  S is sin(A), C is cos(A), T is 1.0-C,
  Front is atan(X*S-T*Y*Z,T*Z*Z+C),
  Side is atan(T*X*Z+Y*S,T*Z*Z+C).

slack(A,Th,0.0) :- abs(A)=<Th, !.
slack(A,Th,As)  :- A >  Th, !, As is A-Th.
slack(A,Th,As)  :- A < -Th, !, As is A+Th.

sawtooth(A,A) :- A >= -pi/4, A =< pi/4, !.
sawtooth(A,0) :- A =< -pi/2, !.
sawtooth(A,0) :- A >= pi/2, !.
sawtooth(A,A1) :- A > pi/4, A < pi/2, !, A1 is pi/2 - A.
sawtooth(A,A1) :- A < -pi/4, A > -pi/2, !, A1 is -pi/2 - A.

gimbal_key([_,_,_,A],[0,0,0,0]) :- A=:=0, !.

gimbal_key([X0,Y0,_,_],[0,0,0,0]) :- X0=:=0, Y0=:=0, !. /* CM362g */

gimbal_key([X0,Y0,Z0,A],[Xg,Yg,0,Ag]) :-
  R is sqrt(X0*X0+Y0*Y0+Z0*Z0),
  X is X0/R, Y is Y0/R, Z is Z0/R,
  S is sin(A), C is cos(A), T is 1.0-C,
  Xg1 is -X*S+T*Y*Z, Yg1 is -Y*S-T*X*Z,
  Rg is sqrt(Xg1*Xg1+Yg1*Yg1), Xg is Xg1/Rg, Yg is Yg1/Rg,
  Ag is acos(T*Z*Z+C).

merge_key_frames(_,_,_,[],[]) :- !.

merge_key_frames(File,Model,AnimName,[NodeName,NodeRef|List],Frames) :-
  setof(T,key_frame(File,Model,NodeName,NodeRef,AnimName,T),These),
  merge_key_frames(File,Model,AnimName,List,Those),
  append(These,Those,All),
  sort(All,Frames),
  !.

key_frame(File,Model,NodeName,NodeRef,AnimName,T) :- gotdata(File,Model,NodeName,NodeRef,AnimName,orientationkey(_,T,_,_,_,_)).

/* ==================== */
/* kilt_skin_weights/10 */
/* falloff/3            */
/* enforce4bpv/2        */
/* enforce4bpv1/2       */
/* enforce4bpv2/2       */
/* ==================== */

kilt_skin_weights(X,Y,Z,X0,Y0,Z0,Hem,Pelvis,[_,Left,Right,Front,Back],WList) :-

  /* simplistic scheme */

  Y1 is Y-Y0, Z1 is Z-Z0, ZY is Z1-Y1,
  (ZY > 0 -> WList=[Pelvis,1.0] ;
    falloff(ZY,Z0,Wp1),
    (Z<Z0 -> Wp is Wp1*(Z-Hem)/(Z0-Hem) ; Wp = Wp1),
    falloff(X,X0,Wm),
    Wmid is Wm*(1.0-Wp),
    Wside is (1.0-Wm)*(1.0-Wp),
    (Y1>0 -> MidBone=Front; MidBone=Back),
    (X>0 -> SideBone=Right ; SideBone=Left),
    enforce4bpv([Pelvis,Wp,SideBone,Wside,MidBone,Wmid],WList)
  ).

falloff(X,X0,F) :- F is sqrt(max(0.0,1.0-sqrt(abs(X/X0)))).

enforce4bpv(W,W1) :-
  enforce4bpv1(W,Wt),
  sort(Wt,Ws),
  reverse(Ws,Wr),
  enforce4bpv2(Wr,W1).

enforce4bpv1([],[]).
enforce4bpv1([_,W|L],L1) :- W=:=0.0, !, enforce4bpv1(L,L1).
enforce4bpv1([B,W|L],[[A,W,B]|L1]) :- A is abs(W), enforce4bpv1(L,L1).

enforce4bpv2([],[]) :- !.

enforce4bpv2([[_,_,B1]],[B1,1.0]).

enforce4bpv2([[_,W1,B1],[_,W2,B2]],[B1,W1n,B2,W2n]) :-
  T is W1+W2,
  W1n is W1/T, W2n is W2/T.

enforce4bpv2([[_,W1,B1],[_,W2,B2],[_,W3,B3]],[B1,W1n,B2,W2n,B3,W3n]) :-
  T is W1+W2+W3,
  W1n is W1/T, W2n is W2/T, W3n is W3/T.

enforce4bpv2([[_,W1,B1],[_,W2,B2],[_,W3,B3],[_,W4,B4]|_],[B1,W1n,B2,W2n,B3,W3n,B4,W4n]) :-
  T is W1+W2+W3+W4,
  W1n is W1/T, W2n is W2/T, W3n is W3/T, W4n is W4/T.

/* ========================= */
/* force_zero_orientations/2 */
/* ========================= */

force_zero_orientations(File,Model) :-
  clause(gotdata(File,Model,NodeName,NodeRef,orientation(_,_,_,A)),true,Eref),
  A =\= 0.0,
  erase(Eref),
  assertz(gotdata(File,Model,NodeName,NodeRef,orientation(0,0,0,0))),
  fail.

force_zero_orientations(_,_) :-
  tab(2), write('orientations forced to zero'), nl.

