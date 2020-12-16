/* ======================================================= */ 
/*                                                         */
/* Predicates for importing binary (compiled) models       */
/* Part of the CleanModels 3 suite by OldMansBeard         */
/*                                                         */
/* This version dated 2013-08-13                           */
/* Later modifications by orth                             */
/*                                                         */
/* ======================================================= */ 

:- dynamic gotbinary/2.
:- dynamic gotbinary/3.
:- dynamic gotparent/5.
:- dynamic name_checked/5.
:- dynamic nth_node/1.
:- dynamic sg_assigned/3.
:- dynamic compiler/1.

/* =============== */
/* import_binary/1 */
/* =============== */

import_binary(File) :-
  write('Binary file '), write(File), write(' detected, attempting import.'), nl,
  clear_binary_data,
  open(File,read,Stream,[type(binary)]),
  get_file_header(File,Stream),
  get_model_header(File,Stream,ModelName,RootNodePtr,AnimHeaderPtrList),
  get_geometry_nodes(File,Stream,ModelName,RootNodePtr),
  get_animation_data(File,Stream,ModelName,AnimHeaderPtrList),
  /* CM362g: postpone skinweighting until all nodes have been read */
  (gotdata(File,Model,NodeName,NodeRef,weights(N)), do_skinbones(File,Model,NodeName,NodeRef,N,0), fail ; true), 
  close(Stream),
  write('Binary model '), write(File), write(' imported'), nl,
  (compiler(C) -> write('Detected '), write(C), write(' compiler'), nl ; true).

/* =================== */
/* clear_binary_data/0 */
/* =================== */

clear_binary_data :-
  retractall(gotbinary(_,_)),
  retractall(gotbinary(_,_,_)),
  retractall(gotparent(_,_,_,_,_,_,_)),
  retractall(name_checked(_,_,_,_,_)),
  retractall(nth_node(_)),
  retractall(sg_assigned(_,_,_)),
  retractall(compiler(_)),
  !,
  trim_stacks.

/* ==================== */
/* get_animation_data/4 */
/* ==================== */

get_animation_data(_,_,_,[]) :- write('.'), nl, !.

get_animation_data(File,Stream,ModelName,[Ptr|Rest]) :-
  Ptr =:= 0,
  !,
  get_animation_data(File,Stream,ModelName,Rest).


get_animation_data(File,Stream,ModelName,[Ptr|Rest]) :-
  Start is Ptr+12,
  seek(Stream,Start,bof,_),
  get_animation_header(File,Stream,ModelName,AnimName,EventPtr,NumEvents,RootNodePtr),
  get_animation_events(File,Stream,ModelName,AnimName,EventPtr,NumEvents),
  get_animation_nodes(File,Stream,ModelName,AnimName,RootNodePtr),
  !,
  write('.'), ttyflush,
  get_animation_data(File,Stream,ModelName,Rest).

/* ====================== */
/* get_animation_events/6 */
/* get_animation_events/5 */
/* ====================== */

get_animation_events(_,_,_,_,_,NumEvents) :- NumEvents=<0, !.

get_animation_events(_,_,_,_,EventPtr,_) :- EventPtr =:= 0, !.

get_animation_events(File,Stream,ModelName,AnimName,EventPtr,NumEvents) :-
  NumEvents>0,
  Start is EventPtr+12,
  seek(Stream,Start,bof,_),
  get_animation_events(File,Stream,ModelName,AnimName,NumEvents).

get_animation_events(_,_,_,_,N) :- N=<0, !.

get_animation_events(File,Stream,ModelName,AnimName,N) :-
  get_float(Stream,After),
  get_char32(Stream,EventName),
  assertz(gotdata(File,ModelName,anim(AnimName),event(After,EventName))),
  N1 is N-1,
  !,
  get_animation_events(File,Stream,ModelName,AnimName,N1).
  
/* ====================== */
/* get_file_header/2      */
/* get_model_header/5     */
/* get_animation_header/7 */
/* get_geometry_header/4  */
/* ====================== */

get_file_header(File,Stream) :-
  seek(Stream,0,bof,_),
  get_int32(Stream,BType),
  get_int32(Stream,RawDataOffset),
  get_int32(Stream,RawDataSize),
  !,
  assertz(gotbinary(File,file_header(BType,RawDataOffset,RawDataSize))).

get_model_header(File,Stream,ModelName,RootNodePtr,AnimHeaderPtrList) :-
  seek(Stream,12,bof,_),
  get_geometry_header(File,Stream,ModelName,RootNodePtr),
  ignore_bytes(2,Stream),
  get_byte(Stream,ClassificationCode),
  get_byte(Stream,IgnoreFog),
  ignore_bytes(4,Stream),
  get_ptr_array(Stream,AnimHeaderPtrList),
  ignore_bytes(4,Stream),
  get_float(Stream,BBMinX),
  get_float(Stream,BBMinY),
  get_float(Stream,BBMinZ),
  get_float(Stream,BBMaxX),
  get_float(Stream,BBMaxY),
  get_float(Stream,BBMaxZ),
  get_float(Stream,ModelRadius),
  get_float(Stream,AnimationScale),
  get_char64(Stream,SuperModelName),
  !,
  assertz(gotbinary(File,model_header(ModelName,ClassificationCode,IgnoreFog,AnimationScale,SuperModelName,bbMin(BBMinX,BBMinY,BBMinZ),bbMax(BBMaxX,BBMaxY,BBMaxZ),ModelRadius,RootNodePtr,AnimHeaderPtrList))),
  assertz(gotdata(File,ModelName,newmodel(ModelName))),
  (classification_code(ClassificationCode,Classification) -> assertz(gotdata(File,ModelName,classification(Classification))) ; true),
  write('Binary model classified as '), write(Classification), nl,
  assertz(gotdata(File,ModelName,setsupermodel(ModelName,SuperModelName))),
  assertz(gotdata(File,ModelName,setanimationscale(AnimationScale))),
  assertz(gotdata(File,ModelName,bmin(BBMinX,BBMinY,BBMinZ))), /* Not recognised by NWMax */
  assertz(gotdata(File,ModelName,bmax(BBMaxX,BBMaxY,BBMaxZ))), /* Not recognised by NWMax */
  assertz(gotdata(File,ModelName,radius(ModelRadius))),        /* Not recognised by NWMax */
  assertz(gotdata(File,ModelName,ignorefog(IgnoreFog))),       /* Not recognised by NWMax */
  assertz(gotdata(File,ModelName,beginmodelgeom(ModelName))).

get_animation_header(File,Stream,ModelName,AnimName,EventPtr,NumEvents,RootNodePtr) :-
  get_geometry_header(File,Stream,AnimName,RootNodePtr),
  get_float(Stream,AnimLength),
  get_float(Stream,TransTime),
  get_char64(Stream,AnimRoot),
  get_int32(Stream,EventPtr),
  get_int32(Stream,NumEvents),
  ignore_bytes(4,Stream),
  !,
  assertz(gotbinary(File,anim_header(ModelName,AnimName,AnimLength,TransTime,AnimRoot,EventPtr,NumEvents,RootNodePtr))),
  assertz(gotdata(File,ModelName,newanim(AnimName,ModelName))),
  assertz(gotdata(File,ModelName,anim(AnimName),length(AnimLength))),
  assertz(gotdata(File,ModelName,anim(AnimName),transtime(TransTime))),
  assertz(gotdata(File,ModelName,anim(AnimName),animroot(AnimRoot))).
  
get_geometry_header(File,Stream,ModelName,RootNodePtr) :-
  get_int32(Stream,P1),
  get_int32(Stream,P2),
  get_char64(Stream,ModelName), (ModelName =='' -> halt ; true),
  get_int32(Stream,RootNodePtr),
  get_int32(Stream,NumNodes),
  ignore_bytes(24,Stream),
  get_int32(Stream,RefCount),
  get_byte(Stream,GeomType),
  ignore_bytes(3,Stream),
  !,
  assertz(gotbinary(File,geometry_header(P1,P2,ModelName,NumNodes,RootNodePtr,RefCount,GeomType))).

/* ==================== */
/* classification codes */
/* ==================== */

classification_code(1,'EFFECT').
classification_code(2,'TILE').
classification_code(4,'CHARACTER').
classification_code(8,'DOOR').

/* ========================== */
/* get_geometry_nodes/4       */
/* get_geometry_nodes/6       */
/* get_geometry_child_nodes/6 */
/* ========================== */

get_geometry_nodes(_,_,_,RootNodePtr) :- RootNodePtr =:= 0, !.

get_geometry_nodes(File,Stream,ModelName,RootNodePtr) :-
  get_geometry_nodes(File,Stream,ModelName,RootNodePtr,'NULL',-1).

get_geometry_nodes(File,Stream,ModelName,NodePtr,ParentName,ParentNodeRef) :-
  get_node_header(File,Stream,ModelName,NodePtr,NodeName,BNodeType,NodeRef,ParentName,ParentNodeRef,ChildPtrList,Controllers),
  get_node_specific_data(File,Stream,ModelName,NodeName,NodeRef,BNodeType),
  get_controllers(File,Stream,ModelName,NodeName,BNodeType,NodeRef,Controllers),
  get_geometry_child_nodes(File,Stream,ModelName,NodeName,NodeRef,ChildPtrList).

get_geometry_child_nodes(_,_,_,_,_,[]) :- !.

get_geometry_child_nodes(File,Stream,ModelName,ParentName,ParentRef,[H|T]) :-
  get_geometry_nodes(File,Stream,ModelName,H,ParentName,ParentRef),
  get_geometry_child_nodes(File,Stream,ModelName,ParentName,ParentRef,T),
  !.

/* ========================= */
/* get_node_header/11        */
/* node_type_special_check/4 */
/* ========================= */

get_node_header(_,_,_,NodePtr,_,_,_,_,_,[],[0,0,0,0]) :- NodePtr =:= 0, !.

get_node_header(File,Stream,ModelName,NodePtr,NodeName,BNodeType,NodeRef,ParentName,ParentNodeRef,ChildPtrList,Controllers) :-
  Start is NodePtr+12,
  seek(Stream,Start,bof,_),
  get_int32(Stream,F1),
  get_int32(Stream,F2),
  get_int32(Stream,F3),
  get_int32(Stream,F4),
  get_int32(Stream,F5),
  get_int32(Stream,F6),
  get_int32(Stream,InheritColor),
  get_int32(Stream,NodeNumber),
  byte_count(Stream,Here),
  get_char32(Stream,RawNodeName), (RawNodeName =='' -> halt ; true), upcase_special(ModelName,RawNodeName,NodeName),
  ignore_bytes(8,Stream),
  get_ptr_array(Stream,ChildPtrList),
  get_int32(Stream,C_Key_StartPtr),
  get_int32(Stream,C_Key_NumElements),
  ignore_bytes(4,Stream),
  get_int32(Stream,C_Data_StartPtr),
  get_int32(Stream,C_Data_NumElements),
  ignore_bytes(4,Stream),
  get_int32(Stream,BNodeType),
  !,
  assertz(gotbinary(File,node_header(F1,F2,F3,F4,F5,F6,InheritColor,NodeNumber,NodeName,BNodeType))),
  Controllers = [C_Key_StartPtr,C_Key_NumElements,C_Data_StartPtr,C_Data_NumElements],
  node_type(BNodeType,NT), node_type_special_check(ModelName,NodeName,NT,Type),
  asserta(gotdata(File,ModelName,node(Type,NodeName)),NodeRef),
  (ParentName=='NULL' -> P=parent('NULL') ; P=parent(ParentName/ParentNodeRef)),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,P)),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,'#part-number'(NodeNumber))),
  assertz(gotparent(File,ModelName,NodeName,NodeRef,NodeNumber,ParentName,ParentNodeRef)),
  (InheritColor =\= 0 -> assertz(gotdata(File,ModelName,NodeName,NodeRef,inheritcolor(InheritColor))) ; true),
  assertz(name_checked(File,ModelName,RawNodeName,Here,32)),
  assertz(nth_node(NodeRef)),
  !.

node_type_special_check(ModelName,NodeName,NT,Type) :-
  NT==dummy,
  atom_concat(ModelName,Suffix,NodeName),
  (atom_concat('ml',Digit,Suffix); atom_concat('sl',Digit,Suffix)), cm3_is_digit(Digit),
  !,
  Type=light.

node_type_special_check(_,_,Type,Type).
  

/* =========================== */
/* get_animation_nodes/5       */
/* get_animation_nodes/6       */
/* get_animation_child_nodes/6 */
/* =========================== */

get_animation_nodes(_,_,_,_,NodePtr) :- NodePtr =:= 0, !.

get_animation_nodes(File,Stream,ModelName,AnimName,NodePtr) :- 
  get_animation_nodes(File,Stream,'NULL',ModelName,AnimName,NodePtr).

get_animation_nodes(_,_,_,_,_,NodePtr) :- NodePtr =:= 0, !.

get_animation_nodes(File,Stream,ParentName,ModelName,AnimName,NodePtr) :- 
  get_animation_node_header(File,Stream,ModelName,ParentName,NodePtr,NodeName,BNodeType,NodeRef,ChildPtrList,Controllers),
  get_animation_node_specific_data(File,Stream,ModelName,NodeName,NodeRef,AnimName,BNodeType),
  get_controllers(File,Stream,ModelName,NodeName,BNodeType,NodeRef,AnimName,Controllers),
  get_animation_child_nodes(File,Stream,NodeName,ModelName,AnimName,ChildPtrList).

get_animation_child_nodes(_,_,_,_,_,[]) :- !.

get_animation_child_nodes(File,Stream,NodeName,ModelName,AnimName,[H|T]) :-
  get_animation_nodes(File,Stream,NodeName,ModelName,AnimName,H),
  get_animation_child_nodes(File,Stream,NodeName,ModelName,AnimName,T),
  !.

/* ============================ */
/* get_animation_node_header/10 */
/* ============================ */

get_animation_node_header(_,_,_,_,NodePtr,_,_,_,[],[0,0,0,0]) :- NodePtr =:= 0, !.

get_animation_node_header(File,Stream,Model,ParentName,NodePtr,NodeName,BNodeType,NodeRef,ChildPtrList,Controllers) :-
  Start is NodePtr+12,
  seek(Stream,Start,bof,_),
  get_int32(Stream,F1),
  get_int32(Stream,F2),
  get_int32(Stream,F3),
  get_int32(Stream,F4),
  get_int32(Stream,F5),
  get_int32(Stream,F6),
  get_int32(Stream,InheritColor),
  get_int32(Stream,NodeNumber),
  byte_count(Stream,Here),
  get_char32(Stream,RawNodeName), (RawNodeName =='' -> halt ; true), upcase_special(Model,RawNodeName,NodeName),
  ignore_bytes(8,Stream),
  get_ptr_array(Stream,ChildPtrList),
  get_int32(Stream,C_Key_StartPtr),
  get_int32(Stream,C_Key_NumElements),
  ignore_bytes(4,Stream),
  get_int32(Stream,C_Data_StartPtr),
  get_int32(Stream,C_Data_NumElements),
  ignore_bytes(4,Stream),
  get_int32(Stream,BNodeType),
  assertz(gotbinary(File,animation_node_header(F1,F2,F3,F4,F5,F6,InheritColor,NodeNumber,NodeName,BNodeType))),
  Controllers = [C_Key_StartPtr,C_Key_NumElements,C_Data_StartPtr,C_Data_NumElements],
  /* check if this is a new external node */
  (clause(gotdata(File,Model,node(_,NodeName)),true,NodeRef) -> assertz(name_checked(File,Model,RawNodeName,Here,32)) ;
   (clause(external_node(File,Model,NodeName),true,NodeRef) -> true ;
    assertz(external_node(File,Model,NodeName),NodeRef), assertz(external_node_parent(File,Model,NodeName,ParentName))
   )
  ), !.

/* ========== */
/* node types */
/* ========== */

node_type(1,dummy)        :- !.
node_type(3,light)        :- !.
node_type(5,emitter)      :- !.
node_type(17,reference)   :- !.
node_type(33,trimesh)     :- !.
node_type(97,skin)        :- !.
node_type(161,animmesh)   :- !.
node_type(289,danglymesh) :- !.
node_type(545,aabb)       :- !.
node_type(_,unknown).

/* ======================== */
/* get_node_specific_data/6 */
/* get_common_mesh_data/5   */
/* ======================== */

get_node_specific_data(_,_,_,_,_,1) :- !. /* dummy */

get_node_specific_data(File,Stream,ModelName,NodeName,NodeRef,3) :-
  /* light */
  get_float(Stream,FlareRadius),
  ignore_bytes(12,Stream),
  get_int32(Stream,FlareSizesPtr),
  get_int32(Stream,NumFlareSizes),
  ignore_bytes(4,Stream),
  get_int32(Stream,FlarePositionsPtr),
  get_int32(Stream,NumFlarePositions),
  ignore_bytes(4,Stream),
  get_int32(Stream,FlareColorShiftsPtr),
  get_int32(Stream,NumFlareColorShifts),
  ignore_bytes(4,Stream),
  get_ptr_array(Stream,FlareTexNamesPtrList),
  get_int32(Stream,LightPriority),
  get_int32(Stream,AmbientOnly),
  get_int32(Stream,DynamicType),
  get_int32(Stream,AffectDynamic),
  get_int32(Stream,Shadow),
  get_int32(Stream,GenerateFlare),
  get_int32(Stream,FadingLight),
  get_float1array(File,Stream,ModelName,NodeName,NodeRef,FlareSizesPtr,NumFlareSizes,'flaresizes'),
  get_float1array(File,Stream,ModelName,NodeName,NodeRef,FlarePositionsPtr,NumFlarePositions,'flarepositions'),
  get_float3array(File,Stream,ModelName,NodeName,NodeRef,FlareColorShiftsPtr,NumFlareColorShifts,'flarecolorshifts'),
  get_flaretextnames(File,Stream,ModelName,NodeName,NodeRef,FlareTexNamesPtrList),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,flareradius(FlareRadius))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,lightpriority(LightPriority))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,ambientonly(AmbientOnly))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,ndynamictype(DynamicType))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,affectdynamic(AffectDynamic))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,shadow(Shadow))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,generateflare(GenerateFlare))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,fadinglight(FadingLight))),
  !.

get_node_specific_data(File,Stream,ModelName,NodeName,NodeRef,5) :-
  /* emitter */
  get_float(Stream,DeadSpace), assertz(gotdata(File,ModelName,NodeName,NodeRef,deadspace(DeadSpace))),
  get_float(Stream,BlastRadius), assertz(gotdata(File,ModelName,NodeName,NodeRef,blastradius(BlastRadius))),
  get_float(Stream,BlastLength), assertz(gotdata(File,ModelName,NodeName,NodeRef,blastlength(BlastLength))),
  get_int32(Stream,XGrid), assertz(gotdata(File,ModelName,NodeName,NodeRef,xgrid(XGrid))),
  get_int32(Stream,YGrid), assertz(gotdata(File,ModelName,NodeName,NodeRef,ygrid(YGrid))),
  get_int32(Stream,SpawnType), assertz(gotdata(File,ModelName,NodeName,NodeRef,spawntype(SpawnType))),
  get_char32(Stream,Update), assertz(gotdata(File,ModelName,NodeName,NodeRef,update(Update))),
  get_char32(Stream,Render), assertz(gotdata(File,ModelName,NodeName,NodeRef,render(Render))),
  get_char32(Stream,Blend), assertz(gotdata(File,ModelName,NodeName,NodeRef,blend(Blend))),
  get_char64(Stream,Texture), assertz(gotdata(File,ModelName,NodeName,NodeRef,texture(Texture))),
  get_char16(Stream,ChunkName), (ChunkName@>'' -> assertz(gotdata(File,ModelName,NodeName,NodeRef,chunkname(ChunkName))) ; true),
  get_int32(Stream,TwoSidedTex), assertz(gotdata(File,ModelName,NodeName,NodeRef,twosidedtex(TwoSidedTex))),
  get_int32(Stream,Loop), assertz(gotdata(File,ModelName,NodeName,NodeRef,loop(Loop))),
  get_int16(Stream,RenderOrder), assertz(gotdata(File,ModelName,NodeName,NodeRef,renderorder(RenderOrder))),
  ignore_bytes(2,Stream),
  get_int32(Stream,EmitterFlags),
  P2P is EmitterFlags /\ 1, assertz(gotdata(File,ModelName,NodeName,NodeRef,p2p(P2P))),
  P2P_Sel is (EmitterFlags /\ 2) >> 1, assertz(gotdata(File,ModelName,NodeName,NodeRef,p2p_sel(P2P_Sel))),
  AffectedByWind is (EmitterFlags /\ 4) >> 2, assertz(gotdata(File,ModelName,NodeName,NodeRef,affectedbywind(AffectedByWind))),
  M_IsTinted is (EmitterFlags /\ 8) >> 3, assertz(gotdata(File,ModelName,NodeName,NodeRef,m_istinted(M_IsTinted))),
  Bounce is (EmitterFlags /\ 16) >> 4, assertz(gotdata(File,ModelName,NodeName,NodeRef,bounce(Bounce))),
  Random is (EmitterFlags /\ 32) >> 5, assertz(gotdata(File,ModelName,NodeName,NodeRef,random(Random))),
  Inherit is (EmitterFlags /\ 64) >> 6, assertz(gotdata(File,ModelName,NodeName,NodeRef,inherit(Inherit))),
  InheritVel is (EmitterFlags /\ 128) >> 7, assertz(gotdata(File,ModelName,NodeName,NodeRef,inheritvel(InheritVel))),
  InheritLocal is (EmitterFlags /\ 256) >> 8, assertz(gotdata(File,ModelName,NodeName,NodeRef,inherit_local(InheritLocal))),
  Splat is (EmitterFlags /\ 512) >> 9, assertz(gotdata(File,ModelName,NodeName,NodeRef,splat(Splat))),
  InheritPart is (EmitterFlags /\ 1024) >> 10, assertz(gotdata(File,ModelName,NodeName,NodeRef,inherit_part(InheritPart))),
  !.

get_node_specific_data(File,Stream,ModelName,NodeName,NodeRef,17) :-
  /* reference node */
  get_char64(Stream,RefModel),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,refmodel(RefModel))),
  get_int32(Stream,Reattachable),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,reattachable(Reattachable))),
  !.
  
get_node_specific_data(File,Stream,ModelName,NodeName,NodeRef,33) :-
  /* trimesh */
  get_common_mesh_data(File,Stream,ModelName,NodeName,NodeRef),
  !.

get_node_specific_data(File,Stream,ModelName,NodeName,NodeRef,97) :-
  /* skin */
  get_common_mesh_data(File,Stream,ModelName,NodeName,NodeRef),
  ignore_bytes(12,Stream),
  get_int32(Stream,SkinWeightsRawPtr),
  get_int32(Stream,SkinBoneRefRawPtr),
  ignore_bytes(44,Stream),
  get_bonepartnumbers(File,Stream,NodeRef,64,0),
  gotbinary(File,file_header(_,RawDataOffset,_)),
  gotdata(File,ModelName,NodeName,NodeRef,verts(NVerts)),
  Pos1 is RawDataOffset+12+SkinWeightsRawPtr,
  seek(Stream,Pos1,bof,_),
  get_skinweights(File,Stream,NodeRef,NVerts,0),
  Pos2 is RawDataOffset+12+SkinBoneRefRawPtr,
  seek(Stream,Pos2,bof,_),
  get_skinbonerefs(File,Stream,NodeRef,NVerts,0),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,weights(NVerts))),
  /* CM362g: defer resolving bone weights until all nodes have been read */
  /* do_skinbones(File,ModelName,NodeName,NodeRef,NVerts,0), */
  !.

get_node_specific_data(File,Stream,ModelName,NodeName,NodeRef,161) :-
  /* animmesh - the interesting bits go in the animations */
  get_common_mesh_data(File,Stream,ModelName,NodeName,NodeRef),
  !.

get_node_specific_data(File,Stream,ModelName,NodeName,NodeRef,289) :-
  /* danglymesh */
  get_common_mesh_data(File,Stream,ModelName,NodeName,NodeRef),
  get_int32(Stream,ConstraintPtr),
  get_int32(Stream,NumConstraints),
  ignore_bytes(4,Stream),
  get_float(Stream,Displacement), assertz(gotdata(File,ModelName,NodeName,NodeRef,displacement(Displacement))),
  get_float(Stream,Tightness), assertz(gotdata(File,ModelName,NodeName,NodeRef,tightness(Tightness))),
  get_float(Stream,Period), assertz(gotdata(File,ModelName,NodeName,NodeRef,period(Period))),
  get_float1array(File,Stream,ModelName,NodeName,NodeRef,ConstraintPtr,NumConstraints,'constraints'),
  !.

get_node_specific_data(File,Stream,ModelName,NodeName,NodeRef,545) :- 
  /* aabb - discard the tree and rebuild */
  get_common_mesh_data(File,Stream,ModelName,NodeName,NodeRef),
  !.

get_node_specific_data(_,_,_,_,_,_) :- !.

get_common_mesh_data(File,Stream,ModelName,NodeName,NodeRef) :-
  ignore_bytes(8,Stream),
  get_int32(Stream,FaceArrayPtr),
  get_int32(Stream,NFaces),
  ignore_bytes(44,Stream),
  get_float(Stream,DiffuseR),
  get_float(Stream,DiffuseG),
  get_float(Stream,DiffuseB),
  get_float(Stream,AmbientR),
  get_float(Stream,AmbientG),
  get_float(Stream,AmbientB),
  get_float(Stream,SpecularR),
  get_float(Stream,SpecularG),
  get_float(Stream,SpecularB),
  get_float(Stream,Shininess),
  get_int32(Stream,Shadow),
  get_int32(Stream,Beaming),
  get_int32(Stream,Render),
  get_int32(Stream,TransparencyHint),
  get_int32(Stream,RenderHint),
  get_char64(Stream,Bitmap),
  byte_count(Stream,Here),
  get_char64(Stream,Texture1),
  get_char64(Stream,Texture2),
  get_char64(Stream,MaterialName),
  get_int32(Stream,Tilefade),
  ignore_bytes(24,Stream),
  get_int32(Stream,VertIndCntPtr),
  get_int32(Stream,NumVertIndCnt),
  ignore_bytes(4,Stream),
  get_int32(Stream,VertIndOffPtr),
  get_int32(Stream,NumVertIndOff),
  ignore_bytes(4,Stream),
  ignore_bytes(16,Stream),
  get_int32(Stream,VertexRawDataPtr),
  get_int16(Stream,VertexCount),
  get_int16(Stream,TextureCount),
  get_int32(Stream,Tverts0RawDataPtr),
  get_int32(Stream,Tverts1RawDataPtr),
  get_int32(Stream,Tverts2RawDataPtr),
  get_int32(Stream,Tverts3RawDataPtr),
  get_int32(Stream,VertexNormalsPtr),
  get_int32(Stream,VertexColorsPtr),
  ignore_bytes(12,Stream),
  get_int32(Stream,VertexTangentsPtr),
  ignore_bytes(4,Stream),
  get_int32(Stream,VertexBitangentsPtr),
  get_byte(Stream,LightMapped),
  get_byte(Stream,RotateTexture),
  ignore_bytes(10,Stream),
  byte_count(Stream,Return),
  get_vertexsets(File,Stream,ModelName,NodeName,NodeRef,VertIndCntPtr,NumVertIndCnt,VertIndOffPtr,NumVertIndOff),
  get_vertices(File,Stream,ModelName,NodeName,NodeRef,VertexRawDataPtr,VertexCount),
  get_tverts(File,Stream,ModelName,NodeName,NodeRef,Tverts0RawDataPtr,VertexCount,'tverts'),
  (TextureCount > 1 -> get_tverts(File,Stream,ModelName,NodeName,NodeRef,Tverts1RawDataPtr,VertexCount,'tverts1') ; true),
  (TextureCount > 2 -> get_tverts(File,Stream,ModelName,NodeName,NodeRef,Tverts2RawDataPtr,VertexCount,'tverts2') ; true),
  (TextureCount > 3 -> get_tverts(File,Stream,ModelName,NodeName,NodeRef,Tverts3RawDataPtr,VertexCount,'tverts3') ; true),
  get_vertexnormals(File,Stream,ModelName,NodeName,NodeRef,VertexNormalsPtr,VertexCount),
  get_vertexcolors(File,Stream,ModelName,NodeName,NodeRef,VertexColorsPtr,VertexCount),
  get_vertexbitangents(File,Stream,VertexBitangentsPtr,VertexCount,VertexBitangentsList),
  get_vertextangents(File,Stream,ModelName,NodeName,NodeRef,VertexTangentsPtr,VertexBitangentsList,VertexCount),
  get_faces(File,Stream,ModelName,NodeName,NodeRef,FaceArrayPtr,NFaces),
  seek(Stream,Return,bof,_),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,diffuse(DiffuseR,DiffuseG,DiffuseB))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,ambient(AmbientR,AmbientG,AmbientB))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,specular(SpecularR,SpecularG,SpecularB))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,shininess(Shininess))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,shadow(Shadow))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,beaming(Beaming))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,render(Render))),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,transparencyhint(TransparencyHint))),
  (RenderHint\=0, RenderHint\=2 -> assertz(gotdata(File,ModelName,NodeName,NodeRef,renderhint('None'))) ; true),
  (RenderHint==2 -> assertz(gotdata(File,ModelName,NodeName,NodeRef,renderhint('NormalAndSpecMapped'))) ; true),
  (Bitmap @> '' -> assertz(gotdata(File,ModelName,NodeName,NodeRef,bitmap(Bitmap))) ; true),
  (Bitmap @> '' -> assertz(gotdata(File,ModelName,NodeName,NodeRef,texture0(Bitmap))) ; true),
  (Texture1 @> '' -> assertz(gotdata(File,ModelName,NodeName,NodeRef,texture1(Texture1))) ; true),
  (Texture2 @> '' -> assertz(gotdata(File,ModelName,NodeName,NodeRef,texture2(Texture2))) ; true),
  (MaterialName @> '' -> assertz(gotdata(File,ModelName,NodeName,NodeRef,materialname(MaterialName))) ; true),
  (Tilefade\=0 -> assertz(gotdata(File,ModelName,NodeName,NodeRef,tilefade(Tilefade))) ; true),
  (LightMapped\=0 -> assertz(gotdata(File,ModelName,NodeName,NodeRef,lightmapped(LightMapped))) ; true),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,rotatetexture(RotateTexture))),
  There1 is Here,
  assertz(name_checked(File,ModelName,dummy,There1,64)),
  There2 is Here+64,
  assertz(name_checked(File,ModelName,dummy,There2,64)),
  There3 is Here+128,
  assertz(name_checked(File,ModelName,dummy,There3,64)),
  !.

/* ===================== */
/* get_bonepartnumbers/5 */
/* get_skinweights/5     */
/* get_skinbonerefs/5    */
/* do_skinbones/6        */
/* do_skinbones/5        */
/* ===================== */

get_bonepartnumbers(_,_,_,NBones,ThisBone) :- ThisBone>=NBones, !.

get_bonepartnumbers(File,Stream,NodeRef,NBones,ThisBone) :-
  get_int16(Stream,Part),
  assertz(gotbinary(NodeRef,File,bonepartnumber(ThisBone,Part))),
  NextBone is ThisBone+1,
  !,
  get_bonepartnumbers(File,Stream,NodeRef,NBones,NextBone).

get_skinweights(_,_,_,NVerts,ThisVert) :- ThisVert>=NVerts, !.

get_skinweights(File,Stream,NodeRef,NVerts,ThisVert) :-
  get_float_list(Stream,4,[W1,W2,W3,W4]),
  assertz(gotbinary(NodeRef,File,skinweights(ThisVert,W1,W2,W3,W4))),
  NextVert is ThisVert+1,
  !,
  get_skinweights(File,Stream,NodeRef,NVerts,NextVert).

get_skinbonerefs(_,_,_,NVerts,ThisVert) :- ThisVert>=NVerts, !.

get_skinbonerefs(File,Stream,NodeRef,NVerts,ThisVert) :-
  get_int16(Stream,B1),
  get_int16(Stream,B2),
  get_int16(Stream,B3),
  get_int16(Stream,B4),
  assertz(gotbinary(NodeRef,File,skinbonerefs(ThisVert,B1,B2,B3,B4))),
  NextVert is ThisVert+1,
  !,
  get_skinbonerefs(File,Stream,NodeRef,NVerts,NextVert).

do_skinbones(_,_,_,_,NVerts,ThisVert) :- ThisVert>=NVerts, !.

do_skinbones(File,Model,NodeName,NodeRef,NVerts,ThisVert) :-
  gotbinary(NodeRef,File,skinweights(ThisVert,W1,W2,W3,W4)),
  gotbinary(NodeRef,File,skinbonerefs(ThisVert,B1,B2,B3,B4)), 
  do_skinbones(File,NodeRef,W1,B1,L1),
  do_skinbones(File,NodeRef,W2,B2,L2),
  do_skinbones(File,NodeRef,W3,B3,L3),
  do_skinbones(File,NodeRef,W4,B4,L4),
  flatten([L1,L2,L3,L4],List),
  assertz(gotdata(File,Model,NodeName,NodeRef,weights(ThisVert,List))),
  NextVert is ThisVert+1,
  !,
  do_skinbones(File,Model,NodeName,NodeRef,NVerts,NextVert).  

do_skinbones(_,_,Weight,_,[]) :- Weight=:=0.0, !.

do_skinbones(_,_,_,BoneRef,[]) :- BoneRef<0, !.

do_skinbones(_,_,_,BoneRef,[]) :- BoneRef>64, !.

do_skinbones(File,NodeRef,Weight,BoneRef,List) :-
  gotbinary(NodeRef,File,bonepartnumber(BoneRef,BonePartNumber)),
  B is BonePartNumber+1,
  nth_clause(nth_node(_),B,Ref),
  clause(nth_node(BNodeRef),true,Ref),
  clause(gotdata(File,_,node(_,BoneName)),true,BNodeRef),
  List=[BoneName,Weight],
  !.

/* ==================== */
/* get_flaretextnames/6 */
/* get_flaretextnames/8 */
/* ==================== */

get_flaretextnames(_,_,_,_,_,[]) :- !.

get_flaretextnames(File,Stream,ModelName,NodeName,NodeRef,PtrList) :-
  length(PtrList,N),
  getflaretextnames(File,Stream,ModelName,NodeName,NodeRef,PtrList,N,0),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,texturenames(N))),
  !.

getflaretextnames(_,_,_,_,_,_,N,This) :- This>=N, !.

getflaretextnames(_,_,_,_,_,[],_,_) :- !.

getflaretextnames(File,Stream,ModelName,NodeName,NodeRef,[H|T],N,This) :-
  Start is H+12,
  seek(Stream,Start,bof,_),
  get_char64(Stream,TexName),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,texturenames(This,TexName))),
  Next is This+1,
  !,
  getflaretextnames(File,Stream,ModelName,NodeName,NodeRef,T,N,Next).

/* ==================== */
/* get_faces/7          */
/* get_faces1/7         */
/* get_vertices/7       */
/* get_vertices1/7      */
/* get_tverts/8         */
/* get_tverts1/8        */
/* get_vertexnormals/7  */
/* get_vertexnormals1/7 */
/* get_vertexcolors/7   */
/* get_vertexcolors1/7  */
/* get_vertexbitangents/5  */
/* get_vertexbitangent_list/4  */
/* get_vertextangents/7  */
/* get_vertextangents1/7 */
/* ==================== */

get_faces(_,_,_,_,_,0,_) :- !.

get_faces(_,_,_,_,_,_,0) :- !.

get_faces(File,Stream,ModelName,NodeName,NodeRef,FaceArrayPtr,NFaces) :-
  Start is FaceArrayPtr+12,
  seek(Stream,Start,bof,_),
  get_faces1(File,Stream,ModelName,NodeName,NodeRef,NFaces,0),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,faces(NFaces))),
  assign_smoothing_groups(NodeRef),
  !.

get_faces1(_,_,_,_,_,NFaces,ThisFace) :- ThisFace>=NFaces, !.

get_faces1(File,Stream,ModelName,NodeName,NodeRef,NFaces,ThisFace) :-
 get_float(Stream,Nx),
 get_float(Stream,Ny),
 get_float(Stream,Nz),
 get_float(Stream,D),
 get_int32(Stream,SurfaceID),
 get_int16(Stream,Adj1),
 get_int16(Stream,Adj2),
 get_int16(Stream,Adj3),
 get_int16(Stream,V1),
 get_int16(Stream,V2),
 get_int16(Stream,V3),
 assertz(gotbinary(NodeRef,File,face_data(ThisFace,Nx,Ny,Nz,D,SurfaceID,Adj1,Adj2,Adj3,V1,V2,V3))),
 assertz(gotdata(File,ModelName,NodeName,NodeRef,faces(ThisFace,V1,V2,V3,0,V1,V2,V3,SurfaceID))),
 NextFace is ThisFace+1,
 !,
 get_faces1(File,Stream,ModelName,NodeName,NodeRef,NFaces,NextFace).

get_vertices(_,_,_,_,_,-1,_) :- !.

get_vertices(_,_,_,_,_,_,0) :- !.

get_vertices(File,Stream,ModelName,NodeName,NodeRef,VertexRawDataPtr,VertexCount) :-
  gotbinary(File,file_header(_,RawDataOffset,_)),
  Start is RawDataOffset+12+VertexRawDataPtr,
  seek(Stream,Start,bof,_),
  get_vertices1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,0),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,verts(VertexCount))),
  !.

get_vertices1(_,_,_,_,_,VertexCount,ThisVertex) :- ThisVertex>=VertexCount, !.

get_vertices1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,ThisVertex) :-
 get_float(Stream,X),
 get_float(Stream,Y),
 get_float(Stream,Z),
 assertz(gotdata(File,ModelName,NodeName,NodeRef,verts(ThisVertex,X,Y,Z))),
 NextVertex is ThisVertex+1,
 !,
 get_vertices1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,NextVertex).
  
get_tverts(_,_,_,_,_,-1,_,_) :- !.

get_tverts(_,_,_,_,_,_,0,_) :- !.

get_tverts(File,Stream,ModelName,NodeName,NodeRef,RawDataPtr,VertexCount,Parm) :-
  gotbinary(File,file_header(_,RawDataOffset,_)),
  Start is RawDataOffset+12+RawDataPtr,
  seek(Stream,Start,bof,_),
  get_tverts1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,0,Parm),
  Q=..[Parm,VertexCount],
  assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)),
  !.

get_tverts1(_,_,_,_,_,VertexCount,ThisVertex,_) :- ThisVertex>=VertexCount, !.

get_tverts1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,ThisVertex,Parm) :-
 get_float(Stream,U),
 get_float(Stream,V),
 Q=..[Parm,ThisVertex,U,V,0],
 assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)),
 NextVertex is ThisVertex+1,
 !,
 get_tverts1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,NextVertex,Parm).
  
get_vertexnormals(_,_,_,_,_,-1,_) :- !.

get_vertexnormals(_,_,_,_,_,_,0) :- !.

get_vertexnormals(File,Stream,ModelName,NodeName,NodeRef,VertexNormalsPtr,VertexCount) :-
  gotbinary(File,file_header(_,RawDataOffset,_)),
  Start is RawDataOffset+12+VertexNormalsPtr,
  seek(Stream,Start,bof,_),
  get_vertexnormals1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,0),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,normals(VertexCount))),
  !.

get_vertexnormals1(_,_,_,_,_,VertexCount,ThisVertex) :- ThisVertex>=VertexCount, !.

get_vertexnormals1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,ThisVertex) :-
 get_float(Stream,X),
 get_float(Stream,Y),
 get_float(Stream,Z),
 assertz(gotbinary(NodeRef,File,vertex_normals(ThisVertex,X,Y,Z))),
 assertz(gotdata(File,ModelName,NodeName,NodeRef,normals(ThisVertex,X,Y,Z))),
 NextVertex is ThisVertex+1,
 !,
 get_vertexnormals1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,NextVertex).
  
get_vertexcolors(_,_,_,_,_,-1,_) :- !.

get_vertexcolors(_,_,_,_,_,_,0) :- !.

get_vertexcolors(File,Stream,ModelName,NodeName,NodeRef,VertexColorsPtr,VertexCount) :-
  gotbinary(File,file_header(_,RawDataOffset,_)),
  Start is RawDataOffset+12+VertexColorsPtr,
  seek(Stream,Start,bof,_),
  get_vertexcolors1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,0),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,colors(VertexCount))),
  !.

get_vertexcolors1(_,_,_,_,_,VertexCount,ThisVertex) :- ThisVertex>=VertexCount, !.

get_vertexcolors1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,ThisVertex) :-
 get_byte(Stream,R), /* R1 is R/255, */
 get_byte(Stream,G), /* G1 is G/255, */
 get_byte(Stream,B), /* B1 is B/255, */
 get_byte(Stream,_),
 /* assertz(gotdata(File,ModelName,NodeName,NodeRef,colors(ThisVertex,R1,G1,B1))), */
 assertz(gotdata(File,ModelName,NodeName,NodeRef,colors(ThisVertex,R,G,B))),
 NextVertex is ThisVertex+1,
 !,
 get_vertexcolors1(File,Stream,ModelName,NodeName,NodeRef,VertexCount,NextVertex).
  
get_vertexbitangents(_,_,-1,_,_) :- !.

get_vertexbitangents(_,_,_,0,_) :- !.

get_vertexbitangents(File,Stream,VertexBitangentsPtr,VertexCount,VertexBitangentList) :-
  gotbinary(File,file_header(_,RawDataOffset,_)),
  Start is RawDataOffset+12+VertexBitangentsPtr,
  seek(Stream,Start,bof,_),
  get_vertexbitangent_list(Stream,VertexCount,0,VertexBitangentList),
  !.

get_vertexbitangent_list(_,Num,This,[]) :- This>=Num, !.

get_vertexbitangent_list(Stream,Num,This,[H|T]) :-
  get_float(Stream,H),
  Next is This+1,
  !,
  get_vertexbitangent_list(Stream,Num,Next,T).
  
get_vertextangents(_,_,_,_,_,-1,_,_) :- !.

get_vertextangents(_,_,_,_,_,_,[],_) :- !.

get_vertextangents(_,_,_,_,_,_,_,0) :- !.

get_vertextangents(File,Stream,ModelName,NodeName,NodeRef,VertexTangentsPtr,VertexBitangentsList,VertexCount) :-
  gotbinary(File,file_header(_,RawDataOffset,_)),
  Start is RawDataOffset+12+VertexTangentsPtr,
  seek(Stream,Start,bof,_),
  get_vertextangents1(File,Stream,ModelName,NodeName,NodeRef,VertexBitangentsList,VertexCount,0),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,tangents(VertexCount))),
  !.

get_vertextangents1(_,_,_,_,_,_,VertexCount,ThisVertex) :- ThisVertex>=VertexCount, !.

get_vertextangents1(File,Stream,ModelName,NodeName,NodeRef,VertexBitangentsList,VertexCount,ThisVertex) :-
 get_float(Stream,X),
 get_float(Stream,Y),
 get_float(Stream,Z),
 get_list_element(VertexBitangentsList,ThisVertex,Bitangent),
 assertz(gotdata(File,ModelName,NodeName,NodeRef,tangents(ThisVertex,X,Y,Z,Bitangent))),
 NextVertex is ThisVertex+1,
 !,
 get_vertextangents1(File,Stream,ModelName,NodeName,NodeRef,VertexBitangentsList,VertexCount,NextVertex).
  
/* ======================= */
/* get_vertexsets/9        */
/* get_vertex_index_list/4 */
/* get_vertexsets1/9       */
/* get_vertexsets2/8       */
/* ======================= */

get_vertexsets(_,_,_,_,_,0,_,_,_) :- !.
get_vertexsets(_,_,_,_,_,_,0,_,_) :- !.
get_vertexsets(_,_,_,_,_,_,_,0,_) :- !.
get_vertexsets(_,_,_,_,_,_,_,_,0) :- !.

get_vertexsets(File,Stream,ModelName,NodeName,NodeRef,VertIndCntPtr,NumVertIndCnt,VertIndOffPtr,NumVertIndOff) :-
  NumSets is min(NumVertIndCnt,NumVertIndOff),
  Pos1 is VertIndCntPtr+12,
  seek(Stream,Pos1,bof,_),
  get_vertex_index_list(Stream,NumSets,0,VertexIndexCountList),
  Pos2 is VertIndOffPtr+12,
  seek(Stream,Pos2,bof,_),
  get_vertex_index_list(Stream,NumSets,0,VertexIndexPtrList),
  get_vertexsets1(File,Stream,ModelName,NodeName,NodeRef,VertexIndexCountList,VertexIndexPtrList,NumSets,0),
  !.

get_vertex_index_list(_,Num,This,[]) :- This>=Num, !.

get_vertex_index_list(Stream,Num,This,[H|T]) :-
  get_int32(Stream,H),
  Next is This+1,
  !,
  get_vertex_index_list(Stream,Num,Next,T).

get_vertexsets1(_,_,_,_,_,[],[],NumSets,ThisSet) :- ThisSet>=NumSets, !.

get_vertexsets1(File,Stream,ModelName,NodeName,NodeRef,[_|Counts],[_|Ptrs],NumSets,0) :-
  !,
  get_vertexsets1(File,Stream,ModelName,NodeName,NodeRef,Counts,Ptrs,NumSets,1).

get_vertexsets1(File,Stream,ModelName,NodeName,NodeRef,[Count|Counts],[Ptr|Ptrs],NumSets,ThisSet) :-
  gotbinary(File,file_header(_,RawDataOffset,_)),
  atom_concat('texindices',ThisSet,Param),
  NRows is Count//3,
  Start is RawDataOffset+Ptr+12,
  seek(Stream,Start,bof,_),
  get_vertexsets2(File,Stream,ModelName,NodeName,NodeRef,Param,NRows,0),
  Q=..[Param,NRows],
  assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)),
  NextSet is ThisSet+1,
  !,
  get_vertexsets1(File,Stream,ModelName,NodeName,NodeRef,Counts,Ptrs,NumSets,NextSet).
 
get_vertexsets2(_,_,_,_,_,_,NRows,ThisRow) :- ThisRow>=NRows, !.

get_vertexsets2(File,Stream,ModelName,NodeName,NodeRef,Param,NRows,ThisRow) :-
  get_int16(Stream,V1),
  get_int16(Stream,V2),
  get_int16(Stream,V3),
  Q=..[Param,ThisRow,V1,V2,V3],
  assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)),
  NextRow is ThisRow+1,
  !,
  get_vertexsets2(File,Stream,ModelName,NodeName,NodeRef,Param,NRows,NextRow).
  
/* ========================= */
/* assign_smoothing_groups/1 */
/* ========================= */

assign_smoothing_groups(NodeRef) :-
  sg_graph(NodeRef,Graph),
  sg_extract(Graph,Groups),
  sg_graph2(NodeRef,Graph2),
  sg_assign(NodeRef,Groups,Graph2),
  !.

assign_smoothing_groups(NodeRef) :-
  clause(gotdata(_,_,node(_,NodeName)),true,NodeRef),
  write('CM3 error: failed to allocate smoothing groups to '), write(NodeName), nl.

sg_assign(_,[],_) :- !.

sg_assign(NodeRef,[G|G0],Graph2) :-
  sg_collapse(G,Graph2,NewGraph2,F0),
  sg_allocate_number(NodeRef,NewGraph2,F0,GNumber),
  sg_assign1(NodeRef,G,GNumber),
  assertz(sg_assigned(NodeRef,GNumber,G)),
  !,
  sg_assign(NodeRef,G0,NewGraph2).

sg_assign1(_,[],_) :- !.

sg_assign1(NodeRef,[F|F0],G) :-
  clause(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,_,T1,T2,T3,M)),true,CRef),
  erase(CRef),
  assertz(gotdata(File,Model,NodeName,NodeRef,faces(F,V1,V2,V3,G,T1,T2,T3,M))),
  !,
  sg_assign1(NodeRef,F0,G).  
  
sg_collapse([F0],Graph2,Graph2,F0) :- !.

sg_collapse([F0|G1],Graph2,NewGraph2,F0) :-
  /* changed in V3.5.1 because V6 of SWI-Prolog changed the argument order of del_vertices */
  vertices(Graph2,All),
  ord_subtract(All,[F0|G1],Others),
  Others\=[],
  !,
  del_vertices(Graph2,Others,IntGraph),
  edges(IntGraph,IntEdges),
  del_edges(Graph2,IntEdges,ExtGraph),
  compose([F0-G1],ExtGraph,OutEdgeGraph),
  edges(OutEdgeGraph,E1),
  transpose_ugraph(OutEdgeGraph,InEdgeGraph),
  edges(InEdgeGraph,E2),
  ord_union(E1,E2,CrossEdges),
  del_vertices(ExtGraph,G1,RimGraph),
  add_edges(RimGraph,CrossEdges,NewGraph2).

sg_collapse([F0|_],_,[],F0) :- !.
  
sg_allocate_number(_,[],_,1) :- !.

sg_allocate_number(NodeRef,Graph2,F0,GNumber) :-
  neighbours(F0,Graph2,Neighbours),
  between(0,31,GBit),
  GNumber is 1 << GBit,
  (sg_assigned(NodeRef,GNumber,[F1|_]) -> \+member(F1,Neighbours) ; true),  
  !.

sg_extract(Graph,[G|G0]) :-
  /* changed in V3.5.1 because V6 of SWI-Prolog changed the argument order of del_vertices */
  Graph = [F-_|_],
  reachable(F,Graph,G),
  del_vertices(Graph,G,NewGraph),
  !,
  sg_extract(NewGraph,G0).

sg_extract(_,[]).

sg_graph(NodeRef,Graph) :-
  setof(F1-F2,sg_smooth(NodeRef,F1,F2),S),
  !,
  vertices_edges_to_ugraph([],S,Graph).

sg_graph(_,[]).

sg_graph2(NodeRef,Graph) :-
  setof(F1-F2,sg_adj(NodeRef,F1,F2),S),
  !,
  vertices_edges_to_ugraph([],S,Graph).

sg_graph2(_,[]).

sg_smooth(NodeRef,F1,F2) :-
  sg_adj(NodeRef,F1,F2,V1a,V2a),
  sg_adj(NodeRef,F2,F1,V2b,V1b),
  gotbinary(NodeRef,_,vertex_normals(V1a,X1,Y1,Z1)),
  gotbinary(NodeRef,_,vertex_normals(V1b,X1,Y1,Z1)),
  gotbinary(NodeRef,_,vertex_normals(V2a,X2,Y2,Z2)),
  gotbinary(NodeRef,_,vertex_normals(V2b,X2,Y2,Z2)).

sg_adj(NodeRef,F1,F2) :- gotbinary(NodeRef,_,face_data(F1,_,_,_,_,_,F2,_,_,_,_,_)), F2\= -1.
sg_adj(NodeRef,F1,F2) :- gotbinary(NodeRef,_,face_data(F1,_,_,_,_,_,_,F2,_,_,_,_)), F2\= -1.
sg_adj(NodeRef,F1,F2) :- gotbinary(NodeRef,_,face_data(F1,_,_,_,_,_,_,_,F2,_,_,_)), F2\= -1.

sg_adj(NodeRef,F1,F2,V1,V2) :- gotbinary(NodeRef,_,face_data(F1,_,_,_,_,_,F2,_,_,V1,V2,_)), F2\= -1.
sg_adj(NodeRef,F1,F2,V1,V2) :- gotbinary(NodeRef,_,face_data(F1,_,_,_,_,_,_,F2,_,_,V1,V2)), F2\= -1.
sg_adj(NodeRef,F1,F2,V1,V2) :- gotbinary(NodeRef,_,face_data(F1,_,_,_,_,_,_,_,F2,V2,_,V1)), F2\= -1.

/* ================================== */
/* get_animation_node_specific_data/7 */
/* get_animvert_list/10               */
/* get_animtvert_list/10              */
/* ================================== */

get_animation_node_specific_data(File,Stream,ModelName,NodeName,NodeRef,AnimName,161) :-
  byte_count(Stream,Here),
  There1 is Here+184,
  assertz(name_checked(File,ModelName,dummy,There1,64)),
  There2 is Here+248,
  assertz(name_checked(File,ModelName,dummy,There2,64)),
  There3 is Here+312,
  assertz(name_checked(File,ModelName,dummy,There3,64)),
  ignore_bytes(448,Stream),
  get_int16(Stream,VertexCount), /* In binary, nverts = ntverts */
  ignore_bytes(62,Stream),
  get_float(Stream,SamplePeriod),
  assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,sampleperiod(SamplePeriod))),
  ignore_bytes(36,Stream),
  get_int32(Stream,VertexPtr),
  get_int32(Stream,TvertPtr),
  get_int32(Stream,NVertexSets),
  get_int32(Stream,NTvertSets),
  Nanimverts is VertexCount*NVertexSets,
  assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,animverts(Nanimverts))),
  Start1 is 12+VertexPtr,
  seek(Stream,Start1,bof,_),
  get_animvert_list(File,Stream,ModelName,NodeName,NodeRef,AnimName,VertexCount,0,NVertexSets,0),
  Nanimtverts is VertexCount*NTvertSets,
  assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,animtverts(Nanimtverts))),
  Start2 is 12+TvertPtr,
  seek(Stream,Start2,bof,_),
  get_animtvert_list(File,Stream,ModelName,NodeName,NodeRef,AnimName,VertexCount,0,NTvertSets,0),
  !.

get_animation_node_specific_data(_,Stream,ModelName,_,_,_,BNodeType) :-
  BNodeType /\ 32 =:= 32,
  byte_count(Stream,Here),
  There1 is Here+184,
  assertz(name_checked(File,ModelName,dummy,There1,64)),
  There2 is Here+248,
  assertz(name_checked(File,ModelName,dummy,There2,64)),
  There3 is Here+312,
  assertz(name_checked(File,ModelName,dummy,There3,64)),
  !.

get_animation_node_specific_data(_,_,_,_,_,_,_) :- !.


get_animvert_list(_,_,_,_,_,_,NVerts,ThisVert,_,_) :- ThisVert>=NVerts, !.

get_animvert_list(_,_,_,_,_,_,_,_,NSets,ThisSet) :- ThisSet>=NSets, !.

get_animvert_list(File,Stream,ModelName,NodeName,NodeRef,AnimName,NVerts,ThisVert,NSets,ThisSet) :-
  get_float(Stream,X),
  get_float(Stream,Y),
  get_float(Stream,Z),
  ThisOne is NVerts*ThisSet+ThisVert,
  assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,animverts(ThisOne,X,Y,Z))),
  NextSet is ThisSet+1,
  NextVert is ThisVert+1,
  !,
  (NextSet<NSets -> !, get_animvert_list(File,Stream,ModelName,NodeName,NodeRef,AnimName,NVerts,ThisVert,NSets,NextSet) ;
   NextVert<NVerts -> !, get_animvert_list(File,Stream,ModelName,NodeName,NodeRef,AnimName,NVerts,NextVert,NSets,0) ; !).

get_animtvert_list(_,_,_,_,_,_,NVerts,ThisVert,_,_) :- ThisVert>=NVerts, !.

get_animtvert_list(_,_,_,_,_,_,_,_,NSets,ThisSet) :- ThisSet>=NSets, !.

get_animtvert_list(File,Stream,ModelName,NodeName,NodeRef,AnimName,NVerts,ThisVert,NSets,ThisSet) :-
  get_float(Stream,U),
  get_float(Stream,V),
  ThisOne is NVerts*ThisSet+ThisVert,
  assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,animtverts(ThisOne,U,V,0))),
  NextSet is ThisSet+1,
  NextVert is ThisVert+1,
  !,
  (NextSet<NSets -> !, get_animtvert_list(File,Stream,ModelName,NodeName,NodeRef,AnimName,NVerts,ThisVert,NSets,NextSet) ;
   NextVert<NVerts -> !, get_animtvert_list(File,Stream,ModelName,NodeName,NodeRef,AnimName,NVerts,NextVert,NSets,0) ; !).

/* ========================== */
/* get_controllers/7          */
/* get_controllers/8          */
/* get_controller_structure/7 */
/* ========================== */

get_controllers(File,Stream,ModelName,NodeName,BNodeType,NodeRef,[C_Key_StartPtr,C_Key_NumElements,C_Data_StartPtr,_]) :-
  get_controllers(File,Stream,ModelName,NodeName,BNodeType,NodeRef,[],[C_Key_StartPtr,C_Key_NumElements,C_Data_StartPtr,_]).

get_controllers(_,_,_,_,_,_,_,[_,0,_,_]) :- !.

get_controllers(_,_,_,_,_,_,_,[C_Key_StartPtr,_,_,_]) :- C_Key_StartPtr =:= 0, !.

get_controllers(File,Stream,ModelName,NodeName,BNodeType,NodeRef,AnimName,[C_Key_StartPtr,C_Key_NumElements,C_Data_StartPtr,_]) :-
  get_controller_structure(Stream,C_Key_StartPtr,ControllerType,NumRows,FirstTimeKeyIndex,FirstDataValueIndex,NumCols),
  get_controller_data(File,Stream,ModelName,NodeName,BNodeType,NodeRef,AnimName,ControllerType,NumRows,FirstTimeKeyIndex,FirstDataValueIndex,NumCols,C_Data_StartPtr),
  Next_Key_StartPtr is C_Key_StartPtr+12,
  Next_Key_NumElements is C_Key_NumElements-1,
  !,
  get_controllers(File,Stream,ModelName,NodeName,BNodeType,NodeRef,AnimName,[Next_Key_StartPtr,Next_Key_NumElements,C_Data_StartPtr,_]).

get_controller_structure(_,C_Key_StartPtr,0,0,0,0,0) :- C_Key_StartPtr =:= 0, !.

get_controller_structure(Stream,C_Key_StartPtr,ControllerType,NumRows,FirstTimeKeyIndex,FirstDataValueIndex,NumCols) :-
  Start is C_Key_StartPtr+12,
  seek(Stream,Start,bof,_),
  get_int32(Stream,ControllerType),
  get_int16(Stream,NumRows),
  get_int16(Stream,FirstTimeKeyIndex),
  get_int16(Stream,FirstDataValueIndex),
  get_byte(Stream,NumCols),
  get_byte(Stream,_),
  !.

/* ================================ */
/* get_controller_data/13           */
/* ================================ */

get_controller_data(_,_,_,_,_,_,_,_,NumRows,_,_,_,_) :- NumRows=<0, !.

get_controller_data(_,_,_,_,BNodeType,_,_,ControllerType,_,_,_,_,_) :-
  \+ controller_type(ControllerType,BNodeType,_),
  !,
  write('Unknown controller '), write(ControllerType), write(' ignored'), nl.

get_controller_data(_,_,_,_,_,_,_,_,_,_,_,_,C_Data_StartPtr) :- C_Data_StartPtr =:= 0, !.

get_controller_data(File,Stream,ModelName,NodeName,BNodeType,NodeRef,AnimName,ControllerType,NumRows,FirstTimeKeyIndex,FirstDataValueIndex,NumCols,C_Data_StartPtr) :-
  once(controller_type(ControllerType,BNodeType,Parameter)),
  Bezier is NumCols /\ 16,
  TrueNumCols is NumCols /\ 15,
  get_controller_data_row(Stream,FirstTimeKeyIndex,FirstDataValueIndex,TrueNumCols,C_Data_StartPtr,TimeValue,FloatList,NextTimeAbsPtr,NextFloatAbsPtr),
  get_controller_data_start(Stream,File,ModelName,NodeName,NodeRef,AnimName,Parameter,Bezier,NumRows,TrueNumCols,TimeValue,FloatList,NextTimeAbsPtr,NextFloatAbsPtr).

/* ================================ */
/* get_controller_data_row/9        */
/* get_controller_data_start/14     */
/* get_controller_data_continue/12  */
/* ================================ */

get_controller_data_row(_,_,_,_,C_Data_StartPtr,0.0,[],0,0) :- C_Data_StartPtr =:= 0, !.

get_controller_data_row(_,_,_,NumCols,_,0.0,[],0,0) :- NumCols =:= 15, !.

get_controller_data_row(Stream,TimeKeyIndex,DataValueIndex,NumCols,C_Data_StartPtr,TimeValue,FloatList,NextTimeAbsPtr,NextFloatAbsPtr) :-
  Start1 is 12 + C_Data_StartPtr + 4*TimeKeyIndex,
  seek(Stream,Start1,bof,_),
  get_float(Stream,TimeValue),
  byte_count(Stream,NextTimeAbsPtr),
  Start2 is 12 + C_Data_StartPtr + 4*DataValueIndex,
  seek(Stream,Start2,bof,_),
  get_float_list(Stream,NumCols,RawFloatList),
  /* Intercept Quarternions */
  (NumCols=:=4 -> quarternion2angleaxis(RawFloatList,FloatList) ; FloatList=RawFloatList),
  byte_count(Stream,NextFloatAbsPtr),
  !.

/* V3.4.1k: We no longer assert single keys as static parameters except in geometry (AnimName==[]) */
get_controller_data_start(_,File,ModelName,NodeName,NodeRef,[],Parameter,_,NumRows,_,TimeValue,FloatList,_,_) :-
  NumRows =:= 1,
  TimeValue=:=0.0,
  Q=..[Parameter|FloatList],
  assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)),
  !.

get_controller_data_start(Stream,File,ModelName,NodeName,NodeRef,AnimName,Parameter,Bezier,NumRows,NumCols,TimeValue,FloatList,NextTimeAbsPtr,NextFloatAbsPtr) :-
  (Bezier=:=16 -> atom_concat(Parameter,'bezierkey',Q0) ; atom_concat(Parameter,'key',Q0)),
  Q1=..[Q0,NumRows],
  (AnimName==[] -> assertz(gotdata(File,ModelName,NodeName,NodeRef,Q1)) ; assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Q1))),
  Q=..[Q0,0,TimeValue|FloatList],
  (AnimName==[] -> assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)) ; assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Q))),
  !,
  get_controller_data_continue(Stream,File,ModelName,NodeName,NodeRef,AnimName,Q0,NumRows,1,NumCols,NextTimeAbsPtr,NextFloatAbsPtr).

get_controller_data_continue(_,_,_,_,_,_,_,NumRows,ThisRow,_,_,_) :- ThisRow>=NumRows, !.

get_controller_data_continue(Stream,File,ModelName,NodeName,NodeRef,AnimName,Q0,NumRows,ThisRow,NumCols,TimeAbsPtr,FloatAbsPtr) :-
  ThisRow<NumRows,
  seek(Stream,TimeAbsPtr,bof,_),
  get_float(Stream,TimeValue),
  byte_count(Stream,NextTimeAbsPtr),
  seek(Stream,FloatAbsPtr,bof,_),
  get_float_list(Stream,NumCols,RawFloatList),
  /* Intercept Quarternions */
  (NumCols=:=4 -> quarternion2angleaxis(RawFloatList,FloatList) ; FloatList=RawFloatList),
  byte_count(Stream,NextFloatAbsPtr),
  Q=..[Q0,ThisRow,TimeValue|FloatList],
  (AnimName==[] -> assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)) ; assertz(gotdata(File,ModelName,NodeName,NodeRef,AnimName,Q))),
  NextRow is ThisRow+1,
  !,  
  get_controller_data_continue(Stream,File,ModelName,NodeName,NodeRef,AnimName,Q0,NumRows,NextRow,NumCols,NextTimeAbsPtr,NextFloatAbsPtr).

/* ================= */
/* controller_type/3 */
/* ================= */

/* All nodes */
controller_type(8,_,position).
controller_type(20,_,orientation).
controller_type(36,_,scale).
/* Lights */
controller_type(76,3,color).
controller_type(88,3,radius).
controller_type(140,3,multiplier).
controller_type(96,3,shadowradius).
controller_type(100,3,verticaldisplacement).
/* Emitters */
controller_type(84,5,alphastart).
controller_type(448,5,alphamid) :- compiler(bioware).
controller_type(464,5,alphamid) :- compiler(nwnmdlcomp).
controller_type(80,5,alphaend).
controller_type(88,5,birthrate).
controller_type(204,5,blurlength).
controller_type(92,5,bounce_co).
controller_type(108,5,colorstart).
controller_type(452,5,colormid) :- compiler(bioware).
controller_type(468,5,colormid) :- compiler(nwnmdlcomp).
controller_type(96,5,colorend).
controller_type(120,5,combinetime).
controller_type(228,5,detonate). /* Special case because it expects NumCols = -1 */
controller_type(124,5,drag).
controller_type(128,5,fps).
controller_type(136,5,framestart).
controller_type(132,5,frameend).
controller_type(140,5,grav).
controller_type(144,5,lifeexp).
controller_type(208,5,lightningdelay).
controller_type(212,5,lightningradius).
controller_type(216,5,lightningscale).
controller_type(148,5,mass).
controller_type(152,5,p2p_bezier2).
controller_type(156,5,p2p_bezier3).
controller_type(160,5,particlerot).
controller_type(464,5,percentstart) :- compiler(bioware).
controller_type(480,5,percentstart) :- compiler(nwnmdlcomp).
controller_type(465,5,percentmid) :- compiler(bioware).
controller_type(481,5,percentmid) :- compiler(nwnmdlcomp).
controller_type(466,5,percentend) :- compiler(bioware).
controller_type(482,5,percentend) :- compiler(nwnmdlcomp).
controller_type(164,5,randvel).
controller_type(168,5,sizestart).
controller_type(468,5,sizemid) :- compiler(bioware).
controller_type(484,5,sizemid) :- compiler(nwnmdlcomp).
controller_type(172,5,sizeend).
controller_type(176,5,sizestart_y).
controller_type(472,5,sizemid_y) :- compiler(bioware).
controller_type(488,5,sizemid_y) :- compiler(nwnmdlcomp).
controller_type(180,5,sizeend_y).
controller_type(184,5,spread).
controller_type(188,5,threshold).
controller_type(192,5,velocity).
controller_type(196,5,xsize).
controller_type(200,5,ysize).

/* All meshes */
controller_type(100,NodeType,selfillumcolor) :- NodeType /\ 32 =:= 32.
controller_type(128,NodeType,alpha) :- NodeType /\ 32 =:= 32.

/** controller_type(???,5,lightningsubdiv) is known in BW ascii models but not known to nwnmdlcomp **/

/* ============== */
/* ignore_bytes/2 */
/* ============== */

ignore_bytes(N,_) :- N=<0, !.
ignore_bytes(N,Stream) :- get_byte(Stream,_), N1 is N-1, !, ignore_bytes(N1,Stream).

/* =================== */
/* get_int32/2         */
/* get_int16/2         */
/* get_word/3          */
/* =================== */

get_int32(Stream,X) :-
  get_word(Stream,4,U),
  (U>>31 /\ 1 =:= 1 -> X is \ (U xor 4294967295) ; X = U).

get_int16(Stream,X) :-
  get_word(Stream,2,U),
  (U>>15 /\ 1 =:= 1 -> X is \ (U xor 65535) ; X = U).

get_word(Stream,1,X) :- !, get_byte(Stream,X).

get_word(Stream,NBytes,X) :-
  get_byte(Stream,X0),
  N1 is NBytes-1,
  get_word(Stream,N1,X1), 
  X is (X1<<8) \/ X0.

/* ============ */
/* get_char64/2 */
/* get_char32/2 */
/* get_char16/2 */
/* get_string/3 */
/* sniff_byte/1 */
/* ============ */

get_char64(Stream,Atom) :-
  get_string(64,Stream,String),
  name(X,String),
  (X=='NULL' -> Atom=X ; downcase_atom(X,Atom)).

get_char32(Stream,Atom) :-
  get_string(32,Stream,String),
  name(X,String),
  (X=='NULL' -> Atom=X ; downcase_atom(X,Atom)).

get_char16(Stream,Atom) :-
  get_string(16,Stream,String),
  name(X,String),
  (X=='NULL' -> Atom=X ; downcase_atom(X,Atom)).

get_string(N,_,[]) :- N=<0, !.
get_string(_,Stream,[]) :- at_end_of_stream(Stream), !.
get_string(N,Stream,[]) :- peek_byte(Stream,0), !, seek(Stream,N,current,_).
get_string(N,Stream,[H|T]) :- get_byte(Stream,H), sniff_byte(H), N1 is N-1, !, get_string(N1,Stream,T).

sniff_byte(C) :- C<32, halt. /* Crash out if a string contains non-printing characters */
sniff_byte(_).

/* ================ */
/* upcase_special/3 */
/* cm3_is_digit/1   */
/* get_list_element/1 */
/* ================ */

upcase_special(Model,RawNodeName,NodeName) :-
  atom_concat(Model,'_u0',Prefix),
  atom_concat(Prefix,Suffix,RawNodeName),
  cm3_is_digit(Suffix),
  !,
  concat_atom([Model,'_U0',Suffix],NodeName).

upcase_special(Model,RawNodeName,NodeName) :-
  atom_concat(Model,'_d0',Prefix),
  atom_concat(Prefix,Suffix,RawNodeName),
  cm3_is_digit(Suffix),
  !,
  concat_atom([Model,'_D0',Suffix],NodeName).

upcase_special(_,Name,Name).

cm3_is_digit('0').   cm3_is_digit('1').   cm3_is_digit('2').   cm3_is_digit('3').   cm3_is_digit('4').
cm3_is_digit('5').   cm3_is_digit('6').   cm3_is_digit('7').   cm3_is_digit('8').   cm3_is_digit('9').

/* =========== */
/* get_float/2 */
/* =========== */

get_float(Stream,X) :- get_int32(Stream,Z), interpret_as_float(Z,X).

interpret_as_float(Z,X) :-
  SignBit is (Z /\ 2147483648) >> 31,
  (SignBit =:=0 -> Sign is 1.0 ; Sign is -1.0),
  Mantissa is ((Z /\ 8388607) \/ 8388608) / 4194304.0,
  Exponent is (Z /\ 2145386496) >> 23,
  (Exponent =:= 0 -> X is 0.0 ; X is Sign*Mantissa*(2**(Exponent-128))).

/* ================ */
/* get_float_list/3 */
/* ================ */

get_float_list(_,N,[]) :- N=<0, !.

get_float_list(Stream,N,[H|T]) :-
  get_float(Stream,H),
  N1 is N-1,
  get_float_list(Stream,N1,T),
  !.

get_list_element([],N,_) :- N=<0, !.

get_list_element(List,Element,X) :- nth0(Element,List,X).

/* =============== */
/* get_ptr_array/2 */
/* get_ptr_array/3 */
/* =============== */

get_ptr_array(Stream,PtrList) :-
  get_int32(Stream,StartPtr),
  get_int32(Stream,NumElements),
  ignore_bytes(4,Stream),
  check_compiler_fingerprint(StartPtr,NumElements),
  (NumElements =:= 0 -> PtrList=[] ;
   StartPtr =:= 0 -> PtrList = [] ;
    byte_count(Stream,Here),
    Start is StartPtr+12,
    seek(Stream,Start,bof,_),
    get_ptr_array(Stream,NumElements,PtrList),
    seek(Stream,Here,bof,_)
  ),
  !.

get_ptr_array(_,N,[]) :- N=<0, !.

get_ptr_array(Stream,N,[H|T]) :- get_int32(Stream,H), N1 is N-1, !, get_ptr_array(Stream,N1,T).

/* ============================ */
/* check_compiler_fingerprint/2 */
/* ============================ */

check_compiler_fingerprint(_,_) :- compiler(_), !.

check_compiler_fingerprint(0,0) :- !, assertz(compiler(nwnmdlcomp)).

check_compiler_fingerprint(_,0) :- !, assertz(compiler(bioware)).

check_compiler_fingerprint(_,_).

  
/* ===================== */
/* quarternion2angleaxis/2 */
/* ===================== */

quarternion2angleaxis([_,_,_,Q4],[0,0,0,0]) :- abs(Q4)>0.9999, !.

quarternion2angleaxis([Q1,Q2,Q3,Q4],[U,V,W,A]) :-
  R1 is Q1*Q1+Q2*Q2+Q3*Q3,
  (R1=:=0 -> [U,V,W,A]=[0,0,0,0];
   R2 is R1+Q4*Q4,
   (R2=:=0 -> [U,V,W,A]=[0,0,0,0];
    A is 2*acos(Q4/sqrt(R2)),
    F is sqrt(R1),
    U is Q1/F, V is Q2/F, W is Q3/F
   )
  ).

/* ================== */
/* get_float1array/8  */
/* get_float1array1/8 */
/* get_float3array/8  */
/* get_float3array1/8 */
/* ================== */

get_float1array(_,_,_,_,_,0,_,_) :- !.

get_float1array(_,_,_,_,_,_,0,_) :- !.

get_float1array(File,Stream,ModelName,NodeName,NodeRef,StartPtr,Num,Parm) :-
  byte_count(Stream,Here),
  Start is StartPtr+12,
  seek(Stream,Start,bof,_),
  get_float1array1(File,Stream,ModelName,NodeName,NodeRef,Num,0,Parm),
  Q=..[Parm,Num],
  assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)),
  seek(Stream,Here,bof,_),
  !.
  
get_float1array1(_,_,_,_,_,Num,This,_) :- This>=Num, !.

get_float1array1(File,Stream,ModelName,NodeName,NodeRef,Num,This,Parm) :-
  get_float(Stream,X),
  Q=..[Parm,This,X],
  assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)),
  Next is This+1,
  !,
  get_float1array1(File,Stream,ModelName,NodeName,NodeRef,Num,Next,Parm).

get_float3array(_,_,_,_,_,0,_,_) :- !.

get_float3array(_,_,_,_,_,_,0,_) :- !.

get_float3array(File,Stream,ModelName,NodeName,NodeRef,StartPtr,Num,Parm) :-
  byte_count(Stream,Here),
  Start is StartPtr+12,
  seek(Stream,Start,bof,_),
  get_float3array1(File,Stream,ModelName,NodeName,NodeRef,Num,0,Parm),
  Q=..[Parm,Num],
  assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)),
  seek(Stream,Here,bof,_),
  !.
  
get_float3array1(_,_,_,_,_,Num,This,_) :- This>=Num, !.

get_float3array1(File,Stream,ModelName,NodeName,NodeRef,Num,This,Parm) :-
  get_float(Stream,X1),
  get_float(Stream,X2),
  get_float(Stream,X3),
  Q=..[Parm,This,X1,X2,X3],
  assertz(gotdata(File,ModelName,NodeName,NodeRef,Q)),
  Next is This+1,
  !,
  get_float3array1(File,Stream,ModelName,NodeName,NodeRef,Num,Next,Parm).

   