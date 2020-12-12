/* ============================== */ 
/*                                */
/* CleanModels3  by OldMansBeard  */
/*                                */
/* This version dated 2014-05-08  */
/*                                */
/* ============================== */ 

:- dynamic bug_count/2, total_fixes/1.
:- use_module(library(pce)).
:- use_module(library(optparse), [opt_arguments/3]).

:- ensure_loaded(output_models).
:- ensure_loaded(load_binary).
:- ensure_loaded(load_models).
:- ensure_loaded(make_checks).
:- ensure_loaded(fix_pivots).
:- ensure_loaded(tilefade).
:- ensure_loaded(rebuild_wok).
:- ensure_loaded(skinmesh).

version('CleanModels 3 Version 3.5.2d').

go :-
  init_dirs(InDir,Pattern,OutDir,LogFile,SmallLog,Decompile),
  absolute_file_name(LogFile,[relative_to(OutDir)],AbsLogFile),
  absolute_file_name(SmallLog,[relative_to(OutDir)],AbsSmallLog),
  open(AbsSmallLog,write,SmallLogStream,[close_on_abort(true)]),
  protocol(AbsLogFile),
  get_time(T), convert_time(T,TimeStamp),
  g_user_option(classification,Classification),
  g_user_option(snap,Snap),
  g_user_option(tvert_snap,TSnap),
  g_user_option(shadow,Shadow),
  g_user_option(repivot,Repivot),
  g_user_option(allow_split,Split),
  g_user_option(min_Size,MinSize),
  g_user_option(use_Smoothed,Smoothed),
  g_user_option(split_Priority,Priority),
  g_user_option('pivots_below_z=0',AllowBelow),
  g_user_option(move_bad_pivots,MoveBadPivots),
  g_user_option(force_white,ForceWhite),
  g_user_option(do_water,DoWater),
  g_user_option(water_key,WaterKey),
  g_user_option(dynamic_water,DynamicWater),
  g_user_option(wave_height,WaveHeight),
  g_user_option(rotate_water,RotateWater),
  g_user_option(foliage,Foliage),
  g_user_option(foliage_key,FoliageKey),
  g_user_option(rotate_ground,RotateGround),
  g_user_option(ground_key,GroundKey),
  g_user_option(splotch,Splotch),
  g_user_option(splotch_key,SplotchKey),
  g_user_option(fix_overhangs,FixOverhangs),
  g_user_option(map_aabb_material,MapAABB),
  g_user_option(map_aabb_from,AABBFrom),
  g_user_option(map_aabb_to,AABBTo),
  g_user_option(slice,Slice),
  g_user_option(slice_height,SliceHeight),
  g_user_option(chamfer,Chamfer),
  g_user_option(tile_raise,TileRaise),
  g_user_option(tile_raise_amount,TileRaiseAmount),
  g_user_option(tile_water,TileWaterRepeatCount),
  g_user_option(tile_ground,TileGroundRepeatCount),
  g_user_option(merge_by_bitmap,MergeBy),
  g_user_option(placeable_with_transparency,PlaceableWithTransparency),
  g_user_option(transparency_key,TransparencyKey),
  g_user_option(invisible_mesh_cull,InvisibleMeshCull),
  g_user_option(render,RenderAll),
  % g_user_option(skinmesh_bodies,SkinMeshBodies),
  % g_user_option(rescaleXYZ,Rescale),

  version(Version), write(Version), tab(1), write(TimeStamp), nl,
  write('Input Directory: '), write(InDir), nl,
  write('Files: '), write(Pattern), nl,
  write('Output Directory: '), write(OutDir), nl,
  (Decompile==false
    ->
      write('Output Model Class: '), write(Classification), nl,
      write('Snap: '), write(Snap), nl,
      write('Snap tverts: '), write(TSnap), nl,
      write('Shadow: '), write(Shadow), nl,
      (Shadow\=none -> write('Repivot: '), write(Repivot), nl; true),
      (Repivot\=none -> write('Allow Split: '), write(Split), nl; true),
      (Split\=no -> write('Min. Size: '), write(MinSize), nl; true),
      (Split\=no -> write('Smoothing Groups: '), write(Smoothed), nl; true),
      (Split\=no -> write('Split First: '), write(Priority), nl; true),
      (Repivot\=none -> write('Pivots below Z=0: '), write(AllowBelow), nl; true),
      (Repivot\=none -> write('Move Bad Pivots: '), write(MoveBadPivots), nl; true),
      write('Force non-black nodes to ambient/diffuse 1 1 1: '), write(ForceWhite), nl,
      ( once((Classification==tile; Classification==automatic)) ->
        write('Apply Water Fixups: '), write(DoWater), nl,
        (DoWater\=no -> write('Water Bitmap Key(s): '), write(WaterKey), nl; true),
        (DoWater\=no -> write('Dynamic Water: '), write(DynamicWater), nl; true),
        (DoWater\=no, DynamicWater==wavy -> write('Wave Height: '), write(WaveHeight), write('0cm'), nl; true),
        (DoWater\=no -> write('Set rotatetexture to: '), write(RotateWater), nl; true),
        (DoWater\=no, DynamicWater\=wavy -> write('Tile Water Repeat Pattern: '), write(TileWaterRepeatCount), nl; true),
        write('Tree Foliage: '), write(Foliage), nl,
        (Foliage\=ignore -> write('Foliage Bitmap Key(s): '), write(FoliageKey), nl; true),
        write('Splotches: '), write(Splotch), nl,
        (Splotch\=ignore -> write('Splotch Bitmap Key(s): '), write(SplotchKey), nl; true),
        write('Ground rotatetexture to: '), write(RotateGround), nl,
        write('Tile Edge Chamfers: '), write(Chamfer), nl,
        write('Tile ground planes: '), write(TileGroundRepeatCount), nl,
        ((RotateGround==no_change, Chamfer==no_change, TileGroundRepeatCount==no_change) -> true; write('Ground Key(s): '), write(GroundKey), nl),
        write('Repair aabb overhangs: '), write(FixOverhangs), nl,
        write('Change walkmesh materials: '), (MapAABB=='yes' -> write('from '), write(AABBFrom), write(' to '), write(AABBTo) ; write('no')), nl,
        write('Slice: '), write(Slice), nl,
        (Slice==yes -> write('Slice Height: '), write(SliceHeight), write('m'), nl; true),
        write('Tile raise/lower: '), write(TileRaise),
        (TileRaise\='no' -> write(' by '), write(TileRaiseAmount), write('m'); true), nl
        ; true),
      write('MeshMerge by bitmap: '), write(MergeBy), nl,
      (once((Classification==character ; Classification==automatic)) ->
        write('Placeable with transparency: '), write(PlaceableWithTransparency), nl,
        (PlaceableWithTransparency\=no -> write('Transparency bitmap key(s): '), write(TransparencyKey), nl; true) %, 
        % write('Skinmesh Bodies: '), write(SkinMeshBodies), nl
       ; true),
      write('Cull Invisible Meshes: '), write(InvisibleMeshCull), nl,
      write('Render Trimesh: '), write(RenderAll), nl,
      % write('Rescale X,Y,Z: '), write(Rescale), nl,
      (secret(Secret), write('Secret option: '), write(Secret), nl, fail ; true),
      nl,

      write(SmallLogStream,Version), tab(SmallLogStream,1), write(SmallLogStream,TimeStamp), nl(SmallLogStream),
      write(SmallLogStream,'Input Directory: '), write(SmallLogStream,InDir), nl(SmallLogStream),
      write(SmallLogStream,'Files: '), write(SmallLogStream,Pattern), nl(SmallLogStream),
      write(SmallLogStream,'Output Directory: '), write(SmallLogStream,OutDir), nl(SmallLogStream),
      write(SmallLogStream,'Output Model Class: '), write(SmallLogStream,Classification), nl(SmallLogStream),
      write(SmallLogStream,'Snap: '), write(SmallLogStream,Snap), nl(SmallLogStream),
      write(SmallLogStream,'Snap tverts: '), write(SmallLogStream,TSnap), nl(SmallLogStream),
      write(SmallLogStream,'Shadow: '), write(SmallLogStream,Shadow), nl(SmallLogStream),
      (Shadow\=none -> write(SmallLogStream,'Repivot: '), write(SmallLogStream,Repivot), nl(SmallLogStream); true),
      (Repivot\=none -> write(SmallLogStream,'Allow Split: '), write(SmallLogStream,Split), nl(SmallLogStream); true),
      (Split\=no -> write(SmallLogStream,'Min. Size: '), write(SmallLogStream,MinSize), nl(SmallLogStream); true),
      (Split\=no -> write(SmallLogStream,'Smoothing Groups: '), write(SmallLogStream,Smoothed), nl(SmallLogStream); true),
      (Split\=no -> write(SmallLogStream,'Split First: '), write(SmallLogStream,Priority), nl(SmallLogStream); true),
      (Repivot\=none -> write(SmallLogStream,'Pivots below Z=0: '), write(SmallLogStream,AllowBelow), nl(SmallLogStream); true),
      (Repivot\=none -> write(SmallLogStream,'Move Bad Pivots: '), write(SmallLogStream,MoveBadPivots), nl(SmallLogStream); true),
      write(SmallLogStream,'Force non-black nodes to ambient/diffuse 1 1 1: '), write(SmallLogStream,ForceWhite), nl(SmallLogStream),
      ( once((Classification==tile; Classification==automatic)) ->
        write(SmallLogStream,'Apply Water Fixups: '), write(SmallLogStream,DoWater), nl(SmallLogStream),
        (DoWater\=no -> write(SmallLogStream,'Water Bitmap Key(s): '), write(SmallLogStream,WaterKey), nl(SmallLogStream); true),
        (DoWater\=no -> write(SmallLogStream,'Dynamic Water : '), write(SmallLogStream,DynamicWater), nl(SmallLogStream); true),
        (DoWater\=no, DynamicWater==wavy -> write(SmallLogStream,'Wave Height: '), write(SmallLogStream,WaveHeight), write(SmallLogStream,'0cm'), nl(SmallLogStream); true),
        (DoWater\=no -> write(SmallLogStream,'Set rotatetexture to: '), write(SmallLogStream,RotateWater), nl(SmallLogStream); true),
        (DoWater\=no, DynamicWater\=wavy -> write(SmallLogStream,'Tile Water Repeat Pattern: '), write(SmallLogStream,TileWaterRepeatCount), nl(SmallLogStream); true),
        write(SmallLogStream,'Tree Foliage: '), write(SmallLogStream,Foliage), nl(SmallLogStream),
        (Foliage\=ignore -> write(SmallLogStream,'Foliage Bitmap Key(s): '), write(SmallLogStream,FoliageKey), nl(SmallLogStream); true),
        write(SmallLogStream,'Splotches: '), write(SmallLogStream,Splotch), nl(SmallLogStream),
        (Splotch\=ignore -> write(SmallLogStream,'Splotch Bitmap Key(s): '), write(SmallLogStream,SplotchKey), nl(SmallLogStream); true),
        write(SmallLogStream,'Ground rotatetexture to: '), write(SmallLogStream,RotateGround), nl(SmallLogStream),
        write(SmallLogStream,'Tile Edge Chamfers: '), write(SmallLogStream,Chamfer), nl(SmallLogStream),
        write(SmallLogStream,'Tile ground planes: '), write(SmallLogStream,TileGroundRepeatCount), nl(SmallLogStream),
        ((RotateGround==no_change, Chamfer==no_change, TileGroundRepeatCount==no_change) -> true; write(SmallLogStream,'Ground Key(s): '), write(SmallLogStream,GroundKey), nl(SmallLogStream)),
        write(SmallLogStream,'Repair aabb overhangs: '), write(SmallLogStream,FixOverhangs), nl(SmallLogStream),
        write(SmallLogStream,'Change walkmesh materials: '), (MapAABB==yes -> write(SmallLogStream,'from '), write(SmallLogStream,AABBFrom), write(SmallLogStream,' to '), write(SmallLogStream,AABBTo) ; write(SmallLogStream,no)), nl(SmallLogStream),
        write(SmallLogStream,'Slice: '), write(SmallLogStream,Slice), nl(SmallLogStream),
        (Slice==yes -> write(SmallLogStream,'Slice Height: '), write(SmallLogStream,SliceHeight), write(SmallLogStream,'m'), nl(SmallLogStream); true),
        write(SmallLogStream,'Tile raise/lower: '), write(SmallLogStream,TileRaise),
        (TileRaise\='no' -> write(SmallLogStream,' by '), write(SmallLogStream,TileRaiseAmount), write(SmallLogStream,'m'); true), nl(SmallLogStream)
        ; true),
      write(SmallLogStream,'MeshMerge by bitmap: '), write(SmallLogStream,MergeBy), nl(SmallLogStream),
      (once((Classification==character ; Classification==automatic)) ->
       write(SmallLogStream,'Placeable with transparency: '), write(SmallLogStream,PlaceableWithTransparency), nl(SmallLogStream),
       (PlaceableWithTransparency\=no -> write(SmallLogStream,'Transparency bitmap key(s): '), write(SmallLogStream,TransparencyKey), nl(SmallLogStream); true) %,
       % write(SmallLogStream,'Skinmesh Bodies: '), write(SmallLogStream,SkinMeshBodies), nl(SmallLogStream)
       ; true),
      write(SmallLogStream,'Cull Invisible Meshes: '), write(SmallLogStream,InvisibleMeshCull), nl(SmallLogStream),
      write(SmallLogStream,'Render Trimesh: '), write(SmallLogStream,RenderAll), nl(SmallLogStream),
      % write(SmallLogStream,'Rescale X,Y,Z: '), write(SmallLogStream,Rescale), nl(SmallLogStream),
      (secret(Secret), write(SmallLogStream,'Secret option: '), write(SmallLogStream,Secret), nl(SmallLogStream), fail ; true),
      nl(SmallLogStream),

      ( once((Classification==tile; Classification==automatic)) -> load_edge_tiles(InDir) ; true )
    ;
    write('Performing Binary Model Decompilations Only '), nl,
    true
  ),
  get_files(InDir,Pattern),
  load_fix_models(InDir,OutDir,SmallLogStream,Decompile),
  close(SmallLogStream),
  noprotocol,
  halt.

/* ================= */
/* load_fix_models/4 */
/* ================= */

load_fix_models(_,_,_,_) :-
  retractall(total_fixes(_)),
  asserta(total_fixes(0)),
  fail.

load_fix_models(InDir,OutDir,SmallLogStream,Decompile) :-
  mdl_file(File),
  clear_memory,
  cm3_load_file(InDir,File),
  once((secret(align_to_supermodel(Align)) ; Align=no)),
  (Align==yes, gotdata(File,Model,setsupermodel(Model,Super)), Super\=='NULL'
     ->
     atom_concat(Super,'.mdl',SuperFile),
     tab(2), write('attempting to load supermodel '), write(SuperFile), write(' ... '),
     cm3_load_file(InDir,SuperFile)
     ;
     true
  ),
  check_and_output(File,OutDir,SmallLogStream,Decompile),
  fail.

load_fix_models(_,_,SmallLogStream,Decompile) :-
  (Decompile==false
    ->
    total_fixes(Total),
    write('Total Fixes = '), write(Total), nl,
    write(SmallLogStream,'Total Fixes = '), write(SmallLogStream,Total), nl(SmallLogStream)
    ;
    true
  ).

/* ============== */
/* clear_memory/0 */
/* ============== */

clear_memory :-

  /* declared in load_models */
  retractall(load_failed(_)),
  retractall(gotdata(_,_,_)),
  retractall(gotdata(_,_,_,_)),
  retractall(gotdata(_,_,_,_,_)),
  retractall(gotdata(_,_,_,_,_,_)),
  retractall(external_node(_,_,_)),
  retractall(external_node_parent(_,_,_,_)),

  /* declared in make_checks */
  retractall(face_normal(_,_,_,_,_,_)),
  retractall(check_failed(_)),
  retractall(tvert_group(_,_,_,_,_,_)),
  retractall(fixlist(_)),
  retractall(slice_height(_,_)),

  /* declared in fix_pivots */
  retractall(face_group(_,_)),
  retractall(neighbourhood(_,_,_,_,_,_,_)),
  retractall(ground_face(_)),

  /* declared in tilefade */
  retractall(abs_vertex(_,_,_)),

  /* declared in load_binary */
  retractall(gotbinary(_,_)),
  retractall(gotbinary(_,_,_)),
  retractall(gotparent(_,_,_,_,_,_,_)),
  retractall(name_checked(_,_,_,_,_)),
  retractall(nth_node(_)),
  retractall(sg_assigned(_,_,_)),
  retractall(compiler(_)),

  garbage_collect.

/* ================== */
/* check_and_output/4 */
/* ================== */

check_and_output(File,_,SmallLogStream,_) :-
  load_failed(File),
  nl, nl,
  write(SmallLogStream,'**** '), write(SmallLogStream,File), write(SmallLogStream,' failed to load'),
  nl(SmallLogStream), nl(SmallLogStream), !.

check_and_output(File,_,SmallLogStream,Decompile) :-
  write(SmallLogStream,File), nl(SmallLogStream),
  (Decompile==false -> check_for(_,File,SmallLogStream); true),
  fail.

check_and_output(File,_,SmallLogStream,_) :-
  check_failed(File),
  write('*** Cannot output model - too buggy ***'), nl, nl,
  write(SmallLogStream,'**** '), write(SmallLogStream,File), write(SmallLogStream,' is too buggy to output'),
  nl(SmallLogStream), nl(SmallLogStream), !.

check_and_output(File,OutDir,SmallLogStream,_) :-
  working_directory(WkDir,OutDir),
  output_file(File),
  nl, nl,
  write(SmallLogStream,'  Exported okay.'),
  nl(SmallLogStream), nl(SmallLogStream),
  working_directory(OutDir,WkDir),
  !.

/* ============== */
/* save_program/1 */
/* ============== */

save_program(File) :-
  qsave_program(File,[goal(go),stand_alone(true),foreign(save)]).

/* =========== */
/* init_dirs/6 */
/* do_gui/0    */
/* =========== */

:- dynamic g_indir/1.
:- dynamic g_outdir/1.
:- dynamic g_logfile/1.
:- dynamic g_small_log/1.
:- dynamic g_pattern/1.
:- dynamic g_user_option/2.
:- dynamic secret/1.

g_indir('.').
g_outdir('.').
g_logfile('CleanModels 3.log').
g_pattern('*.mdl').
g_small_log('CleanModels 3 Summary.log').
g_user_option(classification,automatic).
g_user_option(snap,decimal).
g_user_option(tvert_snap,no).
g_user_option(shadow,default).
g_user_option(repivot,all).
g_user_option(allow_split,yes).
g_user_option(min_Size,4).
g_user_option(use_Smoothed,use).
g_user_option(split_Priority,concave).
g_user_option('pivots_below_z=0',disallow).
g_user_option(move_bad_pivots,no).
g_user_option(force_white,no).
g_user_option(do_water,yes).
g_user_option(water_key,water).
g_user_option(foliage,tilefade).
g_user_option(foliage_key,trefol).
g_user_option(splotch,animate).
g_user_option(splotch_key,splotch).
g_user_option(rotate_ground,1).
g_user_option(ground_key,ground).
g_user_option(dynamic_water,yes).
g_user_option(wave_height,5).
g_user_option(rotate_water,1).
g_user_option(fix_overhangs,yes).
g_user_option(map_aabb_material,no).
g_user_option(map_aabb_from,0).
g_user_option(map_aabb_to,0).
g_user_option(slice,no).
g_user_option(slice_height,5).
g_user_option(chamfer,no_change).
g_user_option(tile_raise,no).
g_user_option(tile_raise_amount,1).
g_user_option(tile_water,no_change).
g_user_option(tile_ground,no_change).
g_user_option(merge_by_bitmap,no).
g_user_option(placeable_with_transparency,no).
g_user_option(transparency_key,glass).
g_user_option(invisible_mesh_cull,yes).
g_user_option(render,default).
% g_user_option(skinmesh_bodies,no_change).
% g_user_option(rescaleXYZ,no).

init_dirs(InDir,Pattern,OutDir,LogFile,SmallLog,Decompile) :-
  /* InitFile = 'last_dirs.pl', */
  OptsSpec =
     [[opt(gui), type(boolean), default(true),
        longflags(['gui']),
        help('opens the ui for flag selection')],
     [opt(decompile), type(boolean), default(false),
        shortflags(['d']), longflags(['decompile']),
        help('only does decompilation, no fixes')],
     [opt(indir), meta('DIR'), type(atom), default(''),
        shortflags([i]), longflags(['indir']),
        help('DIR containing models to be fixed/decompiled')],
     [opt(outdir), type(atom), default(''),
        shortflags([o]), longflags(['outdir']),
        help('output directory of fixed/decompiled models')],
     [opt(pattern), type(atom), default(''),
        shortflags([p]), longflags(['pattern']),
        help('filename pattern of models to process')]
     ],
  opt_arguments(OptsSpec, Opts, PositionalArgs),
  (current_prolog_flag(saved_program,true), PositionalArgs\==[] -> [InitFile|_]=PositionalArgs ; InitFile = 'last_dirs.pl'),
  memberchk(gui(Gui), Opts),
  memberchk(decompile(Decompile), Opts),
  memberchk(indir(OptsInDir), Opts),
  memberchk(outdir(OptsOutDir), Opts),
  memberchk(pattern(OptsPattern), Opts),
  (exists_file(InitFile) -> consult(InitFile); true),
  (OptsInDir\=='' -> set_indir(OptsInDir) ; true),
  (OptsOutDir\=='' -> set_outdir(OptsOutDir) ; true),
  (OptsPattern\=='' -> set_pattern(OptsPattern) ; true),
  (Gui==true -> do_gui; true),
  g_indir(InDir),
  g_pattern(Pattern),
  g_outdir(OutDir),
  (exists_directory(OutDir) -> true; make_directory(OutDir)),
  g_logfile(LogFile),
  g_small_log(SmallLog),
  g_user_option(classification,Classification),
  g_user_option(snap,Snap),
  g_user_option(tvert_snap,TSnap),
  g_user_option(shadow,Shadow),
  g_user_option(repivot,Repivot),
  g_user_option(allow_split,Split),
  g_user_option(min_Size,MinSize),
  g_user_option(use_Smoothed,Smoothed),
  g_user_option(split_Priority,Priority),
  g_user_option('pivots_below_z=0',AllowBelow),
  g_user_option(move_bad_pivots,MoveBadPivots),
  g_user_option(force_white,ForceWhite),
  g_user_option(do_water,DoWater),
  g_user_option(water_key,WaterKey),
  g_user_option(dynamic_water,DynamicWater),
  g_user_option(wave_height,WaveHeight),
  g_user_option(rotate_water,RotateWater),
  g_user_option(foliage,Foliage),
  g_user_option(foliage_key,FoliageKey),
  g_user_option(splotch,Splotch),
  g_user_option(splotch_key,SplotchKey),
  g_user_option(rotate_ground,RotateGround),
  g_user_option(tile_ground,TileGroundRepeatCount),
  g_user_option(ground_key,GroundKey),
  g_user_option(fix_overhangs,FixOverhangs),
  g_user_option(map_aabb_material,MapAABB),
  g_user_option(map_aabb_from,AABBFrom),
  g_user_option(map_aabb_to,AABBTo),
  g_user_option(slice,Slice),
  g_user_option(slice_height,SliceHeight),
  g_user_option(chamfer,Chamfer),
  g_user_option(tile_raise,TileRaise),
  g_user_option(tile_raise_amount,TileRaiseAmount),
  g_user_option(tile_water,TileWaterRepeatCount),
  g_user_option(merge_by_bitmap,MergeBy),
  g_user_option(placeable_with_transparency,PlaceableWithTransparency),
  g_user_option(transparency_key,TransparencyKey),
  g_user_option(invisible_mesh_cull,InvisibleMeshCull),
  g_user_option(render,RenderAll),
  % g_user_option(skinmesh_bodies,SkinMeshBodies),
  % g_user_option(rescaleXYZ,Rescale),

  tell(InitFile),
  writeq(:-asserta(g_indir(InDir))), write('.'), nl,
  writeq(:-asserta(g_outdir(OutDir))), write('.'), nl,
  writeq(:-asserta(g_logfile(LogFile))), write('.'), nl,
  writeq(:-asserta(g_pattern(Pattern))), write('.'), nl,
  writeq(:-asserta(g_small_log(SmallLog))), write('.'), nl,
  writeq(:-asserta(g_user_option(classification,Classification))), write('.'), nl,
  writeq(:-asserta(g_user_option(snap,Snap))), write('.'), nl,
  writeq(:-asserta(g_user_option(tvert_snap,TSnap))), write('.'), nl,
  writeq(:-asserta(g_user_option(shadow,Shadow))), write('.'), nl,
  writeq(:-asserta(g_user_option(repivot,Repivot))), write('.'), nl,
  writeq(:-asserta(g_user_option(allow_split,Split))), write('.'), nl,
  writeq(:-asserta(g_user_option(min_Size,MinSize))), write('.'), nl,
  writeq(:-asserta(g_user_option(use_Smoothed,Smoothed))), write('.'), nl,
  writeq(:-asserta(g_user_option(split_Priority,Priority))), write('.'), nl,
  writeq(:-asserta(g_user_option('pivots_below_z=0',AllowBelow))), write('.'), nl,
  writeq(:-asserta(g_user_option(move_bad_pivots,MoveBadPivots))), write('.'), nl,
  writeq(:-asserta(g_user_option(force_white,ForceWhite))), write('.'), nl,
  writeq(:-asserta(g_user_option(do_water,DoWater))), write('.'), nl,
  writeq(:-asserta(g_user_option(water_key,WaterKey))), write('.'), nl,
  writeq(:-asserta(g_user_option(dynamic_water,DynamicWater))), write('.'), nl,
  writeq(:-asserta(g_user_option(wave_height,WaveHeight))), write('.'), nl,
  writeq(:-asserta(g_user_option(rotate_water,RotateWater))), write('.'), nl,
  writeq(:-asserta(g_user_option(foliage,Foliage))), write('.'), nl,
  writeq(:-asserta(g_user_option(foliage_key,FoliageKey))), write('.'), nl,
  writeq(:-asserta(g_user_option(splotch,Splotch))), write('.'), nl,
  writeq(:-asserta(g_user_option(splotch_key,SplotchKey))), write('.'), nl,
  writeq(:-asserta(g_user_option(rotate_ground,RotateGround))), write('.'), nl,
  writeq(:-asserta(g_user_option(chamfer,Chamfer))), write('.'), nl,
  writeq(:-asserta(g_user_option(ground_key,GroundKey))), write('.'), nl,
  writeq(:-asserta(g_user_option(fix_overhangs,FixOverhangs))), write('.'), nl,
  writeq(:-asserta(g_user_option(map_aabb_material,MapAABB))), write('.'), nl,
  writeq(:-asserta(g_user_option(map_aabb_from,AABBFrom))), write('.'), nl,
  writeq(:-asserta(g_user_option(map_aabb_to,AABBTo))), write('.'), nl,
  writeq(:-asserta(g_user_option(slice,Slice))), write('.'), nl,
  writeq(:-asserta(g_user_option(slice_height,SliceHeight))), write('.'), nl,
  writeq(:-asserta(g_user_option(tile_raise,TileRaise))), write('.'), nl,
  writeq(:-asserta(g_user_option(tile_raise_amount,TileRaiseAmount))), write('.'), nl,
  writeq(:-asserta(g_user_option(tile_water,TileWaterRepeatCount))), write('.'), nl,
  writeq(:-asserta(g_user_option(tile_ground,TileGroundRepeatCount))), write('.'), nl,
  writeq(:-asserta(g_user_option(merge_by_bitmap,MergeBy))), write('.'), nl,
  writeq(:-asserta(g_user_option(placeable_with_transparency,PlaceableWithTransparency))), write('.'), nl,
  writeq(:-asserta(g_user_option(transparency_key,TransparencyKey))), write('.'), nl,
  writeq(:-asserta(g_user_option(invisible_mesh_cull,InvisibleMeshCull))), write('.'), nl,
  writeq(:-asserta(g_user_option(render,RenderAll))), write('.'), nl,
  % writeq(:-asserta(g_user_option(skinmesh_bodies,SkinMeshBodies))), write('.'), nl,
  % writeq(:-asserta(g_user_option(rescaleXYZ,Rescale))), write('.'), nl,

  /* Support for secret options added in CM343f */
  (secret(Secret), writeq(:-asserta(secret(Secret))), write('.'), nl, fail ; true),
  told.

do_gui :-
  version(Version),
  new(A,dialog(Version)),

  g_indir(InDir0),
  absolute_file_name(InDir0,AbsInDir0),
  prolog_to_os_filename(AbsInDir0,OSInDir0),
  send(A,append,new(B,text_item('Input Directory:',OSInDir0))),
  g_pattern(Pattern0),
  send(A,append,new(C,text_item('Files:',Pattern0))),
  g_outdir(OutDir0),
  absolute_file_name(OutDir0,AbsOutDir0),
  prolog_to_os_filename(AbsOutDir0,OSOutDir0),
  send(A,append,new(D,text_item('Output Directory:',OSOutDir0))),

  g_user_option(classification,Classification),
  send(A,append,(new(@class_menu,menu('Output Model Class:',tick_box)))),
  send(@class_menu,append,menu_item(character, message(@prolog,set_class,character))),
  send(@class_menu,append,menu_item(door, message(@prolog,set_class,door))),
  send(@class_menu,append,menu_item(effect, message(@prolog,set_class,effect))),
  send(@class_menu,append,menu_item(item, message(@prolog,set_class,item))),
  send(@class_menu,append,menu_item(tile, message(@prolog,set_class,tile))),
  send(@class_menu,append,menu_item(automatic, message(@prolog,set_class,automatic))),
  send(@class_menu,default(Classification)),

  g_logfile(LogFile0),
  send(A,append,new(E,text_item('Log File:',LogFile0))),
  g_small_log(SmallLog0),
  send(A,append,new(F,text_item('Summary File:',SmallLog0))),

  new(@shadow_options,dialog),

  g_user_option(snap,Snap),
  send(@shadow_options,append,(new(G,menu('Snap:',tick_box)))),
  send(G,append,menu_item(binary, message(@prolog,set_snap,binary))),
  send(G,append,menu_item(decimal, message(@prolog,set_snap,decimal))),
  send(G,append,menu_item(fine, message(@prolog,set_snap,fine))),
  send(G,append,menu_item(none, message(@prolog,set_snap,none))),
  send(G,default(Snap)),

  g_user_option(tvert_snap,TSnap),
  send(@shadow_options,append,(new(@tvert_snap_menu,menu('Snap tverts:',tick_box)))),
  send(@tvert_snap_menu,append,menu_item(256, message(@prolog,set_tvert_snap,256))),
  send(@tvert_snap_menu,append,menu_item(512, message(@prolog,set_tvert_snap,512))),
  send(@tvert_snap_menu,append,menu_item(1024, message(@prolog,set_tvert_snap,1024))),
  send(@tvert_snap_menu,append,menu_item(no, message(@prolog,set_tvert_snap,no))),
  send(@tvert_snap_menu,default(TSnap)),

  g_user_option(force_white,ForceWhite),
  send(@shadow_options,append,(new(@force_white_menu,menu('Ambient/Diffuse to 1:',tick_box)))),
  send(@force_white_menu,append,menu_item(yes, message(@prolog,set_force_white,yes))),
  send(@force_white_menu,append,menu_item(no, message(@prolog,set_force_white,no))),
  send(@force_white_menu,alignment(center)),
  send(@force_white_menu,default(ForceWhite)),

  g_user_option(merge_by_bitmap,MergeBy),
  send(@shadow_options,append,(new(@merge_by_bitmap_menu,menu('MeshMerge by bitmap:',tick_box)))),
  send(@merge_by_bitmap_menu,append,menu_item(yes, message(@prolog,set_meshmerge,yes))),
  send(@merge_by_bitmap_menu,append,menu_item(no, message(@prolog,set_meshmerge,no))),
  send(@merge_by_bitmap_menu,alignment(center)),
  send(@merge_by_bitmap_menu,default(MergeBy)),

  g_user_option(shadow,Shadow),
  send(@shadow_options,append,(new(J,menu('Shadow:',tick_box)))),
  send(J,append,menu_item(all, message(@prolog,set_shadow,all))),
  send(J,append,menu_item(none, message(@prolog,set_shadow,none))),
  send(J,append,menu_item(default, message(@prolog,set_shadow,default))),
  send(J,default(Shadow)),

  (Shadow=none -> asserta(g_user_option(repivot,none)); true),
  g_user_option(repivot,Repivot),
  send(@shadow_options,append,(new(@repivot_menu,menu('Repivot:',tick_box)))),
  send(@repivot_menu,append,menu_item(all, message(@prolog,set_repivot,all))),
  send(@repivot_menu,append,menu_item(none, message(@prolog,set_repivot,none))),
  send(@repivot_menu,append,menu_item(if_needed, message(@prolog,set_repivot,if_needed))),
  send(@repivot_menu,default(Repivot)),
  (Shadow=none -> send(@repivot_menu,active(@off)) ; send(@repivot_menu,active(@on))),

  (Repivot=none -> asserta(g_user_option(allow_split,no)); true),
  g_user_option(allow_split,Split),
  send(@shadow_options,append,(new(@splitting_menu,menu('Allow Splitting:',tick_box)))),
  send(@splitting_menu,append,menu_item(yes, message(@prolog,set_splitting,yes))),
  send(@splitting_menu,append,menu_item(no, message(@prolog,set_splitting,no))),
  send(@splitting_menu,alignment(center)),
  send(@splitting_menu,default(Split)),
  (Repivot=none -> send(@splitting_menu,active(@off)) ; send(@splitting_menu,active(@on))),  

  g_user_option(min_Size,MinSize),
  send(@shadow_options,append,(new(@min_size_menu,int_item('Sub-Object Minimum Faces:',low:=2,high:=8)))),
  send(@min_size_menu,default(MinSize)),
  send(@min_size_menu,alignment(center)),
  (Split=no -> send(@min_size_menu,active(@off)) ; send(@min_size_menu,active(@on))),  

  g_user_option(use_Smoothed,Smoothed),
  send(@shadow_options,append,(new(@use_smoothed_menu,menu('Smoothing Groups:',tick_box)))),
  send(@use_smoothed_menu,append,menu_item(use, message(@prolog,set_use_smoothed,use))),
  send(@use_smoothed_menu,append,menu_item(ignore, message(@prolog,set_use_smoothed,ignore))),
  send(@use_smoothed_menu,append,menu_item(protect, message(@prolog,set_use_smoothed,protect))),
  send(@use_smoothed_menu,alignment(center)),
  send(@use_smoothed_menu,default(Smoothed)),
  (Split=no -> send(@use_smoothed_menu,active(@off)) ; send(@use_smoothed_menu,active(@on))),  

  g_user_option(split_Priority,Priority),
  send(@shadow_options,append,(new(@split_priority_menu,menu('Split First:',tick_box)))),
  send(@split_priority_menu,append,menu_item(convex, message(@prolog,set_split_priority,convex))),
  send(@split_priority_menu,append,menu_item(concave, message(@prolog,set_split_priority,concave))),
  send(@split_priority_menu,alignment(center)),
  send(@split_priority_menu,default(Priority)),
  (Split=no -> send(@split_priority_menu,active(@off)) ; send(@split_priority_menu,active(@on))),  

  g_user_option('pivots_below_z=0',AllowBelow),
  send(@shadow_options,append,(new(@below_z_menu,menu('Pivots below Z=0:',tick_box)))),
  send(@below_z_menu,append,menu_item(disallow, message(@prolog,set_below_z,disallow))),
  send(@below_z_menu,append,menu_item(allow, message(@prolog,set_below_z,allow))),
  send(@below_z_menu,append,menu_item(slice, message(@prolog,set_below_z,slice))),
  send(@below_z_menu,alignment(center)),
  send(@below_z_menu,default(AllowBelow)),
  (Repivot=none -> send(@below_z_menu,active(@off)) ; send(@below_z_menu,active(@on))),  

  g_user_option(move_bad_pivots,MoveBadPivots),
  send(@shadow_options,append,(new(@move_bad_menu,menu('Move Bad Pivots:',tick_box)))),
  send(@move_bad_menu,append,menu_item(top, message(@prolog,set_move_bad,top))),
  send(@move_bad_menu,append,menu_item(middle, message(@prolog,set_move_bad,middle))),
  send(@move_bad_menu,append,menu_item(bottom, message(@prolog,set_move_bad,bottom))),
  send(@move_bad_menu,append,menu_item(no, message(@prolog,set_move_bad,no))),
  send(@move_bad_menu,alignment(center)),
  send(@move_bad_menu,default(MoveBadPivots)),
  (Repivot=none -> send(@move_bad_menu,active(@off)) ; send(@move_bad_menu,active(@on))),  
  
  g_user_option(invisible_mesh_cull,InvisibleMeshCull),
  send(@shadow_options,append,(new(@invisible_mesh_cull_menu,menu('Cull Invisible Meshes:',tick_box)))),
  send(@invisible_mesh_cull_menu,append,menu_item(yes, message(@prolog,set_cull_invisible,yes))),
  send(@invisible_mesh_cull_menu,append,menu_item(no, message(@prolog,set_cull_invisible,no))),
  send(@invisible_mesh_cull_menu,default(InvisibleMeshCull)),
  send(@invisible_mesh_cull_menu,alignment(left)),

  g_user_option(render,RenderAll),
  send(@shadow_options,append,(new(@render_all_menu,menu('Render Trimeshes:',tick_box)))),
  send(@render_all_menu,append,menu_item(all, message(@prolog,set_render_all,all))),
  send(@render_all_menu,append,menu_item(none, message(@prolog,set_render_all,none))),
  send(@render_all_menu,append,menu_item(default, message(@prolog,set_render_all,default))),
  send(@render_all_menu,alignment(left)),
  send(@render_all_menu,default(RenderAll)),

  new(@tile_options,dialog),

  g_user_option(tile_raise,TileRaise),
  send(@tile_options,append,(new(@tile_raise_menu,menu('Raise/Lower Tileset:',tick_box)))),
  send(@tile_raise_menu,append,menu_item(raise, message(@prolog,set_tile_raise,raise))),
  send(@tile_raise_menu,append,menu_item(lower, message(@prolog,set_tile_raise,lower))),
  send(@tile_raise_menu,append,menu_item(no, message(@prolog,set_tile_raise,no))),
  send(@tile_raise_menu,default(TileRaise)),
  send(@tile_raise_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@tile_raise_menu,active(@off)) ; send(@tile_raise_menu,active(@on))),

  g_user_option(tile_raise_amount,TileRaiseAmount),
  send(@tile_options,append,(new(@tile_raise_amount_menu,int_item('Raise/Lower amount (m):',low:=1,high:=10)))),
  send(@tile_raise_amount_menu,default(TileRaiseAmount)),
  send(@tile_raise_amount_menu,right(@tile_raise_menu)),
  send(@tile_raise_amount_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@tile_raise_amount_menu,active(@off)) ;
  (TileRaise\==no -> send(@tile_raise_amount_menu,active(@on)) ; send(@tile_raise_amount_menu,active(@off)))),

  g_user_option(slice,Slice),
  send(@tile_options,append,(new(@slice_menu,menu('Slice for Tilefade:',tick_box)))),
  send(@slice_menu,append,menu_item(yes, message(@prolog,set_slice,yes))),
  send(@slice_menu,append,menu_item(no, message(@prolog,set_slice,no))),
  send(@slice_menu,append,menu_item(undo, message(@prolog,set_slice,undo))),
  send(@slice_menu,default(Slice)),
  send(@slice_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@slice_menu,active(@off)) ; send(@slice_menu,active(@on))),

  g_user_option(slice_height,SliceHeight),
  send(@tile_options,append,(new(@slice_height_menu,int_item('Slice Height above walkmesh (x10cm):',low:=20,high:=100)))),
  SliceHeight10 is round(10*SliceHeight), send(@slice_height_menu,default(SliceHeight10)),
  send(@slice_height_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@slice_height_menu,active(@off)) ;
  (Slice==yes -> send(@slice_height_menu,active(@on)) ; send(@slice_height_menu,active(@off)))),

  g_user_option(fix_overhangs,FixOverhangs),
  send(@tile_options,append,(new(@fix_overhang_menu,menu('Repair aabb overhangs:',tick_box)))),
  send(@fix_overhang_menu,append,menu_item(yes, message(@prolog,set_fix_overhangs,yes))),
  send(@fix_overhang_menu,append,menu_item(no, message(@prolog,set_fix_overhangs,no))),
  send(@fix_overhang_menu,append,menu_item(interior_only, message(@prolog,set_fix_overhangs,interior_only))),
  send(@fix_overhang_menu,alignment(left)),
  send(@fix_overhang_menu,default(FixOverhangs)),
  (Classification\==tile, Classification\==automatic -> send(@fix_overhang_menu,active(@off)) ; send(@fix_overhang_menu,active(@on))),

  g_user_option(map_aabb_material,MapAABB),
  send(@tile_options,append,(new(@map_aabb_menu,menu('Change walkmesh material:',tick_box)))),
  send(@map_aabb_menu,append,menu_item(yes, message(@prolog,set_map_aabb,yes))),
  send(@map_aabb_menu,append,menu_item(no, message(@prolog,set_map_aabb,no))),
  send(@map_aabb_menu,alignment(left)),
  (MapAABB=yes -> send(@map_aabb_menu,default(yes)); send(@map_aabb_menu,default(no))),
  (Classification\==tile, Classification\==automatic -> send(@map_aabb_menu,active(@off)) ; send(@map_aabb_menu,active(@on))),

  g_user_option(map_aabb_from,AABBFrom),
  send(@tile_options,append,(new(@aabb_from_menu,int_item('From:',low:= 0,high:=30)))),
  send(@aabb_from_menu,default(30)),
  send(@aabb_from_menu,default(AABBFrom)),
  send(@aabb_from_menu,right,@map_aabb_menu),
  send(@aabb_from_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@aabb_from_menu,active(@off)) ;
  (MapAABB\==yes -> send(@aabb_from_menu,active(@off)) ; send(@aabb_from_menu,active(@on)))),

  g_user_option(map_aabb_to,AABBTo),
  send(@tile_options,append,(new(@aabb_to_menu,int_item('To:',low:= 0,high:=30)))),
  send(@aabb_to_menu,default(30)),
  send(@aabb_to_menu,default(AABBTo)),
  send(@aabb_to_menu,right,@aabb_from_menu),
  send(@aabb_to_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@aabb_to_menu,active(@off)) ;
  (MapAABB\==yes -> send(@aabb_to_menu,active(@off)) ; send(@aabb_to_menu,active(@on)))),

  g_user_option(foliage,Foliage),
  send(@tile_options,append,(new(@foliage_menu,menu('Tree Foliage:',tick_box)))),
  send(@foliage_menu,append,menu_item(tilefade, message(@prolog,set_foliage,tilefade))),
  send(@foliage_menu,append,menu_item(animate, message(@prolog,set_foliage,animate))),
  send(@foliage_menu,append,menu_item('de-animate', message(@prolog,set_foliage,'de-animate'))),
  send(@foliage_menu,append,menu_item(no_change, message(@prolog,set_foliage,no_change))),
  send(@foliage_menu,append,menu_item(ignore, message(@prolog,set_foliage,ignore))),
  send(@foliage_menu,default(Foliage)),
  send(@foliage_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@foliage_menu,active(@off)) ; send(@foliage_menu,active(@on))),

  g_user_option(foliage_key,FoliageKey),
  send(@tile_options,append,new(@foliage_key,text_item('Foliage Bitmap Key(s):',FoliageKey))),
  (Classification\==tile, Classification\==automatic -> send(@foliage_key,active(@off)) ;
  (Foliage=ignore -> send(@foliage_key,active(@off)) ; send(@foliage_key,active(@on)))),

  g_user_option(splotch,Splotch),
  send(@tile_options,append,(new(@splotch_menu,menu('Splotches:',tick_box)))),
  send(@splotch_menu,append,menu_item(animate, message(@prolog,set_splotch,animate))),
  send(@splotch_menu,append,menu_item(ignore, message(@prolog,set_splotch,ignore))),
  send(@splotch_menu,default(Splotch)),
  send(@splotch_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@splotch_menu,active(@off)) ; send(@splotch_menu,active(@on))),

  g_user_option(splotch_key,SplotchKey),
  send(@tile_options,append,new(@splotch_key,text_item('Splotch Bitmap Key(s):',SplotchKey))),
  (Classification\==tile, Classification\==automatic -> send(@splotch_key,active(@off)) ;
  (Splotch=ignore -> send(@splotch_key,active(@off)) ; send(@splotch_key,active(@on)))),

  g_user_option(placeable_with_transparency,PlaceableWithTransparency),
  send(@tile_options,append,(new(@placeable_with_transparency_menu,menu('Placeable with transparency:',tick_box)))),
  send(@placeable_with_transparency_menu,append,menu_item(yes, message(@prolog,set_transparency,yes))),
  send(@placeable_with_transparency_menu,append,menu_item(no, message(@prolog,set_transparency,no))),
  send(@placeable_with_transparency_menu,default(PlaceableWithTransparency)),
  send(@placeable_with_transparency_menu,alignment(left)),
  (Classification\==character, Classification\==automatic -> send(@placeable_with_transparency_menu,active(@off)) ; send(@placeable_with_transparency_menu,active(@on))),

  g_user_option(transparency_key,TransparencyKey),
  send(@tile_options,append,new(@transparency_key,text_item('Transparent Bitmap Key(s):',TransparencyKey))),
  (Classification\==character, Classification\==automatic -> send(@transparency_key,active(@off)) ;
   (PlaceableWithTransparency=no -> send(@transparency_key,active(@off)) ; send(@transparency_key,active(@on)))),

  /*
  g_user_option(skinmesh_bodies,SkinMeshBodies),
  send(@tile_options,append,(new(@skinmesh_bodies_menu,menu('Skinmesh Bodies:',tick_box)))),
  send(@skinmesh_bodies_menu,append,menu_item(no_change, message(@prolog,set_skinmesh_bodies,no_change))),
  send(@skinmesh_bodies_menu,append,menu_item(make, message(@prolog,set_skinmesh_bodies,make))),
  send(@skinmesh_bodies_menu,alignment(left)),
  send(@skinmesh_bodies_menu,default(SkinMeshBodies)),
  (Classification\==character, Classification\==automatic -> send(@skinmesh_bodies_menu,active(@off)) ; send(@skinmesh_bodies_menu,active(@on))),
  */

  new(@water_options,dialog),

  g_user_option(do_water,DoWater),
  send(@water_options,append,(new(@do_water_menu,menu('Apply Water Fixups:',tick_box)))),
  send(@do_water_menu,append,menu_item(yes, message(@prolog,set_do_water,yes))),
  send(@do_water_menu,append,menu_item(no, message(@prolog,set_do_water,no))),
  send(@do_water_menu,default(DoWater)),
  send(@do_water_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@do_water_menu,active(@off)) ; send(@do_water_menu,active(@on))),

  g_user_option(water_key,WaterKey),
  send(@water_options,append,new(@water_key,text_item('Water Bitmap Key(s):',WaterKey))),
  send(@water_key,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@water_key,active(@off)) ;
  (DoWater=no -> send(@water_key,active(@off)) ; send(@water_key,active(@on)))),

  g_user_option(dynamic_water,DynamicWater),
  send(@water_options,append,(new(@dynamic_water_menu,menu('Dynamic Water:',tick_box)))),
  send(@dynamic_water_menu,append,menu_item(yes, message(@prolog,set_dynamic_water,yes))),
  send(@dynamic_water_menu,append,menu_item(no, message(@prolog,set_dynamic_water,no))),
  send(@dynamic_water_menu,append,menu_item(wavy, message(@prolog,set_dynamic_water,wavy))),
  send(@dynamic_water_menu,default(DynamicWater)),
  send(@dynamic_water_menu,alignment(center)),
  (Classification\==tile, Classification\==automatic -> send(@dynamic_water_menu,active(@off)) ;
  (DoWater=no -> send(@dynamic_water_menu,active(@off)) ; send(@dynamic_water_menu,active(@on)))),

  g_user_option(wave_height,WaveHeight),
  send(@water_options,append,(new(@wave_height_menu,int_item('Wave Height (x10cm):',low:=1,high:=10)))),
  send(@wave_height_menu,default(WaveHeight)),
  send(@wave_height_menu,right(@dynamic_water_menu)),
  send(@wave_height_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@wave_height_menu,active(@off)) ;
  (DoWater=yes, DynamicWater=wavy -> send(@wave_height_menu,active(@on)) ; send(@wave_height_menu,active(@off)))),

  g_user_option(rotate_water,RotateWater),
  send(@water_options,append,(new(@rotate_water_menu,menu('Water rotatetexture to:',tick_box)))),
  send(@rotate_water_menu,append,menu_item(1, message(@prolog,set_rotate_water,1))),
  send(@rotate_water_menu,append,menu_item(0, message(@prolog,set_rotate_water,0))),
  send(@rotate_water_menu,append,menu_item(no_change, message(@prolog,set_rotate_water,no_change))),
  send(@rotate_water_menu,default(RotateWater)),
  send(@rotate_water_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@rotate_water_menu,active(@off)) ;
  (DoWater=no -> send(@rotate_water_menu,active(@off)) ; send(@rotate_water_menu,active(@on)))),

  g_user_option(tile_water,TileWaterRepeatCount),
  (integer(TileWaterRepeatCount) -> concat_atom([TileWaterRepeatCount,x,TileWaterRepeatCount],TWRCDefault) ; TWRCDefault=TileWaterRepeatCount),
  send(@water_options,append,(new(@tile_water_menu,menu('Re-tile water:',tick_box)))),
  send(@tile_water_menu,append,menu_item('1x1', message(@prolog,set_tile_water,1))),
  send(@tile_water_menu,append,menu_item('2x2', message(@prolog,set_tile_water,2))),
  send(@tile_water_menu,append,menu_item('3x3', message(@prolog,set_tile_water,3))),
  send(@tile_water_menu,append,menu_item(no_change, message(@prolog,set_tile_water,no_change))),
  send(@tile_water_menu,default(TWRCDefault)),
  send(@tile_water_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@tile_water_menu,active(@off)) ;
  (DoWater=yes, DynamicWater\=wavy -> send(@tile_water_menu,active(@on)) ; send(@tile_water_menu,active(@off)))),

  new(@ground_options,dialog),

  g_user_option(rotate_ground,RotateGround),
  send(@ground_options,append,(new(@rotate_ground_menu,menu('Ground rotatetexture to:',tick_box)))),
  send(@rotate_ground_menu,append,menu_item(1, message(@prolog,set_rotate_ground,1))),
  send(@rotate_ground_menu,append,menu_item(0, message(@prolog,set_rotate_ground,0))),
  send(@rotate_ground_menu,append,menu_item(no_change, message(@prolog,set_rotate_ground,no_change))),
  send(@rotate_ground_menu,default(RotateGround)),
  send(@rotate_ground_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@rotate_ground_menu,active(@off)) ; send(@rotate_ground_menu,active(@on))),

  g_user_option(chamfer,Chamfer),
  send(@ground_options,append,new(@chamfer_menu,menu('Tile Edge Chamfers:',tick_box))),
  send(@chamfer_menu,append,menu_item(add, message(@prolog,set_chamfer,add))),
  send(@chamfer_menu,append,menu_item(delete, message(@prolog,set_chamfer,delete))),
  send(@chamfer_menu,append,menu_item(no_change, message(@prolog,set_chamfer,no_change))),
  send(@chamfer_menu,default(Chamfer)),
  send(@chamfer_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@chamfer_menu,active(@off)) ; send(@chamfer_menu,active(@on))),

  g_user_option(tile_ground,TileGroundRepeatCount),
  (integer(TileGroundRepeatCount) -> concat_atom([TileGroundRepeatCount,x,TileGroundRepeatCount],TGRCDefault) ; TGRCDefault=TileGroundRepeatCount),
  send(@ground_options,append,(new(@tile_ground_menu,menu('Re-tile ground planes:',tick_box)))),
  send(@tile_ground_menu,append,menu_item('1x1', message(@prolog,set_tile_ground,1))),
  send(@tile_ground_menu,append,menu_item('2x2', message(@prolog,set_tile_ground,2))),
  send(@tile_ground_menu,append,menu_item('3x3', message(@prolog,set_tile_ground,3))),
  send(@tile_ground_menu,append,menu_item(no_change, message(@prolog,set_tile_ground,no_change))),
  send(@tile_ground_menu,default(TGRCDefault)),
  send(@tile_ground_menu,alignment(left)),
  (Classification\==tile, Classification\==automatic -> send(@tile_ground_menu,active(@off)) ; send(@tile_ground_menu,active(@on))), 

  g_user_option(ground_key,GroundKey),
  send(@ground_options,append,new(@ground_key,text_item('Ground Bitmap Key(s):',GroundKey))),
  (Classification\==tile, Classification\==automatic -> send(@ground_key,active(@off)) ;
  ((RotateGround==no_change, Chamfer==no_change, TileGroundRepeatCount==no_change) -> send(@ground_key,active(@off)) ; send(@ground_key,active(@on)))),

  /*
  new(@rescale_options,dialog),

  g_user_option(rescaleXYZ,Rescale),
  send(@rescale_options,append,new(@rescale_menu,menu('Rescale X,Y,Z:',tick_box))),
  send(@rescale_menu,append,menu_item(no,message(@prolog,set_xyz_scale0,no))),
  send(@rescale_menu,append,menu_item(yes,message(@prolog,set_xyz_scale0,yes))),
  (Rescale=[X,Y,Z] ->
	atom_number(Xtext,X), atom_number(Ytext,Y), atom_number(Ztext,Z)
	;
	Xtext='1', Ytext='1', Ztext='1'),
  send(@rescale_options,append,new(@rescaleX,text_item('X',Xtext))),
  send(@rescale_options,append,new(@rescaleY,text_item('Y',Ytext))),
  send(@rescale_options,append,new(@rescaleZ,text_item('Z',Ztext))),
  (Rescale=='no' -> send(@rescale_menu,default(no)) ; send(@rescale_menu,default(yes))),
  (Rescale=='no' ->
	send(@rescaleX,active(@off)), send(@rescaleY,active(@off)),send(@rescaleZ,active(@off))
	;
	send(@rescaleX,active(@on)), send(@rescaleY,active(@on)),send(@rescaleZ,active(@on))
  ),
  */

  new(@button_row,dialog),

  send(@button_row,append,button('OK',and(
               message(@prolog,set_indir,B?selection),
               message(@prolog,set_pattern,C?selection),
               message(@prolog,set_outdir,D?selection),
               message(@prolog,set_logfile,E?selection),
               message(@prolog,set_small_log,F?selection),
               message(@prolog,set_min_size,@min_size_menu?selection),
               message(@prolog,set_aabb_from,@aabb_from_menu?selection),
               message(@prolog,set_aabb_to,@aabb_to_menu?selection),
               message(@prolog,set_water_key,@water_key?selection),
               message(@prolog,set_wave_height,@wave_height_menu?selection),
               message(@prolog,set_foliage_key,@foliage_key?selection),
               message(@prolog,set_splotch_key,@splotch_key?selection),
               message(@prolog,set_ground_key,@ground_key?selection),
               message(@prolog,set_slice_height,@slice_height_menu?selection),
               message(@prolog,set_tile_raise_amount,@tile_raise_amount_menu?selection),
               message(@prolog,set_transparency_key,@transparency_key?selection),
   %            message(@prolog,set_xyz_scale0,@rescale_menu?selection),
   %            message(@prolog,set_xyz_scaleX,@rescaleX?selection),
   %            message(@prolog,set_xyz_scaleY,@rescaleY?selection),
   %            message(@prolog,set_xyz_scaleZ,@rescaleZ?selection),
               message(A,return,'OK')
                                ))),

  send(@button_row,append,button('Cancel',message(@prolog,halt))),

  /* Do the screen layout here */

  send(@tile_options,right,@shadow_options),
  % send(@rescale_options,right,A),
  send(@ground_options,right,@water_options),
  send(@water_options,below,@shadow_options),
  send(A,above,@shadow_options),
  send(@button_row,below,@shadow_options),

  get(A,confirm,_),
  send(A,destroy).

set_indir(F)      :- absolute_file_name(F,F0), prolog_to_os_filename(F1,F0), exists_directory(F1), asserta(g_indir(F1)).
set_pattern(F)    :- asserta(g_pattern(F)).
set_outdir(F)     :- absolute_file_name(F,F0), prolog_to_os_filename(F1,F0), asserta(g_outdir(F1)).
set_logfile(F)    :- prolog_to_os_filename(F1,F), file_base_name(F1,F2), asserta(g_logfile(F2)).
set_small_log(F)  :- prolog_to_os_filename(F1,F), file_base_name(F1,F2), asserta(g_small_log(F2)).
set_snap(F)       :- asserta(g_user_option(snap,F)).

set_shadow(none) :-
  asserta(g_user_option(shadow,none)),
  set_repivot(none),
  send_list(@repivot_menu,[selected(all,false),selected(if_needed,false),selected(none,true),active(@off)]), !.

set_shadow(S)    :-
  asserta(g_user_option(shadow,S)),
  send(@repivot_menu,active(@on)).

set_repivot(none) :-
  asserta(g_user_option(repivot,none)),
  set_splitting(no),
  send_list(@splitting_menu,[selected(yes,false),selected(no,true),active(@off)]),
  send(@split_priority_menu,active(@off)),
  send(@use_smoothed_menu,active(@off)),
  send(@below_z_menu,active(@off)),
  send(@min_size_menu,active(@off)),
  send(@move_bad_menu,active(@off)), !.

set_repivot(R) :-
  asserta(g_user_option(repivot,R)),
  send(@splitting_menu,active(@on)),
  send(@below_z_menu,active(@on)),
  send(@move_bad_menu,active(@on)),
  once(g_user_option(allow_split,Split)),
  (Split=yes ->
     send(@use_smoothed_menu,active(@on)),
     send(@split_priority_menu,active(@on)),
     send(@min_size_menu,active(@on));
     send(@use_smoothed_menu,active(@off)),
     send(@split_priority_menu,active(@off)),
     send(@min_size_menu,active(@off))).

set_splitting(yes) :-
  asserta(g_user_option(allow_split,yes)),
  send(@use_smoothed_menu,active(@on)),
  send(@split_priority_menu,active(@on)),
  send(@min_size_menu,active(@on)).

set_splitting(no) :-
  asserta(g_user_option(allow_split,no)),
  send(@use_smoothed_menu,active(@off)),
  send(@split_priority_menu,active(@off)),
  send(@min_size_menu,active(@off)).

set_do_water(yes) :-
  asserta(g_user_option(do_water,yes)),
  send(@water_key,active(@on)),
  send(@dynamic_water_menu,active(@on)),
  once(g_user_option(dynamic_water,DynamicWater)),
  (DynamicWater==wavy ->
     send(@wave_height_menu,active(@on)),
     send(@tile_water_menu,active(@off))
     ;
     send(@wave_height_menu,active(@off)),
     send(@tile_water_menu,active(@on))
  ),
  send(@rotate_water_menu,active(@on)).

set_do_water(no) :-
  asserta(g_user_option(do_water,no)),
  send(@water_key,active(@off)),
  send(@dynamic_water_menu,active(@off)),
  send(@wave_height_menu,active(@off)),
  send(@tile_water_menu,active(@off)),
  send(@rotate_water_menu,active(@off)).

set_dynamic_water(yes)  :-
  asserta(g_user_option(dynamic_water,yes)),
  send(@wave_height_menu,active(@off)),
  send(@tile_water_menu,active(@on)).

set_dynamic_water(no)  :-
  asserta(g_user_option(dynamic_water,no)),
  send(@wave_height_menu,active(@off)),
  send(@tile_water_menu,active(@on)).

set_dynamic_water(wavy)  :-
  asserta(g_user_option(dynamic_water,wavy)),
  send(@wave_height_menu,active(@on)),
  send(@tile_water_menu,active(@off)).

set_foliage(Foliage) :-
  asserta(g_user_option(foliage,Foliage)),
  (Foliage==ignore -> send(@foliage_key,active(@off)) ; send(@foliage_key,active(@on))).

set_splotch(animate) :-
  asserta(g_user_option(splotch,animate)),
  send(@splotch_key,active(@on)).

set_splotch(ignore) :-
  asserta(g_user_option(splotch,ignore)),
  send(@splotch_key,active(@off)).

set_transparency(no) :-
  asserta(g_user_option(placeable_with_transparency,no)),
  send(@transparency_key,active(off)).

set_transparency(yes) :-
  asserta(g_user_option(placeable_with_transparency,yes)),
  send(@transparency_key,active(on)).

set_rotate_ground(RotateGround) :-
  asserta(g_user_option(rotate_ground,RotateGround)),
  once(g_user_option(chamfer,Chamfer)),
  once(g_user_option(tile_ground,TileGroundRepeatCount)),
  ((RotateGround==no_change, Chamfer==no_change, TileGroundRepeatCount==no_change) -> send(@ground_key,active(@off)); send(@ground_key,active(@on))).
  
set_chamfer(Chamfer) :-
  asserta(g_user_option(chamfer,Chamfer)),
  once(g_user_option(rotate_ground,RotateGround)),
  once(g_user_option(tile_ground,TileGroundRepeatCount)),
  ((RotateGround==no_change, Chamfer==no_change, TileGroundRepeatCount==no_change) -> send(@ground_key,active(@off)); send(@ground_key,active(@on))).

set_tile_ground(TileGroundRepeatCount) :-
  asserta(g_user_option(tile_ground,TileGroundRepeatCount)),
  once(g_user_option(rotate_ground,RotateGround)),
  once(g_user_option(chamfer,Chamfer)),
  ((RotateGround==no_change, Chamfer==no_change, TileGroundRepeatCount==no_change) -> send(@ground_key,active(@off)); send(@ground_key,active(@on))).

set_slice(yes) :-
  asserta(g_user_option(slice,yes)),
  send(@slice_height_menu,active(@on)).

set_slice(no) :-
  asserta(g_user_option(slice,no)),
  send(@slice_height_menu,active(@off)).

set_slice(undo) :-
  asserta(g_user_option(slice,undo)),
  send(@slice_height_menu,active(@off)).

set_class(Classification) :-
  asserta(g_user_option(classification,Classification)),
  (Classification\==tile, Classification\==automatic ->
     send(@do_water_menu,active(@off)),
     send(@water_key,active(@off)),
     send(@dynamic_water_menu,active(@off)),
     send(@wave_height_menu,active(@off)),
     send(@tile_water_menu,active(@off)),
     send(@rotate_water_menu,active(@off)),
     send(@fix_overhang_menu,active(@off)),
     send(@map_aabb_menu,active(@off)),
     send(@aabb_from_menu,active(@off)),
     send(@aabb_to_menu,active(@off)),
     send(@slice_menu,active(@off)),
     send(@slice_height_menu,active(@off)),
     send(@foliage_menu,active(@off)),
     send(@foliage_key,active(@off)),
     send(@splotch_menu,active(@off)),
     send(@splotch_key,active(@off)),
     send(@rotate_ground_menu,active(@off)),
     send(@ground_key,active(@off)),
     send(@chamfer_menu,active(@off)),
     send(@tile_ground_menu,active(@off)),
     send(@tile_raise_menu,active(@off)),
     send(@tile_raise_amount_menu,active(@off))
     ;
     send(@do_water_menu,active(@on)),
     once(g_user_option(do_water,DoWater)),
     (DoWater==no ->
       send(@water_key,active(@off)),
       send(@dynamic_water_menu,active(@off)),
       send(@wave_height_menu,active(@off)),
       send(@rotate_water_menu,active(@off)),
       send(@tile_water_menu,active(@off))
       ;
       send(@water_key,active(@on)),
       send(@dynamic_water_menu,active(@on)),
       once(g_user_option(dynamic_water,DynamicWater)),
       (DynamicWater==wavy ->
          send(@wave_height_menu,active(@on)),
          send(@tile_water_menu,active(@off))
          ;
          send(@wave_height_menu,active(@off)),
          send(@tile_water_menu,active(@on))
       ),
       send(@rotate_water_menu,active(@on))
     ),
     send(@fix_overhang_menu,active(@on)),
     send(@map_aabb_menu,active(@on)),
     once(g_user_option(map_aabb_material,MapAABB)),
     (MapAABB==yes ->
       send(@aabb_from_menu,active(@on)),
       send(@aabb_to_menu,active(@on))
       ;
       send(@aabb_from_menu,active(@off)),
       send(@aabb_to_menu,active(@off))
     ),
     send(@slice_menu,active(@on)),
     once(g_user_option(slice,Slice)),
     (Slice==yes ->
       send(@slice_height_menu,active(@on))
       ;
       send(@slice_height_menu,active(@off))
     ),
     send(@foliage_menu,active(@on)),
     once(g_user_option(foliage,Foliage)),
     (Foliage=ignore ->
        send(@foliage_key,active(@off))
        ;
        send(@foliage_key,active(@on))
     ),
     send(@splotch_menu,active(@on)),
     once(g_user_option(splotch,Splotch)),
     (Splotch=ignore ->
        send(@splotch_key,active(@off))
        ;
        send(@splotch_key,active(@on))
     ),
     send(@rotate_ground_menu,active(@on)),
     send(@chamfer_menu,active(@on)),
     send(@tile_ground_menu,active(@on)),
     once(g_user_option(rotate_ground,RotateGround)),
     once(g_user_option(chamfer,Chamfer)),
     once(g_user_option(tile_ground,TileGroundRepeatCount)),
     ((RotateGround==no_change, Chamfer==no_change, TileGroundRepeatCount==no_change) -> send(@ground_key,active(@off)); send(@ground_key,active(@on))),
     send(@tile_raise_menu,active(@on)),
     once(g_user_option(tile_raise,TileRaise)),
     (TileRaise==no ->
        send(@tile_raise_amount_menu,active(@off))
        ;
        send(@tile_raise_amount_menu,active(@on))
     )
  ),
  (Classification\==character, Classification\=automatic ->
   % send(@skinmesh_bodies_menu,active(@off)),
   send(@placeable_with_transparency_menu,active(@off)),
   send(@transparency_key,active(off))
   ;
   % send(@skinmesh_bodies_menu,active(@on)),
   send(@placeable_with_transparency_menu,active(@on)),
   once(g_user_option(placeable_with_transparency,PlaceableWithTransparency)),
   (PlaceableWithTransparency==no -> send(@transparency_key,active(off)) ; send(@transparency_key,active(on)))
  ).

set_map_aabb(no) :-
  asserta(g_user_option(map_aabb_material,no)),
  send(@aabb_from_menu,active(@off)),
  send(@aabb_to_menu,active(@off)).

set_map_aabb(yes) :-
  asserta(g_user_option(map_aabb_material,yes)),
  send(@aabb_from_menu,active(@on)),
  send(@aabb_to_menu,active(@on)).


set_aabb_from(From)   :- asserta(g_user_option(map_aabb_from,From)).
set_aabb_to(To)       :- asserta(g_user_option(map_aabb_to,To)).


set_tile_raise(TileRaise) :-
  asserta(g_user_option(tile_raise,TileRaise)),
  (TileRaise==no ->
     send(@tile_raise_amount_menu,active(@off))
     ;
     send(@tile_raise_amount_menu,active(@on))
  ).


set_split_priority(F)    :- asserta(g_user_option(split_Priority,F)).
set_use_smoothed(F)      :- asserta(g_user_option(use_Smoothed,F)).
set_min_size(F)          :- asserta(g_user_option(min_Size,F)).
set_force_white(F)       :- asserta(g_user_option(force_white,F)).
set_water_key(F)         :- asserta(g_user_option(water_key,F)).
set_wave_height(F)       :- asserta(g_user_option(wave_height,F)).
set_rotate_water(F)      :- asserta(g_user_option(rotate_water,F)).
set_foliage_key(F)       :- asserta(g_user_option(foliage_key,F)).
set_splotch_key(F)       :- asserta(g_user_option(splotch_key,F)).
set_fix_overhangs(F)     :- asserta(g_user_option(fix_overhangs,F)).
set_tvert_snap(F)        :- asserta(g_user_option(tvert_snap,F)).
set_slice_height(F)      :- Fm is 0.1*F, asserta(g_user_option(slice_height,Fm)).
set_below_z(F)           :- asserta(g_user_option('pivots_below_z=0',F)).
set_move_bad(F)          :- asserta(g_user_option(move_bad_pivots,F)).
set_ground_key(F)        :- asserta(g_user_option(ground_key,F)).
set_tile_raise_amount(F) :- asserta(g_user_option(tile_raise_amount,F)).
set_tile_water(F)        :- asserta(g_user_option(tile_water,F)).
set_meshmerge(F)         :- asserta(g_user_option(merge_by_bitmap,F)).
set_transparency_key(F)  :- asserta(g_user_option(transparency_key,F)).
set_cull_invisible(F)    :- asserta(g_user_option(invisible_mesh_cull,F)).
set_render_all(F)        :- asserta(g_user_option(render,F)).
% set_skinmesh_bodies(F)   :- asserta(g_user_option(skinmesh_bodies,F)).

/*
set_xyz_scale0(no) :-
	asserta(g_user_option(rescaleXYZ,no)),
	send(@rescaleX,active(@off)), send(@rescaleY,active(@off)), send(@rescaleZ,active(@off)).

set_xyz_scale0(yes) :-
	asserta(g_user_option(rescaleXYZ,yes)),
	send(@rescaleX,active(@on)), send(@rescaleY,active(@on)), send(@rescaleZ,active(@on)).

set_xyz_scaleX(_)        :- once(g_user_option(rescaleXYZ,R)), R==no, !.
set_xyz_scaleX(Xs)       :- atom_number(Xs,X), asserta(g_user_option(rescaleXYZ,[X|_])).
set_xyz_scaleY(_ )       :- once(g_user_option(rescaleXYZ,R)), R==no, !.
set_xyz_scaleY(Ys)       :- once(g_user_option(rescaleXYZ,[X|_])), atom_number(Ys,Y), asserta(g_user_option(rescaleXYZ,[X,Y|_])).
set_xyz_scaleZ(_ )       :- once(g_user_option(rescaleXYZ,R)), R==no, !.
set_xyz_scaleZ(Zs)       :- once(g_user_option(rescaleXYZ,[X,Y|_])), atom_number(Zs,Z), asserta(g_user_option(rescaleXYZ,[X,Y,Z])).
*/

