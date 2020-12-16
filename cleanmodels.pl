/* ============================== */ 
/*                                */
/* CleanModels3  by OldMansBeard  */
/*                                */
/* This version dated 2014-05-08  */
/* Later modifications by orth    */
/*                                */
/* ============================== */ 

:- dynamic bug_count/2, total_fixes/1.
:- use_module(library(optparse), [opt_arguments/3]).

:- ensure_loaded(output_models).
:- ensure_loaded(load_binary).
:- ensure_loaded(load_models).
:- ensure_loaded(make_checks).
:- ensure_loaded(fix_pivots).
:- ensure_loaded(tilefade).
:- ensure_loaded(rebuild_wok).

version('CleanModels 3 Version 3.7.0').

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
  downcase_atom(File,F1),
  output_file(F1),
  nl, nl,
  write(SmallLogStream,'  Exported okay.'),
  nl(SmallLogStream), nl(SmallLogStream),
  working_directory(OutDir,WkDir),
  !.

/* ============== */
/* show_help   /1 */
/* ============== */

show_help(OptsSpec) :-
  opt_help(OptsSpec, HelpText),
  write(HelpText), nl,
  halt.

/* ============== */
/* save_program/1 */
/* ============== */

save_program(File) :-
  qsave_program(File,[goal(go),stand_alone(true),foreign(save)]).

/* =========== */
/* init_dirs/6 */
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
% g_user_option(rescaleXYZ,no).

init_dirs(InDir,Pattern,OutDir,LogFile,SmallLog,Decompile) :-
  /* InitFile = 'last_dirs.pl', */
  OptsSpec =
     [[opt(help), type(boolean), default(false),
        shortflags(['h']), longflags(['help']),
        help('provides command line help')],
     [opt(decompile), type(boolean), default(false),
        shortflags(['d']), longflags(['decompile']),
        help('only does decompilation, no fixes')],
     [opt(indir), type(atom), default(''),
        shortflags([i]), longflags(['indir']),
        help('directory containing models to be fixed/decompiled')],
     [opt(outdir), type(atom), default(''),
        shortflags([o]), longflags(['outdir']),
        help('output directory of fixed/decompiled models')],
     [opt(classification), type(atom), default(''),
        shortflags([c]), longflags(['classification']),
        help('model type assumption for fixes')],
     [opt(pattern), type(atom), default(''),
        shortflags([p]), longflags(['pattern']),
        help('filename pattern of models to process')]
     ],
  opt_arguments(OptsSpec, Opts, PositionalArgs),
  (current_prolog_flag(saved_program,true), PositionalArgs\==[] -> [InitFile|_]=PositionalArgs ; InitFile = 'last_dirs.pl'),
  memberchk(help(Help), Opts),
  (Help==true -> show_help(OptsSpec) ; true),
  memberchk(decompile(Decompile), Opts),
  memberchk(indir(OptsInDir), Opts),
  memberchk(outdir(OptsOutDir), Opts),
  memberchk(pattern(OptsPattern), Opts),
  memberchk(classification(OptsClassification), Opts),
  (exists_file(InitFile) -> consult(InitFile); true),
  (OptsClassification\=='' -> set_classification(OptsClassification) ; true),
  (OptsInDir\=='' -> set_indir(OptsInDir) ; true),
  (OptsOutDir\=='' -> set_outdir(OptsOutDir) ; true),
  (OptsPattern\=='' -> set_pattern(OptsPattern) ; true),
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
  % writeq(:-asserta(g_user_option(rescaleXYZ,Rescale))), write('.'), nl,

  /* Support for secret options added in CM343f */
  (secret(Secret), writeq(:-asserta(secret(Secret))), write('.'), nl, fail ; true),
  told.

set_indir(F)          :- absolute_file_name(F,F0), prolog_to_os_filename(F1,F0), exists_directory(F1), asserta(g_indir(F1)).
set_pattern(F)        :- asserta(g_pattern(F)).
set_outdir(F)         :- absolute_file_name(F,F0), prolog_to_os_filename(F1,F0), asserta(g_outdir(F1)).
set_logfile(F)        :- prolog_to_os_filename(F1,F), file_base_name(F1,F2), asserta(g_logfile(F2)).
set_small_log(F)      :- prolog_to_os_filename(F1,F), file_base_name(F1,F2), asserta(g_small_log(F2)).
set_classification(F) :- asserta(g_user_option(classification,F)).
set_snap(F)           :- asserta(g_user_option(snap,F)).




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

