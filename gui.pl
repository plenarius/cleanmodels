/* ==================================================== */
/*                                                      */
/* Predicates for generating skinmesh bodies from parts */
/* Part of the CleanModels 3 suite by OldMansBeard      */
/*                                                      */
/* This version dated 2014-01-16                        */
/*                                                      */
/* ==================================================== */
:- use_module(library(pce)).

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