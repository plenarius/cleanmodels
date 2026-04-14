// oracle_spawn.nss
// Run with: dm_runscript oracle_spawn
// Spawns all oracle test placeables around the PC so NWN loads and compiles their models.
// After running, open the debug console and type: compileloadedmodels
// Copy the resulting binaries from Documents\Neverwinter Nights\compiled_models\ to
// tests/fixtures/oracle/game_binary/ then run: go test ./pkg/mdl/ -run TestOracleCompare -v

void SpawnAt(string sResRef, location lBase, float fAngle, float fDist) {
    float fX = GetPositionFromLocation(lBase).x + fDist * cos(fAngle);
    float fY = GetPositionFromLocation(lBase).y + fDist * sin(fAngle);
    float fZ = GetPositionFromLocation(lBase).z;
    vector vPos = Vector(fX, fY, fZ);
    location lSpawn = Location(GetAreaFromLocation(lBase), vPos, fAngle * 180.0 / 3.14159);
    CreateObject(OBJECT_TYPE_PLACEABLE, sResRef, lSpawn);
}

void main() {
    location lBase = GetLocation(OBJECT_SELF);
    float fStep = 2.0 * 3.14159 / 8.0;

    SpawnAt("squid",          lBase, fStep * 0, 4.0);
    SpawnAt("plc_guillo2",    lBase, fStep * 1, 4.0);
    SpawnAt("plc_nc03",       lBase, fStep * 2, 4.0);
    SpawnAt("plc_dd27",       lBase, fStep * 3, 4.0);
    SpawnAt("plc_crysblu",    lBase, fStep * 4, 4.0);
    SpawnAt("plc_statdwl",    lBase, fStep * 5, 4.0);
    SpawnAt("zlc_o23",        lBase, fStep * 6, 4.0);
    SpawnAt("abp_weaprack_1", lBase, fStep * 7, 4.0);

    SendMessageToPC(OBJECT_SELF, "Oracle placeables spawned. Now run: compileloadedmodels");
}
