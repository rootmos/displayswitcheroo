#include "edid.h"
#include "r.h"
#include "x11.h"

#include <lua.h>
#include <lauxlib.h>

#include <endian.h>
#include <string.h>

// https://en.wikipedia.org/wiki/Extended_Display_Identification_Data#EDID_1.4_data_format

PRIVATE void edid_parse(struct edid* edid, void* buf, size_t len)
{
    memset(edid, 0, sizeof(*edid));

    uint8_t* p = buf;
    if(len < 20) {
        failwith("too few header bytes");
    }

    struct sha1_state sha1;
    sha1_init(&sha1);
    sha1_update(&sha1, p, 20);
    sha1_finalize(&sha1);
    edid->fingerprint = be32toh(*(uint32_t*)sha1.digest);

    uint8_t magic[] = {0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00};
    if(memcmp(p, magic, sizeof(magic)) != 0) {
        failwith("unexpected magic bytes");
    }

    uint16_t vendor = be16toh(*((uint16_t*)&p[8]));
    edid->vendor[0] = 'A' - 1 + ((vendor & 0b11111 << 10) >> 10);
    edid->vendor[1] = 'A' - 1 + ((vendor & 0b11111 << 5) >> 5);
    edid->vendor[2] = 'A' - 1 + (vendor & 0b11111);
    edid->vendor[3] = 0;

    edid->product_code = le16toh(*((uint16_t*)&p[10]));
    edid->serial_number = le32toh(*((uint32_t*)&p[12]));

    edid->week_of_manufacture = p[16];
    if(edid->week_of_manufacture == 0xFF) {
        edid->week_of_manufacture = -1;
        edid->year_of_manufacture = -1;
        edid->year_of_model = 1990 + p[17];
    } else {
        edid->year_of_manufacture = 1990 + p[17];
        edid->year_of_model = -1;
    }

    edid->version_major = p[18];
    edid->version_minor = p[19];
}

PRIVATE int edid_push(lua_State* L, const struct edid* edid)
{
    luaR_stack(L);

    lua_createtable(L, 0, 6);

    if(luaL_newmetatable(L, TYPE_EDID)) {
    }
    lua_setmetatable(L, -2);

    lua_pushinteger(L, edid->fingerprint);
    lua_setfield(L, -2, "fingerprint");

    lua_pushstring(L, edid->vendor);
    lua_setfield(L, -2, "vendor");

    lua_pushinteger(L, edid->product_code);
    lua_setfield(L, -2, "product_code");

    lua_pushinteger(L, edid->serial_number);
    lua_setfield(L, -2, "serial_number");

    if(edid->year_of_model >= 0) {
        lua_pushinteger(L, edid->year_of_model);
        lua_setfield(L, -2, "year_of_model");
    } else {
        lua_pushinteger(L, edid->week_of_manufacture);
        lua_setfield(L, -2, "week_of_manufacture");
        lua_pushinteger(L, edid->year_of_manufacture);
        lua_setfield(L, -2, "year_of_manufacture");
    }

    lua_pushinteger(L, edid->version_major);
    lua_setfield(L, -2, "version_major");
    lua_pushinteger(L, edid->version_minor);
    lua_setfield(L, -2, "version_minor");

    luaR_return(L, 1);
}
