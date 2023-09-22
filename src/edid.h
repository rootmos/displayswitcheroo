#pragma once

#include <stddef.h>
#include <lua.h>

struct edid {
    uint32_t fingerprint;

    char vendor[4];

    uint16_t product_code;
    uint32_t serial_number;

    int week_of_manufacture;
    int year_of_manufacture;
    int year_of_model;

    uint8_t version_major;
    uint8_t version_minor;
};

void edid_parse(struct edid* edid, void* buf, size_t len);
int edid_push(lua_State* L, const struct edid* edid);
