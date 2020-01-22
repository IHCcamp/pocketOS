/***************************************************************************
 *   Copyright 2020 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include <stdio.h>

#include <context.h>

#include "display_driver.h"

// This function overwrites a weak symbol
Context *sys_create_port_fallback(Context *new_ctx, const char *driver_name, term opts)
{

    if (!strcmp(driver_name, "display")) {
        display_driver_init(new_ctx, opts);
    } else {
        context_destroy(new_ctx);
        return NULL;
    }
    return new_ctx;
}
