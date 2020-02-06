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
#include <math.h>

#include <defaultatoms.h>
#include <context.h>
#include <mailbox.h>

#include "display_driver.h"

#include "../../components/gnuboy/cpu.h"
#include "../../components/gnuboy/fb.h"
#include "../../components/gnuboy/lcd.h"
#include "../../components/gnuboy/lcdc.h"
#include "../../components/gnuboy/gnuboy.h"
#include "../../components/gnuboy/loader.h"
#include "../../components/gnuboy/pcm.h"
#include "../../components/gnuboy/regs.h"
#include "../../components/gnuboy/rtc.h"
#include "../../components/gnuboy/hw.h"
#include "../../components/gnuboy/sound.h"

#include "freertos/FreeRTOS.h"
#include "esp_system.h"
#include "esp_event.h"
#include "esp_event_loop.h"
#include "esp_heap_caps.h"

#include "../../AtomVM/src/platforms/esp32/main/esp32_sys.h"

#define AUDIO_SAMPLE_RATE 64
#define AUDIO_BUFFER_SIZE 32
#define audioBufferLength 32

static void gbemu_consume_mailbox(Context *ctx);

struct fb fb;
struct pcm pcm;
uint16_t *displayBuffer[2];
uint16_t *framebuffer;
int frame = 0;

byte *PSRAM;

static xQueueHandle gbemu_queue;

void run_to_vblank()
{
  /* FRAME BEGIN */

  /* FIXME: djudging by the time specified this was intended
  to emulate through vblank phase which is handled at the
  end of the loop. */
  cpu_emulate(2280);

  /* FIXME: R_LY >= 0; comparsion to zero can also be removed
  altogether, R_LY is always 0 at this point */
  while (R_LY > 0 && R_LY < 144)
  {
    /* Step through visible line scanning phase */
    emu_step();
  }

  /* VBLANK BEGIN */

  rtc_tick();

  sound_mix();

  //if (pcm.pos > 100)

  if (!(R_LCDC & 0x80)) {
    /* LCDC operation stopped */
    /* FIXME: djudging by the time specified, this is
    intended to emulate through visible line scanning
    phase, even though we are already at vblank here */
    cpu_emulate(32832);
  }

  while (R_LY > 0) {
    /* Step through vblank phase */
    emu_step();
  }
}

void gb_main_loop(void *arg)
{
    TickType_t xLastWakeTime = xTaskGetTickCount();

    while (1) {
        run_to_vblank();
        frame++;
        xQueueSend(event_queue, &arg, 1);

        uint32_t cmd = 0;
        xQueueReceive(gbemu_queue, &cmd, 0);

        if (cmd != 0) {
            uint8_t button = cmd & 0xFF;
            pad_set(button, cmd >> 8);
        }

        vTaskDelayUntil(&xLastWakeTime, 17 / portTICK_PERIOD_MS);
    }
}

void gbemu_callback(EventListener *listener)
{
    Context *ctx = listener->data;

    Message *message = malloc(sizeof(Message) + (5 + 8) * sizeof(term));
    message->msg_memory_size = 5 + 8;
    list_init(&message->mailbox_list_head);

    term *msg_terms = &message->message + 1;

    msg_terms[0] = (4 << 6);
    term msg = ((term) msg_terms) | 0x2;

    msg_terms[5] = (7 << 6);
    term t = ((term) (msg_terms + 5)) | 0x2;

    message->message = msg;

    term_put_tuple_element(msg, 0, term_invalid_term());
    term_put_tuple_element(msg, 1, term_invalid_term());
    term_put_tuple_element(msg, 2, term_invalid_term());
    term_put_tuple_element(msg, 3, t);

    term_put_tuple_element(t, 0, context_make_atom(ctx, "\xB" "draw_buffer"));
    term_put_tuple_element(t, 1, term_from_int(80));
    term_put_tuple_element(t, 2, term_from_int(48));
    term_put_tuple_element(t, 3, term_from_int(160));
    term_put_tuple_element(t, 4, term_from_int(144));
    term_put_tuple_element(t, 5, term_from_int(((unsigned long) framebuffer) & 0xFFFF));
    term_put_tuple_element(t, 6, term_from_int(((unsigned long) framebuffer) >> 16));

    display_enqueue_message(message);
}

void gbemu_driver_init(Context *ctx, term opts)
{
    ctx->platform_data = NULL;
    ctx->native_handler = gbemu_consume_mailbox;

    struct stat statbuf;
    if (stat("/sdcard/game.gb", &statbuf) != 0) {
        fprintf(stderr, "/sdcard/game.gb is missing\n");
        return;
    }

    fprintf(stderr, "File size: %i\n", (int) statbuf.st_size);

    PSRAM = malloc(statbuf.st_size);
    if (!PSRAM) {
        fprintf(stderr, "Failed to alloc memory.\n");
    }

    FILE *gbfile = fopen("/sdcard/game.gb", "rb");
    if (fread(PSRAM, 1, 1024 * 1024 * 2, gbfile) <= 0) {
        fprintf(stderr, "Failed to load\n");
        return;
    }

    loader_init(NULL);

    displayBuffer[0] = heap_caps_malloc(160 * 144 * 2, MALLOC_CAP_8BIT | MALLOC_CAP_DMA);
    displayBuffer[1] = heap_caps_malloc(160 * 144 * 2, MALLOC_CAP_8BIT | MALLOC_CAP_DMA);

    framebuffer = displayBuffer[0];

    for (int i = 0; i < 2; ++i) {
        memset(displayBuffer[i], 0, 160 * 144 * 2);
    }

    emu_reset();

    memset(&fb, 0, sizeof(fb));
    fb.w = 160;
    fb.h = 144;
    fb.pelsize = 2;
    fb.pitch = fb.w * fb.pelsize;
    fb.indexed = 0;
    fb.ptr = framebuffer;
    fb.enabled = 1;
    fb.dirty = 0;

    memset(&pcm, 0, sizeof(pcm));
    pcm.hz = AUDIO_SAMPLE_RATE;
    pcm.stereo = 1;
    pcm.len = /*pcm.hz / 2*/ audioBufferLength;
    pcm.buf = 0; //TODO: Enable audio buf here: heap_caps_malloc(AUDIO_BUFFER_SIZE, MALLOC_CAP_8BIT | MALLOC_CAP_DMA);
    pcm.pos = 0;

    sound_reset();

    lcd_begin();

    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    EventListener *gbemu_listener = malloc(sizeof(EventListener));
    gbemu_listener->sender = gbemu_listener;
    gbemu_listener->data = ctx;
    gbemu_listener->handler = gbemu_callback;
    list_append(&platform->listeners, &gbemu_listener->listeners_list_head);

    gbemu_queue = xQueueCreate(8, sizeof(uint32_t));

    xTaskCreate(gb_main_loop, "gbemu", 10000, gbemu_listener, 1, NULL);
}

static void gbemu_consume_mailbox(Context *ctx)
{
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;

    term pid = term_get_tuple_element(msg, 1);
    term ref = term_get_tuple_element(msg, 2);
    term req = term_get_tuple_element(msg, 3);

    term cmd = term_get_tuple_element(req, 0);

    if (cmd == context_make_atom(ctx, "\x6" "button")) {
        term button = term_get_tuple_element(req, 1);

        uint32_t cmdq = 0;
        if (button == context_make_atom(ctx, "\x5" "start")) {
            cmdq = PAD_START;
        } else if (button == context_make_atom(ctx, "\x6" "select")) {
            cmdq = PAD_SELECT;
        } else if (button == context_make_atom(ctx, "\x1" "a")) {
            cmdq = PAD_A;
        } else if (button == context_make_atom(ctx, "\x1" "b")) {
            cmdq = PAD_B;
        } else if (button == context_make_atom(ctx, "\x4" "left")) {
            cmdq = PAD_LEFT;
        } else if (button == context_make_atom(ctx, "\x2" "up")) {
            cmdq = PAD_UP;
        } else if (button == context_make_atom(ctx, "\x5" "right")) {
            cmdq = PAD_RIGHT;
        } else if (button == context_make_atom(ctx, "\x4" "down")) {
            cmdq = PAD_DOWN;
        }

        if (cmdq != 0) {
            cmdq |= 0x100;
            xQueueSend(gbemu_queue, &cmdq, 1);
            vTaskDelay(50 / portTICK_PERIOD_MS);
            cmdq &= 0xFF;
            xQueueSend(gbemu_queue, &cmdq, 1);
        }
    }

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        abort();
    }
    term return_tuple = term_alloc_tuple(2, ctx);

    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, OK_ATOM);

    mailbox_send(target, return_tuple);
}
