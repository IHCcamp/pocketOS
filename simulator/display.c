#include <stdio.h>
#include <pthread.h>
#include <SDL.h>
#include <unistd.h>

#include <AtomVM/context.h>
#include <AtomVM/defaultatoms.h>
#include <AtomVM/interop.h>
#include <AtomVM/mailbox.h>
#include <AtomVM/term.h>
#include <AtomVM/utils.h>

#define SCREEN_WIDTH 320
#define SCREEN_HEIGHT 240
#define BPP 4
#define DEPTH 32

#include "font.c"

static void consume_display_mailbox(Context *ctx);

SDL_Surface *screen;

void draw_image(SDL_Surface *screen, int x, int y, int width, int height, const char *data, Uint8 r, Uint8 g, Uint8 b)
{
    Uint32 background_color = SDL_MapRGB(screen->format, r, g, b);
    uint32_t *pixels = (uint32_t *) data;

    for (int i = 0; i < height; i++) {
        Uint32 *pixmem32 = (Uint32*) (((uint8_t *) screen->pixels) + (screen->w * (y + i) + x) * BPP);
        for (int j = 0; j < width; j++) {
            Uint32 color;
            if ((*pixels >> 24) & 0xFF) {
                color = SDL_MapRGB(screen->format, (*pixels) & 0xFF, (*pixels >> 8) & 0xFF, (*pixels >> 16) & 0xFF);
            } else {
                color = background_color;
            }
            pixmem32[j] = color;
            pixels++;
        }
    }
}

void draw_rect(SDL_Surface *screen, int x, int y, int width, int height, Uint8 r, Uint8 g, Uint8 b)
{
    Uint32 color = SDL_MapRGB(screen->format, r, g, b);

    for (int i = 0; i < height; i++) {
        Uint32 *pixmem32 = (Uint32*) (((uint8_t *) screen->pixels) + (screen->w * (y + i) + x) * BPP);
        for (int j = 0; j < width; j++) {
            pixmem32[j] = color;
        }
    }
}

void draw_text(SDL_Surface *screen, int x, int y, const char *text, Uint8 r, Uint8 g, Uint8 b)
{
    int len = strlen(text);
    Uint32 color = SDL_MapRGB(screen->format, r, g, b);

    for (int i = 0; i < len; i++) {
        unsigned const char *glyph = fontdata + ((unsigned char) text[i]) * 16;

        for (int j = 0; j < 16; j++) {
            unsigned char row = glyph[j];

            Uint32 *pixmem32 = (Uint32*) (((uint8_t *) screen->pixels) + (screen->w * (y + j) + x + i * 8) * BPP);
            for (int k = 0; k < 8; k++) {
                if (row & (1 << (7 - k))) {
                    pixmem32[k] = color;
                }
            }
        }
    }
}

static void consume_display_mailbox(Context *ctx)
{
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;

    term pid = term_get_tuple_element(msg, 1);
    term ref = term_get_tuple_element(msg, 2);
    term req = term_get_tuple_element(msg, 3);

    term cmd = term_get_tuple_element(req, 0);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    if (SDL_MUSTLOCK(screen)) {
        if (SDL_LockSurface(screen) < 0) {
            return;
        }
    }

    if (cmd == context_make_atom(ctx, "\xC" "clear_screen")) {
        int color = term_to_int(term_get_tuple_element(req, 1));

        draw_rect(screen, 0, 0, SCREEN_WIDTH, SCREEN_HEIGHT,
                (color >> 11) << 3, ((color >> 5) & 0x3F) << 2, (color & 0x1F) << 3);

    } else if (cmd == context_make_atom(ctx, "\xA" "draw_image")) {
        int x = term_to_int(term_get_tuple_element(req, 1));
        int y = term_to_int(term_get_tuple_element(req, 2));
        term img = term_get_tuple_element(req, 3);
        int color = term_to_int(term_get_tuple_element(req, 4));

        int width = term_to_int(term_get_tuple_element(img, 0));
        int height = term_to_int(term_get_tuple_element(img, 1));
        const char *data = term_binary_data(term_get_tuple_element(img, 2));

        draw_image(screen, x, y, width, height, data, (color >> 11) << 3,
                ((color >> 5) & 0x3F) << 2, (color & 0x1F) << 3);

    } else if (cmd == context_make_atom(ctx, "\x9" "draw_rect")) {
        int x = term_to_int(term_get_tuple_element(req, 1));
        int y = term_to_int(term_get_tuple_element(req, 2));
        int width = term_to_int(term_get_tuple_element(req, 3));
        int height = term_to_int(term_get_tuple_element(req, 4));
        int color = term_to_int(term_get_tuple_element(req, 5));

        draw_rect(screen, x, y, width, height,
                (color >> 11) << 3, ((color >> 5) & 0x3F) << 2, (color & 0x1F) << 3);

    } else if (cmd == context_make_atom(ctx, "\x9" "draw_text")) {
        int x = term_to_int(term_get_tuple_element(req, 1));
        int y = term_to_int(term_get_tuple_element(req, 2));
        term text_term = term_get_tuple_element(req, 3);
        int color = term_to_int(term_get_tuple_element(req, 4));

        int ok;
        char *text = interop_term_to_string(text_term, &ok);

        draw_text(screen, x, y, text, (color >> 11) << 3, ((color >> 5) & 0x3F) << 2, (color & 0x1F) << 3);

        free(text);

    } else {
        fprintf(stderr, "display: ");
        term_display(stderr, req, ctx);
        fprintf(stderr, "\n");
    }

    if(SDL_MUSTLOCK(screen)) {
        SDL_UnlockSurface(screen);
    }

    SDL_Flip(screen);

    free(message);

    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        abort();
    }
    term return_tuple = term_alloc_tuple(2, ctx);

    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, OK_ATOM);

    mailbox_send(target, return_tuple);
}

void display_port_driver_init(Context *ctx, term opts)
{
    ctx->native_handler = consume_display_mailbox;
    ctx->platform_data = NULL;

    if (SDL_Init(SDL_INIT_VIDEO) < 0 ) {
        abort();
    }

    if (!(screen = SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, DEPTH, SDL_HWSURFACE)))
    {
        SDL_Quit();
        abort();
    }

    if (SDL_MUSTLOCK(screen)) {
        if (SDL_LockSurface(screen) < 0) {
            return;
        }
    }

    memset(screen->pixels, 0x80, SCREEN_WIDTH * SCREEN_HEIGHT * BPP);

    if(SDL_MUSTLOCK(screen)) {
        SDL_UnlockSurface(screen);
    }

    SDL_Flip(screen);
}
