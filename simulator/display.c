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

// from AtomVM generic_unix_sys.h

typedef struct EventListener EventListener;

typedef void (*event_handler_t)(EventListener *listener);

struct EventListener {
    struct ListHead listeners_list_head;

    event_handler_t handler;
    void *data;
    int fd;
};

struct GenericUnixPlatformData
{
    struct ListHead *listeners;
};

// end of generic_unix_sys.h private header

struct KeyboardEvent
{
    uint8_t key;
    bool key_down;
};

static term keyboard_pid;
static struct timespec ts0;
static int keyboard_event_fds[2];

static void consume_display_mailbox(Context *ctx);

SDL_Surface *screen;

static void *display_loop();

static pthread_mutex_t ready_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t ready = PTHREAD_COND_INITIALIZER;

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

static void process_message(Context *ctx)
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
                (color >> 16) & 0xFF, (color >> 8) & 0xFF, color & 0xFF);

    } else if (cmd == context_make_atom(ctx, "\xA" "draw_image")) {
        int x = term_to_int(term_get_tuple_element(req, 1));
        int y = term_to_int(term_get_tuple_element(req, 2));
        term img = term_get_tuple_element(req, 3);
        int color = term_to_int(term_get_tuple_element(req, 4));

        int width = term_to_int(term_get_tuple_element(img, 0));
        int height = term_to_int(term_get_tuple_element(img, 1));
        const char *data = term_binary_data(term_get_tuple_element(img, 2));

        draw_image(screen, x, y, width, height, data, (color >> 16),
                (color >> 8) & 0xFF, color & 0xFF);

    } else if (cmd == context_make_atom(ctx, "\x9" "draw_rect")) {
        int x = term_to_int(term_get_tuple_element(req, 1));
        int y = term_to_int(term_get_tuple_element(req, 2));
        int width = term_to_int(term_get_tuple_element(req, 3));
        int height = term_to_int(term_get_tuple_element(req, 4));
        int color = term_to_int(term_get_tuple_element(req, 5));

        draw_rect(screen, x, y, width, height,
                (color >> 16) & 0xFF, (color >> 8) & 0xFF, color & 0xFF);

    } else if (cmd == context_make_atom(ctx, "\x9" "draw_text")) {
        int x = term_to_int(term_get_tuple_element(req, 1));
        int y = term_to_int(term_get_tuple_element(req, 2));
        term text_term = term_get_tuple_element(req, 3);
        int color = term_to_int(term_get_tuple_element(req, 4));

        int ok;
        char *text = interop_term_to_string(text_term, &ok);

        draw_text(screen, x, y, text, (color >> 16) & 0xFF, (color >> 8) & 0xFF, color & 0xFF);

        free(text);

    } else if (cmd == context_make_atom(ctx, "\x6" "listen")) {
        keyboard_pid = term_get_tuple_element(req, 1);

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

static void consume_display_mailbox(Context *ctx)
{
    while (!list_is_empty(&ctx->mailbox)) {
        process_message(ctx);
    }
}

static void send_message(term pid, term message, GlobalContext *global)
{
    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(global, local_process_id);
    mailbox_send(target, message);
}

static void keyboard_callback(EventListener *listener)
{
    Context *ctx = (Context *) listener->data;

    struct KeyboardEvent keyb;
    read(keyboard_event_fds[0], &keyb, sizeof(keyb));

    if (keyboard_pid) {
        struct timespec ts;
        clock_gettime(CLOCK_MONOTONIC, &ts);

        avm_int_t millis = (ts.tv_sec - ts0.tv_sec) * 1000 + (ts.tv_nsec - ts0.tv_nsec) / 1000000;

        if (UNLIKELY(memory_ensure_free(ctx, 5) != MEMORY_GC_OK)) {
            abort();
        }

        term event_tuple = term_alloc_tuple(4, ctx);
        term_put_tuple_element(event_tuple, 0, context_make_atom(ctx, "\xE" "keyboard_event"));
        term_put_tuple_element(event_tuple, 1, term_from_int(keyb.key));
        term_put_tuple_element(event_tuple, 2, keyb.key_down ? TRUE_ATOM : FALSE_ATOM);
        term_put_tuple_element(event_tuple, 3, term_from_int(millis));

        send_message(keyboard_pid, event_tuple, ctx->global);
    }
}

void display_port_driver_init(Context *ctx, term opts)
{
    ctx->native_handler = consume_display_mailbox;
    ctx->platform_data = NULL;

    pipe(keyboard_event_fds);

    UNUSED(opts);

    pthread_t thread_id;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_create(&thread_id, &attr, display_loop, NULL);

    pthread_mutex_lock(&ready_mutex);
    pthread_cond_wait(&ready, &ready_mutex);
    pthread_mutex_unlock(&ready_mutex);

    GlobalContext *glb = ctx->global;
    struct GenericUnixPlatformData *platform = glb->platform_data;

    EventListener *listener = malloc(sizeof(EventListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    listener->fd = keyboard_event_fds[0];
    listener->data = ctx;
    listener->handler = keyboard_callback;
    linkedlist_append(&platform->listeners, &listener->listeners_list_head);

    clock_gettime(CLOCK_MONOTONIC, &ts0);
}

void *display_loop(void *args)
{
    pthread_mutex_lock(&ready_mutex);

    UNUSED(args);

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
            return NULL;
        }
    }

    memset(screen->pixels, 0x80, SCREEN_WIDTH * SCREEN_HEIGHT * BPP);

    if(SDL_MUSTLOCK(screen)) {
        SDL_UnlockSurface(screen);
    }

    SDL_Flip(screen);

    pthread_cond_signal(&ready);
    pthread_mutex_unlock(&ready_mutex);

    SDL_Event event;

    while (SDL_WaitEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT: {
                exit(EXIT_SUCCESS);
                break;
            }

            case SDL_KEYDOWN: {
                struct KeyboardEvent keyb_event;
                keyb_event.key = event.key.keysym.sym;
                keyb_event.key_down = true;
                write(keyboard_event_fds[1], &keyb_event, sizeof(keyb_event));
                break;
            }

            case SDL_KEYUP: {
                struct KeyboardEvent keyb_event;
                keyb_event.key = event.key.keysym.sym;
                keyb_event.key_down = false;
                write(keyboard_event_fds[1], &keyb_event, sizeof(keyb_event));
                break;
            }

            default: {
                break;
            }
        }
    }

    return NULL;
}
