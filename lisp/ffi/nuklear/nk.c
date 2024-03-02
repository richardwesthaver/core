#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <time.h>
#include <limits.h>
#include <unistd.h>
#include <dirent.h>

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#define NK_INCLUDE_FIXED_TYPES
#define NK_INCLUDE_STANDARD_IO
#define NK_INCLUDE_DEFAULT_ALLOCATOR
#define NK_INCLUDE_VERTEX_BUFFER_OUTPUT
#define NK_INCLUDE_FONT_BAKING
#define NK_INCLUDE_DEFAULT_FONT
#define NK_IMPLEMENTATION
#include "nuklear.h"

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

/* macros */
#define WINDOW_WIDTH 1200
#define WINDOW_HEIGHT 800

#define MAX_VERTEX_MEMORY 512 * 1024
#define MAX_ELEMENT_MEMORY 128 * 1024

#define UNUSED(a) (void)a
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) < (b) ? (b) : (a))
#define LEN(a) (sizeof(a)/sizeof(a)[0])

/// enum {EASY, HARD};
/// static int op = EASY;
/// static float value = 0.6f;
/// static int i =  20;
/// struct nk_context ctx;
///
/// nk_init_fixed(&ctx, calloc(1, MAX_MEMORY), MAX_MEMORY, &font);
/// if (nk_begin(&ctx, "Show", nk_rect(50, 50, 220, 220),
///     NK_WINDOW_BORDER|NK_WINDOW_MOVABLE|NK_WINDOW_CLOSABLE)) {
///     // fixed widget pixel width
///     nk_layout_row_static(&ctx, 30, 80, 1);
///     if (nk_button_label(&ctx, "button")) {
///         // event handling
///     }
///
///     // fixed widget window ratio width
///     nk_layout_row_dynamic(&ctx, 30, 2);
///     if (nk_option_label(&ctx, "easy", op == EASY)) op = EASY;
///     if (nk_option_label(&ctx, "hard", op == HARD)) op = HARD;
///
///     // custom widget pixel width
///     nk_layout_row_begin(&ctx, NK_STATIC, 30, 2);
///     {
///         nk_layout_row_push(&ctx, 50);
///         nk_label(&ctx, "Volume:", NK_TEXT_LEFT);
///         nk_layout_row_push(&ctx, 110);
///         nk_slider_float(&ctx, 0, &value, 1.0f, 0.1f);
///     }
///     nk_layout_row_end(&ctx);
/// }
/// nk_end(&ctx);
