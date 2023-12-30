//! ffi/tree-sitter/wrapper.c --- Tree-sitter C wrapper

// based on https://github.com/death/cl-tree-sitter

// it's annoying that we need this, I thought we could get away
// without it. Alas, SB-ALIEN is also unable to fully interoperate
// with the types defined in api.h, so we need to manually create
// pointer type definitions for them.

// build with: sudo clang -g -O2 -Wall -Wno-unused-value -shared wrapper.c -o /usr/local/lib/libtree-sitter-wrapper.so -ltree-sitter

/// Code:
#include <stdlib.h>

#include <tree_sitter/api.h>

TSNode *ts_tree_root_node_pointer(const TSTree *self) {
    TSNode *node = malloc(sizeof(TSNode));

    if (node) {
        *node = ts_tree_root_node(self);
    }

    return node;
}

TSTreeCursor *ts_tree_cursor_new_pointer(TSNode *node) {
    TSTreeCursor *cursor = malloc(sizeof(TSTreeCursor));

    if (cursor) {
        *cursor = ts_tree_cursor_new(*node);
    }

    return cursor;
}

TSNode *ts_tree_cursor_current_node_pointer(const TSTreeCursor *cursor) {
    TSNode *return_node = malloc(sizeof(TSNode));

    if (return_node) {
        *return_node = ts_tree_cursor_current_node(cursor);
    }

    return return_node;
}

int ts_node_is_named_pointer(TSNode *node) {
    return ts_node_is_named(*node);
}

TSPoint ts_node_start_point_pointer(TSNode *node) {
    return ts_node_start_point(*node);
}


TSPoint ts_node_end_point_pointer(TSNode *node) {
    return ts_node_end_point(*node);
}

const char *ts_node_type_pointer(TSNode *node) {
    return ts_node_type(*node);
}
