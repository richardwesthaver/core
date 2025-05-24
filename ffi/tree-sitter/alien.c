//! tree-sitter/alien.c --- Tree-sitter C wrapper

// based on https://github.com/death/cl-tree-sitter

// it's annoying that we need this, I thought we could get away
// without it. Alas, SB-ALIEN is also unable to fully interoperate
// with the types defined in api.h, so we need to manually create
// pointer type definitions for them.

// build with:
/*
  cc -g -O2 -Wall -Wno-unused-value -ltree-sitter -shared lisp/ffi/tree-sitter/alien.c \
  -o .stash/libtree-sitter-alien.so
*/

/// Code:
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

int64_t ts_tree_cursor_goto_first_child_for_point_pointer (TSTreeCursor *cursor, TSPoint *goal_point) {
  return ts_tree_cursor_goto_first_child_for_point(cursor,*goal_point);
}

TSTreeCursor *ts_tree_cursor_copy_pointer (const TSTreeCursor *cursor) {
  TSTreeCursor *ret = malloc(sizeof(TSTreeCursor));
  if (ret) {
    *ret = ts_tree_cursor_copy(cursor);
  }
  return ret;
}
  
bool ts_node_is_named_pointer(TSNode *node) {
  return ts_node_is_named(*node);
}

bool ts_node_is_missing_pointer(TSNode *node) {
  return ts_node_is_missing(*node);
}

bool ts_node_is_extra_pointer(TSNode *node) {
  return ts_node_is_extra(*node);
}

bool ts_node_is_error_pointer(TSNode *node) {
  return ts_node_is_error(*node);
}

bool ts_node_has_error_pointer(TSNode *node) {
  return ts_node_has_error(*node);
}

TSNode *ts_node_parent_pointer(TSNode *node) {
  TSNode *parent = malloc(sizeof(TSNode));
  if (parent) {
    *parent = ts_node_parent(*node);
  }
  return parent;
}

uint32_t ts_node_child_count_pointer(TSNode *node) {
  return ts_node_child_count(*node);
}

uint32_t ts_node_start_byte_pointer(TSNode *node) {
  return ts_node_start_byte(*node);
}

uint32_t ts_node_end_byte_pointer(TSNode *node) {
  return ts_node_end_byte(*node);
}

TSPoint *ts_node_start_point_pointer(TSNode *node) {
  TSPoint *point = malloc(sizeof(TSPoint));
  if (point) {
    *point = ts_node_start_point(*node);
  }
  return point;
}

TSPoint *ts_node_end_point_pointer(TSNode *node) {
  TSPoint *point = malloc(sizeof(TSPoint));
  if (point) {
    *point = ts_node_end_point(*node);
  }
  return point;
}

const char *ts_node_type_pointer(TSNode *node) {
  return ts_node_type(*node);
}

char *ts_node_string_pointer(TSNode *node) {
  return ts_node_string(*node);
}

bool ts_node_is_null_pointer(TSNode *node) {
  return ts_node_is_null(*node);
}

void ts_query_cursor_exec_pointer(TSQueryCursor *cursor, const TSQuery *query, TSNode *node) {
  return ts_query_cursor_exec(cursor,query,*node);
}

void ts_query_cursor_exec_with_options_pointer(TSQueryCursor *cursor, 
					       const TSQuery *query, 
					       TSNode *node,
					       const TSQueryCursorOptions *query_options) {
  return ts_query_cursor_exec_with_options(cursor,query,*node,query_options);
}
