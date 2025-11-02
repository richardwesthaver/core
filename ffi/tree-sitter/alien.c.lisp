;;; alien.c.lisp --- Tree-sitter Alien Wrapper

;; 

;;; Code:
(include <tree_sitter/api.h>)
(fn ts-tree-root-node-pointer (* TSNode) ((const self (* TSTree))))

(fn ts-tree-cursor-current-node-pointer (* TSNode) ((const cursor (* TSTreeCursor))))

(fn ts-tree-cursor-goto-first-child-for-point-pointer int64-t ((const cursor (* TSTreeCursor))
                                                                (goal-point (* TSPoint))))

(fn ts-tree-cursor-copy-pointer (* TSTreeCursor) ((const cursor (* TSTreecursor))))

(fn ts-node-is-name-pointer bool ((node (* TSNode)))
 (return (ts-node-is-named (deref node))))

(fn ts-node-is-missing-pointer bool ((node (* TSNode)))
 (return (ts-node-is-missing (deref node))))

(fn ts-node-is-extra-pointer bool ((node (* TSNode)))
 (return (ts-node-is-extra (deref node))))

(fn ts-node-is-error-pointer bool ((node (* TSNode)))
 (return (ts-node-is-error (deref node))))

(fn ts-node-has-error-pointer bool ((node (* TSNode)))
 (return (ts-node-has-error (deref node))))

(fn ts-node-parent-pointer (* TSNode) ((node (* TSNode))))

(fn ts-node-child-count-pointer uint32-t ((node (* TSNode)))
 (return (ts-node-child-count (deref node))))

(fn ts-node-start-byte-pointer uint32-t ((node (* TSNode)))
 (return (ts-node-start-byte (deref node))))
    
(fn ts-node-end-byte-pointer uint32-t ((node (* TSNode)))
 (return (ts-node-end-byte (deref node))))

(fn ts-node-start-point-pointer (* TSNode) ((node (* TSNode))))
(fn ts-node-end-point-pointer (* TSNode) ((node (* TSNode))))
