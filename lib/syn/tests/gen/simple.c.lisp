(in-package :c)
(include <stdio.h>)

(function hello_world void () (printf "Hello, World!\\n"))

(function main int ()
 (hello_world))
