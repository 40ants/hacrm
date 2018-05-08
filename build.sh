#!/bin/bash


# ros  -l "/Users/art/projects/lisp/hacrm/quicklisp/setup.lisp" \
#      -e "(setf *debugger-hook* #'(lambda (c h) (declare (ignore h)) (uiop:print-condition-backtrace c) (uiop:quit -1)))" \
#      -e "(asdf:load-system :HACRM)" \
#      -e "(setf uiop:*image-entry-point* #'(lambda () (ceramic-entry::hacrm :releasep nil)))" \
#      -e "(uiop:dump-image #P\"/Users/art/.ceramic/working/hacrm\" :executable t
#                #+sb-core-compression :compression #+sb-core-compression t)"


CL_SOURCE_REGISTRY=~/common-lisp// qlot exec ros build app.ros
