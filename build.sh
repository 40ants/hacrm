#!/bin/bash

if [[ ! -e quicklisp/ ]]; then
    qlot install
fi

CL_SOURCE_REGISTRY=/Users/art/projects/lisp/cl-prevalence-multimaster/:/Users/art/common-lisp/log4cl-json/ qlot exec ./ceramic.ros --verbose app.ros
