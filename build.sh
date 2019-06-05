#!/bin/bash

if [[ ! -e quicklisp/ ]]; then
    qlot install
fi

qlot exec ./ceramic.ros --verbose app.ros
