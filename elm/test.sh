#!/bin/bash

go build
./the-island > /dev/null 2>&1 &
PID=$!
node js/test.js
kill $PID
