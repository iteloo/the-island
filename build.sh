#!/bin/bash

# Build the elm outputs, installing relevant packages
cd elm
elm-make Main.elm --yes --output=../web/elm.js
cd ..
