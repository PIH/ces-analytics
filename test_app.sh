#!/bin/bash

usage() {
    echo "Usage: ./test_app.sh <image_name>"
    echo "Runs the packaged Shiny application."
    echo
    echo "e.g.   ./test_app.sh hello-shiny"
}

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    usage
    exit 0
fi

if [ "$#" -ne 1 ]; then
    usage
    exit 1
fi

docker run -it $1
