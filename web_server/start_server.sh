#!/bin/bash

# set flask environment variables
export FLASK_APP=hplus
export FLASK_ENV=development
export FLASK_RUN_PORT=5000
export EXAMPLE_USED=sorted_examples

# start flask server
flask run -h 0.0.0.0
