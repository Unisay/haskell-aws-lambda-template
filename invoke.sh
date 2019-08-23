#!/usr/bin/env bash

aws lambda invoke \
  --profile test \
  --function-name my-lambda \
  --payload '{ "personName": "Chuck Norris", "personAge": 79 }' \
  response.txt

cat response.txt
