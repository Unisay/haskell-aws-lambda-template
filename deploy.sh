#!/usr/bin/env bash

aws lambda create-function \
  --profile test \
  --function-name my-lambda \
  --runtime provided \
  --role 'arn:aws:iam::251426845561:role/service-role/aws-lambda-function-role' \
  --handler 'src/Lib.handler' \
  --zip-file 'fileb://function.zip'
