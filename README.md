# Example: AWS Lambda written in Haskell

## Prerequisites

The following programs have to be installed and available on PATH:

- `docker`
- `stack`
- `aws`
- `make`

## How to build

```sh
make all
```

produces `build/function.zip`

## How to deploy

```sh
aws lambda create-function \
  --profile test \
  --function-name my-lambda \
  --runtime provided \
  --role 'arn:aws:iam::<your account id>:role/service-role/aws-lambda-function-role' \
  --handler 'src/Lib.handler' \
  --zip-file 'fileb://build/function.zip'
```

## How to invoke

```sh
aws lambda invoke \
  --profile test \
  --function-name my-lambda \
  --payload '{ "personName": "Chuck Norris", "personAge": 79 }' \
  response.txt

cat response.txt
```

## Links

- https://theam.github.io/aws-lambda-haskell-runtime/
