# Example: AWS Lambda written in Haskell

## Prerequisites

- [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/GUIDE/)
- [Python 3](https://www.python.org/downloads/)
- [Docker](https://www.docker.com/community-edition)
- `make`
- AWS CLI already configured with Administrator permission
- Optionally: [Serverless Framework](https://serverless.com/)

## How to build

```sh
make all
```

produces `build/function.zip`

## How to deploy

### Manually

```sh
aws lambda create-function \
  --function-name haskell-aws-lambda \
  --runtime provided \
  --role 'arn:aws:iam::<your account id>:role/service-role/aws-lambda-function-role' \
  --handler 'src/Lib.handler' \
  --zip-file 'fileb://build/function.zip'
```

### Using "serverless" framework

```sh
serverless deploy
```

## How to invoke

### Manually

```sh
aws lambda invoke \
  --function-name haskell-aws-lambda \
  --payload '{ "personName": "Chuck Norris", "personAge": 79 }' \
  response.txt

cat response.txt
```

### Using "serverless" framework

```sh
sls invoke \
  -f haskell-aws-lambda \
  -d '{ "personName": "Chuck Norris", "personAge": 79 }'
```

## Links

- https://theam.github.io/aws-lambda-haskell-runtime/
