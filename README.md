# Example: AWS Lambda written in Haskell

## Prerequisites

- [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/GUIDE/)
- [Docker](https://www.docker.com/community-edition)
- AWS CLI already configured with Administrator permission
- [Serverless Framework](https://serverless.com/)

## How to build

```sh
stack build
```

produces `build/function.zip`

## How to setup infrastructure

Use `terraform` (one-time action) to setup infrastructure shared by serverless apps
(S3 buckets, roles, etc)

```sh
cd terraform
env AWS_PROFILE=test AWS_SDK_LOAD_CONFIG=1 terraform apply
```

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
./make.hs deploy --verbose
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
./make.hs invoke --verbose \
  --body '{ "personName": "Chuck Norris", "personAge": 79 }'
```

## Links

1. [The definitive guide to using Terraform with the Serverless Framework](https://serverless.com/blog/definitive-guide-terraform-serverless/)
1. https://serverless.com
1. https://theam.github.io/aws-lambda-haskell-runtime/
