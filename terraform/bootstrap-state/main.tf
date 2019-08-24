# This module solves a so-called "TF state boostrapping problem", as described here:
# https://www.lightenna.com/tech/2018/storing-terraform-state-in-s3/

provider "aws" {
  region = "eu-central-1"
}

resource "aws_s3_bucket" "terraform-state-storage-s3" {
  bucket = "bellroy-terraform-remote-state"
  versioning { enabled = true }
  lifecycle { prevent_destroy = true }
  tags = { Name = "S3 Remote Terraform State Store" }
}

resource "aws_dynamodb_table" "dynamodb-terraform-state-lock" {
  name           = "terraform-state-lock-dynamo"
  hash_key       = "LockID"
  read_capacity  = 20
  write_capacity = 20
  attribute {
    name = "LockID"
    type = "S"
  }
  tags = { Name = "DynamoDB Terraform State Lock Table" }
}
