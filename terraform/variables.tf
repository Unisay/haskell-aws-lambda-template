variable "region" {
  default = "eu-central-1"
}

variable "profile" {
  default = "test"
}

variable "account_no" {
  description = "AWS account number"
}

variable "serverless_application_name" {
  description = "Name of the serverless application"
}

variable "serverless_stage" {
  description = "Name of the serverless stage"
}

variable "serverless_service_name" {
  description = "Name of the serverless service"
}
