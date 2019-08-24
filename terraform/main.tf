provider "aws" {
  version = "~> 2.25"
  region  = var.region
}

terraform {
  backend "s3" {
    encrypt        = true
    bucket         = "bellroy-terraform-remote-state"
    dynamodb_table = "terraform-state-lock-dynamo"
    key            = "terraform-state/terraform.tfstate"
  }
}

resource "aws_iam_role" "serverless_cfn_role" {
  name               = "serverless-cfn-role"
  assume_role_policy = <<EOF
{
  "Version":"2012-10-17",
  "Statement":[
    {
      "Action":"sts:AssumeRole",
      "Principal":{
        "Service":"cloudformation.amazonaws.com"
      },
      "Effect":"Allow",
      "Sid":""
    }
  ]
}
EOF
}

data "template_file" "serverless_cfn_role_policy_tpl" {
  template = file("cfn_role_policy.json.tpl")
  vars = {
    region           = var.region
    account_no       = var.account_no
    stage            = var.serverless_stage
    application_name = var.serverless_application_name
    service_name     = var.serverless_service_name
  }
}

resource "aws_iam_policy" "serverless_cfn_role_policy" {
  name        = "serverless-cfn-role-policy"
  description = "A serverless cfnRole policy"
  policy      = data.template_file.serverless_cfn_role_policy_tpl.rendered
}

resource "aws_iam_role_policy_attachment" "serverless-attach" {
  role       = aws_iam_role.serverless_cfn_role.name
  policy_arn = aws_iam_policy.serverless_cfn_role_policy.arn
}


