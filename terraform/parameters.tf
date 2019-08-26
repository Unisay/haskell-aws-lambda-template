resource "aws_ssm_parameter" "serverlessCfnRoleArn" {
  name        = "/serverless/cfnRole/arn"
  description = "ARN of the cfmRole for the serverless.yaml"
  type        = "SecureString"
  value       = aws_iam_role.serverless_cfn_role.arn
}

resource "aws_ssm_parameter" "serverlessDeploymentBucket" {
  name        = "/serverless/deploymentBucket/name"
  description = "Bucket that stores artifacts deployed with Serverless"
  type        = "SecureString"
  value       = aws_s3_bucket.serverless_deployment_bucket.bucket
}
