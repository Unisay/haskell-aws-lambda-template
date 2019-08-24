resource "aws_ssm_parameter" "serverlessCfnRoleArn" {
  name        = "/serverless/cfnRole/arn"
  description = "ARN of the cfmRole for the serverless.yaml"
  type        = "SecureString"
  value       = aws_iam_role.serverless_cfn_role.arn
}
