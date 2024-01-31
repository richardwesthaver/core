# aws.nu

# secrets: AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY
export-env {
  $env.AWS_REGION = 'us-east-1' 
}

export def --env main [] {}
