#!/usr/bin/env bash

set -eu

export AWS_ACCESS_KEY_ID=test
export AWS_SECRET_ACCESS_KEY=test
export AWS_DEFAULT_REGION=ap-southeast-2

awslocal sqs create-queue --queue-name commands