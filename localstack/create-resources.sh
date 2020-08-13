#!/usr/bin/env bash

set -eu

awslocal sqs create-queue --queue-name commands