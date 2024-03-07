#!/usr/bin/env bash
java -jar openapi/openapi-generator-cli.jar generate -i openapi/schema.json -g haskell-http-client -o neurelo-api --additional-properties=generateLenses=false
