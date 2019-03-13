#!/bin/bash

echo "Attempting to deliver pact file"

PROVIDER_NAME="gform"
CONSUMER_NAME="gform-frontend"

PACT_NAME=${CONSUMER_NAME}"_$PROVIDER_NAME.json"
PACT_FILE="target/pacts/$PACT_NAME"

if [ ! -f $PACT_FILE ]
  then
    echo "Expected pact file did not exist: $PACT_FILE"
    echo "Have you run 'sbt pact-test' in the consumer project?"
    exit 1
fi

PROVIDER_PACT_DIR=../$PROVIDER_NAME/delivered_pacts/$PACT_NAME

if [ -f $PROVIDER_PACT_DIR ]; then rm $PROVIDER_PACT_DIR; fi

echo "copying $PACT_FILE to $PROVIDER_PACT_DIR"

mkdir -p ../$PROVIDER_NAME/delivered_pacts
cp $PACT_FILE $PROVIDER_PACT_DIR
