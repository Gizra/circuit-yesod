#!/bin/bash

if ! hash bzt 2>/dev/null; then
  echo "Taurus is required, visit http://gettaurus.org/docs/Installation"
  exit 1
fi

for TEST in *yml
do
  echo "Executing $TEST"
  bzt "$TEST"
done;
