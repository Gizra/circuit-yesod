#!/bin/bash

if ! hash jmeter 2>/dev/null; then
  echo "JMeter is required, make it available in the path"
  exit 1
fi

for TEST in *jmx
do
  echo "Executing $TEST"
  jmeter -n -j "jmeter-${TEST%.jmx}.log" -t "$TEST"
done;
