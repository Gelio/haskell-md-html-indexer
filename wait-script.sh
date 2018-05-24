#!/bin/bash

NUMBER=$(((RANDOM % 10) + 1))

echo $1 waiting $NUMBER seconds

sleep $NUMBER

echo $1 finished
