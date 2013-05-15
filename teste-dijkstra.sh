#!/bin/bash
tam=$2
matriz=$1
for i in $(seq $tam); do
    for j in $(seq $tam); do
        echo $matriz' '$i' '$j
        ./arthur_dijkstra.out $matriz $i $j
        echo "________________________________"
    done
done
