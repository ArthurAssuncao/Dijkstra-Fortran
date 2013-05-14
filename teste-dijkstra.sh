#!/bin/bash
tam=150
matriz="matriz-212.dat"
for i in $(seq $tam); do
    for j in $(seq $tam); do
        echo $matriz' '$i' '$j
        ./a.out $matriz $i $j
        echo "________________________________"
    done
done
