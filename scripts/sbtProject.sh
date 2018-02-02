#!/usr/bin/env bash
mkdir -p src/{main,test}/{java,resources,scala}
mkdir lib project target

# create an initial build.sbt file
echo 'name := "captalysPOC"
     version := "1.0"
     scalaVersion := "2.10.0"' > build.sbt
