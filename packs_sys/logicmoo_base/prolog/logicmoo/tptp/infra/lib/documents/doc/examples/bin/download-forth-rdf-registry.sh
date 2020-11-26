#!/bin/sh
## 
## http://139.91.183.30:9090/RDF/Examples.html is a registry with RDF examples
## that have been cleaned-up to be parseable.  This script downloads these
## examples for offline use.
## 
## It creates a directory 139.91.183.30:9090. The file
## 139.91.183.30:9090/RDF/Examples.html is an index to the RDF examples.
## 
wget --page-requisites \
     --convert-links \
     --no-parent \
     --recursive \
     --level=1 \
     http://139.91.183.30:9090/RDF/Examples.html
