#!/bin/bash

swipl -g "doc_server(9011), ensure_loaded('*.pl'), ensure_loaded('*/*.pl')."
