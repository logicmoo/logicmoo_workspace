#!/bin/sh

swipl -s loader.pl -g 'generatePageFor(iem,admin,X),view([x,X]),halt.'
