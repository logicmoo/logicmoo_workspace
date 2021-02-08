# Welcome

## LPS syntax ##

[Here](Syntax%20of%20lps.swi)

## Preliminary reference documentation for lps.swi ##

[Here](lps.swi%20Reference)

## Some Smart Contracts Examples ##

[Here](https://bitbucket.org/lpsmasters/lps_corner/wiki/Smart%20Contract%20Examples)

--


## Wiki features

This wiki uses the [Markdown](http://daringfireball.net/projects/markdown/) syntax.

The wiki itself is actually a git repository, which means you can clone it, edit it locally/offline, add images or any other file type, and push it back to us. It will be live immediately.

Go ahead and try:

```
$ git clone https://lpsmasters@bitbucket.org/lpsmasters/lps_corner.git/wiki
```

Wiki pages are normal files, with the .md extension. You can edit them locally, as well as creating new ones.

## Syntax highlighting


You can also highlight snippets of text (we use the excellent [Pygments][] library).

[Pygments]: http://pygments.org/


Here's an example of some Python code:

```
#!python

def wiki_rocks(text):
    formatter = lambda t: "funky"+t
    return formatter(text)
```


You can check out the source of this page to see how that's done, and make sure to bookmark [the vast library of Pygment lexers][lexers], we accept the 'short name' or the 'mimetype' of anything in there.
[lexers]: http://pygments.org/docs/lexers/


Have fun!