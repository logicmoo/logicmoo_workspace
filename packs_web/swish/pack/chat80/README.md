# The classical CHAT80 natural language system

The CHAT80 system has been developed in the 70s and 80s by Fernando C.N.
Pereira and David H.D. Warren. It implements a natural language question
answering system that answers  questions   about  the  world: countries,
cities, rivers, etc. It does so by   parsing the question, translate the
parse to a Prolog query and run this against its database.

This version is derived from the original  via Quintus Prolog after some
compatibility modifications for SWI-Prolog and   adding  a module header
that allows using it safely together with other applications.

The code is definitely dated. Still, it   provides  a nice example using
Prolog for parsing, assigning meaning and querying.

## Legal

The copyright is as far as we know   with  the original authors and made
available under a classical _academic use license_. See `LICENSE` in the
`prolog/chat80` directory. The content of that  file was copied from the
Python [NLTK data package](https://www.kaggle.com/nltkdata/chat-80/home)
that includes the chat80 files.

