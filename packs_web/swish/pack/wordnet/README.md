# SWI-Prolog Wordnet driver

This pack provides access to the Princeton WordNet database.  The
pack provides:

  - Put wordnet into a module
  - Lazy load only those relations you use
  - Maintain _quick load files_ (.qlf) to provide fast loading.
  - PlDoc comments

This is work I did very long ago.   This  version is based on a modified
version I was pointed as by Samer Abdallah. The code has been refactored
for use as a pack, PlDoc comments have been brought up to date.

## Bug

There is a bug in SWI-Prolog  7.6.2/7.7.3   that  causes the first query
after a lazy load to fail.  A fix has been pushed to SWI-Prolog's repo.
