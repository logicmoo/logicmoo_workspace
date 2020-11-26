Using the MITRE Dialogue Workshop back-end program: 

There are two ways to use the provided back-end, either as a
standalone Java program contained in the attached archive, or as an
http server program accessible from newark.mitre.org port 5555.
(There may be a minor delay while the appropriate administrative
procedures for opening a port to the outside world are executed.)


To use the standalone program:

Usage: java -cp diseasedb.jar org.mitre.midiki.workshop.WorkshopDatabaseTest <dbname> <testid>
where
   <dbname> - typically 'diseasedb.ser'
   <testid>
      d - get all disease names
      s - get all symptom names
      t - get all testable symptoms and test names
      h - get all history item names
      n - get all symptom synonyms
      r <name> - get description of named disease
      g <findings> - diagnose given the named findings
        each finding is of the form "name"/<yesno>
        (<yesno> is either yes or no)
        Separate multiple findings with spaces.
        Findings obtained via diagnostic tests are allowed,
        as are items of medical history, as well as symptoms
        reported by the patient. 

Example batch files are included in the archive for reference.

See included javadocs for full API documentation. The test program can also
be used as a guide for incorporating the database directly into your software.



To use the http server: 

Queries are stored in the URL, as in the following examples: 

http://newark.mitre.org:5555/d
http://newark.mitre.org:5555/n
http://newark.mitre.org:5555/s
http://newark.mitre.org:5555/r*malaria
http://newark.mitre.org:5555/g*fever
http://newark.mitre.org:5555/g*jaundice

Arguments have the same format as with the command line version,
except that arguments are separated by * and spaces within arguments
are replaced by %20. The UrlConverter class can be used to encode
spaces, quotes, and other special characters, if desired.

