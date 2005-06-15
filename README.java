This README explains how to use the pre-compiled JVM
version of Skribe. This requires JDK 1.3 or higher.

Installing SKRIBE
*****************

The pre-compiled version of SKRIBE does not need installation procedure.
It is pre-installed. The documentation is pre-compiled. It is located
in the directory doc/html.


Running SKRIBE
**************

Lets assume that SKRIBEDIR is the shell variable containing
the name of the directory where Skribe has been unzipped:

1. To compile a Skribe program "prog.skr" uses:

   java -classpath $SKRIBEDIR/bin/skribe.zip:$SKRIBEDIR/lib/bigloo_s.zip -Dbigloo.SKRIBEPATH=$SKRIBEDIR/skr bigloo.skribe.main prog.skr

2. To convert a Texi file "prog.texi" into Skribe:

   java -classpath $SKRIBEDIR/bin/skribeinfo.zip:$SKRIBEDIR/lib/bigloo_s.zip bigloo.skribe.skribeinfo.main prog.texi

3. To convert a BibTex database "db.bib" into Skribe:

   java -classpath $SKRIBEDIR/bin/skribebibtex.zip:$SKRIBEDIR/lib/bigloo_s.zip bigloo.skribe.skribebibtex.main db.bib


Compiling the examples
**********************

On a Unix platform:

   cd examples; make
