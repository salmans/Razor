CPSA analysis to Razor converter

Converts the output of a CPSA run into a formula in Razor syntax.
Requires SWI-Prolog.

Suppose the CPSA source is or.scm.  To generate the shape analysis
sentences using Make.hs from the CPSA documents directory, type:

$ echo 'sas "or"' | ghci Make.hs
$ swipl
?- [razor].
?- razor('or_sas.text', 'or_sas.razor').
?- halt.
$
