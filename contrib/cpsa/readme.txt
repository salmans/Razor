CPSA analysis to Razor converter

Converts the output of a CPSA run into a formula in Razor syntax.
Requires SWI-Prolog.

Suppose the CPSA source is prot.scm.  To generate the shape analysis
sentences using Make.hs from the CPSA documents directory, type:

$ echo 'sas "prot"' | ghci Make.hs
$ swipl
?- [razor].
?- razor('prot_sas.text', 'prot_sas.razor').
?- halt.
$
