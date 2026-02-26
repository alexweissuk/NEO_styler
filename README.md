"# NEO_styler"
This function generates the personality styles described in the NEO
Inventories Professional Manual (McCrae and Costa, 2010). The function
is designed to be flexible.

x is the input, typically a subset of the domains in the dataframe

The option "domains" is a case insensitive string that indicates the
domains and their order. The default is "NEOAC", so vectors for
Neuroticism, Extraversion, Openness to Experience, Agreeableness, and
Conscientiousness, respectively. Fewer or more domains in any order
can be specified. 

The option "scoretype" is used to specify the type of input
variables. The default is "tscores" and specifies that the variables
are either domain or factor T-scores (mean = 50; SD = 10). The
selection "zscores" specifies that these are standardized scores (mean
= 0; SD = 1). If selecting "rawscores", be warned that the function
does not first convert raw-scores to T-scores using the norms found in
the NEO manual. It simply converts them to z-scores.

The open "vectors" is either TRUE or FALSE (the default). Setting
"vectors" equal to TRUE will cause the problem to include the style
vectors in the output.

Good luck and godspeed!

