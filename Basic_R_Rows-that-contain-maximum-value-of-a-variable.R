### Basic R: rows that contain the maximum value of a variable

# Let's create a data frame which contains five variables, vars, 
# named A - E, each of which appears twice, along with some measurements:

df.orig <- data.frame(vars = rep(LETTERS[1:5], 2), obs1 = c(1:10), obs2 = c(11:20))
df.orig

# Now, let's say we want only the rows that contain the maximum values 
# of obs1 for A - E. In bioinformatics, for example, we might be interested
# in selecting the microarray probeset with the highest sample variance from
# multiple probesets per gene. The answer is obvious in this trivial example
# (6 - 10), but one procedure looks like this:


# use aggregate to create new data frame with the maxima

df.agg <- aggregate(obs1 ~ vars, df.orig, max)

# then simply merge with the original

df.max <- merge(df.agg, df.orig)
df.max


# This also works using min() and, I guess, using any function that returns
# a single value per variable mapping to a value in the original data frame.

install_github('pisa', 'jbryer')