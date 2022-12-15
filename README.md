# cuteR
A set of functions for quick exploratory analyses with aesthetically pleasing graphs

####cuteR_library.R####
z_theme() is the basis of all my ggplots. It's nice and clean.

stats() is a function that will compare groups.

cuteboxes() is a function that generates boxplots and statistically compares them. There is an option for with and without stats.

cuteregs() builds regressions between two variables and lets you quickly edit the type of fit or transform data to test different ways to analyze your data.

meanregs() does the same as cuteregs but it uses mean values of groups instead of all the points. It has an option for error bars.

SDregs() does the same as meanregs() but it calculates the error bars for you.

####tableR_library.R####

stats_table() returns a table with estimates resulting from a linear model with everything you need to put in a supplementary material about your analyses for publication.

stats_table_glm() does the same as stats_table() but for generalized linear model objects.

data_summary() provides mean and SD values for any given variable.