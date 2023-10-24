% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmutate.R
\name{cmutate}
\alias{cmutate}
\title{Create, modify, and delete columns with Concise notation}
\usage{
cmutate(.data, ...)
}
\arguments{
\item{.data}{A data frame}

\item{...}{Any combination of one or more name-value pairs to be evaluated as
columns in the same fashion as in \link[dplyr:mutate]{dplyr::mutate} and two-sided formula of the
form \code{x ~ expr} where \code{x} gives the new name of the column, and \code{expr} denotes
the anonymous function to be iteratively evaluated to create the new column.
As with \link[dplyr:mutate]{dplyr::mutate}, the expression \code{expr} can refer to data columns in
\code{.data} as though they were variables defined in the local scope.}
}
\value{
Returns a data frame with new columns for each name-value pair and/or
formula specified in ... arguments. Number of rows and data frame attributes
are preserved. Groups are preserved unless grouping variables are mutated, in
which case they will be recomputed.
}
\description{
\code{cmutate} functions exactly like \link[dplyr:mutate]{dplyr::mutate} with the additional feature
that column definitions specified using a \code{~} instead of \code{=} are iteratively
evaluated as a lambda function. In practise, this works similarly to using
\link[dplyr:rowwise]{dplyr::rowwise} before \link[dplyr:mutate]{dplyr::mutate}, except the result is calculated much
faster and is less computationally expensive, and does so without overwriting
the existing groupings of the data.

Columns defined using a \code{~} also allow for  the usage of additional "pronouns"
to concisely refer to related objects or properties of the data columns, such
as row number, groups, as well as the anonymous function itself for use in
recursion.

A \verb{?} following the column definition allows for specifying additional arguments
to pass to the lambda function, as well as the output type of the column, e.g.
integer, character, list, etc.
}
\section{Optional column type specification with \verb{?}}{
By default, columns specified with a \code{~} are simplified to a vector if possible,
similar to \code{rmap} with \code{simplify=TRUE}. If a specific output type is desired,
à la \code{rmap_int}, \code{rmap_dbl}, etc., the column definition can be followed by a
\verb{?} and then \code{chr}, \code{dbl}, \code{df}, \code{int}, \code{lgl}, or \code{list}. When using this
notation, the column definition is of the form \code{x ~ expr ? type}. See Examples
for further clarification.
}

\section{Additional lambda funciton arguments with \verb{?}}{
Occasionally it is helpful to pass additional arguments to be used in the
evaluation of the lambda function. Like the type specification above, these
can be defined inside parentheses after a \verb{?} following the column definition.
In this case the column definition is of the form \code{x ~ expr ? (arg = value)}.
If multiple arguments need to be specified, they can be chained together inside
curly braces and separated by semi-colons. This takes the following form:
\code{x ~ expr ? {arg1 = val1; arg2 = val2; ...}} or if type needs to be specified,
\code{x ~ expr ? {type; arg1 = val1; arg2 = val2; ...}}. See examples for further
clarification.
}

\section{Pronouns}{
\code{cmutate} supports a set of useful "pronouns" that allow you to refer
to other objects within the column formula as though they were locally defined
as variables. Note these are not pronouns in the \code{rlang} sense of the term, but
a convenient shorthand to provide additional functionality in a readable format.
Unlike its sister function \code{cmap}, \code{rmap} takes multiple named inputs, so row
elements a referred to by the name of the column (here indicated as \verb{<column_name>})
instead of \code{.x}.
See Examples for further clarification on the usage of each. Supported pronouns
are:
\itemize{
\item \verb{<column_name>} -- in the formula, this refers to an individual element of
the named data column.
\item \code{.i} -- the row index, or, if \code{.l} is grouped, the row index within the group.
Equivalent to \code{\link[dplyr:row_number]{dplyr::row_number()}}.
\item \code{.I} -- the absolute row index; whether or not \code{.l} is grouped, this will
return the overall position of the current row. Equivalent to
\code{\link[dplyr:context]{dplyr::cur_group_rows()}}.
\item \verb{<column_name>.nm} -- if the column \verb{<column_name>} in \code{.l} is named, this
returns the name corresponding to the current element of \verb{<column_name>}; If
this column of \code{.l} is unnamed, returns \code{NULL}. Note that columns in \code{tibble}
data frames retain their \code{names} attribute, but ordinary base R data frames
do not.
\item \code{.n} -- the index of the final row in \code{.l}, or, if \code{.l} is grouped, the final
row in the given group. Equivalent to \code{\link[dplyr:context]{dplyr::n()}}.
\item \code{.N} -- the index of the final row in \code{.l}, whether or not \code{.l} is grouped.
Equivalent to \code{nrow(.l)}.
\item \verb{<column_name>.grp} -- all elements in the current group as a list object.
If \code{.l} is not grouped, returns the same as \verb{<column_name>.col}, i.e. the entire
column \verb{<column_name>} as a list object.
\item \verb{<column_name>.col} -- the entire data column \verb{<column_name>} in \code{.l}, as
opposed to just the current element. Returns the full column whether or not \code{.l}
is grouped.
\item \code{.this} -- the anonymous function itself, to be used in cases where recursion
is needed.
}
}
