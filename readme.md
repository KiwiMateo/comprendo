# Comprendo

This R package provides Python-like list comprehension.

```r
library(comprendo)
```

As a simple example, the follwoing code returns a vector of even squares up to 
100.

```r
v(x^2 | x %in% 1:10, x %% 2 == 0)
```

We can also iterate over multiple variables, for example to generate a list of
small Pythagorean triples.

```r
l(c(x, y, z) | x %in% 1:20 & y %in% x:20 & z %in% y:20,  
  x^2 + y^2 == z^2)
```

Finally, we can have multiple variables come from a list of tuples.

```r
greetings <- c("Hello", "Hola", "Hallo")
leaders   <- c("Keir", "Pedro", "Olaf")
enquiries <- c("how are you?", "¿cómo estás?", "wie geht es dir?")

v(sprintf("%s %s, %s", x, y, z) | c(x, y, z) %in% zip(greetings, leaders, enquiries))
```

## Installation

Can be installed with

```r
remotes::install_github("KiwiMateo/comprendo")
```
