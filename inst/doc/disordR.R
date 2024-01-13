## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/disordR.png", package = "disordR"))

## -----------------------------------------------------------------------------
library("mvp")
a <- as.mvp("5 a c^3 + a^2 d^2 f^2 + 4 a^3 b e^3 + 3 b c f + 2 b^2 e^3")
a

## -----------------------------------------------------------------------------
library("disordR")
set.seed(0)
a <- rdis()
a

## -----------------------------------------------------------------------------
a^2
a+1/a

## -----------------------------------------------------------------------------
max(a)
sort(a)

## ----error=TRUE---------------------------------------------------------------
a[1]  # asking for the first element is inadmissible
a[1] <- 1000 # also cannot replace the first element

## -----------------------------------------------------------------------------
x <- a + 1/a
x
y <- a*2-9
y
x+y

## ----error=TRUE---------------------------------------------------------------
b <- rdis()
b
a
a+b

## -----------------------------------------------------------------------------
a[a<0.5] <- 0  # round down
a
b[b>0.6] <- b[b>0.6] + 3  # add 3 to every element greater than 0.6
b

## -----------------------------------------------------------------------------
d <- disord(1:10)
d
e <- 10 + 3*d - d^2
e
e<4
d[e<4] <- e[e<4]
d

## -----------------------------------------------------------------------------
library("mvp")
set.seed(0)
a <- rmvp()
b <- rmvp()
a
b

## -----------------------------------------------------------------------------
a + 2*b
(a+b)*(a-b) == a^2-b^2   # should be TRUE (expression is quite long)

## -----------------------------------------------------------------------------
coeffs(a)
coeffs(b)

## -----------------------------------------------------------------------------
coeffs(a)[coeffs(a) < 4] <- 0   # set any coefficient of a that is <4 to zero
a
coeffs(b) <- coeffs(b)%%2       # consider coefficients of b modulo 2
b

## -----------------------------------------------------------------------------
x <- rmvp()     # set up new mvp objects x and y
y <- rmvp()

## ----error=TRUE---------------------------------------------------------------
coeffs(x) + coeffs(y)  # order implementation specific
coeffs(x) <- coeffs(y) # ditto
coeffs(x) <- 1:2       # replacement value not length 1
coeffs(x)[coeffs(x) < 3] <- coeffs(x)[coeffs(y) < 3]

## -----------------------------------------------------------------------------
(a <- as.mvp("x^2 + 4 - 3*x*y*z"))
vars(a)
powers(a)
coeffs(a)

## -----------------------------------------------------------------------------
double <- function(x){2*x}
(a <- rmvp())
pa <- powers(a)
va <- vars(a)
ca <- coeffs(a)
pa[ca<4] <- sapply(pa,double)[ca<4]
mvp(va,pa,ca)

## -----------------------------------------------------------------------------
a <- as.mvp("3 + 5*a*b - 7*a*b*x^2 + 2*a*b^2*c*d*x*y -6*x*y + 8*a*b*c*d*x")
a
pa <- powers(a)
va <- vars(a)
ca <- coeffs(a)
va[sapply(pa,length) > 4] <- sapply(va,toupper)[sapply(pa,length) > 4]
mvp(va,pa,ca)

