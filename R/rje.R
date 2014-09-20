is.wholenumber = function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

is.subset = function(x, y){
  return(all(x %in% y))
}

`%subof%` = function(x, y){
  is.subset(x, y)
}

printPercentage = function (i, n, dp = 0, first = 1, last = n, prev = i - 1)
{
    out = ""
    disp = round(100 * i/n, dp)
    if (prev >= first)
        prev.disp = round(100 * prev/n, dp)
    else prev.disp = ""
    if (disp > prev.disp) {
        nc = nchar(prev.disp)
        if (i != first) {
          out = paste(out, paste(rep("\b",nc+1), collapse=""), sep="")
        }
        out = paste(out, disp, "%", sep="")
    }
    if (i == last) out = paste(out, "\n", sep="")
    cat(out)
    return(NULL)
}

printCount = function (i, first = 1, prev = i - 1, last = NULL)
{
    out = ""
    disp = round(i)
    if (prev >= first)
        prev.disp = round(prev)
    else prev.disp = ""
    if (disp > prev.disp) {
        nc = nchar(prev.disp)
        if (i != first) {
          out = paste(out, paste(rep("\b",nc+1), collapse=""), sep="")
        }
        out = c(out, disp)
        out = paste(out, disp, sep="")
    }
    if (!is.null(last) && i == last) out = paste(out, "\n", sep="")
    cat(out)
    return(NULL)
}

# BASED ON e1071::bincombinations
combinations = function(p)
{
    tot = prod(p)
    cp = cumprod(p)
    retval = rep.int(0, tot*length(p))
    dim(retval) = c(tot, length(p))
    for (i in seq_along(p)) {
        retval[, i] <- rep(seq_len(p[i])-1, each=cp[i]/p[i])
    }
    retval
}

powerSet = function(x, rev=FALSE) {
  out = list(x[c()])
  if (length(x) == 1) return(c(out, list(x)))

  for (i in seq_along(x)) {
    if (rev) out = c(lapply(out, function(y) c(y,x[i])), out)
    else out = c(out, lapply(out, function(y) c(y,x[i])))
  }

  out
}

powerSetMat = function(n) {
  combinations(rep.int(2, n))
}

subsetMatrix = function(n) {
  out = matrix(1, 1, 1)

  for (i in seq_len(n)) {
    out = matrix(c(1,-1,0,1),2,2) %x% out
  }

  out
}

expit = function(x) {
  out = exp(x)/(1+exp(x))
  out[x > 100] = 1

  out
}

logit = function(x) {
  no = (x < 0) | (x > 1)
  out = numeric(length(x))
  out[no] = NaN
  out[!no] = log(x[!no]/(1-x[!no]))

  out
}

rdirichlet = function(n, alpha) {
  if (is.matrix(alpha)) {
    k = dim(alpha)[2]
    alpha = as.vector(t(alpha))
  }
  else k = length(alpha)

  out = matrix(rgamma(n*k, shape=alpha),n,k, byrow=TRUE)
  out = out/rowSums(out)

  out
}

ddirichlet <-
function(x, alpha, log = FALSE, tol=1e-10) {
  k = length(alpha)
  if (is.matrix(x)) n = dim(x)[1]
  else if(is.numeric(x)) {
    dim(x) = c(1,length(x))
    n = 1
  }
  else stop("Vales must be numeric")
  if (any(alpha <= 0)) stop("Parameters must be positive")

  out = numeric(n)

  if (dim(x)[2] == k-1) x = cbind(x,1-rowSums(x))
  else if (dim(x)[2] != k) stop("Dimensions do not match")

  out[ (apply(x,1,function(y) any(y < -tol))) | abs(rowSums(x)-1) > tol ] = 0

  out = colSums((alpha-1)*log(t(x))) + lgamma(sum(alpha)) - sum(lgamma(alpha))
  if (!log) out = exp(out)

  return(out)
}

setmatch = function(x, y, nomatch = NA_integer_) {

  if (!is.list(x) && !is.list(y)) stop("Arguments must be lists")

  out = rep.int(nomatch, length(x))

  for (i in seq_along(x)) {
    for (j in seq_along(y)) {
      if((x[[i]] %subof% y[[j]]) && (y[[j]] %subof% x[[i]])) {
        out[i] = j
        break
      }
    }
  }

  out
}

subsetmatch = function(x, y, nomatch = NA_integer_) {

  if (!is.list(x) && !is.list(y)) stop("Arguments must be lists")

  out = rep.int(nomatch, length(x))

  for (i in seq_along(x)) {
    for (j in seq_along(y)) {
      if(x[[i]] %subof% y[[j]]) {
        out[i] = j
        break
      }
    }
  }

  out
}


setsetequal = function(x, y) {
  if (!is.list(x) && !is.list(y)) stop("Arguments must be lists")

  all(c(setmatch(x, y, 0L) > 0L, setmatch(y, x, 0L) > 0L))
}

setsetdiff = function(x, y) {
  if (!is.list(x) && !is.list(y)) stop("Arguments must be lists")

  x[match(x, y, 0L) == 0L]
}

subarray <- function(x, value, drop=TRUE){
  if(length(value) != length(dim(x))){
    stop("Array and indexlist are not compatible!")
  }
  args <- c(quote(x), value, list(drop=drop))
  return(do.call("[", args))
}

subtable <- function(x, variables, value, drop=TRUE){
  indexlist <- lapply(dim(x), seq_len)
  indexlist[variables] <- value
  dims = dim(x)
  dims[variables] = sapply(value, length)
  if (isTRUE(drop)) dims = dims[!(seq_along(dims) %in% variables) | dims > 1]

  out = subarray(x, indexlist)
  out = array(out, dim=dims)

  return(out)
}

condition.table <- function(x, variables, condition = NULL, condition.value = NULL) {
  if (!is.null(condition.value) &&
        length(condition) != length(condition.value)) stop("condition and condition.value must have same length")
  if(length(intersect(variables, condition)) > 0) stop("margin and condition must be disjoint")

  k = length(variables)

  marg = margin.table(x, c(variables, condition))
  if (length(condition) == 0) return(marg/sum(marg))

  variables <- seq_len(k)
  condition <- k + seq_along(condition)
  cond <- prop.table(marg, condition)

  if (is.null(condition.value)) {
    out = array(c(cond), dim=dim(x)[c(variables, condition)])
  }
  else if (is.list(condition.value)) {
    out = subtable(cond, condition, condition.value)
    out = array(c(out), dim=c(dim(x)[variables], sapply(condition.value, length)))
  }
  else {
    out = subtable(cond, condition, condition.value)
    out = array(c(out), dim=dim(x)[variables])
  }

  return(out)
}

# ALLOWS US TO REPEAT PROBABILITIES ETC ACCORDING TO A PATTERN
# x = VECTOR TO BE REPEATED
# which = INDEXES ALREADY INCLUDED
# n = DIMENSION OF EACH INDEX
# careful = DON'T ASSUME PROPERLY FORMATTED INPUT
patternRepeat = function (x, which, n, careful=TRUE)
{
  if (careful) {
    tmp = unique(which)
    if (!all(tmp == which))
      warning("repeated indices ignored")
    which = sort(tmp)
    if (prod(n[which]) != length(x))
      stop("x not of correct length")
    if (length(which) > length(n))
      stop("which too long")
  }

  tmp = setdiff(seq_along(n), which)
  if (length(tmp) == 0)
    return(x)
  add.in = min(tmp)
  bl = prod(n[seq_len(add.in - 1)])
  x.new = x[rep.int(seq_len(bl), n[add.in]) + rep(seq.int(from = 0,
                                                  by = bl, length.out = length(x)/bl), each = bl * n[add.in])]
  which.new = c(seq_len(add.in), which[which > add.in])

  return(Recall(x.new, which.new, n, careful=FALSE))
}


# Partial order on subsets: returns
# -1 if x is strict subset of y
# 1 if y is strict subset of x
# 0 otherwise (either equal or incomparable)
subsetOrder = function(x, y) {
  if (setequal(x,y)) return(0)
  else if (is.subset(x,y)) return(-1)
  else if (is.subset(y,x)) return(1)
  else return(0)
}

greaterThan = function(x, y) {
  return(-1*(x < y) + 1*(x > y))
}

# implement quick sort algorithm for partial orderings
# and pairwise comparisons.
# x is list or vector of objects to be sorted'
# f a binary operation on x which returns -1, 0, or 1.
# random indicates whether to choose comparison element
# for algorithm at random (recommended)
# may fail if f does not give a legitimate partial order.
quickSort = function(x, f=greaterThan, random=TRUE) {
  lst = is.list(x)
  n = length(x)
  if (n < 2) return(seq_len(n))
  if (n == 2) {
    if(lst) com = f(x[[1]], x[[2]])
    else com = f(x[1], x[2])
    if (com == 0) return(c(1,1))
    else if (com == 1) return(c(2,1))
    else if (com == -1) return(c(1,2))
    else stop()
  }

  if (random) mid = sample(n,1)
  else mid = ceiling(n/2)
  comp = numeric(n)
  comp[mid] = 2

  for (i in seq_len(n)[-mid]) {
    if (lst) comp[i] = f(x[[i]], x[[mid]])
    else comp[i] = f(x[i], x[mid])
  }

  lu = Recall(x[comp == 1], f, random)
  ld = Recall(x[comp == -1], f, random)
  lm = Recall(x[comp == 0], f, random)

  rank = numeric(n)
  rank[comp == -1] = ld
  rank[mid] = max(c(0,ld)) + 1
  rank[comp == 0] = lm + max(c(0,ld))
  rank[comp == 1] = lu + max(c(0,rank), na.rm=TRUE)

  return(rank)
}


condition.table2 = function(x, variables, condition) {
  out = aperm(condition.table(x, variables, condition), order(c(variables, condition)))
  tmp = patternRepeat(seq_len(prod(dim(x)[c(variables, condition)])), c(variables, condition), dim(x))
  return(array(out[tmp], dim(x)))
}

intervention.table = function(x, variables, condition) {
  tmp = condition.table2(x, variables, condition)
  out = x/tmp
  out
}

arrayInd = function(ind, .dim, .dimnames = NULL, useNames = FALSE) {
  rank = length(.dim)
  call = if (length(ind)) {
    .C("arrayInd", as.integer(ind-1), as.integer(.dim), length(ind), rank, integer(rank*length(ind)), package="rje")[[5]]
  }
  else integer(0)
  out = matrix(call, nrow=length(ind), ncol=rank)
  if(useNames) dimnames(out) = list(.dimnames[[1L]][ind], if (rank == 2L) c("row", "col") else paste0("dim", seq_len(rank)))

  out
}

indexBox = function(upp, lwr, dim) {
  if (any(upp < 0) || any(lwr > 0)) stop("Incorrect bounds")
  if (any(dim <= 0)) stop("Incorrect dimensions")
  ld = length(dim)
  if (length(upp) != ld) upp = rep(upp, length.out=ld)
  if (length(lwr) != ld) lwr = rep(lwr, length.out=ld)
  out = .C("indexBox", as.integer(upp), as.integer(lwr), as.integer(dim), ld, integer(prod(upp-lwr+1)), package="rje")
  return(out[[5]])
}

and0 = function(x,y) {
  as.logical(x*y)
}

or0 = function(x,y) {
  as.logical(x+y)
}

last = function(x) x[length(x)]

fsapply = function(x, FUN) unlist(lapply(x, FUN))

armijo = function(fun, x, dx, beta = 3, sigma = 0.5, grad, maximise = FALSE, searchup = TRUE, adj.start = 1, ...) {

  sign = ifelse(maximise, -1, 1)
  if (beta <= 1) stop("adjustment factor beta must be larger than 1")
  adj = adj.start
  tol = .Machine$double.eps
  cur = sign*fun(x, ...)
  nok = TRUE
  neg = TRUE
  if (missing(grad)) {
    grad = numeric(length(x))
    for (i in seq_along(x)) {
      x2 = x; x2[i] = x[i] + 1e-8
      grad[i] = 1e8*(fun(x2, ...) - fun(x, ...))
    }
  }
  if (missing(dx)) dx = -grad
  s = -sigma*sum(dx*grad)
  moddx = sqrt(sum(dx^2))
  if (s < 0) stop("Appear to be at minimum, or direction is infeasible")

  # search to find point satisfying Armijo rule
  while (nok) {
    try = sign*fun(x + dx*adj, ...)
    if (!is.na(try) && (cur - try) > adj*s) {
      nok = FALSE
    }
    else {
      adj = adj/beta
      searchup = FALSE
    }

    if(neg && !is.na(try) && (cur - try) > -adj*s) neg = FALSE

    # if adjustment gets too small, return with suggestion of either
    # convergence or wrong direction
    if (adj*moddx < tol) return(list(x=x, adj=adj, move = dx*adj, best=sign*cur, code = 2*neg))
  }

  if (searchup) {
    ok = TRUE
    while (ok) {
      try2 = sign*fun(x + dx*adj, ...)
      if (!is.na(try2) && (cur - try2) < adj*s) {
        try = try2
        adj = adj/beta
        break
      }
      else {
        adj = adj*beta
      }
      if (adj > 1/tol) {
        # if move gets too big, maybe dx is very small
        if (sqrt(sum(adj*dx^2)) < 10*tol) return(list(x = x + dx*adj, adj = adj, move = dx*adj, best = sign*try, code = 3))
        else break
      }
    }
  }

  return(list(x = x + dx*adj, adj = adj, move = dx*adj, best = sign*try, code = 1))
}

coarseLine = function(fun, x, dx, beta = 3, maximise = FALSE, ...) {

  sign = ifelse(maximise, -1, 1)
  if (missing(dx)) {
    # use minus numeric derivative
    dx = numeric(length(x))
    for (i in seq_along(x)) {
      x2 = x; x2[i] = x[i] + 1e-8
      dx[i] = -sign*1e8*(fun(x2, ...) - fun(x, ...))
    }
  }

  if (beta <= 1) stop("adjustment factor beta must be larger than 1")
  adj = 1
  tol = .Machine$double.eps
  best = sign*fun(x, ...)
  nok = TRUE

  # find a step size which gives a valid move
  while (nok) {
    best = sign*fun(x + adj*dx, ...)
    if (!is.na(best)) nok = FALSE
    else adj = adj/beta
    if (adj < tol) stop("Failed to find valid move")
  }

  try = sign*fun(x + dx*adj*beta, ...)
  # determine whether to try smaller steps or bigger ones
  if (is.na(try) || try >= best) down = TRUE
  else down = FALSE

  if(down) fact = 1/beta
  else fact = beta

  nok = TRUE
  while(nok) {
    try = sign*fun(x + dx*adj*fact, ...)
    # keep trying til move outside space or criterion is reduced
    if (is.na(try) || try >= best) {
      nok = FALSE
      # beyond tolerance of size for adj
      if (adj < tol || 1/adj < tol) return(list(x = x + dx*adj, adj=adj, move=dx*adj, best=sign*best, code=2))
    }
    else {
      adj = adj*fact
      best = try
      if (adj < tol || 1/adj < tol) return(list(x = x + dx*adj, adj=adj/fact, move=dx*adj/fact, best=sign*best, code=3))
    }
  }

  return(list(x = x + adj*dx, adj=adj, move=dx*adj, best=sign*best, code=1))
}

cubeHelix = function(n, start=0.5, r=-1.5, hue=1, gamma=1) {
  M = matrix(c(-0.14861, -0.29227, 1.97294, 1.78277, -0.90649, 0), ncol=2)

  lambda = seq(0,1,length.out=n)
  l = rep(lambda^gamma, each=3)

  phi = 2*pi*(start/3 + r*lambda)
  t = rbind(cos(phi), sin(phi))

  out = l + hue*l*(1-l)/2 * (M %*% t)
  out = pmin(pmax(out, 0), 1)
  out = apply(out, 2, function(x) rgb(x[1],x[2],x[3]))

  return(out)
}
