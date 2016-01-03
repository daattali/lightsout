# Version 0.2.0

# Round 1

## Test environments

* Windows 7, R 3.2.2 (local)
* ubuntu 12.04, R 3.2.2 (on travis-ci)
* ubuntu 14.04, R 3.1.3 (on my DigitalOcean droplet)

## Submission comments

2016-01-03

Tested on Windows and 7 and Ubuntu 12.04. There were no ERRORs or WARNINGs and 1 NOTE that informed me who the maintainer is and what the license is.

## Reviewer comments

Kurt Hornik 2016-01-03:

```
We get

Non-FOSS package license (file LICENSE)

I guess you want BSD_2_clause + file LICENSE?

Also,

* checking R code for possible problems ... NOTE
print.lightsout: no visible global function definition for
  ‘write.table’
print.lightsout_solution: no visible global function definition for
  ‘write.table’
random_board: no visible global function definition for ‘runif’
Undefined global functions or variables:
  runif write.table
Consider adding
  importFrom("stats", "runif")
  importFrom("utils", "write.table")
to your NAMESPACE.

Pls fix

Best
```

# Round 2

## Test environments

* Windows 7, R 3.2.2 (local)
* ubuntu 12.04, R 3.2.2 (on travis-ci)
* ubuntu 14.04, R 3.1.3 (on my DigitalOcean droplet)

## Submission comments

2016-01-03

Fixed license line in DESCRIPTION and added namespacing to functions from default packages

## Reviewer comments
