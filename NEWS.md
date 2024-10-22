# RtGam v0.2.0

## Features

## Bugs

## Developer tooling

* Update `{primarycensoreddist}` to `{primarycensored}` to match the name change in the v0.6.0 release (#53)
* Automatically refer to the argument name in error messages (#51)
* Add dependabot for GitHub Actions (#55)
* Error R CMD check for NOTEs when running in CI (#57)
* Set default merge strategy for NEWS.md to `union` to limit merge conflicts (#58)

# RtGam v0.1.0

This initial release focuses on establishing the package's foundation to support future development. It introduces basic functionality for fitting and returning models, while setting up essential components such as package initialization, documentation, and continuous integration systems.

## Features

* Add as package data the simulated dataset from Gostic, 2020 for unit testing and future benchmarking (#37)
* Display CI status in README badges (#27)
* Initial working package, prepping input data, fitting the model, and returning an RtGam object (#14)
* Specify an internal data schema with comprehensive input validation (#7)

## Bugs

* Fix math rendering in pkgdown (#36)
* Update pre-commit hooks from template repo to newest version (#25)
* Clean up public docs (#26)
* Initial CI setup (#11)
