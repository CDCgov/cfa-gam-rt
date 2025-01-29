# RtGam v0.4.0

## Features
* Set up dev and release versions of pkgdown site (#101)
* Allow additional user-specified ggplot2 geoms to be added to the returned object from `plot()`

## Bugs
* Fix spurious diagnostic warnings when using a day-of-week effect (#104)
* Use `.data` pronoun for NSE (#107)

## Developer tooling
* Add R installation to the pre-commit workflow, fixing the failures caused by the upgrade to the default Ubunut runner (#112)

# RtGam v0.3.0

This release introduces support for modeling and correcting day-of-week effects in case reporting. It allows for default day-of-week detection or user-specified custom day-of-week levels, such as for holidays.

## Features
* Add day of week effect to simulated timeseries (#91)
* Estimate day-of-week effects as a random effect in the fitted model and correct for them in growth rate estimation (#89)

# RtGam v0.2.0

This release introduces new functionality for working with RtGam models, including S3 methods for prediction and plotting. It also includes bug fixes, improved handling of input data, and enhanced developer tooling to streamline contributions and maintenance.

## Features
* Predict method for RtGam models, with parameters `obs_cases`, `obs_incidence`, `r`, and `Rt` (#48)
* Plot method for RtGam models built off the predict method (#76)
* Cast dates passed as strings to a `Date` if possible (#67)
* New "Getting Started" vignette walking through the basics on how to use the package (#83)

## Bugs

* Drop old typos that snuck through the WORDLIST setup (#73)
* Turn off credential caching in WORDLIST update action preventing CI from restarting (#74)
* Prepend missing zero to the start of the GI PMF for the sample dataset

## Developer tooling

* Update `{primarycensoreddist}` to `{primarycensored}` to match the name change in the v0.6.0 release (#53)
* Automatically refer to the argument name in error messages (#51)
* Add Dependabot for GitHub Actions (#55)
* Error R CMD check for NOTEs when running in CI (#57)
* Set default merge strategy for NEWS.md to `union` to limit merge conflicts (#58)
* Added a Justfile with some common development tasks (#56)
* Set up automated spellcheck (#61)
* Add slash-command to update WORDLIST in a PR (#64)
* Remove pre-commit hooks from extraneous languages
* Add Justfile recipe to locally update PR with rebase (#75)
* Cancel in-progress GitHub Actions runs on subsequent push to a PR (#72)
* Set default merge strategy for WORDLIST to `union` to limit merge conflicts (#78)

# RtGam v0.1.0

This initial release focuses on establishing the package's foundation to support future development. It introduces basic functionality for fitting and returning models, while setting up essential components such as package initialization, documentation, and continuous integration systems.

## Features

* Add as package data the simulated dataset from Gostic, 2020 for unit testing and future benchmarking (#37)
* Display CI status in README badges (#27)
* Initial working package, prepping input data, fitting the model, and returning an RtGam object (#14)
* Specify an internal data schema with comprehensive input validation (#7)

## Bugs

* Fix math rendering in pkgdown (#36)
* Update pre-commit hooks from template repository to newest version (#25)
* Clean up public docs (#26)
* Initial CI setup (#11)
