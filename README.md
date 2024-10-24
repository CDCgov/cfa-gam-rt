# RtGam: Estimating $R_t$ with Generalized Additive Models

[![codecov](https://codecov.io/gh/CDCgov/cfa-gam-rt/graph/badge.svg?token=78QNZW20IW)](https://codecov.io/gh/CDCgov/cfa-gam-rt)
[![R-CMD-check](https://github.com/CDCgov/cfa-gam-rt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CDCgov/cfa-gam-rt/actions/workflows/R-CMD-check.yaml)

> [!CAUTION]
> This project is an early-stage work-in-progress. Any outputs may be misleading or even incorrect. Despite this project's early stage, all development is in public as part of the Center for Forecasting and Outbreak Analytics' goals around open development. Questions and suggestions are welcome through GitHub issues or a PR.

notnotwordword

## Overview

This project is an in-development R package for $R_t$ estimation using generalized additive models (GAMs).
Over the last few years, GAMs have been used with some success to estimate epidemic growth rates and nowcast cases (see [Ward, 2021](https://doi.org/10.1136/bmjopen-2021-056636) and [Mellor, 2023](https://doi.org/10.1038/s43856-023-00424-4) for an overview of methods).
GAM-based models enable rapid, adaptable prototyping, testing different model structures and data sources with minimal computational and development cost.
The [R package `{mgcv}`](https://cran.r-project.org/web/packages/mgcv/index.html) provides a flexible, fast, and robust interface to fit penalized-spline based models.
However, the substantial research effort into spline-based approaches has not been unified into a shared R package.

This R package aims to become an opinionated re-implementation of the literature methods, with a focus on enabling hierarchical modeling.
Development effort is optimized around real-time use-cases, with potential for right-truncation, noisy reporting, and uncertain data-generating processes.
It is meant to be a simple drop-in tool to be run alongside more computationally intensive implementations like [`{EpiNow2}`](https://github.com/epiforecasts/EpiNow2).

At the moment, the package has some simple functionality to fit a adaptive smooth trend to a single epidemic timeseries and produce a short forecast.
It has not yet been benchmarked relative to other approaches.

## Installation

The package can be installed from GitHub:

```r
remotes::install_github("cdcgov/cfa-gam-rt@v0.1.0")
```

## Use

The package can be used to fit smooth trends of time to noisy epidemic data.
It includes a simulated dataset from Gostic et al., 2020 "Practical considerations for measuring the effective reproductive number, $R_t$".
This snippet uses the simulated dataset to demonstrate how to use the main model-fitting function:

```r
library(RtGam)

fit <- RtGam(
        cases = stochastic_sir_rt[["obs_incidence"]],
        # Randomly chosen date
        reference_date = stochastic_sir_rt[["time"]] + as.Date("2023-01-01")
       )
print(fit)
```

## Project Admin

- Zachary Susswein (@zsusswein)
- Katelyn Gostic (@kgostic)
- Sam Abbott (@seabbs)

------------------------------------------------------------------------------------

## General Disclaimer
This repository was created for use by CDC programs to collaborate on public health related projects in support of the [CDC mission](https://www.cdc.gov/about/organization/mission.htm).  GitHub is not hosted by the CDC, but is a third party website used by CDC and its partners to share information and collaborate on software. CDC use of GitHub does not imply an endorsement of any one particular service, product, or enterprise.

## Public Domain Standard Notice
This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

## License Standard Notice
This repository is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify it under
the terms of the Apache Software License version 2, or (at your option) any
later version.

This source code in this repository is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the Apache Software License for more details.

You should have received a copy of the Apache Software License along with this
program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

The source code forked from other open source projects will inherit its license.

## Privacy Standard Notice
This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](https://github.com/CDCgov/template/blob/master/DISCLAIMER.md)
and [Code of Conduct](https://github.com/CDCgov/template/blob/master/code-of-conduct.md).
For more information about CDC's privacy policy, please visit [http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

## Contributing Standard Notice
Anyone is encouraged to contribute to the repository by [forking](https://help.github.com/articles/fork-a-repo)
and submitting a pull request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including but not limited to the Federal Records Act, and may be archived. Learn more at [http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

## Records Management Standard Notice
This repository is not a source of government records but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).

## Additional Standard Notices
Please refer to [CDC's Template Repository](https://github.com/CDCgov/template)
for more information about [contributing to this repository](https://github.com/CDCgov/template/blob/master/CONTRIBUTING.md),
[public domain notices and disclaimers](https://github.com/CDCgov/template/blob/master/DISCLAIMER.md),
and [code of conduct](https://github.com/CDCgov/template/blob/master/code-of-conduct.md).
