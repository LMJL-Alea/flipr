# flipr 0.2.0

## New features

* Compute p-value surfaces when inferring multiple parameters at once via
`two_sample_pf()`;
* Compute a point estimation for a single parameter of interest via
`two_sample_pe()`;
* Compute a confidence interval for a single parameter of interest via
`two_sample_ci()`;
* Automatically draw the p-value function for a single parameter of interest via
`two_sample_viz()`.

## Full list of changes

* Change argument names `test` and `combining_function` to `type` and
`combine_with` respectively. Add argument `alternative` to specify which kind of
test is performed.
* Add a function to compute p-value functions for sets of null hypotheses.
* Draft of article illustrating the computation of p-value functions with
[**flipr**](https://astamm.github.io/flipr/).
* Add $t$, mean and Fisher test statistics.
* Correct two-tail p-value computation.
* Better API for pvalue function.
* Added a function for two sample confidence interval for a single parameter.
* Added an article about permutation inference overview.
* Added dependency to the [**ggplot2**](https://ggplot2.tidyverse.org) package
for plotting features.
* Added dependency to the [**cli**](https://cli.r-lib.org) package for
displaying important information to the user in an elegant fashion.
* Added function for point estimation of a single parameter.
* Added function for confidence interval of a single parameter.
* Added function for automatic plotting of the p-value function of a single
parameter.
* Added dependency to the [**withr**](https://withr.r-lib.org) package for
properly handle local random seed generator settings.
* Added dependency to the
[**viridisLite**](https://github.com/sjmgarnier/viridisLite) package for using
colors compatible with most common types of color blindness.

# flipr 0.1.1

* Fix warning when installing the package on Oracle Solaris 10, x86, 32 bit,
R-release due to pandoc version not available.
* Switch to GPL-3 license because **purrr** dependency is GPL-3 licensed.

# flipr 0.1.0

* Initial release.
* Added a `NEWS.md` file to track changes to the package.
