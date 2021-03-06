# wama

<!-- badges: start -->
[![.github/workflows/main.yaml](https://github.com/maxheld83/wama/actions/workflows/main.yaml/badge.svg)](https://github.com/maxheld83/wama/actions/workflows/main.yaml)
[![Codecov test coverage](https://codecov.io/gh/maxheld83/wama/branch/master/graph/badge.svg)](https://codecov.io/gh/maxheld83/wama?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/biblids)](https://CRAN.R-project.org/package=biblids)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

> Wama
> [ˈvama] *noun*
> German acronym for *Waschmaschine* (washer)

When iterating during local development, there's a lot of *lather*, *rinse*, *repeat*.
wama does it for you, in the background.

For example, you might often want to run:

1. `devtools::document()`
2. `pkgdown::build_site()`
3. `pkgdown::preview_site()`

and other such workflows.

Existing keyboard shortcuts are a great help, but sometimes even that is too much of a hassle.

For rapid iterations, especially on shiny apps or when polishing the documentation, automatically re-running these steps saves time and helps you stay focused.

wama watches relevant files and folders for changes, and then automatically triggers these jobs in the background.

## Notes

Important to note:

- wama always runs in the *background*, that is *non-interactively*.
    A lot of development iteration is still better accomplished interactively.
    But even there, you may benefit from an always-up-to-date`man/` and `NAMESPACE`.
- wama is strictly a development-time package; you should never need it in your `DESCRIPTION`.
- wama may be most helpful in an editor or IDE paired with a large screen, where you can view many terminals at once.
    For example [vscode](http://code.visualstudio.com), has great support for multiple terminals, website preview and [R](https://marketplace.visualstudio.com/items?itemName=Ikuyadeu.r)


## Related Work

- [`option(shiny.autoreload = TRUE)`](https://shiny.rstudio.com/reference/shiny/0.14/shiny-options.html) is a native shiny feature to automatically reload a shiny app during development when relevant files are changed.
    However, it doesn't work with shiny apps in packages, or which rely on changes in packages under development.
- [`testthat::auto_test()`](https://testthat.r-lib.org/reference/auto_test.html) watches source and test files and reruns relevant tests.
    However, it seems to be [retired](https://github.com/r-lib/testthat/issues/922#issuecomment-520580955).

And I'm really hoping I didn't miss something and am uselessy reinventing the wheel here ... 😬.
