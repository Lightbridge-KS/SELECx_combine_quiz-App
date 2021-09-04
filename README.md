
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SELECx Combine Quiz

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

App currently deployed:
[`here`](https://si-physio-intern.shinyapps.io/SELECx_combine_quiz/)

This Shiny Application combine Moodle’s Quiz report from
[SELECx](http://selecx.si.mahidol.ac.th).

The main business logic was based on
[moodleQuiz](https://github.com/Lightbridge-KS/moodleQuiz) package by
its 4 main functions:

-   **Check Submission:** `check_sub()`

-   **Combine Responses:** `combine_resp()` and `count_resp()`

-   **Combine Grades:** `combine_grades()`

Other data manipulation was performed using R meta-packages:
[tidyverse](https://www.tidyverse.org).
