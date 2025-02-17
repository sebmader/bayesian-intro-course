# This course will get a complete makeover <br> in Feb 2025

# Introduction to Bayesian statistics in R & brms

4 day course: 24-27 February 2025  
German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig  
benjamin.rosenbaum@idiv.de  

### Outline

The course offers a straightforward and practical approach to applied statistics using Bayesian inference for ecologists. It starts with a general introduction to statistical modeling and the concepts of Bayesian statistics (likelihood, priors, posterior distribution, MCMC sampling). We will move step-by-step from classical ANOVA and linear regression to generalized, nonlinear, or mixed-effects models, with a strong conceptual focus on the building blocks of statistical models.

While previous software required users to code in specific modeling languages (JAGS, Stan, NIMBLE), we are focusing on the user-friendly and flexible R-package ‘brms’, which makes the transition easy for people familiar with ‘lm’ or ‘lme4’. An additional introduction to coding in Stan will be provided for interested participants.

Participants learn how to practically conceptualize their research questions into statistical models. They learn how to specify and critically interpret models of varying complexity in R. The course prepares participants to analyze their own data with state-of-the-art methodology.

### Curriculum

|   | Lecture | Practical |
| ------------- | ------------- | ------------- |
| (1) Statistical modeling      | [pdf](https://benjamin-rosenbaum.github.io/bayesian-intro/Lecture_1.pdf) | pdf html R  |
| (2) Bayesian principles       | [pdf](https://benjamin-rosenbaum.github.io/bayesian-intro/Lecture_2.pdf) | pdf html R  |
| (3) Priors and posteriors     | [pdf](https://benjamin-rosenbaum.github.io/bayesian-intro/Lecture_3.pdf) | pdf html R  |
| (4) Linear models             | [pdf](https://benjamin-rosenbaum.github.io/bayesian-intro/Lecture_4.pdf) | pdf html R  |
| (5) Generalized linear models | [pdf](https://benjamin-rosenbaum.github.io/bayesian-intro/Lecture_5.pdf) | pdf html R  |
| (6) Mixed effects models      | [pdf](https://benjamin-rosenbaum.github.io/bayesian-intro/Lecture_6.pdf) | pdf html R  |
| (7) Stan introduction         | [pdf](https://benjamin-rosenbaum.github.io/bayesian-intro/Lecture_7.pdf) | pdf html R  |
| (8) Conclusions               | [pdf](https://benjamin-rosenbaum.github.io/bayesian-intro/Lecture_8.pdf) | pdf html R  |

### Software requirements

- Rstudio: [link](https://posit.co/download/rstudio-desktop/)
- C-toolchain: [link](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#configuring-c-toolchain),
- which involves RTools (for Windows): [link](https://cran.r-project.org/bin/windows/Rtools/)
- Some R-packages:

```r
update.packages()
install.packages("devtools")    # install packages from github
install.packages("brms")        # our main software package
install.packages("ggplot2")     # plotting
install.packages("bayesplot")   # additional plotting tools
install.packages("sfsmisc")     # mathematical integration through data points
install.packages("performance") # model evaluation
install.packages("arm")         # model evaluation
install.packages("GGally")      # pairs plots
install.packages("emmeans")     # post-hoc analysis
install.packages("ecostats")    # some datasets
devtools::install_github("jfieberg/Data4Ecologists") # more datasets
```
