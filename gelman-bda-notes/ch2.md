## General Notes:

The central feature of Bayesian inference, the direct quantification of uncertainty, means that there is no impediment in principle to fitting models with many parameters and complicated multilayered probability specifications. This freedom to set up complex models arises in large part from the fact that the Bayesian paradigm provides a conceptually simple method for coping with multiple parameters.

Throughout this book $\theta$ denotes unobservable vector quantities or population *parameters* of interest, $y$ denotes the observed data, and $\tilde{y}$ denote the unknown, but potentially observable, quantities.

$p(\cdot)$ represents a marginal distribution and $Pr(\cdot)$ represents the probability of an event.

## Single-Parameter Models

In the simple binomial model, the aim is to estimate an unknown population proportion from the results of a sequence of bernoulli trials. That is $\theta$ represents the proportion of successes in the population, or equivalently, the probability of success in each trial. We have that

$$
p(y | \theta) = \binom{n}{y}\theta^y(1-\theta)^{n - y}
$$

To perform Bayesian inference, we must specify a prior distribution for $\theta$. For simplicity at this point, we will assume the prior distribution for $\theta$  is uniform on $[0,1]$. By bayes rule we have that

$$
p(\theta | y) \propto p(y | \theta)p(\theta) = \theta^y (1- \theta)^{n-y}\cdot \frac{1}{1-0} =  \theta^y (1- \theta)^{n-y}
$$

With fixed $n$ and $y$ the factor $\binom{n}{y}$ does not depend on the unknown parameter $\theta$ so it can be treated as a constant. Our posterior density can be recognized as being a *beta* distribution.

$$
\text{Beta}(\alpha | \beta) \propto x^{\alpha - 1}(1-x)^{\beta - 1} \qquad \text{so} \qquad p(\theta | y) \sim \text{Beta}(y + 1, n-y + 1)
$$

