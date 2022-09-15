## General Notes:

The central feature of Bayesian inference, the direct quantification of uncertainty, means that there is no impediment in principle to fitting models with many parameters and complicated multilayered probability specifications. This freedom to set up complex models arises in large part from the fact that the Bayesian paradigm provides a conceptually simple method for coping with multiple parameters.

Throughout this book $\theta$ denotes unobservable vector quantities or population *parameters* of interest, $y$ denotes the observed data, and $\tilde{y}$ denote the unknown, but potentially observable, quantities.

$p(\cdot)$ represents a marginal distribution and $Pr(\cdot)$ represents the probability of an event.

## Single-Parameter Models

### Basic Example

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
\text{Beta}(\alpha | \beta) \propto x^{\alpha - 1}(1-x)^{\beta - 1} \qquad \text{so} \qquad \theta | y \sim \text{Beta}(y + 1, n-y + 1)
$$

The process of bayesian inference involves passing from a prior distribution to a posterior distribution, and we can think of our posterior as being a compromise between the data and the prior information. One nice feature is that the posterior is on average less variable than the prior distribution. We get this immediately from a well known property of the variance

$$
Var(\theta) = E[Var(\theta | y)] + Var(E[\theta | y]) \implies E[Var(\theta | y)] \le Var(\theta)
$$

The compromise between the prior information and the data is controlled to a greater extent by the data as the sample size increases.

### Informative prior distributions

In the *population* interpretation, the prior distribution represents a population of possible parameter valyes. In the *state of knowledge* interpretation, the guiding principle is that we must express our knowledge about $\theta$ as if its value could be thought of as a random realization from the prior distribution.

One option is to choose a prior that matches the *form* of the likelihood. If our likelihood still has the form

$$
p(y | \theta) \propto \theta^y(1-\theta)^{n-y}
$$

then we could choose the prior density 

$$
p(\theta) \propto \theta^{\alpha - 1}(1-\theta)^{\beta - 1}
$$

which would yield the posterior density of

$$
p(\theta | y) \propto \theta^y(1-\theta)^{n-y}\theta^{\alpha - 1}(1-\theta)^{\beta - 1} = \theta^{y + \alpha - 1}(1- \theta)^{n - y + \beta - 1} = \text{Beta}(\alpha + y, n - y + \beta)
$$

We call this property *conjugacy*, when the posterior distribution follows the same parametric form as the prior distribution. Conjugacy is mathematically convenient in that we know the form of the posterior distribution. (i.e it's easy to understand the results, and the computations are usually simpler). That being said, conjugate priors may not be possible, and the good news is that nonconjugate priors do not pose any new *conceptual* problems.


