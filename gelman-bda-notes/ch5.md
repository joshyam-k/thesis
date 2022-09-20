## Hierarchical Models

Generally the idea is that if we have a set of experiments $j = 1 , ..., J$ in which experiment $j$ has data (vector) $y_j$ and parameter (vector) $\theta_j$, with likelihood $p(y_j, \theta_j)$. In this setting it makes more sense to try to estimate the population distribution from all the data and us bayesian methods on this to estimate each $\theta_j$ rather than estimate all values of $\theta_j$ separately. In order to create a joint probability model for all the parameters $\theta$, we use the crucial idea of exchangeability.

The parameters $(\theta_1, ..., \theta_J)$ are exchangeable in their joint distribution if $p(\theta_1, ...,\theta_J)$ is invariant to permutations of the indexes.
