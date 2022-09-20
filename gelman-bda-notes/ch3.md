## Introduction to multiparameter models

Virtually every practical problem in statistics involves more than one unknown or unobservable quantity. Although a problem can include several parameters of interest, conclusions will often be drawn about one. In principle the route to achieving this is clear: we first require the *joint* posterior distribution of all unknowns and then we integrate this distribution over the unnowns that are not of immediate interest to obtain the desired marginal distribution.

$$
p(\theta_1, \theta_2 | y) \propto p(y | \theta_1, \theta_2)p(\theta_1,\theta_2)
$$

We get the posterior denisty for a single parameter by integrating over the "nuisance" parameter:

$$
p(\theta_1 | y) = \int p(\theta_1, \theta_2 | y) d\theta_2 = \int p(\theta_1 | \theta_2, y)p(\theta_2 | y)d\theta_2
$$

We almost never evaluate this integral explicitly, but it suggests an important practical strategy for both constructing and computing with multiparameter models. First we draw $\theta_2$ from its marginal posterior distribution and then $\theta_1$ from its conditional posterior distribution given the drawn value of $\theta_2$.

### Normal data with a noninformative prior distribution

A sensible noninformative prior for $\mu$ and $\sigma$ is uniform on $(\mu, \log{(\sigma)})$ or equivalently $p(\mu, \sigma^2) = (\sigma^2)^{-1}$.

Our joint posterior distribution is then:

$$
\begin{aligned}
p(\mu, \sigma^2 | y) &\propto \sigma^{-n-2}\text{exp}\bigg(-\frac{1}{2\sigma^2}\sum_{i=1}^n(y_i - \mu)^2\bigg) \\
&= \sigma^{-n-2}\text{exp}\bigg(-\frac{1}{2\sigma^2}[(n-1)s^2 + n(\bar{y} - \mu)^2]\bigg)
\end{aligned}
$$

where 

$$
s^2 = \frac{1}{n-1}\sum_{i=1}^n(y_i - \bar{y})^2
$$

As described above we now want $p(\mu | \sigma^2, y)$ and $p(\sigma^2 | y)$ in order to simulate the posterior for $\mu$. We know that the mean of a normal distribution with *known* variance and a uniform prior distribution is just

$$
\mu | \sigma^2 , y \sim \mathcal{N}(\bar{y}, \sigma^2/n)
$$

There is a lot of math done on page 75 that shows that 

$$
\sigma^2 | y \sim \text{Inv}-\chi^2(n-1, s^2)
$$
