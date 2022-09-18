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
