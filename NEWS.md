ceterisParibus 0.3.1
----------------------------------------------------------------

* The `what_if_2d()` function plots 2D ceteris paribus plots. 
This function returns an object of the class `what_if_2d`. 
One can use generic `print()` or `plot()` to show these profiles. 
Note that profiles for all pairs of variables are generated, thus it may be a time consuming operation if number of variables is high.
* New function `calculate_oscillations()` that calculates variable 
importance measures for a single observation.

ceterisParibus 0.3
----------------------------------------------------------------

* The `ceteris_paribus()` function now covers a very flexible grammar for visual exploration of black box models. See https://pbiecek.github.io/DALEX_docs/5-ceterisParibus.html#ceterisParibus for more details.
* The `ceteris_paribus()` function from ver 0.2 is now avaliable as `what_if()`. Here is where the development of older version will take place.

Major refactoring of the code
----------------------------------------------------------------

Please note, that `what_if()` and `local_fit()` functions are	remainders from version 0.2 and they will be deprecated in version 0.4. 
They will not be avaliable in version 1.0. 
From version 0.3 the recommended way to create explainers is through the function `ceteris_paribus()`.

Please note, that `plot_interactive()` function will be deprecated in version 0.4.
It will not be avaliable in version 1.0. 
