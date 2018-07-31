library("DALEX")
library("randomForest")
library("ceterisParibus")
set.seed(59)

# models
apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
                                      no.rooms + district,
                                    data = apartments)
apartments_lm_model <- lm(m2.price ~ construction.year + surface + floor +
                            no.rooms + district, data = apartments)

# explainers
explainer_rf <- explain(apartments_rf_model,
                        data = apartmentsTest[,2:6],
                        y = apartmentsTest$m2.price)
explainer_lm <- explain(apartments_lm_model,
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

apartments_A <- apartmentsTest[958,]
new_apartment <- apartmentsTest[1, ]
