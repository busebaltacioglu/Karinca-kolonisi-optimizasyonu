
library(knitr)
library(dplyr)
library(ggplot2)

n<-10
max_x<-500
max_y<-500

set.seed(123456)
cities <- data.frame(id = 1:n, x = runif(n, max = max_x), y = runif(n, max = max_y))

ggplot(cities, aes(x,y)) + geom_point()

distance <- as.matrix(stats::dist(select(cities, x, y), diag = TRUE, upper = TRUE))
dist_fun <- function(i, j) {
  vapply(seq_along(i), function(k) distance[i[k], j[k]], numeric(1L))
}

library(ompr)

model <- MIPModel() %>%
  # we create a variable that is 1 iff we travel from city i to j
  add_variable(x[i, j], i = 1:n, j = 1:n, 
               type = "integer", lb = 0, ub = 1) %>%
  
  # a helper variable for the MTZ formulation of the tsp
  add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
  
  # minimize travel distance
  set_objective(sum_expr(dist_fun(i, j) * x[i, j], i = 1:n, j = 1:n), "min") %>%
  
  # you cannot go to the same city
  set_bounds(x[i, i], ub = 0, i = 1:n) %>%
  
  # leave each city
  add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
  #
  # visit each city
  add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
  
  # ensure no subtours (arc constraints)
  add_constraint(u[i] >= 2, i = 2:n) %>% 
  add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)

model

library(ompr.roi)
library(ROI.plugin.glpk)

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

result
head(result$solution, 10)

solution <- get_solution(result, x[i, j]) %>% 
  filter(value > 0) 
kable(head(solution, 3))

paths <- select(solution, i, j) %>% 
  rename(from = i, to = j) %>% 
  mutate(trip_id = row_number()) %>% 
  tidyr::gather(property, idx_val, from:to) %>% 
  mutate(idx_val = as.integer(idx_val)) %>% 
  inner_join(cities, by = c("idx_val" = "id"))

kable(head(arrange(paths, trip_id), 4))

ggplot(cities, aes(x, y)) + 
  geom_point() + 
  geom_line(data = paths, aes(group = trip_id)) + 
  ggtitle(paste0("Optimal route with cost: ", round(objective_value(result), 2)))



