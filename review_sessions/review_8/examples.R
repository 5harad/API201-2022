library(tidyverse)

# Dog example
nf <- 30
nm <- 30
pfhat <- .2
pmhat <- .1

d <- pfhat - pmhat
se <- sqrt(pfhat * (1 - pfhat) / nf + pmhat * (1 - pmhat) / nm)
z <- d / se
2 * pnorm(-abs(z))

c(lb = d - 2 * se, ub = d + 2 * se)

# Racial concordance example
concordance <- tibble(
   gender = c("Women", "Men"),
   n = c(500, 500),
   phat = c(.48, .41),
   se = sqrt(phat * (1 - phat) / n),
   lb = phat + qnorm(.025) * se,
   ub = phat + qnorm(.975) * se,
)

concordance %>% 
   summarize(diff = phat[gender == "Women"] - phat[gender == "Men"],
             diff_se = sqrt(sum(phat * (1 - phat) / n))) %>% 
   mutate(z = diff / diff_se,
          p = 2 * pnorm(-abs(z)))

concordance_plot <- ggplot(concordance,
       aes(x = gender, y = phat)) +
   geom_col() +
   geom_errorbar(aes(ymin = lb, ymax = ub), 
                 color = "red", width = .1) +
   labs(x = NULL, y = "Proportion with black doctor")

ggsave("concordance_plot.png", concordance_plot, 
       width = 7, height = 5, scale = .7)
