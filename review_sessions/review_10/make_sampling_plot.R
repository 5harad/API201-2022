# Load packages
library(tidyverse)
library(haven)
library(gridExtra)

theme_set(theme_minimal() + 
             theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5)))

# Set population mean and standard deviation
set.seed(1)
mu <- .1
sigma <- 8

# Read data
fastfood_pre <- read_dta("fastfood.dta")

# Create analysis sample
fastfood <- fastfood_pre %>% 
   filter(!is.na(fte_post) & !is.na(fte_pre)
          & state == "NJ") %>%
   mutate(fte_change = fte_post - fte_pre) 

# Plot
plots <- list()

plots$pop_distribution <- 
   ggplot(tibble(fte_change = 0, y = 15, 
                 label = "?"), 
          aes(x = fte_change, y = y, label = label)) +
   geom_text(size = 40) +
   labs(title = "Population Distribution",
        subtitle = bquote(atop(
           "Distribution: ?",
              Mean:mu=="?, "~
              Std.~Deviation:sigma=="?"
        )),
        x = "Change in FTE",
        y = NULL) +
   coord_cartesian(x = c(-25, 25), y = c(0, 30)) 

plots$sample_distribution <-
   ggplot(fastfood) +
   geom_histogram(aes(x = fte_change, y = ..density..),
                  binwidth = 1) +
   labs(title = "Sample Distribution",
        subtitle = bquote(atop(
           "Distribution: Empirical",
           Mean:~hat(mu)==.(round(mean(fastfood$fte_change), 2))~","~
              Std.~Deviation:hat(sigma)==.(round(sd(fastfood$fte_change), 2))
        )),
        x = "Change in FTE",
        y = NULL) +
   coord_cartesian(x = c(-25, 25), y = c(0, .1)) 

plots$sampling_distribution <-
   tibble(x = rnorm(1e5, mu, sigma / sqrt(nrow(fastfood)))) %>% 
   ggplot(aes(x = x)) +
   geom_histogram(aes(y = ..density..), 
                  binwidth = .05) +
   labs(title = "Sampling Distribution",
        subtitle = bquote(atop(
           "Distribution: Normal",
              Mean:~mu=="?,"~
              Std.~Deviation:frac(sigma, sqrt(n))
           %~~%frac(hat(sigma), sqrt(309))
           ==.(round(sd(fastfood$fte_change) / 
                        sqrt(nrow(fastfood)), 2))
        )),
        x = "Sample Average Change in FTE",
        y = NULL) +
   coord_cartesian(x = c(-25, 25)) 


final_plot <- do.call(function(...) 
   grid.arrange(..., nrow = 1), plots)

ggsave("distribution_plots.png", final_plot, scale = 1.6)
