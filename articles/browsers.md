# browsers

``` r

library(lpsugar)
```

In this vignette we’ll optimize a set of parameters to prove that our
browser is the best browser.

## The Problem

Imagine we’re the developers of Arc browser, and we want to prove that
it’s better than other any other browser. We have a table of features
for each of the browsers, that we can quickly make up.

``` r

browsers <- c("Arc", "Chrome", "Firefox", "Opera", "Safari")
features <- c("Speed", "Ease of use", "Customizability", "Privacy", "Extensions",
              "Reading mode", "Ad-blocker")

# Make up ratings from 0 to 10 for each feature
set.seed(8456)
ratings <- parameter(0, features, browsers)
ratings[] <- sample(0:10, size = length(ratings), replace = TRUE)

ratings
#>                  browsers
#> features          Arc Chrome Firefox Opera Safari
#>   Speed             7      2       7     8      4
#>   Ease of use       2      1       7     5      7
#>   Customizability   1     10       9     0      3
#>   Privacy           1      5       8     2      8
#>   Extensions        9      0       7     9      4
#>   Reading mode      0      2      10     0      0
#>   Ad-blocker       10      2       2     8      6
```

The question is: how important is each feature? Let’s try giving all of
them the same weight and calculate the score each browser gets.

``` r

colSums(ratings)
#>     Arc  Chrome Firefox   Opera  Safari 
#>      30      22      50      32      32
```

Nonsense, this would mean Arc is worse than Opera or Safari, and much
worse than Firefox! Let’s give each feature $`i`$ an optimal weight
$`w_i`$. Then we maximize the gap between Arc’s score and the second
best score.

``` math
\max_w {\text{ArcScore}(w) - \text{SecondBestScore}(w)}
```

We’ll make sure each feature *matters* by having $`1 < w_i < 5`$, and
make them integers for simplicity.

``` r

browser_problem <- lp_problem() |> 
    lp_variable(weights[features], lower = 1, upper = 5, integer = TRUE) |> 
    lp_variable(arc_score) |> 
    lp_variable(second_best) |>
    lp_maximize(arc_score - second_best) |> 
    lp_subject_to(
        arc = {
            arc_ratings <- ratings[, "Arc"]
            arc_score == sum(arc_ratings * weights)
        },
        others = for (b in browsers[browsers != "Arc"]){
            b_ratings <- ratings[, b]
            second_best >= sum(b_ratings * weights)
        }
    )

library(ROI.plugin.highs)
browser_solution <- lp_solve(browser_problem)
browser_solution$variables
#> $weights
#> features
#>           Speed     Ease of use Customizability         Privacy      Extensions 
#>               1               1               2               1               5 
#>    Reading mode      Ad-blocker 
#>               1               5 
#> 
#> $arc_score
#> [1] 107
#> 
#> $second_best
#> [1] 100
```

Who cares about speed anyway? Extensions are where it’s at. Take a look
at the new scores:

``` r

optimal_weights <- browser_solution$variables$weights
apply(ratings, 2, \(r) sum(r * optimal_weights))
#>     Arc  Chrome Firefox   Opera  Safari 
#>     107      40      95     100      75
```

That’s much better. We have successfully proved Arc’s unquestionable
superiority!
