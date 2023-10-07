# How to generate tracked changes

## Steps

* Install `latexdiff`. On MacOS, run `brew install latexdiff` in the terminal.
* In the terminal, navigate to the `submission2` directory containing the `outbreak_response_models_review_v2.tex` file.

* While in the directory in the previous step, run `latexdiff ../submission1/outbreak_response_models_review.tex outbreak_response_models_review_v2.tex --disable-citation-markup > ./tracked_changes/outbreak_response_models_review_v1_v2_tracked_changes.tex`

* Accept all changes with:
`latexrevise -a ./tracked_changes/outbreak_response_models_review_v1_v2_tracked_changes.tex > ./tracked_changes/outbreak_response_models_review_v1_v2_tracked_changes_accepted.tex`