# Climate & Culture Survey-Processing Code

The code in this repository visualizes the 2025 climate and culture survey results. **Survey data _is not_ shared in this repository.**

## Workflow Explanation

_Exports of any of these scripts share the prefix of the script that created them! For example, a graph produced by `climate-03_visualize.r` will have a file name like "climate-03\_\<informative-graph-name\>.png"._

- `climate-01_process.r` -- Take Qualtrics-format data and get it into a format that can be used for analysis / visualization. There should be _no_ exclusion of rows in this step (i.e., the output should be useable regardless of question)
- `climate-02_summarize.r` -- Summarize data within question and calculate "composite scores"
- `climate-03_visualize.r` -- Graph all questions in climate survey on a network level and per-site basis.
