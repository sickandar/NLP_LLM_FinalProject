## Customer Review Dashboard using Shiny

### Project Overview
This project is an interactive R Shiny dashboard. The app pulls in sample product review data. The app then allows the users to filter reviews using natural language, view summary statistics, inspect review-level data. Furthermore, an end-user can use natural language to analyze sentiment patterns, identify common product issues, run topic modeling, and ask an AI assistant questions about the current filtered dataset.

### Mechanism
The dashboard uses `querychat` with Anthropic Claude Haiku to let users filter the review dataset using plain English instructions. The app combines a Shiny dashboard interface with rule-based text processing, statistical summaries, topic modeling, and large language model support. The main goal is to make review data easier to filter, summarize, and interpret without requiring the user to manually write code or build filters.

### How does it work
The app uses `querychat` and `ellmer` to allow users to filter the dataset with natural-language prompts.
Instead of manually selecting filters, users can type requests such as:
- Show reviews with rating below 3 
- Show electronics reviews from April

### Dataset
The dashboard is built around a fake customer reviews dataset. The app keeps only original reviews, cleans the product category labels, creates review IDs, assigns review dates, and prepares the data for filtering, visualization, text analysis, and AI-assisted interpretation. A full citation of the dataset is listed under the Citation section.

### Link to R-Shiny app
https://019e002d-d85a-66a8-3068-709e3b74002c.share.connect.posit.cloud/

### Citation
Data used from Mexwell at Kaggle, licensed under CC BY 4.0 https://creativecommons.org/licenses/by/4.0/
Citation: Salminen, J., Kandpal, C., Kamel, A. M., Jung, S., & Jansen, B. J. (2022). Creating and detecting fake reviews of online products. Journal of Retailing and Consumer Services, 64, 102771. https://doi.org/10.1016/j.jretconser.2021.102771
https://www.kaggle.com/datasets/mexwell/fake-reviews-dataset
 
