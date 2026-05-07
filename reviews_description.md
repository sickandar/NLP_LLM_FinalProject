# Customer Review Dataset

This dataset contains customer review data. Each row represents one review.

## Columns

- `primary_id`: Unique identifier for each review. This is auto-generated, and has no identifiable purpose, except as a record locator.
- `review_date`: Date when the review was submitted. The timespan is between January 1st 2026, till May 12th 2026.
- `category`: Product or review category.
- `rating`: Numeric rating given by the reviewer. These are scaled between 1 to 5, 1 being the lowest and 5 being the highest.
- `text`: Full written review text.

## Dataset Purpose

The dataset is used to explore customer review patterns, including review volume, ratings, product categories, labels, and written feedback.

## Filtering Guidance

Users may filter the data by:

- review date
- category
- rating
- review text

## Important Notes

When users ask about reviews, use the `text` column.

When users ask about ratings, use the `rating` column.

When users ask about product types or groups, use the `category` column.

When users ask about time periods, dates, trends, or recent reviews, use the `review_date` column.