## remulate v2.0.0 (Release Date: 11-02-2024)

### Major Updates

-   Refactored Output Structure for `remulateTie` and `remulateActor`
    -   The main output is now a data frame, making it easier to work with.
    -   Metadata (e.g., `riskset`, `statistics`, `density`) are now stored as attributes instead of list elements.

### Breaking Changes

-   The outputs of `remulateTie()` and `remulateActor()` are no longer lists.
    -   Access event sequences directly via `head(output)`.
    -   Use `attr(output, "attribute_name")` instead of `output$attribute_name` for metadata.

### How to Update Your Code

-   Old way (no longer works):

    ``` r
    sim_data <- remulateTie(...)
    sim_data$density  # This will not work anymore
    ```

-   New way (use attributes instead):

    ``` r
    sim_data <- remulateTie(...)
    attr(sim_data, "density")  # Correct way to access metadata
    ```
