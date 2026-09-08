# Add a New Region Input in vegClass

This guide documents the full set of changes needed to add a new `region` option to this codebase.

## 1. Choose the Region Key and Output Fields

Pick the region identifier and use it consistently everywhere.

- Numeric style (for example `9`) is easiest because current validation already handles numeric regions.
- String style (for example `"R9"`) is also possible, but requires explicit string checks in more places.

Define the output variables this region should produce (for example `COVERTYPE_R9`, `TREE_SIZE_CLASS_R9`).

## 2. Add a Region Function in R package folder

Create a new file and function for the region logic, such as:

- `R/R9.r`
- function `R9()`

Follow the same input/output pattern used by existing region functions:

- `R/R1.r`
- `R/R2.r`
- `R/domType.r` (Region 3 path)
- `R/denSize.r` and `R/domType.r` (Region 8 path)

Expected behavior:

- Accept stand tree list plus stand-level summary inputs (`TPA`, `BA`, `CC`, `plotvals`) as needed.
- Return a named list of region outputs.
- Return `NA` defaults when required columns are missing.

## 3. Wire the Region into `vegOut()`

`vegOut()` is the main region dispatcher.

File to edit:

- `R/vegOut.r`

Add a new region branch near the existing region blocks (`1`, `2`, `3`, `8`, `"MPSG"`):

- call your new region function
- assign returned values into `vegData$...` columns

Reference branch locations in current file:

- `if(region==1)`
- `if(region==2)`
- `if(region==3)`
- `if(region==8)`
- `if(region=="MPSG")`

## 4. Allow the New Region in `main()` Validation

`main()` rejects unknown regions before processing.

File to edit:

- `R/main_ParallelProcess.r`

Update:

- valid region checks (`if(is.numeric(region) && (!region %in% c(1, 2, 3, 8)))`)
- error/help text that currently says valid values are `1, 2, 3, 8, or MPSG`

## 5. Export New Function to Parallel Workers

`main()` uses `foreach` with explicit `.export`, so worker processes only know exported symbols.

File to edit:

- `R/main_ParallelProcess.r`

Update:

- add your new function (for example `"R9"`) to `.export`
- add any new helpers/constants used by that function

## 6. Update Shared Helpers Only If Needed

If your region needs custom canopy/attribute behavior, update shared utilities:

- `R/plotAttr.r` for region-specific attribute accumulation
- `R/canClass.r` for alternate canopy-class thresholds
- other helpers in `R/` with hard-coded region behavior

Important note:

- `R/plotAttr.r` already has region-specific logic for `region %in% c(8,9)`. If adding a true Region 9 workflow, review this section carefully.

## 7. Expose the Region in User Entry Points

### A. Script path (`run_vegClass.R`)

Update comments/help text near `region = ...` so users see the new supported value.

File:

- `run_vegClass.R`

### B. App UI path (`app_working_v2.R`)

Add the new option to the app.

Files:

- `app_working_v2.R`

Minimum UI updates:

- add region to `selectInput("region", ..., choices = c("1", "2", "3", "8", "MPSG"))`
- add region-to-markdown mapping in the region description switch
- add region logic to attribute explanation maps (`region_map`) so click-explanations are available

## 8. Add Region Description Markdown

Create a new region description file under:

- `region_descriptions/`

Recommended filename:

- `region_descriptions/region_9.md` (or region name equivalent)

Use existing files as templates:

- `region_descriptions/region_1.md`
- `region_descriptions/region_2.md`
- `region_descriptions/region_3.md`
- `region_descriptions/region_8.md`
- `region_descriptions/region_mpsg.md`

## 9. Update Roxygen Docs and Regenerate `man/*.Rd`

Edit source roxygen where supported regions are listed.

Files:

- `R/main_ParallelProcess.r`
- `R/vegOut.r`

Then regenerate docs:

```r
devtools::document()
```

Verify generated outputs:

- `man/main.Rd`
- `man/vegOut.Rd`

## 10. Run a Smoke Test

Run at least one small test database and verify:

- `main(..., region = <new_region>)` runs without region validation errors
- output CSV includes new region-specific columns
- app region selector includes the new option
- region description panel loads the new markdown
- table click-explanations work for the new region attributes

## Suggested Minimal Function Contract

Use this as a starting pattern for a new region function.

```r
R9 <- function(data,
               stand = "StandID",
               species = "SpeciesPLANTS",
               dbh = "DBH",
               expf = "TPA",
               ht = "Ht",
               TPA,
               BA,
               CC,
               plotvals,
               debug = FALSE) {

  results <- list(
    COVERTYPE_R9 = NA,
    TREE_SIZE_CLASS_R9 = NA,
    CROWN_CLASS_R9 = NA
  )

  missing <- c(stand, species, dbh, expf, ht) %in% colnames(data)
  if (FALSE %in% missing) return(results)

  # Region-specific logic here.

  results
}
```

## Checklist Summary

- [ ] Region function created in `R/`
- [ ] `vegOut()` branch added
- [ ] `main()` region validation updated
- [ ] `.export` list updated for parallel workers
- [ ] Shared helpers updated if required
- [ ] `run_vegClass.R` comments updated
- [ ] `app_working.R` and `app_working_v2.R` region choices and mappings updated
- [ ] `region_descriptions/region_<new>.md` created
- [ ] roxygen docs updated and `devtools::document()` run
- [ ] smoke test completed
