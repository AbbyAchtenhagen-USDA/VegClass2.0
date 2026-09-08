# Core Attributes for Forest Management

This guide explains each core vegClass attribute in plain language and why it matters for forest management decisions.

## How to Read This Guide

- "What it is" explains the metric.
- "Why it matters" explains how managers can use it.
- Core attributes are produced for all regions, so they are useful for cross-region planning and reporting.

## Core Attribute Explanations

### `CAN_COV` (Corrected Canopy Cover)
What it is:
- Percent of ground area covered by tree crowns after correcting for crown overlap.

Why it matters:
- Indicates how open or closed a stand is.
- Supports habitat screening, fuels assessments, and regeneration planning.
- Helps identify where light levels may favor understory growth or shade-tolerant species.

### `BA` (Basal Area)
What it is:
- Total stand basal area per acre (`ft^2/ac`).

Why it matters:
- A primary measure of stand stocking and tree biomass concentration.
- Used to guide thinning intensity, residual density targets, and growth comparisons.
- Helps track structural change over time after treatment.

### `TPA` (Trees Per Acre)
What it is:
- Total number of trees per acre from expansion factors.

Why it matters:
- Shows stand density in stems, independent of tree size.
- Useful for spacing, regeneration success, and competition evaluations.
- Complements BA by showing whether density is driven by many small trees or fewer large trees.

### `QMD` (Quadratic Mean Diameter)
What it is:
- Diameter of the tree with mean basal area for the stand.
- More influenced by larger trees than a simple arithmetic mean DBH.

Why it matters:
- Represents overall stand size structure in a single value.
- Useful for growth and yield interpretation and treatment timing.
- Often used with TPA to evaluate density-size relationships.

### `ZSDI` (Zeide Stand Density Index)
What it is:
- Density index that combines tree size and stem density.

Why it matters:
- Indicates relative competition and crowding pressure.
- Useful for comparing stands with different tree sizes on a common scale.
- Supports thinning and density management decisions.

### `RSDI` (Reineke Stand Density Index)
What it is:
- Reineke-based density index using stand TPA and QMD.

Why it matters:
- Another standard index for stand occupancy and competition.
- Commonly used for density target setting and stand trajectory comparisons.
- Helpful for treatment prescriptions and monitoring post-treatment response.


## Why These Core Attributes Matter Together

- `CAN_COV`, `BA`, and `TPA` describe canopy closure and stocking intensity.
- `QMD`, `BA_WT_DIA`, and `BA_WT_HT` describe size and dominant structure.
- `ZSDI` and `RSDI` (plus stem-only versions) describe density pressure and competition.
- `QMD_TOP20` adds a dominant-cohort lens that is important for habitat, resilience, and late-structure goals.

Used together, these metrics provide a practical stand condition profile for treatment design, monitoring, and adaptive management.
