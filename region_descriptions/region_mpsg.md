# MPSG Attributes

## Attribute List

### `COVERTYPE`
- Cover type from selected MPSG algorithm (`MPSGcovTyp` 1/2/3).

### `TREE_SIZE_CLASS`
- Size class (`s/p/m/l/v`, or `h` for open stands) from BA-weighted DBH bins.

### `CROWN_CLASS`
- Crown class (`n/o/m/c`) from stand canopy cover thresholds.

### `VERTICAL_STRUCTURE`
- Vertical story class from `baStory()` with MPSG-specific remap rules.

### `HSS1_4C`
- Optional habitat stage output from Region 2 HSS logic.

### `HSS1_5`
- Optional extended habitat stage output from Region 2 HSS logic.

## Notes

- MPSG also includes all attributes in `core_attributes.md`.
- `HSS1_4C` and `HSS1_5` appear when the `Add HSS` option is enabled.
