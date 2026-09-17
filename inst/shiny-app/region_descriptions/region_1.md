# Region 1 Attributes

## Attribute List

### `DOM6040`
- Dominance class from species composition using 60/40 mixed-stand rules.

### `COVERTYPE_R1`
- Cover type class mapped from `DOM6040` plus PV-code override logic.

### `VEGTYPE`
- Concatenated potential vegetation type and cover type for Region 1.

### `VERTICAL_STRUCTURE`
- Number/shape of canopy layers from BA distribution by diameter class.

### `SIZECLASS_NTG`
- Size class bin based on BA-weighted stand diameter.

### `STRCLSSTR`
- Combined size-density stratum class based on canopy cover and stand size.

## Notes

- Region 1 also includes all attributes in `core_attributes.md`.
- `VEGTYPE` requires inventory DB linkage (`InvDB`, `InvStandTbl`) to derive `PV_CODE`.
