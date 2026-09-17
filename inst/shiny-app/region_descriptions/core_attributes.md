# Core Attributes

These attributes are produced for all regions.

## Attribute List

### `CAN_COV`
- Corrected stand canopy cover (%) after overlap correction.

### `BA`
- Total live tree basal area per acre (ft^2/ac).

### `TPA`
- Total tree density per acre from expansion factors.

### `QMD`
- Diameter representing mean tree size weighted by basal area.

### `ZSDI`
- Stand density index using Zeide's equation.

### `RSDI`
- Reineke SDI from stand TPA and QMD.

### `BA_WT_DIA`
- DBH weighted by tree basal area contribution.

### `BA_WT_HT`
- Height weighted by tree basal area contribution.

### `BA_STM`
- Basal area with only trees `DBH >= 1`.

### `TPA_STM`
- Tree density with only trees `DBH >= 1`.

### `QMD_STM`
- QMD with only trees `DBH >= 1`.

### `ZSDI_STM`
- Zeide SDI with only trees `DBH >= 1`.

### `RSDI_STM`
- Reineke SDI with only trees `DBH >= 1`.

### `QMD_TOP20`
- QMD of dominant larger trees (top 20% TPA rule).

## Optional Cross-Module Attributes

### `HSS1_4C`, `HSS1_5`
- Region 2 / MPSG habitat structure stages when `Add HSS` is enabled.

### `CUSTOM_*`
- User-defined variables from the Custom Vars workbook.

### Compute, PotFire, Fuels, Carbon Fields
- Additional fields pulled from selected FVS module tables.
