# Attribute Logic Dictionary
Include output logic 
Code references are to the implementation in `R/`.

## Abbreviation Legend (Used in Formulas and Logic)
- `BA`: Basal Area.
- `DBH`: Diameter at Breast Height.
- `QMD`: Quadratic Mean Diameter.
- `TPA`: Trees Per Acre.
- `TREEBA`: Per-tree Basal Area contribution.
- `TREECC`: Per-tree Canopy Cover contribution (uncorrected).
- `UNCC`: Uncorrected stand canopy cover.
- `CC`: Corrected canopy cover.
- `ZSDI`: Zeide Stand Density Index.
- `RSDI`: Reineke Stand Density Index.
- `BAWTD`: Basal-Area-Weighted Diameter numerator term (`DBH * TREEBA`).
- `BAWTH`: Basal-Area-Weighted Height numerator term (`Ht * TREEBA`).
- `DBHSQ`: Sum of `DBH^2 * TPA` contributions.
- `TPASUM`: Accumulated TPA used in partial-pool QMD calculations.
- `TPA20`: Target trees-per-acre amount for the top-20-percent QMD calculation (`max(0.2 * standTPA, 20)`).
- `STCC`: Stand canopy cover used in Region 2 and MPSG logic.
- `SP1`, `SP2`, `SP3`: First, second, and third ranked dominant species codes.
- `DCC1`, `DCC2`: First and second dominant canopy components.
- `XDCC1`, `XDCC2`: Corrected canopy cover percentages for dominant canopy components.
- `DOMTYPE` / `DOM_TYPE`: Dominance type code.
- `BARAT`: Basal-area ratio term (`BA16 / BA9`) in HSS 1-5 logic.
- `TPAMAX`: Maximum reference trees-per-acre estimate used in HSS 1-5.
- `BAMAX`: Maximum reference basal area estimate used in HSS 1-5.
- `BAMIN`: Minimum basal area threshold used in HSS 1-5.
- `TSC`: Tree Size Class internal stage variable in HSS 1-5.
- `CHESS`: Intermediate habitat structure stage code before final translation.
- `CHESStrans`: Translation lookup from CHESS code to final HSS label.
- `OGSC`: Old Growth Structural Condition score.
- `PVT`: Potential Vegetation Type.
- `PV_CODE`: Potential vegetation code read from inventory.

## Core Attributes (All Regions)
# Important Note #
As of August 2026, no attributes are directly from an FVS output table, they are all calculated using metrics from the FVS_TreeList table and aggregated to stand level metrics / year in vegClass. 

### CAN_COV (Percent Canopy Cover (Corrected))
- Source: `R/vegOut.r` -> `plotAttr()` in `R/plotAttr.r` -> `correctCC()` in `R/plotAttr.r`.
- Units: percent canopy cover (`%`).
- Steps:
  1. For each tree, uncorrected canopy contribution: `TREECC = pi * (CrWidth/2)^2 * (TPA/43560) * 100`.
  2. Sum to stand-level `UNCC`.
  3. Overlap-correct canopy: `CC = 100 * (1 - exp(-0.01 * UNCC))`.

### BA (Basal Area)
- Source: `plotAttr()`.
- Units: basal area per acre (`ft^2/ac`).
- Steps:
  1. For each tree: `TREEBA = DBH^2 * TPA * 0.005454154`.
  2. Sum `TREEBA` to stand BA.

### TPA (Trees Per Acre)
- Source: `plotAttr()`.
- Units: trees per acre (`trees/ac`).
- Steps: sum expansion factors (`TPA`) for included records.

### QMD (Quadratic Mean Diameter)
- Source: `plotAttr()`.
- Units: diameter (`in`, same units as input `DBH`).
- Steps:
  1. Accumulate `sum(DBH^2 * TPA)`.
  2. Divide by stand TPA and square-root.

### ZSDI (Zeide Stand Density Index)
- Source: `plotAttr()`.
- Units: stand density index (`SDI`, index units).
- Formula: `sum(TPA * (DBH/10)^1.605)`.

### RSDI (Reineke Stand Density Index)
- Source: `plotAttr()`.
- Units: stand density index (`SDI`, index units).
- Formula: `TPA * (QMD/10)^1.605`.

### BA_WT_DIA (Basal-Area-Weighted Diameter)
- Source: `plotAttr()`.
- Units: diameter (`in`, same units as input `DBH`).
- Formula: `sum(DBH * TREEBA) / sum(TREEBA)`.

### BA_WT_HT (Basal-Area-Weighted Height)
- Source: `plotAttr()`.
- Units: height (same units as input `Ht`, typically `ft`).
- Formula: `sum(Ht * TREEBA) / sum(TREEBA)`.

### Stem-only Metrics (`BA_STM`, `TPA_STM`, `QMD_STM`, `ZSDI_STM`, `RSDI_STM`) (Stem-only Stand Metrics)
- Source: `vegOut()` calls `plotAttr(min=1)`.
- Units:
  1. `BA_STM`: `ft^2/ac`.
  2. `TPA_STM`: `trees/ac`.
  3. `QMD_STM`: `in` (same units as `DBH`).
  4. `ZSDI_STM`: `SDI` index units.
  5. `RSDI_STM`: `SDI` index units.
- Rule: only records with `DBH >= 1` are included.

### QMD_TOP20 (Quadratic Mean Diameter of Largest Trees, Top-20% Rule)
- Source: `qmdTop20()` in `R/qmdTop20.r`.
- Units: diameter (`in`, same units as input `DBH`).
- Steps:
  1. Sort trees descending by DBH.
  2. Determine target accumulation (`TPA20`, the top-20-percent target trees per acre): `TPA20 = max(0.2 * standTPA, 20)`.
  3. DBH lower bound is `0.2` when stand CC >= 10, else `0.1`.
  4. Accumulate DBH^2 * TPA until `TPA20` is met (partial final tree contribution allowed).
  5. Return `sqrt(DBHSQ / TPASUM)`.

## Region 1 Attributes

### DOM6040 (Dominance 60/40 Class)
- Source: `R1()` in `R/R1.r` -> `computeDominance6040()` in `R/DOM6040.r`.
- Units: N/A (categorical dominance class code).
- Branch logic:
  1. If `BA < 20` and `TPA > 100`, use TPA proportions.
  2. Else if `BA > 20`, use BA proportions.
  3. Else return `NONE`.
- Dominance rules in `computeDominance6040()`:
  1. Sort species descending by `prop1`; if tied, prefer larger `height1`; if still tied, prefer larger `diam1`.
  2. Compute single-species dominance:
    - If `prop1[1] >= 0.6`, map top species through `MapSpecies` and return that class.
  3. If no 60% dominant species, compute subgroup sums across all species:
    - `TmixProp = sum(prop1 where subclass map is TMIX)`
    - `ImixProp = sum(prop1 where subclass map is IMIX)`
    - `HmixProp = sum(prop1 where subclass map is HMIX)`
    - `TotalMixProp = TmixProp + ImixProp + HmixProp`
  4. Select mixed subclass (`str`) in order:
    - If `HmixProp / TotalMixProp >= 0.4`, set `str = HMIX`.
    - If `round((HmixProp + ImixProp) / TotalMixProp, 1) >= 0.5`, set `str = IMIX`.
    - Else set `str = TMIX`.
  5. Decide whether to prepend a species code:
    - If `prop1[1] >= 0.4`, return `<mapped top species>-<str>`.
    - Else return `str` only.
  6. Any unmapped/invalid outcome is forced to `NONE`.
 - Compact pseudocode:
  1. `if topProp >= 0.6 -> DOM6040 = speciesMap(topSpecies)`
  2. `else str = mixSubclass(TmixProp, ImixProp, HmixProp)`
  3. `if topProp >= 0.4 -> DOM6040 = speciesMap(topSpecies) + '-' + str else DOM6040 = str`

### COVERTYPE_R1 (Region 1 Cover Type)
- Source: `R1()`.
- Units: N/A (categorical cover type code/label).
- Steps:
  1. Map `DOM6040` through `MapDominance6040toCovertype`.
  2. Special override: if mapped to `mixedmesiccon` and stand `PV_CODE` is in configured hot/warm dry lists, remap to `dryDouglasfir`.

### VEGTYPE (Vegetation Type, R1 PVT-Cover)
- Source: `R1()`.
- Units: N/A (categorical `PVT-COVABBR` string).
- Steps:
  1. Pull `PV_CODE` from inventory DB (`InvDB`, `InvStandTbl`).
  2. Convert `PV_CODE` to PVT class via `MapADPtoStSimPVT`.
  3. Abbreviate cover type, then concatenate as `PVT-COVABBR`.

### VERTICAL_STRUCTURE (R1 Number of Canopy Layers)
- Source: `R1()` + `computeVerticalStructure()`.
- Units: N/A (structural class code: `NONE`, `1`, `2`, `3`, or `C`).
- Branch logic:
  1. `BA < 20` and `TPA < 100` -> `NONE`.
  2. `BA < 20` and `TPA >= 100` -> `1`.
  3. Else evaluate BA proportions by diameter classes.
- `computeVerticalStructure()` details:
  1. Checks two-peak conditions across separated diameter classes (>=2% BA and dominance ratios).
  2. If not multi-peak, evaluates alternate three-bin pattern.
  3. Returns `1/2/3` when limited layers are detected, otherwise `C` for continuous.

### SIZECLASS_NTG (Basal Area Weighted Diameter Size Class)
- Source: `R1()`.
- Units: N/A (categorical size class bin; bin thresholds are in inches).
- Input: stand BA weighted DBH.
- Bins:
  - `<=0.1` -> `Seedling`
  - `(0.1,5)` -> `00.1-04.9`
  - `[5,10)` -> `05.0-09.9`
  - `[10,15)` -> `10.0-14.9`
  - `[15,20)` -> `15.0-19.9`
  - `[20,25)` -> `20.0-24.9`
  - `>=25` -> `25.0+`

### STRCLSSTR (Size and Density Structure Class Strata)
- Source: `R1()`.
- Units: N/A (categorical strata letter code).
- Inputs: `StBAWTDBH`, stand CC.
- Rule set:
  1. If `CC < 10` -> `X`.
  2. Else assign strata letter by size bin and CC class:
     - low CC: `10-<40`
     - medium CC: `40-<60`
     - high CC: `>=60`
  3. Result letters span `E` through `Q`.

## Region 2 Attributes

### DOM_TYPE_R2 and DOM_TYPE_R2_CC1/2/3 (R2 Dominant Species Type and Ranked Canopy Shares)
- Source: `R2()` in `R/R2.r`.
- Units:
  1. `DOM_TYPE_R2`: N/A (categorical species-composition code).
  2. `DOM_TYPE_R2_CC1`: percent canopy cover (`%`).
  3. `DOM_TYPE_R2_CC2`: percent canopy cover (`%`).
  4. `DOM_TYPE_R2_CC3`: percent canopy cover (`%`).
- Steps:
  1. Compute stand CC (`STCC`).
  2. If `STCC < 10`: set `DOM_TYPE_R2` and CC1/2/3 to `NONE` and force open-stand defaults.
  3. Else compute species-level corrected canopy shares from `plotvals`.
  4. Sort descending and populate top 1-3 ranks.
 - Explicit ranking details:
   1. Build `SpeciesCC[species] = plotvals[[species]]["CC"]` for every species except `ALL`.
   2. Sort `SpeciesCC` decreasing.
   3. Assign:
      - 3+ species: `DOM_TYPE_R2 = SP1:SP2:SP3`, `CC1=SpeciesCC[1]`, `CC2=SpeciesCC[2]`, `CC3=SpeciesCC[3]`.
      - 2 species: `DOM_TYPE_R2 = SP1:SP2`, `CC3=0`.
      - 1 species: `DOM_TYPE_R2 = SP1`, `CC2=0`, `CC3=0`.

### COVERTYPE_R2 (Region 2 Cover Type)
- Source: `R2()`.
- Units: N/A (categorical cover type code).
- Steps:
  1. Base assignment from top species (`SP1`) using SAF/SRM map rules.
  2. White-fir exception where second/third Douglas-fir can force Douglas-fir type.
  3. Grouped override checks:
     - spruce/fir grouped canopy dominates all top-3 shares -> `T206`.
     - pinyon/juniper grouped canopy dominates -> `T239`.
     - Douglas-fir group dominates -> `T210`.
  4. Special fallback for codes `2TB` and `2TN` -> `T999`.
  5. Crosswalk SAF/SRM-like code to final `COVERTYPE_R2` via `saf2R2` map.
   - Explicit override calculations:
    1. Compute grouped canopy totals from top-three ranks:
      - `totsf = sum(CCi for SPi in {PIEN, ABLA, ABLAA, ABAR2, ABBI2})`
      - `totpj = sum(CCi for SPi in {PIED, JUSC2, SAUT3, JUNIP, JUOS, JUMO})`
      - `totdf = sum(CCi for SPi in {PSME, PSMEG, ABCO})`
    2. Apply dominance override condition separately to each group:
      - override when grouped total is greater than each of `CC1`, `CC2`, `CC3`, and `> 0`.
    3. Override precedence follows code order:
      - spruce-fir check, then pinyon-juniper check, then Douglas-fir check, then 2TB/2TN fallback.

### TREE_SIZE_CLASS_R2 (Region 2 Tree Size Class)
- Source: computed as `TREE_SIZE_CLASS` in `R2()` then consumed by `vegOut()`.
- Units: N/A (categorical size class code).
- Steps:
  1. Build canopy bins by DBH: `[0,1), [1,5), [5,9), [9,16), [16,+)`.
  2. Compare grouped canopy totals:
     - `ES = E + S`
     - `M`
     - `LV = L + V`
  3. Choose dominant group and tie-break:
     - `LV` resolved to `V` if `V >= L`, else `L`.
     - `ES` resolved to `S` if `S >= E`, else `E`.
  4. If unresolved and `STCC < 10`, class becomes `n`.

### CROWN_CLASS_R2 (Region 2 Crown Cover Class)
- Source: computed as `CROWN_CLASS` in `R2()` then consumed by `vegOut()`.
- Units: N/A (ordinal crown-cover class code).
- Thresholds:
  - `10 <= STCC < 40` -> `1`
  - `40 <= STCC < 70` -> `2`
  - `STCC >= 70` -> `3`
  - open path sets `0`.

### HSS1_4C (Habitat Structure Stage, 1-4C)
- Source: `HSS(HSStype=1)` in `R/HSS.r`.
- Units: N/A (categorical habitat stage code).
- Steps:
  1. If `STCC < 10`, set group size class `N`.
  2. Else build diameter canopy bins (`E,S,M,L,V`) and grouped class (`ES`, `LV`) with tie-breaks.
  3. Map to stage:
     - `N -> 1`
     - `E -> 2`
     - `S` or `M` -> `3A/3B/3C` by STCC `<40`, `40-<70`, `>=70`
     - `L` or `V` -> `4A/4B/4C` using same cover bins.

### HSS1_5 (Habitat Structure Stage, 1-5)
- Source: `HSS(HSStype=2)` in `R/HSS.r`.
- Units: N/A (categorical habitat stage code).
- Computation style: translated `hss_wi5.kcp` phase/state machine.
- Major internals:
  1. Derives BA at multiple DBH thresholds (`BA1, BA5, BA9, BA16, BA125, BA529`) and QMD from repeated `plotAttr()` calls.
  2. Computes intermediate values (`BARAT`, `TPAMAX`, `BAMIN`, `TSC`, `PHS_H`).
  3. Determines structural phase and canopy subclass (CHESS code).
  4. Mature-phase branch can evaluate OGSC score (`A..L`) with species-specific checks for `PIPO`, `PIGL`, `POTR5`.
  5. Translates CHESS to stage: `10->1`, `20->2`, `31->3A`, `32->3B`, `33->3C`, `41->4A`, `42->4B`, `43->4C`, `50->5`.
 - Explicit calculation sequence (simplified in code order):
  1. Compute stand terms:
    - `BARAT = BA16 / BA9` (if `BA9 > 0`)
    - `TPAMAX = 18641 / (QMD^1.659925)`
    - `BAMAX = 101.67 * (QMD^0.34007)`
    - `BAMIN = max(20, 0.10 * BAMAX)`; if `STCC > 10` and `BAMIN >= BA5`, set `BAMIN = BA5`.
  2. Assign tree size class `TSC` in priority order:
    - `6` if large-tree conditions and `BARAT > 0.50`
    - `5` if large-tree conditions and `BARAT <= 0.50`
    - `4` if `BA9 < BA529` under mature-BA gate
    - `3` if `BA5 >= BAMIN` and `BA5 < BA125`
    - `2` if `BA5 < BAMIN` and `TPA >= 300`
    - `1` if `STCC < 10`
  3. Map `TSC` to base CHESS stage family:
    - `TSC in {5,6}` -> stage 4 with cover subclass by STCC (`<40 => 41`, `40-<70 => 42`, `>=70 => 43`)
    - `TSC in {3,4}` -> stage 3 with cover subclass (`31/32/33`)
    - `TSC==2` -> `20`
    - Non-stocked condition -> `10`
  4. If stage family is 4, run old-growth scoring (`OGSC`):
    - Evaluate components `A..L` from species presence, BA-by-size distributions, canopy subclass terms, and stocking terms.
    - `OGSC = A+B+C+D+E+F2+G+H+I+J+K+L`.
    - If `OGSC >= 42`, force CHESS `50` (stage 5).
  5. Final stage is `CHESStrans[CHESS]`.

## Region 3 Attributes

### DOM_TYPE, DCC1, XDCC1, DCC2, XDCC2 (R3 Dominance Type and Dominant Canopy Components)
- Source: `domTypeR3()` in `R/domType.r`.
- Units:
  1. `DOM_TYPE`: N/A (categorical dominance code).
  2. `DCC1`: N/A (categorical dominant component code).
  3. `XDCC1`: percent canopy cover (`%`, corrected).
  4. `DCC2`: N/A (categorical secondary component code).
  5. `XDCC2`: percent canopy cover (`%`, corrected).
- How categories are assigned (decision order):
  1. If missing `TREECC`, compute per-tree uncorrected canopy:
    - `TREECC = pi * (CrWidth/2)^2 * (TPA/43560) * 100`.
  2. Build canopy totals from `TREECC` for:
    - each species,
    - each genus,
    - leaf retention groups (`EVERGREEN`, `DECIDUOUS`),
    - shade tolerance groups (`INT`, `TOL`).
  3. Run the first matching rule below (later rules are skipped once one matches):
    - Open/non-vegetated: if `correctCC(CC) < 10` and `TPA < 100` -> `DOM_TYPE = NVG`.
    - Single-species dominance: if top species canopy `>= 60%` of stand canopy -> `DOM_TYPE = <species>`.
    - Two-species dominance: if top two species are each `>= 20%` and together `>= 80%` -> `DOM_TYPE = <sp1>_<sp2>`.
    - Single-genus dominance: if top genus canopy `>= 60%` -> `DOM_TYPE = <genus>`.
    - Species+genus dominance: if top species and a mutually exclusive genus are each `>= 20%` and together `>= 80%` -> `DOM_TYPE = <name1>_<name2>`.
    - Two-genus dominance: if top two genera are each `>= 20%` and together `>= 80%` -> `DOM_TYPE = <gen1>_<gen2>`.
    - Fallback mixed classes:
      - `TDMX` if deciduous canopy `> 75%` of stand canopy.
      - `TEDX` if neither evergreen nor deciduous exceeds `75%`.
      - `TETX` if evergreen canopy dominates and `TOL > INT`.
      - `TEIX` if evergreen canopy dominates and `INT >= TOL`.
  4. `DCC1`/`DCC2` are the first and second components from the winning rule.
  5. `XDCC1`/`XDCC2` are component canopy values passed through `correctCC()` before output.

### CAN_SIZCL, CAN_SZTMB, CAN_SZWDL (Canopy Size Classes: General, Timberland, Woodland)
- Source: `canSizCl(type=1/2/3)` in `R/canSizCl.r`.
- Units: N/A (categorical canopy-size class codes; class logic uses DBH bins in inches).
- Shared assignment steps:
  1. If `CC < 10` and `TPA < 100`, output class `0`.
  2. If `CC < 10` and `TPA >= 100`, output class `1`.
  3. Otherwise, assign each tree to a DBH class, sum `TREECC` by class, and choose the class with maximum canopy.
- Bin definitions and category outputs:
  1. `CAN_SIZCL` (`type = 1`, midscale bins)
    - class `1`: `0 <= DBH < 5`
    - class `2`: `5 <= DBH < 10`
    - class `3`: `10 <= DBH < 20`
    - class `4`: `20 <= DBH < 30`
    - class `5`: `DBH >= 30`
  2. `CAN_SZTMB` (`type = 2`, timberland bins)
    - class `1`: `0 <= DBH < 5`
    - class `2`: `5 <= DBH < 10`
    - class `3`: `10 <= DBH < 20`
    - class `4`: `DBH >= 20`
    - adjustment: if initial winner is class `2` and `sum(class 3:5) >= class 2`, re-pick from larger classes.
    - adjustment: if initial winner is class `1` and `sum(class 2:5) >= class 1`, re-pick from larger classes.
  3. `CAN_SZWDL` (`type = 3`, woodland bins)
    - class `1`: `0 <= DBH < 5`
    - class `2`: `5 <= DBH < 10`
    - class `3`: `DBH >= 10`
    - adjustment: if initial winner is class `1` and `sum(class 2:5) >= class 1`, re-pick from larger classes.

### BA_STORY (Basal Area Story Class)
- Source: `baStory()` in `R/baStory.r`.
- Units: N/A (ordinal story class code `0`, `1`, `2`, or `3`; thresholds use BA proportions).
- How story class is assigned:
  1. Open-stand checks:
    - if `CC < 10` and `TPA < 100` -> `BA_STORY = 0`.
    - if `CC < 10` and `TPA >= 100` -> `BA_STORY = 1`.
  2. Otherwise initialize as `3` (multi-story), then test for one-story dominance:
    - if BA from trees `DBH >= 24` is `>= 70%` of total BA, set `BA_STORY = 1`.
  3. If still unresolved, use sliding DBH windows:
    - evaluate windows `[0,8)`, `[1,9)`, `[2,10)`, ... , `[23,31)`.
    - compute `window_BA / total_BA` for each window.
    - if any window share is `>= 70%`, set `BA_STORY = 1` and stop.
    - else if any window share is `>= 60%` and `< 70%`, set `BA_STORY = 2`.
    - else keep `BA_STORY = 3`.

## Region 8 Attributes (Custom North Carolina Classification)
## Not official USFS R8 Classification

### Region 8 Attribute Name Crosswalk
- `SSDOMSPP`: Advanced Regeneration Dominant Species Code.
- `NMDOMSPP`: Non-Merchantable Pool Dominant Species Code.
- `PWDOMSPP`: Pulpwood Pool Dominant Species Code.
- `STDOMSPP`: Sawtimber Pool Dominant Species Code.
- `DOMTYPE`: Stand-Level Dominant Species Code.
- `SSTPA`: Advanced Regeneration Trees Per Acre.
- `SSSIZE`: Advanced Regeneration TPA-Weighted Mean Height.
- `NMBA`: Non-Merchantable Basal Area.
- `NMSIZE`: Non-Merchantable Basal-Area-Weighted Mean Diameter.
- `PWBA`: Pulpwood Basal Area.
- `PWSIZE`: Pulpwood Basal-Area-Weighted Mean Diameter.
- `STBA`: Sawtimber Basal Area.
- `STSIZE`: Sawtimber Basal-Area-Weighted Mean Diameter.
- `VEGCLASS`: Region 8 Size-Density Vegetation Class Code.

### Shared R8 Tree Pool Assignment Logic
- Source: Region 8 branch in `plotAttr()` (`R/plotAttr.r`).
- Tree records are assigned to one pool before pool metrics are summarized:
  1. Advanced regeneration pool: `DBH < 1.5`.
  2. Non-merchantable pool: `DBH >= 1.5` and `vol1 <= 0`.
  3. Pulpwood pool: `vol1 > 0` and `vol2 <= 0`.
  4. Sawtimber pool: `vol3 > 0`.
- Per-tree terms used by all pools:
  1. `TREEBA = DBH^2 * TPA * 0.005454154`.
  2. `BAWTD = DBH * TREEBA`.
  3. Advanced regeneration height contribution: `Ht * TPA`.

### SSDOMSPP (Advanced Regeneration Dominant Species Code)
- Source: `domTypeR8()` in `R/domType.r`.
- Units: N/A (categorical species-composition code).
- Full name meaning: species composition code for the advanced regeneration pool, based on `SSTPA`.
- Logic:
  1. Build species vector from `attrList[[species]]["SSTPA"]`.
  2. Sort descending by `SSTPA`.
  3. If stand-level `SSTPA <= 0`, set to `NONE`.
  4. If top species contributes at least 70% of `SSTPA`, return one-species code.
  5. Else if top two species sum to at least 70%, return `sp1-sp2`.
  6. Else return `sp1-sp2-sp3`.

### NMDOMSPP (Non-Merchantable Pool Dominant Species Code)
- Source: `domTypeR8()` in `R/domType.r`.
- Units: N/A (categorical species-composition code).
- Full name meaning: species composition code for the non-merchantable pool, based on `NMBA`.
- Logic:
  1. Build species vector from `attrList[[species]]["NMBA"]`.
  2. Sort descending by `NMBA`.
  3. If stand-level `NMBA <= 0`, set to `NONE`.
  4. If top species contributes at least 70% of `NMBA`, return one-species code.
  5. Else if top two species sum to at least 70%, return `sp1-sp2`.
  6. Else return `sp1-sp2-sp3`.

### PWDOMSPP (Pulpwood Pool Dominant Species Code)
- Source: `domTypeR8()` in `R/domType.r`.
- Units: N/A (categorical species-composition code).
- Full name meaning: species composition code for the pulpwood pool, based on `PWBA`.
- Logic:
  1. Build species vector from `attrList[[species]]["PWBA"]`.
  2. Sort descending by `PWBA`.
  3. If stand-level `PWBA <= 0`, set to `NONE`.
  4. If top species contributes at least 70% of `PWBA`, return one-species code.
  5. Else if top two species sum to at least 70%, return `sp1-sp2`.
  6. Else return `sp1-sp2-sp3`.

### STDOMSPP (Sawtimber Pool Dominant Species Code)
- Source: `domTypeR8()` in `R/domType.r`.
- Units: N/A (categorical species-composition code).
- Full name meaning: species composition code for the sawtimber pool, based on `STBA`.
- Logic:
  1. Build species vector from `attrList[[species]]["STBA"]`.
  2. Sort descending by `STBA`.
  3. If stand-level `STBA <= 0`, set to `NONE`.
  4. If top species contributes at least 70% of `STBA`, return one-species code.
  5. Else if top two species sum to at least 70%, return `sp1-sp2`.
  6. Else return `sp1-sp2-sp3`.

### DOMTYPE (Stand-Level Dominant Species Code)
- Source: `domTypeR8()` in `R/domType.r`.
- Units: N/A (categorical species-composition code).
- Full name meaning: species composition code for the full stand, based on total stand `BA`.
- Logic:
  1. Build species vector from `attrList[[species]]["BA"]`.
  2. Sort descending by stand `BA` contribution.
  3. If top species contributes at least 70% of stand `BA`, return one-species code.
  4. Else if top two species sum to at least 70%, return `sp1-sp2`.
  5. Else return `sp1-sp2-sp3`.

### SSTPA (Advanced Regeneration Trees Per Acre)
- Source: Region 8 branch in `plotAttr()`.
- Units: trees per acre (`trees/ac`).
- Full name meaning: total tree density in the advanced regeneration pool.
- Logic:
  1. For each tree with `DBH < 1.5`, add tree expansion factor `TPA` to `SSTPA`.
  2. Stand `SSTPA` is the sum across all advanced regeneration trees.

### SSSIZE (Advanced Regeneration TPA-Weighted Mean Height)
- Source: Region 8 branch in `plotAttr()`.
- Units: height (same units as input `Ht`, typically `ft`).
- Full name meaning: mean tree height for advanced regeneration, weighted by tree expansion factor.
- Logic:
  1. For each tree with `DBH < 1.5`, accumulate `Ht * TPA`.
  2. Divide accumulated value by `SSTPA` when `SSTPA > 0`.
  3. If `SSTPA == 0`, value remains at initialized default.

### NMBA (Non-Merchantable Basal Area)
- Source: Region 8 branch in `plotAttr()`.
- Units: basal area per acre (`ft^2/ac`).
- Full name meaning: stand basal area in the non-merchantable pool.
- Logic:
  1. Non-merchantable condition: `DBH >= 1.5` and `vol1 <= 0`.
  2. Sum `TREEBA` for trees meeting that condition.

### NMSIZE (Non-Merchantable Basal-Area-Weighted Mean Diameter)
- Source: Region 8 branch in `plotAttr()`.
- Units: diameter (`in`, same units as input `DBH`).
- Full name meaning: basal-area-weighted DBH for non-merchantable trees.
- Logic:
  1. For non-merchantable trees, accumulate `BAWTD = DBH * TREEBA`.
  2. Finalize with `NMSIZE = sum(BAWTD) / NMBA` when `NMBA > 0`.

### PWBA (Pulpwood Basal Area)
- Source: Region 8 branch in `plotAttr()`.
- Units: basal area per acre (`ft^2/ac`).
- Full name meaning: stand basal area in the pulpwood pool.
- Logic:
  1. Pulpwood condition: `vol1 > 0` and `vol2 <= 0`.
  2. Sum `TREEBA` for trees meeting that condition.

### PWSIZE (Pulpwood Basal-Area-Weighted Mean Diameter)
- Source: Region 8 branch in `plotAttr()`.
- Units: diameter (`in`, same units as input `DBH`).
- Full name meaning: basal-area-weighted DBH for pulpwood trees.
- Logic:
  1. For pulpwood trees, accumulate `BAWTD = DBH * TREEBA`.
  2. Finalize with `PWSIZE = sum(BAWTD) / PWBA` when `PWBA > 0`.

### STBA (Sawtimber Basal Area)
- Source: Region 8 branch in `plotAttr()`.
- Units: basal area per acre (`ft^2/ac`).
- Full name meaning: stand basal area in the sawtimber pool.
- Logic:
  1. Sawtimber condition: `vol3 > 0`.
  2. Sum `TREEBA` for trees meeting that condition.

### STSIZE (Sawtimber Basal-Area-Weighted Mean Diameter)
- Source: Region 8 branch in `plotAttr()`.
- Units: diameter (`in`, same units as input `DBH`).
- Full name meaning: basal-area-weighted DBH for sawtimber trees.
- Logic:
  1. For sawtimber trees, accumulate `BAWTD = DBH * TREEBA`.
  2. Finalize with `STSIZE = sum(BAWTD) / STBA` when `STBA > 0`.

### VEGCLASS (Region 8 Size-Density Vegetation Class Code)
- Source: `denSizeR8()` in `R/denSize.r`.
- Units: N/A (categorical size-density code, for example `3C`).
- Full name meaning: two-part class code combining a size class (`1` to `4`) and a density class (`A` to `D`).
- Logic:
  1. If stand `BA < 10`, force size class `1` (advanced regeneration) and derive density from stand `TPA`:
     - `A`: `0 <= TPA < 200`
     - `B`: `200 <= TPA < 400`
     - `C`: `400 <= TPA < 600`
     - `D`: `TPA >= 600`
  2. If stand `BA >= 10`, choose size class by dominant BA pool:
     - `2` when `NMBA` is maximum.
     - `3` when `PWBA` is maximum.
     - `4` when `STBA` is maximum.
  3. For `BA >= 10`, derive density from stand `BA`:
     - `A`: `0 <= BA < 40`
     - `B`: `40 <= BA < 80`
     - `C`: `80 <= BA < 120`
     - `D`: `BA >= 120`
  4. Concatenate size and density: `VEGCLASS = paste0(sizeClass, densityClass)`.

## MPSG Attributes

### COVERTYPE (Cover Type Output)
- Source: `MPSG()` in `R/MPSG.r` (returns `COVERTYPE_MPSG`, assigned to `COVERTYPE` in `vegOut()`).
- Units: N/A (categorical cover type code/label).
- Steps:
  1. If `STCC < 10`, set `NONE`.
  2. Else branch by `MPSGcovTyp`:
     - `1`: call `R1()` and use `COVERTYPE_R1`.
     - `2`: call `R2()` then crosswalk R2 code to MPSG label.
     - `3`: call `domTypeR3()` and use `DOMTYPE`.

### TREE_SIZE_CLASS (Tree Size Class Output)
- Source: `MPSG()`.
- Units: N/A (categorical size class code).
- Rules:
  - if open-stand path (`STCC < 10`) then `h`.
  - else use stand BA weighted DBH bins: `<5 s`, `5-<10 p`, `10-<15 m`, `15-<20 l`, `>=20 v`.

### CROWN_CLASS (Crown Cover Class Output)
- Source: `MPSG()`.
- Units: N/A (categorical crown cover class code).
- Bins: `<10 n`, `10-<40 o`, `40-<60 m`, `>=60 c`.

### VERTICAL_STRUCTURE (Vertical Structure Output)
- Source: `MPSG()` -> `baStory()`.
- Units: N/A (categorical/ordinal structure class code).
- Post-processing in MPSG:
  - if NA -> `0`.
  - if not `NONE` and equals `3`, remap to `2`.

### HSS1_4C and HSS1_5 (Habitat Structure Stage Outputs)
- Source: optional in `MPSG()` when `addHSS=TRUE`.
- Units: N/A (categorical habitat stage codes).
- Logic: direct call to same `HSS()` implementations used by Region 2.

## Custom Attributes (`CUSTOM_*`) (User-Defined Custom Attributes)
- Source: `customAttr()` in `R/customAttr.r`.
- Units: varies by configured metric; examples include `trees/ac`, `ft^2/ac`, `SDI` index units, canopy percent (`%`), diameter (`in`), height (`ft` or input `Ht` units), and volume units from the selected source columns.
- Configuration source: Excel workbook (`Custom Variables` and `Species Groups` sheets).
- Per-row processing order:
  1. Filter by species or species-group label.
  2. Filter by DBH range (`MIN_DBH`, `MAX_DBH`).
  3. Filter by height range (`MIN_HT`, `MAX_HT`).
  4. Compute selected metric:
     - trees/acre, BA/acre, SDI, canopy %, QMD, BA weighted diameter, average height, total/merch/board-foot volume.


