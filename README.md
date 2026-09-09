# VegClass2.0
This package provides a suite of functions that are used to derive vegetation classifications from Forest Vegetation Simulator output. 

## Overview
VegClass2.0 is an R package for converting Forest Vegetation Simulator (FVS) output into stand-level vegetation classification and summary attributes. It reads FVS database output, processes each stand or case, and calculates a standard set of vegetation metrics such as basal area, trees per acre, quadratic mean diameter, canopy cover, and size/density measures.

The package supports multiple regional rule sets, including Regions 1, 2, 3, and 8, as well as MPSG workflows. It can also add optional outputs such as HSS, compute variables, potential fire metrics, fuels, carbon, and volume measures when the required database tables are available.

VegClass2.0 is designed to run on single databases or in parallel across many stands, producing a combined CSV output that can be used for reporting, analysis, or downstream processing. Custom variables and custom output scripts can also be included to support project-specific workflows.

## High-level workflow

```mermaid
flowchart TB
	A[Input DB\nand run filters] --> ACASE[FVS_Cases columns used:\nRunTitle, CaseID, StandID,\nVariant, Groups, Stand_CN]
	A --> ATBL[FVS_TreeList columns used:\nCaseID, StandID, Year, SpeciesPLANTS, TPA, MortPA, DBH, Ht, CrWidth,\nWest variants: TCuFt, MCuFt, BdFt,\nEast variants: MCuFt, SCuFt, SBdFt]
	ACASE --> B[Run main processing]
	ATBL --> B
	B --> C[Compute core stand metrics]
	C --> COREO[Core outputs:\nCAN_COV, BA, TPA,\nQMD, ZSDI, RSDI,\nBA_WT_DIA, BA_WT_HT,\nBA_STM, TPA_STM, QMD_STM,\nZSDI_STM, RSDI_STM,\nQMD_TOP20]
	C --> D{Region selection}
	D --> R1O[Region 1 outputs:\nDOM6040, COVERTYPE_R1, VEGTYPE,\nVERTICAL_STRUCTURE, SIZECLASS_NTG, STRCLSSTR]
	D --> R2O[Region 2 outputs:\nDOM_TYPE_R2,\nDOM_TYPE_R2_CC1, DOM_TYPE_R2_CC2, DOM_TYPE_R2_CC3,\nCOVERTYPE_R2,\nTREE_SIZE_CLASS_R2, CROWN_CLASS_R2]
	D --> R3O[Region 3 outputs:\nDOM_TYPE, DCC1, XDCC1\nDCC2, XDCC2,\nCAN_SIZCL, CAN_SZTMB,\nCAN_SZWDL, BA_STORY]
	D --> R8O[Region 8 outputs:\nSSDOMSPP, NMDOMSPP,\nPWDOMSPP, STDOMSPP, DOMTYPE,\nSSSIZE, SSTPA, NMBA, NMSIZE,\nPWBA, PWSIZE, STBA, STSIZE,\nVEGCLASS]
	D --> MPO[MPSG outputs:\nCOVERTYPE, TREE_SIZE_CLASS,\nCROWN_CLASS, VERTICAL_STRUCTURE,\nHSS1_4C, HSS1_5,\nwhen add HSS = TRUE]
	D --> CUO[CUSTOM outputs:\ncustom vars and\ncustom script columns]
	B --> OPT{Optional modules}
	OPT --> OALL[Optional module outputs:\naddCompute: FVS_Compute\naddPotFire: FVS_PotFire\naddFuels: FVS_Fuels\naddCarbon: FVS_Carbon\naddVolume: VOL1, VOL2, VOL3\nDEADVOL\nadd HSS: HSS1_4C, HSS1_5]
	COREO --> M[Merge columns]
	R1O --> M[Merge columns]
	R2O --> M
	R3O --> M
	R8O --> M
	MPO --> M
	CUO --> M
	OALL --> M
	M --> X[Exclude selected\nattributes]
	X --> Y[Write final CSV]
	Y --> Z[Preview table\nand build plots]

	classDef core fill:#E8F1FF,stroke:#2B5CAA,stroke-width:1.4px,color:#0E2A52
	classDef region fill:#EAF9F0,stroke:#1D7D46,stroke-width:1.2px,color:#0B3E24
	classDef out fill:#FFF9D6,stroke:#8A7A00,stroke-width:1.2px,color:#4A4300
	classDef opt fill:#F0E8FF,stroke:#6D3FB0,stroke-width:1.2px,color:#391A66
	classDef src fill:#E7F7FF,stroke:#1E6A8D,stroke-width:1.2px,color:#08364D
	class A,ACASE,ATBL src
	class B,C,D,M,X,Y,Z core
	class R1O,R2O,R3O,R8O,MPO,CUO,COREO out
	class OPT,OALL opt
```