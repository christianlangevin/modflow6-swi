<img src="./image/swi.png" alt="swi" style="width:500;height:100">

# modflow6-swi
Repository of materials used in the development of the sea water intrusion (SWI) package for MODFLOW 6 

The SWI Package for MODFLOW 6 is being developed on a [feature branch](https://github.com/christianlangevin/modflow6/tree/feat-swi).  Recent copies of the input instructions and executable for several different operating systems can be downloaded from a custom fork of the [modflow6 nightly build repository](https://github.com/christianlangevin/modflow6-nightly-build).

## Status (August 2026)

The SWI Package was reimplemented in August 2026 as an alternative flow formulation for the NPF and STO Packages.  The input files are the same as before, but the executable and the flopy classes must both be updated: executables built before August 21, 2026 and flopy classes generated from the earlier branch will not work with the current notebooks.  The previous implementation is preserved on the [backup/feat-swi-2026-08-20](https://github.com/christianlangevin/modflow6/tree/backup/feat-swi-2026-08-20) branch.

If you want to try the new SWI Package for MODFLOW 6, the following steps will get you started:

1.  Go to the custom fork of the modflow6 nightly build repository and view the [MODFLOW 6 SWI nightly build](https://github.com/christianlangevin/modflow6-nightly-build/actions/workflows/nightly-build-swi.yml).  Click on the most recent nightly build for SWI and download the proper artifact for your system.  Artifacts will be named according to operating system and will contain binary executables and documentation.  For Windows the artifact will be called `mf6.8.0.dev0_win64` or something similar.  Use the workflow artifacts rather than the Releases page of that repository; the releases are not kept current.

2. Regenerate the flopy classes to match the capabilities in the feature branch using the following command.  Note that running this command will change your flopy installation as described in the [instructions for generating classes](https://flopy.readthedocs.io/en/latest/md/generate_classes.html).

```
python -m flopy.mf6.utils.generate_classes --owner christianlangevin --ref feat-swi
```

3.  To run the notebooks in this repository, create the file `notebook/mf6exe.txt` containing a single line with the path to the downloaded `mf6` executable.  The file is not under version control.  The notebooks also require `numpy`, `matplotlib`, `pandas`, and `jupyter` in the same Python environment as the regenerated flopy.

4.  There are a collection of notebooks in this repository that may provide inspiration.  There are also tests that are being developed as part of the SWI implementation in MODFLOW 6.  The names of these tests start with `test_gwf_swi` and can be found in the [autotest folder of the feature branch](https://github.com/christianlangevin/modflow6/tree/feat-swi/autotest).

## Notebooks

The notebooks are in the `notebook` folder.  Each one reads the path to the SWI-capable `mf6` executable from `notebook/mf6exe.txt` (one line, not under version control) and writes its model files to `notebook/temp`.

| Notebook | Description |
|---|---|
| case1 | Steady state, one layer, freshwater only; confined, unconfined, and unconfined with Newton |
| case2 | Transient freshwater flow with a recharge period followed by no recharge; one and three layers, with a well |
| case3 | Steady coastal problem with constant sea level, unconfined |
| case4 | Same problem compared with MODFLOW-USG results, Newton formulation |
| case5 | Same problem compared with MODFLOW-USG results, without Newton |
| case6 | Cape Cod model from the SWI2 examples, converted from MODFLOW-2005 with mf5to6 |
| case7 | Ameland island case study |
| case8 | Two-fluid, two stress periods, one and two layers |
| case9 | Two-fluid and single-fluid vertical column with recharge and withdrawal |
| case10 | Rotating interface compared with the Keulegan solution (SHARP verification problem 1) |
| case11 | Retreating and intruding interface, Bear and Dagan Hele-Shaw experiments (SHARP verification problem 2) |
| case12 | Layered aquifer with a semiconfining layer, Mualem and Bear (SHARP verification problem 3) |
| case13 | Cape May cross section compared with SHARP using its published input and output (SHARP example 2) |

Cases 10 through 13 use the SHARP 1.1 distribution and figure panels in the `ref/sharp` and `data` folders.

## Usage notes

- The interface elevation is computed from the heads as `zeta = -alphaf * hf + alphas * hs`, where `alphaf = rhof / (rhos - rhof)` and `alphas = rhos / (rhos - rhof)`.  The densities are currently fixed at 1000 (freshwater) and 1025 (seawater), so `alphaf = 40` and `alphas = 41`.  In single-fluid mode `hs` is the saltwater head described below.
- Use the `ZETA FILEOUT` option of the SWI Package (`zeta_filerecord` in flopy) to save the interface elevation each time step, and read it back with `flopy.utils.HeadFile(fname, text="zeta")`.  The saved zeta is limited to the extent of each cell; the unclamped interface can always be recomputed from the heads with the formula above (see the notebooks for examples of both).
- The SWI Package requires the Newton formulation (`NEWTON` in the GWF name file) and specific yield in the STO Package.
- Single-fluid mode adds the SWI Package to one GWF model; the saltwater is static and the interface is computed from the freshwater head.  Two-fluid mode uses separate freshwater and saltwater GWF models coupled with a SWI-SWI exchange.
- In single-fluid mode the saltwater head (sea level) is zero unless it is set.  Use the `SALTWATER_HEAD` option of the SWI Package for a constant non-zero sea level, or a TVA file (`TVA6 FILEIN`) with a `SALTWATER_HEAD` auxiliary array for a sea level that changes by stress period; in flopy, `swi.tva.initialize(auxiliary=["saltwater_head"], aux={kper: [value], ...})` (see `test_gwf_swi06.py`).  TVA input overrides the option.
- Heads in the freshwater model are freshwater heads.  A sea boundary on the seafloor (GHB or CHD) must use the equivalent freshwater head `h_f = h_s + (h_s - z) * (rho_s - rho_f) / rho_f`, where `h_s` is sea level and `z` is the seafloor elevation; with the default densities the factor is 0.025 (see `test_gwf_swi04.py` and case6).  In the saltwater model of a two-fluid simulation heads are saltwater heads and the sea boundary is sea level itself.
- Two-fluid simulations generally need backtracking in the IMS Package.  The settings used in the notebooks are `BACKTRACKING_NUMBER 20`, `BACKTRACKING_TOLERANCE 1.05`, `BACKTRACKING_REDUCTION_FACTOR 0.1`, and `BACKTRACKING_RESIDUAL_LIMIT 0.002`, with `LINEAR_ACCELERATION BICGSTAB`.
- The `AUTO_FLOW_REDUCE` option of the WEL Package is supported and is SWI-aware: the reduction is referenced to the part of the cell occupied by the fluid being simulated, so a freshwater well tapers smoothly as the interface rises toward it (and a saltwater well as the interface falls toward the cell bottom).  Under SWI the reduction also applies to confined cells, whose fluid zone can vanish as the interface moves through them.  The taper responds strongly to the interface position, so pair it with DBD under-relaxation in the IMS Package (`UNDER_RELAXATION DBD` with `UNDER_RELAXATION_GAMMA 0.1`, `UNDER_RELAXATION_THETA 0.7`, `UNDER_RELAXATION_KAPPA 0.07`, `UNDER_RELAXATION_MOMENTUM 0.0`); backtracking does not help with the taper.  See case2 and `test_gwf_swi_wel_afr.py`.  Requires an executable built from `feat-swi` commit `fc4cebd1` or later.
- The interface is represented as one elevation per cell, so the grid should be refined where a freshwater or saltwater zone pinches out.
- SWI is not yet compatible with XT3D, ghost-node correction, the HFB Package, model coupling through GWF-GWF exchanges, parallel simulations, transport (GWT and GWE), particle tracking, the BUY and VSC Packages, or the advanced stress packages (MAW, SFR, LAK, UZF).
