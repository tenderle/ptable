# ptable 0.4.0

* Added a `NEWS.md` file to track changes to the package.

# Version 0.4.1
- update: prepare CRAN publication
- update: vignette

# Version 0.4.0
- feature: new function `modify_cnt_ptable()` to generate modified ptables (for count/frequency tables)
- update: minor code improvement

# Version 0.3.4.
- update: generic error message when constraints are violated
- update: replaced `lattice` by `ggplot2` in density plot function `pt_plot_pD()` 
- feature: Github actions (code coverage and *CMD* check)

# Version 0.3.3.
- feature:  new functions `create_cnt_table()` (for count/frequency tables) and `create_num_table()` (for numerical/magnitude tables) that both wrap `pt_create_pParams()` and `pt_create_pTable()`
- code cleanup and improved documentation
- updated vignette
- Thx to @bernhard-da for the commits.

# Version 0.3.2.
- feature: generic function `plot` instead of `fifi_function`
- feature: functions `pt_ex_cnts()` and `pt_ex_nums()` for quick examples 
- updated vignette
- updated dashboard

# Version 0.3.1.
- minor updates (e.g. updated argument `step`)

# Version 0.3.0
- feature: ptables for magnitude tables
- new arguments in function `pt_create_pParams()`: `icat`, `table`, `step`, `type`
- updated arguments in function `pt_create_pPtable()`: argument `type` removed


# Version 0.2.0
- Prototype Version for Testing (Census)

# Version 0.1.14
- feature: new argument `SDCtool` of function `pt_export()`: either "TauArgus" or "SAS"
- feature: vignette and new function `pt_vignette()` to show it
- updated default for argument `pstay` in function `pt_create_pParams()`
- updated default for argument `type` in function `pt_create_pTable()`

# Version 0.1.12/13
- Prepared for prototype testing

# Version 0.1.11
- Some minor updates of graphic outputs (e.g. font sizes)
- Alternative allocation for abs implementation (argument `type="abs2"`) (intended to replace actual argument `type="abs"` and will be removed afterwards)

# Version 0.1.10
- feature: graphic output of transition matrix `fifi_plot(..., type="t")` (first version) 
- feature: graphic output of perturbation panel `fifi_plot(..., type="p")` (first version)

# Version 0.1.9
- Some updates of the dashboard (e.g. now directly opens in web browser, download buttons)

# Version 0.1.8
- Updated live output of the main function `pt_create_pTable(..., monitoring=FALSE, debugging=FALSE)` (i.e., monitoring is for users, debugging for own purposes and will be removed in a stable version)  
- Some first minor fixes of extended parameter setting (i.e. argument `pstay`)

# Version 0.1.7
- compatibility with R-package [**cellKey**](https://github.com/sdcTools/cellKey) (type of ptable: *abs* or *destatis* formatted ptable)

# Version 0.1.6
- feature: new function `pt_export()` to export ptable (csv format used by [**TauArgus**](https://github.com/sdcTools/tauargus))
- feature: new argument of function `fifi_plot()` in order to save graphical pdf output

# Version 0.1.5
- bug-fixing: dashboard

# Version 0.1.1 - 0.1.4
- bug-fixing: CRAN compatibility and minor fixes (e.g. depends) 

# Version 0.1.0
- first uploaded version

