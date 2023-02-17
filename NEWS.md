
# ptable 0.4.1
- prepare package for CRAN publication
- update: vignette
- update: shiny app
- update: perturbation panel plot
- code cleanup and improved documentation
- changed `pt_create_pParams()` and `pt_create_pTable()` to internal functions

# ptable 0.4.0
- feature: new function `modify_cnt_ptable()` to generate modified ptables (for count/frequency tables)
- update: minor code improvement
- added a `NEWS.md` file to track changes to the package.

# ptable 0.3.4.
- update: generic error message when constraints are violated
- update: replaced `lattice` by `ggplot2` in density plot function `pt_plot_pD()` 
- feature: Github actions (code coverage and *CMD* check)

# ptable 0.3.3.
- feature:  new functions `create_cnt_table()` (for count/frequency tables) and `create_num_table()` (for numerical/magnitude tables) that both wrap `pt_create_pParams()` and `pt_create_pTable()`
- code cleanup and improved documentation
- updated vignette
- Thanks to [Bernhard Meindl](https://github.com/bernhard-da) for the commits.

# ptable 0.3.2.
- feature: generic function `plot` instead of `fifi_function`
- feature: functions `pt_ex_cnts()` and `pt_ex_nums()` for quick examples 
- updated vignette
- updated dashboard

# ptable 0.3.1.
- minor updates (e.g. updated argument `step`)

# ptable 0.3.0
- feature: ptables for magnitude tables
- new arguments in function `pt_create_pParams()`: `icat`, `table`, `step`, `type`
- updated arguments in function `pt_create_pPtable()`: argument `type` removed


# ptable 0.2.0
- Prototype Version for Testing (Census)

# ptable 0.1.14
- feature: new argument `SDCtool` of function `pt_export()`: either "TauArgus" or "SAS"
- feature: vignette and new function `pt_vignette()` to show it
- updated default for argument `pstay` in function `pt_create_pParams()`
- updated default for argument `type` in function `pt_create_pTable()`

# ptable 0.1.12/13
- Prepared for prototype testing

# ptable 0.1.11
- Some minor updates of graphic outputs (e.g. font sizes)
- Alternative allocation for abs implementation (argument `type="abs2"`) (intended to replace actual argument `type="abs"` and will be removed afterwards)

# ptable 0.1.10
- feature: graphic output of transition matrix `fifi_plot(..., type="t")` (first version) 
- feature: graphic output of perturbation panel `fifi_plot(..., type="p")` (first version)

# ptable 0.1.9
- Some updates of the dashboard (e.g. now directly opens in web browser, download buttons)

# ptable 0.1.8
- Updated live output of the main function `pt_create_pTable(..., monitoring=FALSE, debugging=FALSE)` (i.e., monitoring is for users, debugging for own purposes and will be removed in a stable version)  
- Some first minor fixes of extended parameter setting (i.e. argument `pstay`)

# ptable 0.1.7
- compatibility with R-package [**cellKey**](https://github.com/sdcTools/cellKey) (type of ptable: *abs* or *destatis* formatted ptable)

# ptable 0.1.6
- feature: new function `pt_export()` to export ptable (csv format used by [**TauArgus**](https://github.com/sdcTools/tauargus))
- feature: new argument of function `fifi_plot()` in order to save graphical pdf output

# ptable 0.1.5
- bug-fixing: dashboard

# ptable 0.1.1 - 0.1.4
- bug-fixing: CRAN compatibility and minor fixes (e.g. depends) 

# ptable 0.1.0
- first uploaded version

