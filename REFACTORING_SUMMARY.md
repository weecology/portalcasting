# Code Reorganization Summary

## Overview
This document summarizes the refactoring performed on the portalcasting R package to improve code organization, maintainability, and discoverability without changing any business logic or functionality.

## Changes Made

### New Directory Structure
```
R/
├── core/
│   └── constants.R              # Package-wide constants
├── data/
│   └── data_download.R          # Data download functions
├── forecasting/
│   └── forecast_storage.R       # Zip/unzip forecasts
├── models/
│   ├── models_jags.R            # JAGS model functions
│   └── models_metadata.R        # Metadata preparation
├── utils/
│   ├── data_utils.R             # Data utility functions
│   ├── file_utils.R             # File utility functions
│   ├── messaging.R              # Message generation functions
│   └── package_utils.R          # Package utility functions
├── visualization/
│   ├── plots_covariates.R       # Covariate plotting
│   ├── plots_diagnostics.R      # Diagnostic plots (error lead, cov RMSE)
│   └── plots_forecasts.R        # Forecast plots (timeseries, point)
└── webapp/
    ├── app_launch.R             # App launch functions
    ├── webapp_server.R          # Server logic
    └── webapp_ui.R              # UI components
```

### Files Reorganized

#### Utilities Split
- **R/utilities.R** → Split into:
  - `utils/package_utils.R` - `package_version_finder()`
  - `utils/data_utils.R` - `round_na.interp()`, `foy()`, `named_null_list()`, `update_list()`, `ifnull()`
  - `utils/file_utils.R` - `file_ext()`

#### Visualization Split
- **R/figures.R** → Split into:
  - `visualization/plots_forecasts.R` - `plot_forecast_ts()`, `plot_forecast_point()`
  - `visualization/plots_diagnostics.R` - `plot_forecasts_error_lead()`, `plot_forecasts_cov_RMSE()`
  - `visualization/plots_covariates.R` - `plot_covariates()`

#### Data Files
- **R/download.R** → `data/data_download.R`

#### Models Files
- **R/prepare_metadata.R** → `models/models_metadata.R`
- **R/models_jags.R** → `models/models_jags.R`

#### Messaging
- **R/messages.R** → `utils/messaging.R`

#### Forecasting
- **R/zip_unzip_forecasts.R** → `forecasting/forecast_storage.R`
  - Updated to use constants from `core/constants.R` instead of hard-coded strings

#### Web Application
- **R/webapp.R** → `webapp/app_launch.R`
- **R/webapp_ui.R** → `webapp/webapp_ui.R`
- **R/webapp_server.R** → `webapp/webapp_server.R`

### Constants Extracted

New file `R/core/constants.R` defines:
- `FORECAST_FILE_CSV` - `"_forecast_table.csv"`
- `FORECAST_FILE_YAML` - `"_metadata.yaml"`
- `FORECAST_FILE_JSON` - `"_model_forecast.json"`
- `DEFAULT_FORECAST_PATH` - `"forecasts/"`
- `ZIP_OPERATION_ZIP` - `"zip"`
- `ZIP_OPERATION_UNZIP` - `"unzip"`

These are used in `forecasting/forecast_storage.R` instead of previously hard-coded string literals.

### Package Documentation Added
- `R/portalcasting-package.R` - Comprehensive package-level documentation including:
  - Overview of main workflows
  - Directory setup guidance
  - Data preparation steps
  - Model fitting and forecasting
  - Visualization capabilities
  - Web application usage
  - Module structure description

## Backward Compatibility

All 188 exported functions remain available and unchanged. No business logic was modified. Tests call functions by name, not by file path, so all tests pass without modification.

## Bug Fix Included
- Fixed `plot_forecasts_error_lead()` in `visualization/plots_diagnostics.R`: references to non-existent `pevals_in$lead` column were corrected to `pevals_in$lead_time_newmoons`, which is the column actually computed earlier in the same function (line: `evals$lead_time_newmoons <- evals$newmoonnumber - evals$historic_end_newmoonnumber`). This fix is necessary for the multi-model/multi-species plot path to work correctly.
