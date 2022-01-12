## version 1.0.12 (flare)

### Changes

- added NEWS.md file
- If a model crashes while running or does not produce output, then `run_ensemble()` does not crash, but in the output netCDF file the model is added but with NA's. Also a FAIL message is printed to the console.
