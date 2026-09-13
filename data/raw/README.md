# Local raw-data staging

Raw third-party source data belong in this directory **locally** and are ignored by Git through the repository `.gitignore`.

The publication-facing repository does not redistribute raw source datasets through this directory.

## Current expected inputs

The population-weighted workflow currently expects paths including:

```text
data/raw/csv/GTD_A.csv
data/raw/Iraq Ethnicity.zip
data/raw/LandScan_2008.tif
```

The modular import framework can also use:

```text
data/raw/dat/
data/raw/shp/
data/raw/xlsx/
```

Obtain source data from the authoritative providers and follow their licensing, registration, EULA, attribution, and citation requirements.

Do not commit credentials, access tokens, restricted source data, or local data extracts here.

See `../../DATA.md` for the repository-wide data-governance policy.
