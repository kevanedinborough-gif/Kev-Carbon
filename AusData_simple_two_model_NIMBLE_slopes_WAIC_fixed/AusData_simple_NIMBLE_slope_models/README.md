# Simple two-model NIMBLE slope comparison

Open `AusData_simple_NIMBLE_slope_models.Rmd` in RStudio and click **Knit**.

## Main controls

The YAML header contains the complete user control panel. The revised default window is:

```yaml
start_calbp: 2000
end_calbp: 0
calendar_display: "calAD"
```

This is approximately 50 BC to AD 1950. It gives the models a substantial pre-colonial baseline instead of fitting only the terminal part of the curve.

Try sensitivity runs with 1000, 2000 and 4000 cal BP starts. Interpret a colonial association only when the change-point credible interval overlaps the historically relevant period and the later posterior curve declines more strongly toward the present.

Radiocarbon frequency is an archaeological proxy, not a direct population count or a causal test of colonisation.

## WAIC extraction fix

This revision supports WAIC objects returned by different NIMBLE versions, including
numeric, list, S4/nimbleList and reference-environment structures. It also prevents
the first model in the table from being reported as the winner when WAIC is missing.
The fit chunk prints both extracted WAIC values before the comparison section.
