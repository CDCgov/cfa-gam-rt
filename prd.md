# Product Requirements: Local-level $R_t$

**Goal & Scope**
This project implements a model and evaluation pipeline for local estimation of the time-varying reproduction number $R_t$ via hierarchical GAMs.
The goal of this project is generate sub-state Rt estimates and reduce sensitivity to reporting dropout or inconsistent reporting patterns.
Using an approach pioneered by UKHSA, we implement partial pooling across smooth functions to produce stable but locally responsive estimates with penalization to improve extrapolation properties.
The plan is to improve on the current state-of-the-art by using cheap, flexible models to fit on much more data in one joint model than would be computationally feasible in an HMC-based approach.
The model is implemented in an R package with a separate deployment workflow into our cloud environment.

The plan is written for two full-time staff working jointly to develop the model, develop a modeling pipeline, and conduct rigorous testing.
One FTE focuses on model development and the other focuses on pipeline development and deployment.
There are two planned moments for evaluation: Mid-March and Mid-May.
The mid-March deadline is an NNH-internal deadline to check that required features (hierarchy and nowcasting) are in place and the model can feasibly be fit to the data.
The mid-May deadline is a RTM-internal deadline to report up to leadership on model reliability for production next season.
For the March deadline we aim to have all model functionality in place and for the May deadline we aim to have competitive model performance.


---

## Model Implementation

The plan is to focus on implementing core functionality first, with a feasibility check in mid-March.
After core features are implemented, the goal is to tune settings to maximize performance (minimize both model error and runtime).
The model is developed as an R package using `{mgcv}` as the computational engine.

1. **v0.4.0: User Research & Infrastructure Improvements** (Mid Feb)
   - Add core SIR simulator for simulation-based testing
   - Improve defaults
2. **v0.5.0: Hierarchical Modeling** (Early March)
   - Partial pooling across multiple groups
   - Simulation of multiple timeseries
   - Rt from the partially pooled estimates
3. **v0.6.0: Basic Nowcasting** (Mid March)
   - Right-truncation data simulator.
   - Basic and flexible nowcasting with PMFs + splines.
4. **v0.7.0: Performance improvements** (Early April)
   - Explore GP-based global trend
   - Tuning of nowcast-based extrapolation
   - Support “reporting triangle” data (report date by reference date).

Technical specifications and development are tracked via GitHub issues: https://github.com/CDCgov/cfa-gam-rt/issues

---

## Modeling Pipeline

Target building modeling pipeline in parallel with package development in March. The goal is to build a pipeline to enable model development iteration cycles on large segments of the available data.

1. **Data pre-processing for local inference**
1. **Data Quality Checks**
   - Identify non-nowcastable outliers in reporting structure.
      - This process will be iterative and rely on features in the package's v0.6.0
   - Distinguish between true zeros and nulls.
   - Preprocess data for use with the model
2. **Cloud Deployment**
   - Automated deployment to cloud environment
   - Enable large-scale testing of input parameters and model structure
   - Compare to frozen baseline, EpiNow2. and PyRenew for state-level nowcast and forecast performance via forecast-based measures (WIS/CRPS for nowcast, 7 day horizon, 14 day horizon)
   - Compare to frozen baseline and non-hierarchical fit for county-level performance
4. **Model Evaluation**
   - Generate diagnostic summaries (basis checks, warnings).
   - Use forecast-based scoring machinery to identify systematic trends in poor performance from 1000s of input timeseries
   - Summary-statistic based evaluation

---

## Success Criteria
- Convergence: Model runs reliably and can fit in a reasonable amount of time
    - It isn't derailed by difficult to nowcast moments like the holidays
    - Measured as the number of week-unit combinations with model convergence
    - Runtime measured as mins per estimate
- Performance: Match up to 80% of current production performance on high-quality timeseries with fewer non-estimable timeseries and faster runtimes.
- Implemented and running in parallel to production by May '25 and into production for the '25-'26 respiratory virus season.

---

## Key Risks
- **Staffing**: Staffing is lean, relying on substantial commitment from a few individuals. Superseding demands or loss of contract support would place this timeline at risk.
- **Shared tooling**: The product relies on externally-supplied data, parameter estimates, and potentially model validation infrastucture. It does not account for staff time for these required features.
- **Deployment**: Deploying code into CFA's Azure evironment can require substantial additional staffing time and expertise, even for small tasks. Recommended implementations have changed frequently and unpredictably, requiring substantial engineering effort.
