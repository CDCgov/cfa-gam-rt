# Product Requirements: Local-level $R_t$

**Goal & Scope**
This project implements a model and evaluation pipeline for local estimation of the time-varying reproduction number $R_t$ via hierarchical GAMs.
Using an approach pioneered by UKHSA, we implement partial pooling across smooth functions to produce stable but locally responsive estimates with penalization to improve extrapolation properties.
The model is implemented in an R package with a seperate deployment workflow into our cloud environment.

---

## Model Implementation

### Current release roadmap
Plan is to "tic-toc" between infrastructure releases ("tic") and feature releases ("toc").
Releases are currently planned through mid-March to implement the core feature set, with additional improvements planned but not yet mapped.
Additional development will focus on responding to user feedback and enhance model performance.

1. **v0.4.0: User Research & Infrastructure Improvements** (Late Jan)
   - User research has started, with 5+ more sessions planned.
     - Includes members of NNH, ARB, and Inform
   - Add core simulator for simulation-based testing
   - Improve defaults

2. **v0.5.0: Basic Nowcasting** (Mid Feb)
   - Right-truncation data simulator.
   - Basic and flexible nowcasting with PMFs + splines.
   - Support “reporting triangle” data (report date by reference date).

3. **v0.6.0: Hierarchical Modeling** (Early March)
   - Partial pooling across multiple groups
   - Preprocessing pipeline for multi-region or multi-group data.
   - Handling for local-specific nowcasts

### Future Features
- Advanced nowcasting (Partially pooled nowcast, day-of-week report date effects).
- Performance improvements (potentially moving to GP-based global smooth).
- Benchmarking runtime and accuracy against current production approach.

Technical specifications and development are tracked via GitHub issues: https://github.com/CDCgov/cfa-gam-rt/issues

---

## Modeling Pipeline

Target building modeling pipeline in parallel with package development in March. The goal is to build a pipeline to enable model development iteration cycles on large segments of the available data.

1. **Data Quality Checks**
   - Identify non-nowcastable outliers in reporting structure.
   - Distinguish between true zeros and nulls.
2. **Model Fitting**
   - Prepare delay/nowcasting components as needed.
   - Fit core GAM to case data.
   - Store posterior draws for inference.
4. **Model Evaluation**
   - Generate diagnostic summaries (basis checks, warnings).
   - Use forecast-based scoring machinery to identify systematic trends in poor performance
   - Summary-statistic based evaluation

---

## Success Criteria
- Adoption by external-to-NNH partners
   - Maintain key engagement with UKHSA.
   - Socialization with ARB and Inform.
   - Generate new engagement with a state or local health partner.
- Performance: Match up to 80% of current production performance on high-quality timeseries with fewer non-estimable timeseries and faster runtimes.
- Implemented into production for the '25-'26 respiratory virus season.

---

## Key Risks
- **Staffing**: Staffing is lean, relying on substantial commitment from a few individuals. Superseding demands or loss of contract support would place this timeline at risk.
- **Shared tooling**: The product relies on externally-supplied data, parameter estimates, and potentially model validation infrastucture. It does not account for staff time for these required features.
- **Deployment**: Deploying code into CFA's Azure evironment can require substantial additional staffing time and expertise, even for small tasks. Recommended implementations have changed frequently and unpredictably, requiring substantial engineering effort.
