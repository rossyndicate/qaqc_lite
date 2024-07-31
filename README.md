This is a "lite" workflow to perform simple quality assurance/quality control (QA/QC) steps across water quality data pulled from HydroVu. Specifically, this workflow identifies the following:

1.  Observations that fall outside In-Situ's sensor specification ranges

2.  Observations that fall outside seasonal thresholds for slope and mean ((ideally)) developed on historical, clean data

3.  Observations during times when the sonde was not submerged in water

4.  Observations during times when the water was freezing

5.  Observations with missing data

6.  Observations that repeat through time

Plus, several "**experimental**" flags that aim at finding:

7.  DO observations with "noise" (i.e., dropping/hopping DO)
8.  Turbidity drift from biofouling
9.  Identifying observations "sandwiched" between flagged data (called "suspect" observations)

We have also developed several approaches to reduce over-flagging by:

-   Removing slope flags for sensor data that occurs at the same time as a change in temperature or depth

-   Removing (appropriate) flags that occurred at roughly the same time at an upstream or downstream location. **The associated function, `network_check()`, will require heavy modification to perform across a different network of sites than ROSSyndicate's Poudre Sonde Network.**
