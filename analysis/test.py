import numpy as np

def test(ckd, ckd15_date, ckd35_date):
    old = ckd or (np.isnan(ckd15_date) and (ckd35_date >= ckd15_date) or (np.isnan(ckd35_date) and not np.isnan(ckd15_date)))
    new = ckd or (np.isnan(ckd15_date) and (ckd35_date >= ckd15_date)) or (np.isnan(ckd35_date) and not np.isnan(ckd15_date))
    out = f"ckd={ckd}, ckd15_date={ckd15_date}, ckd35_date={ckd35_date}, old={old}, new={new}"
    if old == new:
        print(f"YES {out}")
    else:
        print(f"NO {out}")
    
for ckd in [True, False]:
    for ckd15_date in [np.nan, 10]:
        for ckd35_date in [np.nan, 5, 20]:
            test(ckd, ckd15_date, ckd35_date)