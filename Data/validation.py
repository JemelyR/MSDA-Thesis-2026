import pandas as pd
import numpy as np
from scipy import stats

scores = pd.read_csv("csv/all_participant_scores.csv")
background = pd.read_csv("csv/participant_background.csv")

background = background.rename(columns={"Participant_ID": "Participant ID"})

df = scores.merge(background[["Participant ID", "Location"]], on="Participant ID", how="left")

PRE_COLS = ["Pre_PT", "Pre_FS", "Pre_EC", "Pre_PD"]

def cohens_d(a, b):
    """Pooled Cohen's d effect size."""
    pooled_std = np.sqrt((np.std(a, ddof=1) ** 2 + np.std(b, ddof=1) ** 2) / 2)
    return (np.mean(a) - np.mean(b)) / pooled_std if pooled_std > 0 else 0.0

def interpret_d(d):
    d = abs(d)
    if d < 0.2:   return "negligible"
    elif d < 0.5: return "small"
    elif d < 0.8: return "medium"
    else:         return "large"

def run_comparison(df, split_col, label_a, label_b, pre_cols, section_title):
    print(f"\n{'='*65}")
    print(f"  {section_title}")
    print(f"{'='*65}")

    group_a = df[df[split_col] == label_a]
    group_b = df[df[split_col] == label_b]
    print(f"  {label_a}: n={len(group_a)}   |   {label_b}: n={len(group_b)}\n")

    any_warning = False
    rows = []
    for col in pre_cols:
        a_vals = group_a[col].dropna()
        b_vals = group_b[col].dropna()

        t_stat, p_val = stats.ttest_ind(a_vals, b_vals, equal_var=False)  # Welch's t-test
        d = cohens_d(a_vals, b_vals)
        flag = "⚠  WARNING" if p_val < 0.05 else "✓  OK"
        if p_val < 0.05:
            any_warning = True

        rows.append({
            "Scale": col,
            f"{label_a} Mean (SD)": f"{a_vals.mean():.2f} ({a_vals.std(ddof=1):.2f})",
            f"{label_b} Mean (SD)": f"{b_vals.mean():.2f} ({b_vals.std(ddof=1):.2f})",
            "t": round(t_stat, 3),
            "p-value": round(p_val, 4),
            "Cohen's d": round(d, 3),
            "Effect size": interpret_d(d),
            "Status": flag,
        })

    results_df = pd.DataFrame(rows)
    print(results_df.to_string(index=False))

    print()
    if any_warning:
        print("oop")
    else:
        print("yay")

    return results_df


#control v test
group_results = run_comparison(
    df, split_col="Group",
    label_a="Control", label_b="Test",
    pre_cols=PRE_COLS,
    section_title="Control vs Test group"
)

#hallway vs lab
location_results = run_comparison(
    df, split_col="Location",
    label_a="Hallway", label_b="Lab",
    pre_cols=PRE_COLS,
    section_title="Hallway vs Lab group"
)

#cross check
crosstab = pd.crosstab(df["Group"], df["Location"])
print(crosstab.to_string())
chi2, p_chi, dof, _ = stats.chi2_contingency(crosstab)
print(f"\n  Chi-square test: χ²={chi2:.3f}, p={p_chi:.4f}, df={dof}")
if p_chi < 0.05:
    print("nope")
else:
    print("yes")

print(f"\n{'='*65}\n")