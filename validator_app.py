import streamlit as st
import pandas as pd
import os
import re

# File paths
DATA_PATH = "data/elasmo_sightings_20250415.csv"
SAVE_PATH = "data/validated_sightings.csv"

# Load data
@st.cache_data
def load_data():
    base_df = pd.read_csv(DATA_PATH)
    if os.path.exists(SAVE_PATH):
        df = pd.read_csv(SAVE_PATH)
        for col in ["validation", "notes", "N_observed"]:
            if col not in df.columns:
                df[col] = ""
    else:
        df = base_df.copy()
        df["validation"] = ""
        df["notes"] = ""
        df["N_observed"] = ""
    df["validation"] = df["validation"].fillna("").astype(str)
    return df

df = load_data()

# Initialize session state
if "skipped_urls" not in st.session_state:
    st.session_state.skipped_urls = set()

if "validation_log" not in st.session_state:
    st.session_state["validation_log"] = {}

if "skipped_indices" not in st.session_state:
    st.session_state.skipped_indices = set()

if "current_row_index" not in st.session_state:
    unreviewed = df[df["validation"] == ""]
    start_index = unreviewed.index.min() if not unreviewed.empty else 0
    st.session_state.current_row_index = start_index

# Update df with session-state validation log
for idx, val in st.session_state["validation_log"].items():
    for k, v in val.items():
        df.at[idx, k] = v

# Progress bar function
def update_progress_bars(df, row_index):
    with st.sidebar:
        grouped = df.groupby("url")["validation"].apply(lambda x: (x == "").sum() == 0)
        validated_urls = grouped.sum()
        total_urls = df["url"].nunique()
        progress = validated_urls / total_urls if total_urls else 0
        st.progress(progress, text=f"{validated_urls} of {total_urls} URLs reviewed")

        url_row_progress = df[df["url"] == df.iloc[row_index]["url"]]
        completed = (url_row_progress["validation"] != "").sum()
        total = len(url_row_progress)
        url_progress = completed / total if total else 0
        st.progress(url_progress, text=f"{completed} of {total} rows for this URL reviewed")

# --- Sidebar ---
st.sidebar.title("Validation Controls")
row_index = st.sidebar.number_input("Select row to review", min_value=0, max_value=len(df)-1, step=1, key="row_input")
st.sidebar.markdown("---")
save_validation = st.sidebar.button("✅ Save Validation")
skip_url = st.sidebar.button("🚫 Skip all from this URL")
next_url = st.sidebar.button("➡️ Next URL to Review")


update_progress_bars(df, row_index)

# --- Main ---
st.title("Shark/Ray Sighting Validation Tool")
st.session_state.current_row_index = row_index
record = df.iloc[row_index]

if record["url"] in st.session_state.skipped_urls:
    st.warning("This record's URL is marked as skipped.")
else:
    st.write("### Original Data")
    st.markdown(f"**URL:** [{record['url']}]({record['url']})")
    st.markdown(f"**Title:** {record['title']}")

    if record["validation"]:
        st.info(f"This row was already marked as **{record['validation']}**.")

    # Highlight keywords in excerpt using HTML
    highlighted_excerpt = record["excerpt"]
    for kw in sorted(df["species"].dropna().unique(), key=len, reverse=True):
        if isinstance(kw, str) and kw.strip():
            pattern = re.compile(rf"(?<!\\w)({re.escape(kw)})(?!\\w)", re.IGNORECASE)
            highlighted_excerpt = pattern.sub(r"<span style='font-weight:bold'>\1</span>", highlighted_excerpt)

    st.markdown(f"<strong>Excerpt:</strong> {highlighted_excerpt}", unsafe_allow_html=True)
    
    validation = st.radio("Validation Status:", ["✅ Valid", "🚩 Needs Review", "❌ Not Valid"], key="radio_buttons")

    if validation == "✅ Valid":
        validation = "valid"
    elif validation == "🚩 Needs Review":
        validation = "needs review"
    elif validation == "❌ Not Valid":
        validation = "not valid"

    # Pre-fill species based on highlighted word
    guessed_species = ""
    for kw in df["species"].unique():
        if isinstance(kw, str) and kw.lower() in record["excerpt"].lower():
            guessed_species = kw
            break

    species = st.text_input("Species", value=record.get("species", guessed_species or ""))
    sighting_date = st.text_input("Date", value=record["sighting_date"])
    dive_site = st.text_input("Dive Site", value=record["dive_site"])
    N_observed = st.text_input("N Observed", value=record.get("N_observed", ""))

    st.markdown("---")
    st.write("### 🧪 Validate This Sighting")



    notes = st.text_area("Notes", value=record.get("notes", ""))

    if save_validation:
        df.at[row_index, "species"] = species
        df.at[row_index, "sighting_date"] = sighting_date
        df.at[row_index, "dive_site"] = dive_site
        df.at[row_index, "validation"] = validation
        df.at[row_index, "notes"] = notes
        df.at[row_index, "N_observed"] = N_observed
        df.to_csv(SAVE_PATH, index=False)
        st.session_state["validation_log"][row_index] = {
            "species": species,
            "sighting_date": sighting_date,
            "dive_site": dive_site,
            "validation": validation,
            "notes": notes,
            "N_observed": N_observed
        }
        st.success("Saved!")

# Skip entire URL option
if skip_url:
    url_to_skip = df.iloc[row_index]["url"]
    st.session_state.skipped_urls.add(url_to_skip)
    for idx in df[df["url"] == url_to_skip].index:
        if df.at[idx, "validation"] == "":
            df.at[idx, "validation"] = "not valid"
            st.session_state["validation_log"][idx] = {
                "species": df.at[idx, "species"],
                "sighting_date": df.at[idx, "sighting_date"],
                "dive_site": df.at[idx, "dive_site"],
                "validation": "not valid",
                "notes": df.at[idx, "notes"],
                "N_observed": df.at[idx, "N_observed"]
            }
    df.to_csv(SAVE_PATH, index=False)
    st.success(f"All unreviewed records from {url_to_skip} marked as not valid and skipped.")
    st.rerun()

# Move to next URL button
if next_url:
    current_url = df.iloc[row_index]["url"]
    seen_urls = set()
    found_next = False
    for idx in range(row_index + 1, len(df)):
        url = df.iloc[idx]["url"]
        if url not in seen_urls and df.at[idx, "validation"] == "":
            st.session_state.current_row_index = idx
            st.rerun()
            found_next = True
            break
    if not found_next:
        st.info("No more URLs to review.")

# Option to hide reviewed rows
hide_reviewed = st.sidebar.checkbox("Hide reviewed rows")

if hide_reviewed:
    reviewed_df = df[df["validation"] == ""]
    st.sidebar.markdown(f"Showing {len(reviewed_df)} unreviewed records.")
else:
    reviewed_df = df

# Allow download of validated results
st.sidebar.markdown("---")
st.sidebar.download_button("📅 Download Validated CSV", df.to_csv(index=False), file_name="validated_sightings.csv", mime="text/csv")

# --- Show Summary ---
st.markdown("### 📊 Summary of Validated Sightings")
st.write(pd.DataFrame.from_dict(st.session_state["validation_log"], orient="index"))
