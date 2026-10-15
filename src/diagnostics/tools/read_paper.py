from pathlib import Path
from pypdf import PdfReader
import sys
sys.stdout.reconfigure(encoding="utf-8")
root = Path(r"C:\Users\dudup\OneDrive\NT Macro\work\psi_diagnostic")
pdf = Path(r"C:\Users\dudup\Downloads\2501.16711v1.pdf")
reader = PdfReader(pdf)
texts = [p.extract_text(extraction_mode="layout") or "" for p in reader.pages]
combined = "\n\n".join(f"=== PDF PAGE {i+1} ===\n{s}" for i, s in enumerate(texts))
(root / "paper_v1_text.txt").write_text(combined, encoding="utf-8")
print("PAGES", len(texts), "CHARACTERS", len(combined))
for i, s in enumerate(texts):
    lines = [x.strip() for x in s.splitlines() if x.strip()]
    print(f"PAGE {i+1}: {len(s)} characters; " + " | ".join(lines[:3]))
