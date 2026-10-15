from pathlib import Path
from pypdf import PdfReader
import sys
sys.stdout.reconfigure(encoding="utf-8")
src = Path(r"C:\Users\dudup\Downloads\bvar aplicação macro.pdf")
out = Path(r"C:\Users\dudup\OneDrive\NT Macro\work\psi_diagnostic\application_text.txt")
r = PdfReader(src)
texts = [p.extract_text(extraction_mode="layout") or "" for p in r.pages]
out.write_text("\n\n".join(f"=== PAGE {i+1} ===\n{s}" for i,s in enumerate(texts)), encoding="utf-8")
print("PAGES", len(texts), "CHARS", sum(map(len,texts)))
for i,s in enumerate(texts):
    lines = [v.strip() for v in s.splitlines() if v.strip()]
    print(f"PAGE {i+1}: " + " | ".join(lines[:2]))
links = set()
for p in r.pages:
    for ref in p.get("/Annots", []):
        a = ref.get_object().get("/A", {})
        if a.get("/URI"): links.add(str(a["/URI"]))
print("LINKS\n" + "\n".join(sorted(links)))
