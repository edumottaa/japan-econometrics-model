import pathlib,csv,re,json
from pypdf import PdfReader
root=pathlib.Path(r"C:/Users/dudup/OneDrive/japan-econometrics-model/data/processed/boj_forecast_evaluation_20261018")
manifest=list(csv.DictReader((root/"manifest.csv").open(encoding="utf-8")))
rows=[];failed=[]
for rec in manifest:
 fy=int(rec["publication_date"][:4]);reader=PdfReader(root/"pdf"/rec["file"]);found=False
 for page,p in enumerate(reader.pages,1):
  txt=p.extract_text(extraction_mode="layout").replace("−","-").replace("–","-"); txt=re.sub(r"(Fiscal\s+20\d)\s+(\d)",r"\1\2",txt)
  if not re.search(r"Forecasts of the Majority",txt,re.I):continue
  ls=txt.splitlines()
  for j,line in enumerate(ls):
   if not re.search(r"Fiscal\s+"+str(fy)+r"\b",line):continue
   block=[]
   for z in ls[j:j+6]:
    if "Forecasts made" in z or (block and re.search(r"Fiscal\s+[0-9]{4}",z)):break
    block.append(z)
   vals=re.findall(r"\[\s*([+-]?[0-9]+(?:[.][0-9]+)?)\s*\]","\n".join(block))
   if len(vals)>=2:
    rows.append(dict(publication_date=rec["publication_date"],fiscal_year=fy,gdp=float(vals[0]),cpi_core=float(vals[1]),median_count=len(vals),pdf=rec["file"],page=page,view_url=rec["view_url"],pdf_url=rec["url"],row_text=" | ".join(block).strip()))
    found=True;break
  if found:break
 if not found:failed.append(rec["file"])
if failed==["gor2004a.pdf"]:
 rec=next(r for r in manifest if r["file"]=="gor2004a.pdf");rows.append(dict(publication_date=rec["publication_date"],fiscal_year=2020,gdp=None,cpi_core=None,median_count=0,pdf=rec["file"],page=6,view_url=rec["view_url"],pdf_url=rec["url"],row_text="Range forecasts only; no point median in April2020"));failed=[]
rows.sort(key=lambda r:r["publication_date"])
print("parsed",len(rows),"failed",failed)
with (root/"boj_forecasts.csv").open("w",newline="",encoding="utf-8") as f:
 w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
print([(r["publication_date"],r["fiscal_year"],r["gdp"],r["cpi_core"],r["page"]) for r in rows])
# Renderizar amostras de formatos distintos e o valor extrema para conferencia visual.
try:
 import pypdfium2 as pdfium
 for name in ["gor1410a.pdf","k150121a.pdf","gor1904b.pdf","gor2604a.pdf"]:
  rr=next(r for r in rows if r["pdf"]==name);doc=pdfium.PdfDocument(root/"pdf"/name)
  doc[rr["page"]-1].render(scale=1.3).to_pil().save(root/(name+".png"))
 print("render_samples_ok")
except ImportError:print("fitz_unavailable")
for name in ["gaku-jg2622.csv","gaku-jk2622.csv","gaku-jfy2622.csv"]:
 b=(root/name).read_bytes().decode("cp932")
 print(name);print("\n".join(b.splitlines()[:13]))