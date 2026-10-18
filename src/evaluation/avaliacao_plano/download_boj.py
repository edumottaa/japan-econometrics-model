import urllib.request,re,html,datetime,csv,json,hashlib,pathlib,concurrent.futures
from pypdf import PdfReader
ROOT=pathlib.Path(r"C:/Users/dudup/OneDrive/japan-econometrics-model")
OUT=ROOT/"data/processed/boj_forecast_evaluation_20261018"
(OUT/"pdf").mkdir(parents=True,exist_ok=True)
(OUT/"text").mkdir(exist_ok=True)
URL="https://www.boj.or.jp/en/mopo/outlook/"
h=urllib.request.urlopen(URL,timeout=40).read().decode()
(OUT/"outlook_index.html").write_text(h,encoding="utf-8")
months={m.lower():i for i,m in enumerate(["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],1)}
rows=[]
for tr in re.findall(r"<tr\b[^>]*>(.*?)</tr>",h,re.S|re.I):
 if not re.search(r"gor[0-9]{4}a[.](?:pdf|htm)",tr): continue
 href=re.search(r'href="([^"]+[.](?:pdf|htm))"',tr)
 td=re.search(r"<td\b[^>]*>(.*?)</td>",tr,re.S|re.I)
 if not href or not td:continue
 date=html.unescape(re.sub("<[^>]+>"," ",td[1]));date=" ".join(date.split())
 m=re.search(r"([A-Za-z]+)[.]?\s+(\d+),\s*(\d{4})",date)
 if not m:raise RuntimeError(date)
 dt=datetime.date(int(m[3]),months[m[1][:3].lower()],int(m[2]))
 if datetime.date(2014,10,1)<=dt<=datetime.date(2026,6,30):rows.append(dict(publication_date=dt.isoformat(),url="https://www.boj.or.jp"+href[1]))
rows.extend([dict(publication_date="2015-01-21",url="https://www.boj.or.jp/en/mopo/mpmdeci/mpr_2015/k150121a.pdf"),dict(publication_date="2015-07-15",url="https://www.boj.or.jp/en/mopo/mpmdeci/mpr_2015/k150715a.pdf")])
rows.sort(key=lambda x:x["publication_date"])
assert len(rows)==47,len(rows)
def fetch(r):
 r["view_url"]=r["url"]; r["url"]=r["url"].replace("a.htm","b.pdf"); name=r["url"].rsplit("/",1)[1];p=OUT/"pdf"/name
 if not p.exists():
  b=urllib.request.urlopen(r["url"],timeout=60).read()
  assert b.startswith(b"%PDF"),r["url"]
  p.write_bytes(b)
 reader=PdfReader(p)
 texts=[page.extract_text(extraction_mode="layout") for page in reader.pages]
 (OUT/"text"/(name+".txt")).write_text("\n\n".join(f"--- PAGE {i+1} ---\n{t}" for i,t in enumerate(texts)),encoding="utf-8")
 r.update(file=name,pages=len(reader.pages),sha256=hashlib.sha256(p.read_bytes()).hexdigest())
 return r
with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:rows=list(pool.map(fetch,rows))
with (OUT/"manifest.csv").open("w",newline="",encoding="utf-8") as f:
 w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
base="https://www.esri.cao.go.jp/jp/sna/data/data_list/sokuhou/files/2026/qe262_2/tables/"
for name in ["gaku-jg2622.csv","gaku-jk2622.csv","gaku-jfy2622.csv"]:
 b=urllib.request.urlopen(base+name,timeout=40).read();(OUT/name).write_bytes(b)
 print(name,b[:120].decode("cp932",errors="replace"))
print("downloaded_outlooks",len(rows),"pages",sum(x["pages"] for x in rows))
for name in ["gor1410a.pdf","k150121a.pdf","gor1904b.pdf","gor2604a.pdf"]:
 reader=PdfReader(OUT/"pdf"/name)
 for i,page in enumerate(reader.pages):
  t=page.extract_text(extraction_mode="layout")
  if "Forecasts of the Majority" in t or "Forecasts of the majority" in t or "Majority" in t:
   print("TABLE",name,"page",i+1);print(t[:7000]);break