import pathlib,csv,re,math
root=pathlib.Path(r"C:/Users/dudup/OneDrive/japan-econometrics-model/data/processed/boj_forecast_evaluation_20261018")
quarters=[];fiscal=[]
for fname in ["gaku-jg2622.csv","gaku-jk2622.csv"]:
 year=None
 for row in csv.reader((root/fname).read_text(encoding="cp932").splitlines()):
  if not row:continue
  s=row[0].strip();m=re.match(r"(\d{4})/\s*(\d+)-",s)
  if m:year=int(m[1]);month=int(m[2])
  else:
   m=re.match(r"(\d+)-\s*\d+[.]",s)
   if not m or year is None:continue
   month=int(m[1])
  value=float(row[1].replace(",","").strip())
  if fname=="gaku-jg2622.csv":quarters.append(dict(date=f"{year}-{month:02d}-01",year=year,quarter=(month-1)//3+1,qindex=year*4+(month-1)//3,gdp_nsa=value))
  else:
   ix=year*4+(month-1)//3
   z=next(z for z in quarters if z["qindex"]==ix);z["gdp_sa_annualized"]=value
for row in csv.reader((root/"gaku-jfy2622.csv").read_text(encoding="cp932").splitlines()):
 if row and re.match(r"\d{4}/",row[0]):fiscal.append(dict(fiscal_year=int(row[0][:4]),gdp=float(row[1].replace(",","").strip())))
maxdiff=0
for r in fiscal:
 qs=[q["gdp_nsa"] for q in quarters if (q["year"] if q["quarter"]>1 else q["year"]-1)==r["fiscal_year"]]
 if len(qs)==4:maxdiff=max(maxdiff,abs(sum(qs)-r["gdp"]))
assert maxdiff<.3,maxdiff
for name,rows in [("gdp_quarterly.csv",quarters),("gdp_fiscal.csv",fiscal)]:
 with (root/name).open("w",newline="",encoding="utf-8") as f:
  w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
print("quarters",len(quarters),"fiscal",len(fiscal),"fiscal_sum_maxdiff",maxdiff)