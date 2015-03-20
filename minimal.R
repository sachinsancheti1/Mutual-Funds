Minimal Examples of the functions
url = "http://economictimes.indiatimes.com/uti-equity-fund/mffactsheet/schemeid-247.cms"
ps = parseIt(url)
list(smry = summarydata(ps),
     sector = sectorinfo(ps),
     fundbasics = fundinfo(ps),
     manager = fundmgrinfo(ps),
     risk = riskinfo(ps))
