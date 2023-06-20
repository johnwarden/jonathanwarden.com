serve:
  hugo server

linkcheck:
  linklint -http -host localhost:1313 /@ -doc linklint.out && open linklint.out/index.html