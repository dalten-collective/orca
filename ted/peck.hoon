/-  spider
/+  *strandio
=,  strand=strand:spider
=,  dejs-soft:format
=,  strand-fail=strand-fail:libstrand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  url
  "https://api.coingecko.com/api/v3/simple/price?ids=bitcoin&vs_currencies="
=/  cur  !<((unit @tas) arg)
?~  cur  (strand-fail %no-arg ~)
=.  u.cur  (crip (cass (trip u.cur)))
?.  ((sane %tas) u.cur)  (strand-fail %bad-currency-format ~)
;<  =json  bind:m  (fetch-json (weld url (trip u.cur)))
=/  price=(unit @ta)  ((ot ~[bitcoin+(ot [u.cur no]~)]) json)
?~  price  ((slog 'Currency not found.' ~) (pure:m !>(~)))
%-  (slog leaf+"{(trip u.price)} {(cuss (trip u.cur))}" ~)
(pure:m !>(~))