::
::  control plane for orca
::
/-  *orca, store=graph-store, *resource, mdst=metadata-store
/+  rudder
::
^-  (page:rudder state-0 srkw)
::
|_  $:  bol=bowl:gall
        odo=order:rudder
        sat=state-0
    ==
::
++  final  (alert:rudder 'orca' build)
::
++  argue
  |=  [headers=header-list:http body=(unit octs)]
  ^-  $@(brief:rudder srkw)
  ?~  body  ~
  =/  argyle=(list [k=@t v=@t])
    (fall (rush q.u.body yquy:de-purl:html) ~)
  =/  args=(map @t @t)
    (malt argyle)
  ::  form-pod
  ?:  ?&  (~(has by args) 'action-form')
          (~(has by args) 'fin')
          (~(has by args) 'res')
      ==
    =/  rus=(unit resource)
      %+  rush  (~(got by args) 'res')
      ;~((glue bar) ;~(pfix sig fed:ag) sym)
    ?~  rus  '%orca error -> invalid resource selection'
    =/  fus=(unit term)
      (rush (~(got by args) 'fin') ;~(pfix cen sym))
    ?~  fus  '%orca error -> invalid fluke pattern'
    `srkw`[%form u.fus u.rus]
  ::  call-mates
  ?:  ?&  (~(has by args) 'action-call')
          (~(has by args) 'res')
          (~(has by args) 'add')
      ==
    =/  rus=(unit resource)
      %+  rush  (~(got by args) 'res')
      %+  ifix  [sel ser]
      ;~((glue ace) ;~(pfix sig fed:ag) ;~(pfix cen sym))
    ?~  rus  '%orca error -> invalid resource selection'
    =/  dud=(unit (list @p))
      %+  rush  (~(got by args) 'add')
      (most ace ;~(pfix sig fed:ag))
    ?~  dud  '%orca error -> invalid ship selection'
    `srkw`[%call u.rus (sy u.dud)]
  ::  swim-away
  ?:  ?&  (~(has by args) 'action-swim')
          (~(has by args) 'res')
      ==
    =/  rus=(unit resource)
      %+  rush  (~(got by args) 'res')
      %+  ifix  [sel ser]
      ;~((glue ace) ;~(pfix sig fed:ag) ;~(pfix cen sym))
    ?~  rus  '%orca error -> invalid pod-mother'
    `srkw`[%swim u.rus]
  ::  hear-call
  ?:  ?&  (~(has by args) 'action-hear')
          (~(has by args) 'myn')
          (~(has by args) 'urs')
      ==
    =/  myn=(unit resource)
      %+  rush  (~(got by args) 'myn')
      ;~((glue bar) ;~(pfix sig fed:ag) sym)
    =/  urs=(unit resource)
      %+  rush  (~(got by args) 'urs')
      %+  ifix  [sel ser]
      ;~((glue ace) ;~(pfix sig fed:ag) ;~(pfix cen sym))
    ?:  |(?=(~ urs) ?=(~ myn))
      '%orca error -> lost tune on hear'
    `srkw`[%hear u.urs %.y u.myn]
  ::  form-echo
  ?:  ?&  (~(has by args) 'action-echo')
          (~(has by args) 'fin')
          (~(has by args) 'res')
      ==
    =/  rel=(list resource)
      %+  roll  argyle
      |=  [[k=@t v=@t] o=(list resource)]
      ?.  ?=(%res k)  o
      ?~  r=(rush v ;~((glue bar) ;~(pfix sig fed:ag) sym))
        o
      [u.r o]
    ?~  rel  '%orca error -> invalid resource selection'
    =/  fus=(unit term)
      (rush (~(got by args) 'fin') ;~(pfix cen sym))
    ?~  fus  '%orca error -> invalid fluke pattern'
    `srkw`[%echo u.fus (silt `(list resource)`rel)]
  ::  %more-echo
  ?:  ?&  (~(has by args) 'action-more')
          (~(has by args) 'res')
          (~(has by args) 'new')
      ==
    =/  rus=(unit resource)
      %+  rush  (~(got by args) 'res')
      %+  ifix  [sel ser]
      ;~((glue ace) ;~(pfix sig fed:ag) ;~(pfix cen sym))
    ?~  rus  '%orca error -> invalid pod-mother'
    =/  uwu=(list resource)
      %+  roll  argyle
      |=  [[k=@t v=@t] o=(list resource)]
      ?.  ?=(%new k)  o
      ?~  r=(rush v ;~((glue bar) ;~(pfix sig fed:ag) sym))
        o
      [u.r o]
    ?~  uwu  '%orca error -> invalid cousins'
    `srkw`[%more u.rus (silt uwu)]
  ::
  '%orca error -> invalid POST method'
::
++  build
  |=  $:  args=(list [k=@t v=@t])
          msg=(unit [gud=? txt=@t])
      ==
  |^  ^-  reply:rudder
  [%page page]
  ::
  ++  page
    ^-  manx
    ;html
      ;head
        ;title:"🐳 %orca control plane"
        ;style:"{(trip style)}"
        ;meta(charset "utf-8");
        ;meta
          =name     "viewport"
          =content  "width=device-width, initial-scale=1";
      ==
      ;body
      ::  show status
        ;+  ?~  msg  :/""
            ?:  gud.u.msg
              ;div#status.green:"{(trip txt.u.msg)}"
            ;div#status.red:"{(trip txt.u.msg)}"
      ::
        ;h3:"open calls:"
        ;h4:"select a local chat and click hear call to join"
        ;br;
        ;div(class "calls")
          ;*  `marl`(~(rep in cal.sat) sound)
        ==
      ::
        ;h3:"your herds:"
        ;h4:"for remote pods, you can"
        ;h4:"  - enter some ship names and click \"call mates\" to invite them"
        ;h4:"for echo podes, you can"
        ;h4:"  - enter some chat names and click \"call cousins\" to add more local resources"
        ;h4:"for both, clicking \"swim away\" disbands the pod"
        ;br;
        ;div(class "podes")
          ;*  `marl`(~(rep by fam.sat) round)
        ==
      ::
        ;h3:"form remote pod:"
        ;h4:"to form a remote pod, enter a fluke pattern, select a local resource and click \"form pod\""
        ;br;
        ;div(class "remote")
          ;form(method "post")
            ;div(class "remote-internals")
              ;lable(for "fin"):"fluke pattern: "
              ;input
                =name         "fin"
                =type         "text"
                =required     ""
                =placeholder  "%my-pod";
            ::
              ;label(for "res"):"pod-mother: "
              ;select
                =name      "res"
                =required  ""
                ;option(value "", hidden ""):"pik me ⬇️"
                ;*  ?:  =(~ metas)
                      ;=  ;option(value "", hidden ""):"Empty"
                      ==
                    ^-  marl
                    %-  ~(rep in metas)
                    |=  [res=resource out=marl]
                    ^-  marl
                    :_  out
                    ;option
                        =name  "resource"
                        =value  "{(scow %p -.res)}|{(scow %tas +.res)}"
                    ;div:"{(scow %p -.res)} {(scow %tas +.res)}"
                    ==
              ==
            ::
              ;button
                =name   "action-form"
                =type   "submit"
                =value  "form"
                ; %form pod 🤰
              ==
            ==
          ==
        ==
      ::
        ;h3:"form echo pod:"
        ;h4:"to form an echo pod, enter a fluke pattern, select more than 1 local resource and click \"form pod\""
        ;br;
        ;div(class "echo")
          ;form(method "post")
            ;div(class "echo-internals")
              ;lable(for "fin"):"fluke pattern: "
              ;input
                =name         "fin"
                =type         "text"
                =required     ""
                =placeholder  "%my-pod";
            ::
              ;label(for "res"):"pod-cousins: "
              ;select
                =name      "res"
                =required  ""
                =multiple  ""
                ;option(value "", disabled "", selected ""):"pik many ⬇️⬇️"
                ;*  ?:  =(~ metas)
                      ;=  ;option(value "", hidden ""):"Empty"
                      ==
                    ^-  marl
                    %-  ~(rep in metas)
                    |=  [res=resource out=marl]
                    ^-  marl
                    ?.  =(our.bol -.res)
                      out
                    :_  out
                    ;option
                        =name  "resource"
                        =value  "{(scow %p -.res)}|{(scow %tas +.res)}"
                    ;div:"{(scow %p -.res)} {(scow %tas +.res)}"
                    ==
              ==
            ::
              ;button
                =name   "action-echo"
                =type   "submit"
                =value  "echo"
                ; %form echo 👪
              ==
            ==
          ==
        ==
      ::
      ==
    ==
  ::
  ++  sound
    |=  [[p=pod r=resource] out=marl]
    :_  [[[%br ~] ~] out]
    ;div(class "cal-{(scow %tas fin.p)}")
      ;form(method "post")
        ;div(class "cal-internals")
          ;label(for "fin"):"fluke pattern: "
          ;input
            =name      "fin"
            =type      "text"
            =required  ""
            =readonly  ""
            =value     "%{(scow %tas fin.p)}";
        ::
          ;label(for "urs"):"pod-father: "
          ;input
            =name      "urs"
            =type      "text"
            =required  ""
            =readonly  ""
            =value     "[{(scow %p entity.r)} %{(scow %tas name.r)}]";
        ::
          ;label(for "who"):"pod-mates: "
          ;+  =;  whose=[dem=tape cou=@ud]
                ;textarea
                  =name         "who"
                  =class        "members"
                  =readonly     ""
                  =rows         "cou.whose"
                  =placeholder  "{dem.whose}";
              %-  ~(rep in who.p)
              |=  [re=resource ou=[t=tape c=@ud]]
              :_  +(c.ou)
              %-  weld  :_  `tape`['\0d' t.ou]
              "[{(scow %p entity.re)} {(scow %tas name.re)}]"
        ::
          ;label(for "myn"):"myn: "
          ;select
            =name      "myn"
            =required  ""
            ;option(value "", hidden ""):"pik me ⬇️"
            ;*  ?:  =(~ metas)
                  ;=  ;option(value "", hidden ""):"Empty"
                  ==
                ^-  marl
                %-  ~(rep in metas)
                |=  [res=resource out=marl]
                ^-  marl
                :_  out
                ;option
                    =name  "resource"
                    =value  "{(scow %p -.res)}|{(scow %tas +.res)}"
                ;div:"{(scow %p -.res)} {(scow %tas +.res)}"
                ==
          ==
        ::
          ;button
            =name   "action-hear"
            =type   "submit"
            =name   "hear"
            ; %hear call 🎶
          ==
        ==
      ==
    ==
  ::
  ++  round
    |=  [[r=resource p=pod] out=marl]
    :_  [[[%br ~] ~] out]
    ;div(class "pod-{(scow %tas fin.p)}")
      ;form(method "post")
        ;div(class "pod-internals")
          ;label(for "res"):"pod-mother: "
          ;input
            =name      "res"
            =type      "text"
            =required  ""
            =readonly  ""
            =value     "[{(scow %p entity.r)} %{(scow %tas name.r)}]";
        ::
          ;label(for "fin"):"fin: "
          ;input
            =name      "fin"
            =type      "text"
            =required  ""
            =readonly  ""
            =value     "%{(scow %tas fin.p)}";
        ::
          ;label(for "who"):"pod-mates: "
          ;+  =;  whose=[dem=tape cou=@ud]
                ;textarea
                  =name         "who"
                  =class        "members"
                  =readonly     ""
                  =rows         "cou.whose"
                  =placeholder  "{dem.whose}";
              %-  ~(rep in who.p)
              |=  [re=resource ou=[t=tape c=@ud]]
              :_  +(c.ou)
              %-  weld  :_  `tape`['\0d' t.ou]
              "[{(scow %p entity.re)} {(scow %tas name.re)}]"
        ::
          ;label(for "eco"):"eco? "
          ;+  ?.  eco.p
                ;input
                  =name      "eco"
                  =type      "checkbox"
                  =disabled  "";
              ;input
                  =name      "eco"
                  =type      "checkbox"
                  =disabled  ""
                  =checked   "";
        ::
          ;*  ?.  eco.p
                ;=  ;label(for "add"):"mates: "
                    ;input
                      =name         "add"
                      =type         "text"
                      =placeholder  "~walrus ~samlun";
                    ;button
                      =name   "action-call"
                      =type   "submit"
                      =value  "call"
                      ; %call mates 🎶
                    ==
                ==
              ;=  ;label(for "res"):"pod-cousins: "
                  ;select
                    =name      "new"
                    =required  ""
                    =readonly  ""
                    =multiple  ""
                    ;option(value "", disabled "", selected ""):"pik many ⬇️⬇️"
                    ;*  ?:  =(~ metas)
                          ;=  ;option(value "", hidden ""):"Empty"
                          ==
                        ^-  marl
                        %-  ~(rep in metas)
                        |=  [res=resource out=marl]
                        ^-  marl
                        ?.  =(our.bol -.res)
                          out
                        :_  out
                        ;option
                          =name  "resource"
                          =value  "{(scow %p -.res)}|{(scow %tas +.res)}"
                          ; {(scow %p -.res)} \%{(scow %tas +.res)}
                        ==
                  ==
                  ;button
                    =name   "action-more"
                    =type   "submit"
                    =value  "more"
                    ; %call cousins 🎶
                  ==
              ==
        ::
          ;button
            =name   "action-swim"
            =type   "submit"
            =value  "swim"
            ; %swim away 🌊
          ==
        ==
      ==
    ==
  ::
  ++  metas
    %-  sy
    ^-  (list resource)
    %-  %~  rep  by
        ^-  associations:mdst
        =-  .^(associations:mdst %gx -)
        :~  (scot %p our.bol)   %metadata-store
            (scot %da now.bol)  %associations  %noun
        ==
    |=  $:  [p=md-resource:mdst q=association:mdst]
            out=(list resource)
        ==
    ?.  ?=(%graph -.p)
      out
    =*  met  metadatum.q
    ?.  ?=([%graph %chat] config.met)
      out
    [resource.p out]
  ::
  ++  style
    '''
    * { margin: 0.2em; padding: 0.2em; font-family: monospace; }
    .red {
      color: red;
    }

    .green {
      color: green;
    }
    '''
  ::
  --
--