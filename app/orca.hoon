::
::  orca app file
::
::  For my Father-in-Law. Here, in this application, are 
::  the required elements to make shared songs between
::  many individuals.
::
::  May they grow harmoneously and may the melody of life
::  and The Word play for 10,000 years, to guide them.
::
/-  *orca, post, store=graph-store, medal=metadata-store
/+  default-agent, dbug, *resource, signs=signatures, rudder
/~  pages  (page:rudder state-0 srkw)  /app/orca
::
|%
+$  versioned-state
    $%  state-zero
    ==
::
::  state-zero
::
::  fam - your currently federated resources,
::        and their pods
::  pup - outgoing requests to increase your pod
::  cal - incoming requests to join a pod
::  mem - avoid infinite loop
::
+$  state-zero
  $:  %0
      state-0
  ==
::
+$  card      card:agent:gall
+$  content   content:post
+$  eyre-id   @ta
--
::
%-  agent:dbug
=|  state-zero
=*  state  -
^-  agent:gall
=<
::!.
|_  =bowl:gall
+*  this  .
    fish  ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  %-  (slog leaf+"%orca lifts fluke (online)" ~)
  :_  this
  ^-  (list card)
  :~  :*  %pass   [%orca %spyhop ~]
          %agent  [our.bowl %graph-store]
          %watch  [%updates ~]
      ==
      :*  %pass   [%orca %migrate %pods %init (scot %da now.bowl) ~]
          %arvo   %b
          %wait   (add now.bowl ~m10)
      ==
      :*  %pass   [%eyre %connect ~]
          %arvo     %e
          %connect  [[~ [%apps %orca ~]] dap.bowl]
  ==  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  ole=vase
  =+  cards=*(list card)
  =/  old=versioned-state  !<(versioned-state ole)
  ?-    -.old
      %0
    =?    cards
        =;  eyre-stat=(list [binding:eyre duct action:eyre])
          !(~(has in ~(key by (malt eyre-stat))) [~ /apps/orca])
        .^  (list [binding:eyre duct action:eyre])  %e 
            ~[(scot %p our.bowl) %bindings (scot %da now.bowl)]
        ==
      :_  cards
      =+  [[~ [%apps %orca ~]] dap.bowl]
      [%pass /eyre/connect %arvo %e %connect -]    
    =?    cards
        ?~  kez=(~(get by wex.bowl) [/orca/spyhop our.bowl %graph-store])
        %.y  !-.u.kez
      :_  cards
      :*  %pass   [%orca %spyhop ~]
          %agent  [our.bowl %graph-store]
          %watch  [%updates ~]
      ==
    [cards this(state old)]
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?:  =(%handle-http-request mark)
    =;  out=(quip card state-0)
      [-.out this(state [%0 +.out])]
    %.  [bowl !<(order:rudder vase) +.state]
    %:  (steer:rudder state-0 srkw)
      pages
    ::
      |=  =trail:rudder
      ^-  (unit place:rudder)
      ?~  site=(decap:rudder /apps/orca site.trail)  ~
      ?+  u.site  ~
        ~       `[%page & %control-plane]
        [%$ ~]  `[%away /apps/orca]
      ==
    ::
      |=  =order:rudder
      ^-  [[(unit reply:rudder) (list card)] state-0]
      =;  msg=@t  [[`[%code 404 msg] ~] +.state]
      %+  rap  3
      ~['orca error -> page=' url.request.order ' not found']
    ::
      |=  act=srkw
      ^-  $@(@t [brief:rudder (list card) state-0])
      (bro:fish act)
    ==
  =^  cards  state
    ?+    mark  `state
        %orca-song
      ::  mostly handled in +on-agent, save agree and tempt
      =/  vaz=sing  !<(sing vase)
      ?-    -.vaz
          %biggs
        ::  add a new message to your version.
        ::  make sure this doesn't create an infinite loop.
        `state
      ::
          %diggs
        ::  add new signatures to your version.
        ::  make sure this doesn't create an infinite loop.
        `state
      ::
          %tempt
        ::  receives an offer to join a pod
        ?:  (~(has in cal) por.vaz)
          `state
        `state(cal (~(put in cal) por.vaz))
      ::
          %agree
        ::  accept agrees, but check pup, then subscribe.
        ?.  (~(has ju pup) src.bowl pod.por.vaz)
          `state
        =/  wiu=(unit resource)
          (~(wit mom:fish pod.por.vaz) our.bowl)
        ?~  wiu
          `state(pup (~(del ju pup) src.bowl pod.por.vaz))
        ?.  (~(has by fam) u.wiu)
          `state(pup (~(del ju pup) src.bowl pod.por.vaz))
        =/  pew=pod  (~(got by fam) u.wiu)
        =/  new=(set resource)
          (~(put in who.pew) res.por.vaz)
        =.  pew  pew(who new)
        =.  fam  (~(put by fam) u.wiu pew)
        =/  wir=path
          :~  %spake
              ::  us
              (scot %p entity.u.wiu)        (scot %tas name.u.wiu)
              ::  them
              (scot %p entity.res.por.vaz)  (scot %tas name.res.por.vaz)
              (scot %tas fin.pod.por.vaz)
          ==   
        =/  pat=path
          :~  %spake
              ::  them
              (scot %p entity.res.por.vaz)  (scot %tas name.res.por.vaz)
              (scot %tas fin.pod.por.vaz)
          ==
        =/  pad=path
          :~  %spake
              ::  us
              (scot %p entity.u.wiu)        (scot %tas name.u.wiu)
              (scot %tas fin.pod.por.vaz)
          ==
        :_  state(pup (~(del ju pup) src.bowl pod.por.vaz))
        ::  announce the newcomer and subscribe to them
        %-  flop  ^-  (list card)
        :~  :^  %give  %fact  [pad]~
            [%orca-song !>(`sing`[%unite who.pew])]
            :*  %pass   wir
                %agent  [src.bowl %orca]
                %watch  pat
        ==  == 
      ::
          %leave
        ::  make sure to remove them from who in the pod
        `state
      ::
          %unite
        ::  instruct everyone to listen
        `state
      ==
    ::
        %orca-dive
      ::  user actions, only (team:title our.bowl src.bowl)
      ?>  (team:title our.bowl src.bowl)
      =/  vaz=srkw  !<(srkw vase)
      ?-    -.vaz
          %form
        =/  old=(set resource)
          =-  (~(uni in ~(key by fam)) -)
          %+  roll  ~(val by fam)
          |=  [[@ who=(set resource) @] o=(set resource)]
          %-  ~(uni in o)
          ^-  (set resource)
          %-  ~(rep in who)
          |=  [i=resource o=(set resource)]
          ?.(=(our.bol -.i) o (~(put in o) i))
        ?:  (~(has in old) res.vaz)
          %-  (slog leaf+"%orca-error - resource already in use" ~)
          `state
        =+  pud=[fin.vaz (silt ~[res.vaz]) %.n]
        =.  fam  (~(put by fam) [res.vaz pud])
        %-  (slog leaf+"%orca formed pod {<fin.vaz>}" ~)
        `state
      ::
          %call
        ?.  (~(has by fam) res.vaz)
          %-  (slog leaf+"%orca-error - could not find pod-father" ~)
          `state
        =/  per=por
          [(~(got by fam) res.vaz) res.vaz]
        ?<  eco.pod.per
        =/  adl=(list ship)
          %~  tap  in
          (~(dif in add.vaz) ~(aar mom:fish pod.per))
        =+  coz=*(list card)
        |-  
        ?~  adl
          [coz state]
        =/  wir=path
          :~  %tempt
              (scot %p i.adl)
              (scot %p entity.res.vaz)
              (scot %tas name.res.vaz)
          ==
        %=    $
            adl
          t.adl
        ::
            coz
          %+  welp  coz
          :~  :*
            %pass   wir
            %agent  [i.adl %orca]
            %poke   %orca-song
            !>(`sing`[%tempt per])
          ==  ==
        ==
      ::
          %swim
        =/  podes=(unit pod)
          (~(get by fam) res.vaz)
        ?~  podes
          %-  (slog leaf+"%orca-error - could not find pod-father" ~)
          `state
        =.  fam  (~(del by fam) res.vaz)
        %-  (slog leaf+"%orca left pod {<fin.u.podes>}" ~)
        `state
      ::
          %hear
        =/  pam=pods
          %-  ~(rep in cal)
          |=  [inn=por out=pods]
          (~(put by out) res.inn pod.inn)
        ?.  (~(has by pam) urs.vaz)
          %-  (slog leaf+"%orca-error - could not find {<name.urs.vaz>}" ~)
          `state
        =+  pid=(~(got by pam) urs.vaz)
        ?.  luv.vaz
          `state(cal (~(del in cal) [pid urs.vaz]))
        ?.  =(our.bowl entity.myn.vaz)
          %-  (slog leaf+"%orca-error - cannot add foreign resource to pod" ~)
          `state
        =.  fam  (~(put by fam) myn.vaz pid)
        =/  wir=path
          :~  
            %range
            (scot %p entity.myn.vaz)
            (scot %tas name.myn.vaz)
            (scot %p entity.urs.vaz)  
            (scot %tas name.urs.vaz)
            (scot %tas fin.pid)
          ==
        %-  (slog leaf+"%orca follows pod {<fin.pid>}" ~)
        :_  state(cal (~(del in cal) [pid urs.vaz]))
        :~  :*
          %pass   wir
          %agent  [entity.urs.vaz %orca]
          %poke   %orca-song
          !>(`sing`[%agree pid myn.vaz])
        ==  ==
      ::
          %echo
        =/  new=(set resource)
          =-  %-  ~(dif in rez.vaz) 
              (~(uni in ~(key by fam)) -)
          %+  roll  ~(val by fam)
          |=  [[@ who=(set resource) @] o=(set resource)]
          %-  ~(uni in o)  ^-  (set resource)
          %-  ~(rep in who)
          |=  [i=resource o=(set resource)]
          ?.(=(our.bol -.i) o (~(put in o) i))
        ?:  =(~(wyt in new) 0)
          %-  (slog leaf+"%orca-error - need >1 resource for echo" ~)
          `state
        =+  pud=[fin.vaz rez.vaz %.y]
        =+  lew=(snag 0 ~(tap in new))
        =.  fam  (~(put by fam) [lew pud])
        %-  (slog leaf+"%orca formed echo-pod {<fin.vaz>}" ~)
        `state
      ::
          %more
        =/  new=(set resource)
          =-  %-  ~(dif in rez.vaz) 
              (~(uni in ~(key by fam)) -)
          %+  roll  ~(val by fam)
          |=  [[@ who=(set resource) @] o=(set resource)]
          %-  ~(uni in o)  ^-  (set resource)
          %-  ~(rep in who)
          |=  [i=resource o=(set resource)]
          ?.(=(our.bol -.i) o (~(put in o) i))
        ?>  (~(has by fam) res.vaz)
        =+  pew=(~(got by fam) res.vaz)
        ?.  eco.pew
          %-  (slog leaf+"%orca-error - more is only for echo pods" ~)
          `state
        ?:  =(~(wyt in new) 0)
          %-  (slog leaf+"%orca-error - >= new resources required" ~)
          `state
        =.  pew  pew(who (~(uni in who.pew) new))
        =.  fam  (~(put by fam) [res.vaz pew])
        %-  (slog leaf+"%orca added to {<fin.pew>}" ~)
        `state
      ==
    ==
  [cards this]
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  |^  ^-  (quip card _this)
  ?+    wire  `this
      [%orca %spyhop ~]                                  ::  watch graph-store [%updates ~]
    ?+    -.sign  `this
        %kick                                            ::  resubscribe on kick
      :_  this
      =-  [%pass /orca/spyhop %agent -]~
      [[our.bowl %graph-store] %watch [%updates ~]]
    ::
        %watch-ack                                       ::  error on watch-nack
      ?~  p.sign  `this
      %.  `this
      (slog leaf+"%orca-error -failed-subscription-to-graph")
    ::
        %fact                                            ::  handle incoming messages on [%updates ~]
      ?.  ?=(%graph-update-3 p.cage.sign)  `this
      =/  act=action  +:!<(update q.cage.sign)
      ?+    -.act  `this
          %add-signatures                                ::::  pass signatures around
        ?~  podos=(~(get by fam) resource.uid.act)  `this
        :_  this
        %-  ~(mar mom:fish u.podos)
        [resource.uid.act index.uid.act signatures.act]
      ::
          %add-nodes                                     ::::  pass nodes around
        =/  mec=(map resource resource)
          (~(rep by fam) dad:fish)
        =/  rus=(unit resource)
          ^-  (unit resource)
          ?.  (~(has in ~(key by fam)) resource.act)
          (~(get by mec) resource.act)  `resource.act
        ?~  rus  `this
        ?:  (~(has in mem) nodes.act)
          `this(mem (~(del in mem) nodes.act))
        :_  this
        (~(sea mom:fish (~(got by fam) u.rus)) act)
      ==
    ==
  ::
      [%range @ @ @ @ @ ~]                               ::  Check for poke ack inter-ship comms
    ?+    -.sign  `this
        %poke-ack
      =/  mines=resource
        [(slav %p +<.wire) (slav %tas +>-.wire)]
      =/  yours=resource
        [(slav %p +>+<.wire) (slav %tas +>+>-.wire)]
      =/  fluke=term  (slav %tas +>+>+<.wire)
      =/  podes=pod   (~(got by fam) mines)
      ?>  =(fluke fin.podes)
      =;  [wir=path pat=path]
        :_  this
        [%pass wir %agent [entity.yours %orca] %watch pat]~
      :_
        ;:  weld
          /spake
          /(scot %p entity.yours)
          /(scot %tas name.yours)
          /(scot %tas fluke)
        ==
      %+  weld
        /spake/(scot %p entity.mines)/(scot %tas name.mines)
      /(scot %p entity.yours)/(scot %tas name.yours)/(scot %tas fluke)
    ==
  ::
      [%tempt @ @ @ ~]
    ?+    -.sign  `this
        %poke-ack
      =/  your=ship  (slav %p +<.wire)
      =/  mine=resource
        [(slav %p +>-.wire) (slav %tas +>+<.wire)]
      ?>  (~(has by fam) mine)
      =.  pup
        (~(put ju pup) your (~(got by fam) mine))
      `this
    ==
  ::
  ::  wire=[%spake our-resource their-resource fin]
      [%spake @ @ @ @ @ ~]
    ?+    -.sign  `this
        %fact
      ?.  ?=(%orca-song p.cage.sign)  `this
      =/  mines=resource
        [(slav %p +<.wire) (slav %tas +>-.wire)]
      =/  yours=resource
        [(slav %p +>+<.wire) (slav %tas +>+>-.wire)]
      =/  fluke=term         (slav %tas +>+>+<.wire)
      =/  podes=(unit pod)   (~(get by fam) mines)
      ::
      ?~  podes
        :_  this
        [%pass wire %agent [src.bowl %orca] %leave ~]~
      ::
      ?.  =(fluke fin.u.podes)
        :_  this
        [%pass wire %agent [src.bowl %orca] %leave ~]~
      ::
      =/  note=sing  !<(sing +>.sign)
      ?+    -.note  `this
          %biggs
        =/  wav
          %-  ~(rep by seal.note)
          |=  [[ind=index:store nod=node:store] out=wave]
          ?.  ?=(%.y -.post.nod)  out
          =+  pb=p.post.nod
          =.  p.post.nod
            =-  p.post.nod(contents -)
            %+  turn  contents.p.post.nod
            |=  con=content:store
            ?.  ?=([%reference @ *] con)  con
            ?.  ?=([%graph [@ @] [[@ @] *]] +.con)  con
            =*  cuz  ~(has in who:(~(got by fam) mines))
            ?.  (cuz [+>+<-.con +>+<+.con])  con
            :+  %reference  %graph
            :_  [mines +>+>.con]
            %.  [%graph mines]
            =-  ~(got by .^((map md-resource:medal resource) %gy -))
            ;:  weld
              /(scot %p our.bowl)
              /metadata-store
              /(scot %da now.bowl)
              /resource-indices
            ==
          =?    hash.p.post.nod
              !=(pb p.post.nod)
            ^-  (unit @ux)
            :-  ~  ;;  @ux
            %-  sham
            :+  ~  author.p.post.nod
            [time-sent.p.post.nod contents.p.post.nod]
          =?    signatures.p.post.nod
              !=(pb p.post.nod)
            ~
            ::(sy ~[(sign:signs our.bowl now.bowl (need hash.p.post.nod))])
          (~(put by out) ind [[%.y p.post.nod] children.nod])
        =/  notes=action:store  [%add-nodes mines wav]
        :_  this(mem (~(put in mem) wav))
        :~  :*  %pass   /blip/(scot %tas fluke)
                %agent  [our.bowl %graph-store]
                %poke   %graph-update-3
                !>(`update:store`[now.bowl notes])
        ==  ==
      ::
          %diggs
        =/  exists=path
          :*  (scot %p our.bowl)  %graph-store  (scot %da now.bowl)
              %graph  (scot %p entity.mines)  (scot %tas name.mines)
              [%node %exists (weld (turn ind.note (cury scot %ud)) /noun)]
          ==
        =/  result=path
          :*  (scot %p our.bowl)  %graph-store  (scot %da now.bowl)
              %graph  (scot %p entity.mines)  (scot %tas name.mines)
              [%node %index %lone (weld (turn ind.note (cury scot %ud)) /noun)]
          ==
        ?.  .^(flag %gx exists)  `this
        =/  pengu=update:store
          .^(update:store %gx result)
        ?>  ?=([%add-nodes ^ *] q.pengu)
        =+  czech=`(map index:store node:store)`+>.q.pengu
        =/  sigur=signatures:store
          %-  ~(rep by czech)
          |=  [[i=index:store n=node:store] o=signatures:store]
          ?.  ?=(%.y -.post.n)  o
          (~(uni in o) signatures.p.post.n)
        =/  ros=signatures:store
          (~(dif in sig.note) sigur)
        ?:  =(~ ros)  `this
        =/  signs=action:store
          [%add-signatures [mines ind.note] ros]
        :_  this
        :~  :*  %pass   /blip/(scot %tas fluke)
                %agent  [our.bowl %graph-store]
                %poke   %graph-update-3
                !>(`update:store`[now.bowl signs])
        ==  ==
      ::
          %leave
        =.  podes  podes(who.u (~(del in who.u.podes) res.note))
        =.  fam    (~(put by fam) mines u.podes)
        `this
      ::
          %unite
        =.  podes  podes(who.u (~(uni in who.u.podes) ser.note))
        =;  cards=(list card)
          [cards this(fam (~(put by fam) mines u.podes))]
        %+  murn  ~(tap in ser.note)
        |=  res=resource
        ?:  =(entity.res our.bowl)  ~
        =/  wir=path
          :~  %spake
              (scot %p entity.mines)  (scot %tas name.mines)
              (scot %p entity.res)    (scot %tas name.res)
              (scot %tas fluke)
          ==
        =/  pat=path
          :~  %spake
              (scot %p entity.res)  (scot %tas name.res)
              (scot %tas fluke)
          ==
        =/  haz=(unit [acked=? =path])
          (~(get by wex.bowl) [wir entity.res %orca])
        ?~  haz  `[%pass wir %agent [entity.res %orca] %watch pat]
        ?:  =(u.haz [%.y pat])  ~
        `[%pass wir %agent [entity.res %orca] %watch pat]
      ==  
    ==
  ==
  --  
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
  ::  [%http-response *]
      [%http-response *]
    `this
  ::  [%spake watched-resource fin]
      [%spake @ @ @ ~]
    =/  theme=resource  [(slav %p +<.path) (slav %tas +>-.path)]
    =/  fluke=term      (slav %tas +>+<.path)
    ?~  podos=(~(get by fam) theme)  !!
    ?.  =(fluke fin.u.podos)         !!
    ?>  (~(huh mom:fish u.podos) src.bowl)
      ::  everything seems right
    =/  lyric=(list content)
      ~[[%text 'pod-chat click:'] [%mention src.bowl] [%text 'hears this song']]
    =/  moist=wave
      %+  ~(put by *wave)  ~[now.bowl]
      [[%& [our.bowl ~[now.bowl] now.bowl lyric ~ ~]] [%empty ~]]
    =/  wired=^path
      [%echo (scot %da now.bowl) (scot %tas fin.u.podos) ~]
    :_  this
    :~
      :^  %give  %fact  ~
      [%orca-song !>(`sing`[%biggs u.podos moist])]
    ::
      :^  %give  %fact  ~
      [%orca-song !>(`sing`[%unite who.u.podos])]
    ::
      :^  %pass  wired  %agent
      :^  [our.bol %graph-store]  %poke  %graph-update-3
      !>(`update:store`[now.bol [%add-nodes theme moist]])
    ==
  ==
::
++  on-leave
  |=  pat=path
  ^-  (quip card _this)
  ?+    pat  `this
  ::  [%spake watched-resource fin]
      [%spake @ @ @ ~]
    =/  theme=resource
      [(slav %p +<.pat) (slav %tas +>-.pat)]
    =/  fluke=@tas  (slav %tas +>+<.pat)
    =/  podes=(unit pod)  (~(get by fam) theme)
    ?~  podes
      `this
    ?.  =(fluke fin.u.podes)
      `this
    =+  gosos=(~(wit mom:fish u.podes) src.bowl)
    ?~  gosos
      `this
    =+  copla=[%leave u.gosos]
    :_  this
    `(list card)`[%give %fact [pat]~ %orca-song !>(`sing`copla)]~
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  =/  alarm=(list card)
    :~  :+  %pass  [%orca %migrate %pods %arvo (scot %da now.bowl) ~]
        [%arvo %b [%wait (add now.bowl ~m30)]]
    ==
  ?.  ?=([%orca %migrate %pods @ @ ~] wire)  [alarm this]
  :_  this(mem ~)
  %+  welp  alarm
  %-  ~(rep in (sy ~(val by fam)))
  |=  [inn=pod out=(list card)]
  ?~  wiu=(~(wit mom:fish inn) our.bowl)  out
  =/  hav=(set resource)
    %-  sy
    %+  murn  ~(tap by wex.bowl)
    |=  inn=$:(* [a=? p=path])
    ?.  ?=([%spake @ @ @ ~] p.inn)  ~
    `[(slav %p +<.p.inn) (slav %tas +>-.p.inn)]
  =+  ned=~(tap in (~(dif in who.inn) hav))
  |-
  ?~  ned  out
  ?:  =(our.bowl entity.i.ned)  out
  =/  wir=path
    :~  %spake
        (scot %p entity.u.wiu)  (scot %tas name.u.wiu)
        (scot %p entity.i.ned)  (scot %tas name.i.ned)
        (scot %tas fin.inn)
    ==   
  =/  pat=path
    :~  %spake
        (scot %p entity.i.ned)  (scot %tas name.i.ned)
        (scot %tas fin.inn)
    ==
  %=    $
    ned  t.ned
  ::
      out
    %+  welp  out  
    :~  :^  %pass  wir  %agent
        [[entity.i.ned %orca] %watch pat]
    ==
  ==
::
++  on-peek  on-peek:def
++  on-fail  on-fail.def
--
::
|_  bol=bowl:gall
::
++  dad
  |=  [inn=[res=resource =pod] out=(map resource resource)]
  ^-  (map resource resource)
  ?.  eco.pod.inn  out
  =+  rel=~(tap in who.pod.inn)
  |-
  ?~  rel  out
  %=    $
    rel  t.rel
    out  (~(put by out) i.rel res.inn)
  ==
::
++  mom
  |_  p=pod
  ::
  ++  aar
    =+  [luq=~(tap in who.p) kig=*(set ship)]
    ^-  (set ship)
    |-
    ?~  luq  kig
    $(luq t.luq, kig (~(put in kig) entity.i.luq))
  ::
  ++  wit
    |=  sip=ship
    ^-  (unit resource)
    =+  woh=(malt ~(tap in who.p))
    ?.  (~(has by woh) sip)  ~
    `[sip (~(got by woh) sip)]
  ::
  ++  huh
    |=  =ship
    (~(any in who.p) |=(who=resource =(entity.who ship)))
  ::
  ++  mar
    |=  act=[res=resource ind=index sig=signatures]
    ^-  (list card)
    =+  tun=[%diggs p ind.act sig.act]
    =/  pat=path
      :~  %spake
          (scot %p entity.res.act)  (scot %tas name.res.act)
          (scot %tas fin.p)
      ==
    [%give %fact [pat]~ [%orca-song !>(`sing`tun)]]~
  ::
  ++  sea
    |=  act=[%add-nodes res=resource nodes=wave]
    ^-  (list card)
    =+  [herd=~(tap in who.p) out=*(list card)]
    =+  tun=[%biggs p nodes.act]
    =/  pat=path
      :~  %spake
          (scot %p entity.res.act)  (scot %tas name.res.act)
          (scot %tas fin.p)
      ==
    |-
    ?~  herd
      :_  out
      [%give %fact [pat]~ [%orca-song !>(`sing`tun)]]
    ?.  =(our.bol entity.i.herd)  $(herd t.herd)
    ?:  =(res.act i.herd)  $(herd t.herd)
    =/  n=(map index:store node:store)  nodes.act
    =.  n
      %-  ~(rep by n)
      |=  [[ind=index:store nod=node:store] out=wave]
      ?.  ?=(%.y -.post.nod)  out
      =+  pb=p.post.nod
      =.  p.post.nod
        =-  p.post.nod(contents -, hash ~)
        %+  turn  contents.p.post.nod
        |=  con=content:store
        ?.  ?=([%reference @ *] con)  con
            ::  graph  group  uid/resource
        ?.  ?=([%graph [@ @] [[@ @] *]] +.con)  con
        ?.  (~(has in who.p) [+>+<-.con +>+<+.con])  con
        :+  %reference  %graph
        :_  [i.herd +>+>.con]
        %.  [%graph i.herd]
        =-  ~(got by .^((map md-resource:medal resource) %gy -))
        ;:  weld
          /(scot %p our.bol)
          /metadata-store
          /(scot %da now.bol)
          /resource-indices
        ==
        =?    hash.p.post.nod
            !=(pb p.post.nod)
          ^-  (unit @ux)
          :-  ~  ;;  @ux
          %-  sham
          :+  ~  author.p.post.nod
          [time-sent.p.post.nod contents.p.post.nod]
        =?    signatures.p.post.nod
            !=(pb p.post.nod)
          ~
          ::(sy ~[(sign:signs our.bol now.bol (need hash.p.post.nod))])
      (~(put by out) ind [[%.y p.post.nod] children.nod])
    %=    $
      herd  t.herd
    ::
        out
      %+  welp  out
      :~  
        :+  %pass   /echo/(scot %da now.bol)/(scot %tas fin.p)
        :+  %agent  [our.bol %graph-store]
        =-  [%poke %graph-update-3 -]
        !>(`update:store`[now.bol [%add-nodes i.herd n]])
      ==
    ==
  --
::
++  bro
  |=  act=srkw
  ^-  $@(@t [brief:rudder (list card) state-0])
  ?-    -.act
      %form
    =/  old=(set resource)
      =-  (~(uni in ~(key by fam)) -)
      %+  roll  ~(val by fam)
      |=  [[@ who=(set resource) @] o=(set resource)]
      %-  ~(uni in o)
      ^-  (set resource)
      %-  ~(rep in who)
      |=  [i=resource o=(set resource)]
      ?.(=(our.bol -.i) o (~(put in o) i))
    ?:  (~(has in old) res.act)
      ['orca-fail-re-use' ~ +.state]
    =+  pud=[fin.act (silt ~[res.act]) %.n]
    =.  fam  (~(put by fam) [res.act pud])
    ['orca-success-form' ~ +.state]
  ::
      %call
    ?.  (~(has by fam) res.act)
      ['orca-fail-bad-pod-mom' ~ +.state]
    =/  per=por
      [(~(got by fam) res.act) res.act]
    ?<  eco.pod.per
    =/  adl=(list ship)
      %~  tap  in
      (~(dif in add.act) ~(aar mom pod.per))
    =+  coz=*(list card)
    |-  
    ?~  adl
      ['orca-success-send-call' coz +.state]
    =/  wir=path
      :~  %tempt
          (scot %p i.adl)
          (scot %p entity.res.act)
          (scot %tas name.res.act)
      ==
    %=    $
      adl  t.adl
    ::
        coz
      %+  welp  coz
      :~  :*
        %pass   wir
        %agent  [i.adl %orca]
        %poke   %orca-song
        !>(`sing`[%tempt per])
      ==  ==
    ==
  ::
      %swim
    =/  podes=(unit pod)  (~(get by fam) res.act)
    ?~  podes
      ['orca-fail-bad-pod-mom' ~ +.state]
    =.  fam  (~(del by fam) res.act)
    ['orca-success-swim-away' ~ +.state]
  ::
      %hear
    =/  pam=pods
      (~(rep in cal) |=([inn=por out=pods] (~(put by out) res.inn pod.inn)))
    ?.  (~(has by pam) urs.act)
      ['orca-fail-pod-father-smokes' ~ +.state]
    =+  pid=(~(got by pam) urs.act)
    ?.  luv.act
      :+  'orca-success-ignore-call'
        ~ 
      +.state(cal (~(del in cal) [pid urs.act]))
    ?.  =(our.bol entity.myn.act)
      ['orca-fail-bad-resource' ~ +.state]
    =.  fam  (~(put by fam) myn.act pid)
    =/  wir=path
      :~  
        %range
        ::  us
        (scot %p entity.myn.act)  (scot %tas name.myn.act)
        ::  them
        (scot %p entity.urs.act)  (scot %tas name.urs.act)
        (scot %tas fin.pid)
      ==
    :-  'orca-success-hear-call'
    :_  +.state(cal (~(del in cal) [pid urs.act]))
    :~  :*
      %pass   wir
      %agent  [entity.urs.act %orca]
      %poke   %orca-song
      !>(`sing`[%agree pid myn.act])
    ==  ==
  ::
      %echo
    =/  new=(set resource)
      =-  %-  ~(dif in rez.act) 
          (~(uni in ~(key by fam)) -)
      %+  roll  ~(val by fam)
      |=  [[@ who=(set resource) @] o=(set resource)]
      %-  ~(uni in o)
      ^-  (set resource)
      %-  ~(rep in who)
      |=  [i=resource o=(set resource)]
      ?.(=(our.bol -.i) o (~(put in o) i))
    ?:  =(~(wyt in new) 0)
      ['orca-fail-bad-resources' ~ +.state]
    =+  pud=[fin.act rez.act %.y]
    =+  lew=(snag 0 ~(tap in new))
    ?.  =(our.bol -.lew)
      ['orca-fail-bad-lead-resource' ~ +.state]
    =.  fam  (~(put by fam) [lew pud])
    ['orca-success-form-echo' ~ +.state]
  ::
      %more
    =/  new=(set resource)
      =-  %-  ~(dif in rez.act) 
          (~(uni in ~(key by fam)) -)
      %+  roll  ~(val by fam)
      |=  [[@ who=(set resource) @] o=(set resource)]
      %-  ~(uni in o)  ^-  (set resource)
      %-  ~(rep in who)
      |=  [i=resource o=(set resource)]
      ?.(=(our.bol -.i) o (~(put in o) i))
    ?.  (~(has by fam) res.act)
      ['orca-fail-bad-resource' ~ +.state]
    =+  pew=(~(got by fam) res.act)
    ?.  eco.pew
      ['orca-fail-only-echo-more' ~ +.state]
    ?:  =(~(wyt in new) 0)
      ['orca-fail-bad-resources' ~ +.state]
    =.  pew  pew(who (~(uni in who.pew) new))
    =.  fam  (~(put by fam) [res.act pew])
    ['orca-success-find-more' ~ +.state]
  ==
--