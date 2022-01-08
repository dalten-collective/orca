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
/-  *orca, post, store=graph-store
/+  default-agent, dbug, *resource
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
      fam=pods
      pup=(jug ship pod)
      cal=(set por)
      mem=(set wave)
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
  ~&  >  [%orca %fluke %lift]
  :_  this
  ^-  (list card)
  :~  :*
    %pass   [%orca %spyhop ~]
    %agent  [our.bowl %graph-store]
    %watch  [%updates ~]
  ==  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  ole=vase
  ~&  >>  [%orca %fluke %slap]
  =/  old=versioned-state  !<(versioned-state ole)
  ?-    -.old
      %0
    =/  cards=(list card)
      :~  :*
        %pass   [%orca %spyhop ~]
        %agent  [our.bowl %graph-store]
        %watch  [%updates ~]
      ==  ==
    :_  this(state old)
    =/  wux  :-  [/orca/spyhop our.bowl %graph-store]
             [%.y /updates]
    ?:  (~(has in wex.bowl) wux)
      ~
   cards
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state
    ?+    mark  `state
        %orca-song
      ::  mostly handled in wires, save agree
      =/  vaz=sing  !<(sing vase)
      ?-    -.vaz
          %biggs
        ::  add a new message to your version.
        ::  make sure this doesn't create an infinite loop.
        `state
      ::
          %tempt
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
          !!
        =/  new=(set resource)
          (~(put in who.pod.por.vaz) res.por.vaz)
        =/  pew=pod  pod.por.vaz
        =.  pew  pew(who (~(uni in who.pew) new))
        =.  fam
          (~(put by fam) u.wiu pew)
        =/  wir=path
          :~  %spake
              (scot %p entity.u.wiu)  (scot %tas name.u.wiu)
              (scot %p entity.res.por.vaz)
              (scot %tas name.res.por.vaz)
              (scot %tas fin.pod.por.vaz)
          ==   
        =/  pat=path
          :~  %spake
              (scot %p entity.res.por.vaz)
              (scot %tas name.res.por.vaz)
              (scot %tas fin.pod.por.vaz)
          ==
        =/  pad=path
          :~  %spake
              (scot %p entity.u.wiu)
              (scot %tas name.u.wiu)
              (scot %tas fin.pod.por.vaz)
          ==
        :_  state(pup (~(del ju pup) src.bowl pod.por.vaz))
        :~  :*
          %pass   wir
          %agent  [src.bowl %orca]
          %watch  pat
            ==
            :*
          %give  %fact  [pad]~  %orca-song
          !>(`sing`[%unite who.pew])
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
        =+  pud=[fin.vaz (silt ~[res.vaz]) %.n]
        =.  fam  (~(put by fam) [res.vaz pud])
        ~&  >  [%orca %form %pod fin.vaz]
        `state
      ::
          %call
        ?.  (~(has by fam) res.vaz)
          ~&  >>>
            [%orca %spyhop %blank `@tas`'-' name.res.vaz]
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
          ~&  >>>  [%orca %spyhop %blank name.res.vaz ~]
          `state
        ~&  >  
          [%orca %spyhop name.res.vaz `@tas`'-' %left %pod ~]
        =.  fam
          (~(del by fam) res.vaz u.podes)
        `state
      ::
          %hear
        =/  pam=pods
          %-  ~(rep in cal)
          |=  [inn=por out=pods]
          (~(put by out) res.inn pod.inn)
        ?.  (~(has by pam) urs.vaz)
          ~&  >>>
            [%orca %spyhop name.urs.vaz ~]
          `state
        =+  pid=(~(got by pam) urs.vaz)
        ?.  luv.vaz
          `state(cal (~(del in cal) [pid urs.vaz]))
        ?.  =(our.bowl entity.myn.vaz)
          ~&  >>>  
            :~  %lobtail
                `@tas`(scot %p entity.myn.vaz)
                name.myn.vaz
            ==
          `state
        ~&  >  [%follow %pod]
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
        :_  state(cal (~(del in cal) [pid urs.vaz]))
        :~  :*
          %pass   wir
          %agent  [entity.urs.vaz %orca]
          %poke   %orca-song
          !>(`sing`[%agree pid myn.vaz])
        ==  ==
      ::
          %echo
        =+  new=(~(dif in rez.vaz) ~(key by fam))
        ?:  =(~(wyt in new) 0)
          ~&  >>>  [%orca %echo %need %rez ~]
          `state
        ~&  >  [%orca %hears %itself `@tas`'-' fin.vaz]
        =+  pud=[fin.vaz rez.vaz %.y]
        =+  lew=(snag 0 ~(tap in new))
        =.  fam  (~(put by fam) [lew pud])
        `state
      ::
          %more
        =+  new=(~(dif in rez.vaz) ~(key by fam))
        ?>  (~(has by fam) res.vaz)
        =+  pew=(~(got by fam) res.vaz)
        ?.  eco.pew
          ~&  >>>  [%orca %more %only %echo %pod ~]
          `state
        ?:  =(~(wyt in new) 0)
          ~&  >>>  [%orca %more %need %rez ~]
          `state
        ~&  >  [%orca %finds %more `@tas`'-' fin.pew]
        =.  pew  pew(who (~(uni in who.pew) new))
        =.  fam  (~(put by fam) [res.vaz pew])
        `state
      ==
    ==
  [cards this]
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  |^  ^-  (quip card _this)
  ?-    -.sign
      %kick
    ?+    wire  `this
        [%orca %spyhop ~]
      :_  this
      :~  :*
        %pass   [%orca %spyhop ~]
        %agent  [our.bowl %graph-store]
        %watch  [%updates ~]
      ==  ==
    ==
  ::
      %watch-ack
    ?~  p.sign
      `this
    ~&  >>>  [%orca %panic %nack wire ~]
    `this
  ::
      %poke-ack
    ?+    wire  `this
        [%range @ @ @ @ @ ~]
      =/  mines=resource
        [(slav %p +<.wire) (slav %tas +>-.wire)]
      =/  yours=resource
        [(slav %p +>+<.wire) (slav %tas +>+>-.wire)]
      =/  fluke=term  (slav %tas +>+>+<.wire)
      =/  podes=pod   (~(got by fam) mines)
      =/  wired=path
        :~  
          %spake
          (scot %p entity.mines)  (scot %tas name.mines)
          (scot %p entity.yours)  (scot %tas name.yours)
          (scot %tas fluke)
        ==
      =/  pathd=path
        :~  %spake
            (scot %p entity.yours)
            (scot %tas name.yours)
            (scot %tas fluke)
        ==    
      ?>  =(fluke fin.podes)
      :_  this
      :~  :*
        %pass   wired
        %agent  [entity.yours %orca]
        %watch  pathd
      ==  ==
    ::
        [%tempt @ @ @ ~]
      =/  your=ship  (slav %p +<.wire)
      =/  mine=resource
        [(slav %p +>-.wire) (slav %tas +>+<.wire)]
      ?>  (~(has by fam) mine)
      =.  pup
        (~(put ju pup) your (~(got by fam) mine))
      `this
    ==
  ::
      %fact
    ?+    +<.sign
      ~&  >>>  [%wat %mean]  `this
    ::
        %graph-update-3
      =/  act=action  +:!<(update +>.sign)
      ?+    -.act  `this
          %add-nodes
        ~&  >  nodes.act
        =/  mec=(map resource resource)
          (~(rep by fam) dad:fish)
        =/  rus=(unit resource)
          ^-  (unit resource)
          ?.  (~(has in ~(key by fam)) resource.act)
            ?.  (~(has in ~(key by mec)) resource.act)
              ~
            (~(get by mec) resource.act)
          ~&  >  "correct"
          `resource.act
        ?~  rus
          `this
        ?:  (~(has in mem) nodes.act)
          `this(mem (~(del in mem) nodes.act))
        :_  this
        (~(sea mom:fish (~(got by fam) u.rus)) act)
      ==
    ::
        %orca-song
      =/  note=sing  !<(sing +>.sign)
      :: wire=[%spake our-resource their-resource fin]
      ?>  ?=([%spake @ @ @ @ @ ~] wire)
      =/  mines=resource
        [(slav %p +<.wire) (slav %tas +>-.wire)]
      =/  yours=resource
        [(slav %p +>+<.wire) (slav %tas +>+>-.wire)]
      =/  fluke=term  (slav %tas +>+>+<.wire)
      =/  podes=(unit pod)   (~(get by fam) mines)
      ?~  podes
        :_  this
        [%pass wire %agent [src.bowl %orca] %leave ~]~
      ?.  =(fluke fin.u.podes)
        :_  this
        [%pass wire %agent [src.bowl %orca] %leave ~]~
      ::
      ?+    -.note  `this
          %biggs
        =/  notes=action:store  [%add-nodes mines seal.note]
        :_  this(mem (~(put in mem) seal.note))
        :~  :*
          %pass   /blip/(scot %tas fluke)
          %agent  [our.bowl %graph-store]
          %poke   %graph-update-3
          !>(`update:store`[now.bowl notes])
        ==  ==
      ::
          %leave
        =.  podes
          podes(who.u (~(del in who.u.podes) res.note))
        =.  fam  (~(put by fam) mines u.podes)
        `this
      ::
          %unite
        =/  brood=(list resource)
          ~(tap in (~(dif in who.u.podes) ser.note))
        =.  podes
          podes(who.u (~(uni in who.u.podes) ser.note))
        =+  cards=*(list card)
        |-
        ?~  brood
          [cards this(fam (~(put by fam) mines u.podes))]
        ?:  =(entity.i.brood our.bowl)
          $(brood t.brood)
        =/  wir=path
          :~  
            %spake
            (scot %p entity.mines)    (scot %tas name.mines)
            (scot %p entity.i.brood)  (scot %tas name.i.brood)
            (scot %tas fluke)
          ==
        =/  pat=path
          :~  %spake
              (scot %p entity.i.brood)
              (scot %tas name.i.brood)
              (scot %tas fluke)
          ==
        =/  haz=(unit [acked=? =path])
            (~(get by wex.bowl) [wir entity.i.brood %orca])
        ?~  haz
          %=    $
              brood
            t.brood
          ::
              cards
            %+  welp  cards
            :~  :*
              %pass   wir
              %agent  [entity.i.brood %orca]
              %watch  pat
            ==  ==
          ==
        ?:  =(u.haz [%.y pat])
          $(brood t.brood)
        %=    $
            brood
          t.brood
        ::
            cards
          %+  welp  cards
          :~  :*
            %pass   wir
            %agent  [entity.i.brood %orca]
            %watch  pat
          ==  ==
        ==
      ==  
    ==
  ==
  --  
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
  ::  [%spake watched-resource fin]
      [%spake @ @ @ ~]
    =/  theme=resource
      [(slav %p +<.path) (slav %tas +>-.path)]
    =/  podes=pod   (~(got by fam) theme)
    =/  fluke=term  (slav %tas +>+<.path)
    ::
    ?.  =(fluke fin.podes)
      (on-watch:def path)
    ::
    ?>  (~(huh mom:fish podes) src.bowl)
    =/  lyric=(list content)
      :~  [%text 'pod-chat click:']
          [%mention src.bowl]
          [%text 'hears this song']
      ==
    =/  poast=maybe-post:store
      :-  %&
      [our.bowl ~[now.bowl] now.bowl lyric ~ ~]
    =/  moist=wave
      %-  ~(put by *wave)
      [~[now.bowl] [poast [%empty ~]]]
    =/  wired=^path
      [%echo (scot %da now.bowl) (scot %tas fin.podes) ~]
    :_  this
    :~  :*
      %give  %fact  ~
      [%orca-song !>(`sing`[%biggs podes moist])]
        ==
        :*
      %give  %fact  ~
      [%orca-song !>(`sing`[%unite who.podes])]
        ==
        :*
      %pass   wired
      %agent  [our.bol %graph-store]
      %poke   %graph-update-3
      !>(`update:store`[now.bol [%add-nodes theme moist]])
    ==  ==
  ==
::
++  on-leave
  |=  pat=path
  ^-  (quip card _this)
  ?+    pat  `this
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
++  on-arvo  on-arvo:def
::
++  on-peek  on-peek:def
::
++  on-fail  on-fail.def
--
::
|_  bol=bowl:gall
::
++  dad
  |=  [inn=[res=resource =pod] out=(map resource resource)]
  ^-  (map resource resource)
  ?.  eco.pod.inn
    out
  =+  rel=~(tap in who.pod.inn)
  |-
  ?~  rel
    out
  $(rel t.rel, out (~(put by out) i.rel res.inn))
++  mom
  |_  p=pod
  ++  aar
    =+  [luq=~(tap in who.p) kig=*(set ship)]
    ^-  (set ship)
    |-
    ?~  luq
      kig
    $(luq t.luq, kig (~(put in kig) entity.i.luq))
  ::
  ::
  ++  wit
    |=  sip=ship
    ^-  (unit resource)
    =+  woh=(malt ~(tap in who.p))
    ?.  (~(has by woh) sip)
      ~
    `[sip (~(got by woh) sip)]
  ::
  ++  huh
    |=  =ship
    ^-  ?
    =/  czk=(list ?)
      %-  ~(rep in who.p)
      |=  [who=resource out=(list ?)]
      [=(entity.who ship) out]
    =+  uni=(find [%.y]~ czk)
    !?=(~ uni)
  ::
  ++  sea
    |=  act=[%add-nodes res=resource nodes=wave]
    ^-  (list card)
    =+  [herd=~(tap in who.p) out=*(list card)]
    ~&  herd
    |-
    ?~  herd
      =+  tun=[%biggs p nodes.act]
      =/  pat=path
        :~  %spake
            (scot %p entity.res.act)
            (scot %tas name.res.act)
            (scot %tas fin.p)
        ==
      %+  welp  out
      [%give %fact [pat]~ [%orca-song !>(`sing`tun)]]~
    =+  upd=[%add-nodes i.herd nodes.act]
    ?.  =(our.bol entity.i.herd)
      $(herd t.herd)
    %=    $
        herd
      t.herd
    ::
        out
      %+  welp  out
      :~  :*
        %pass   /echo/(scot %da now.bol)/(scot %tas fin.p)
        %agent  [our.bol %graph-store]
        %poke   %graph-update-3
        !>(`update:store`[now.bol upd])
      ==  ==
    ==
  --
--